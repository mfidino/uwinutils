######################################################### CSV COLUMN FORMATTING ASSUMPTIONS #################################################################
#                                                                                                                                                           #
# CameraLocations:      LocationID, StudyAreaID, LocationName,  UTM_E,   UTM_N,      UTMZone,    Landowner Abbrev,   FullLocationName                       #
#                       line[0]     line[1]      line[2]        line[3]  line[4]     line[5]     line[6]             line[7]                                #
# LandOwnerLookup:      ID,         Landowner,  LandownerAbbrev,    StudyAreaID                                                                             #
#                       line[0]     line[1]     line[2]             line[3]                                                                                 #
# Cameras:              ID,         CameraID,   ModelNumber,    Retired,    Mfg                                                                             #
#                       line[0]     line[1]     line[2]         line[3]     line[4]                                                                         #
# Species:              SpeciesID,  CommonName,     Genus,      Species,    ShortName                                                                       #
#                       line[0]     line[1]         line[2]     line[3]     line[4]                                                                         #
# SpeciesDetails:       SpeciesShortName,   DetailText                                                                                                      #
#                       line[0]             line[1]                                                                                                         #
# Users:                ObserverID,     LastName,   FirstName,  Role,       Email,      UserAccountStatus                                                   #
#                       line[0]         line[1]     line[2]     line[3]     line[4]     line[6]     line[7]                                                 #                                                                                 #
############################################################ INSTRUCTIONS TO RUN SCRIPT #####################################################################
#

library(uwinutils)
library(lubridate)

connect2db()


my_path <- "../uwin-api/csv-data/mawi/"
areaabbr <- "MAWI"

file_paths <- list.files(
  my_path,
  full.names = TRUE
)

file_names <- list.files(
  my_path
)
# read in and name the files
files <- lapply(
  file_paths,
  function(x) read.csv(x, stringsAsFactors = FALSE)
)
names(files) <- file_names

# get the study area ID
aid <- SELECT(
  paste0(
    "SELECT * FROM StudyAreas sa WHERE sa.areaAbbr = '",areaabbr,"';"
  )
)
# Map old locationID to new locationID
location_map <- rep(NA, nrow(files$CameraLocations.csv))
for(i in 1:length(location_map)){
  # put together query for a given location
  tmp_qry <- paste0(
    "SELECT locationID FROM CameraLocations cl\n",
    "WHERE cl.locationAbbr = '",
    files$CameraLocations.csv$LocationName[i],"';"
  )
  response <- try(
    SELECT(tmp_qry),
    silent = TRUE
  )
  # Provide locationID so long as we have it in
  #  the database
  if(is.data.frame(response)){
    location_map[i] <- response$locationID
  }
}
if(any(is.na(location_map))){
  stop("You need to upload new locations")
}
files$CameraLocations.csv$newID <- location_map

# and the same with visits
# Map old locationID to new locationID
visits_map <- rep(NA, nrow(files$Visits.csv))
for(i in 1:length(visits_map)){
  # put together query for a given location
  vdt <- paste(
    strsplit(files$Visits.csv$VisitDate[i], " ")[[1]][1],
    strsplit(files$Visits.csv$VisitTime[i], " ")[[1]][2]
  )
  vdt <- as.character(mdy_hms(vdt))
  tmp_qry <- paste0(
    "SELECT visitID FROM Visits vi\n",
    "WHERE vi.locationID = ",
    files$CameraLocations.csv$newID[
      files$CameraLocations.csv$LocationID ==
      files$Visits.csv$LocationID[i]
    ], " AND vi.visitDatetime = '",
    vdt,"' AND vi.visitTypeID = ",
    files$Visits.csv$VisitTypeID[i],";"
  )
  response <- try(
    SELECT(tmp_qry),
    silent = TRUE
  )
  # Provide locationID so long as we have it in
  #  the database
  if(is.data.frame(response)){
    visits_map[i] <- response$visitID
  }
}
if(any(is.na(visits_map))){
  stop("You need to upload new visits")
}
 #files$Visits.csv <- files$Visits.csv[-c(51),]
 #visits_map <- visits_map[-51]
files$Visits.csv$newID <- visits_map
# update the datetime stuff

files$Photos.csv$ImageDate <- as.character(
  mdy_hms(files$Photos.csv$ImageDate)
)


pb <- txtProgressBar(min = 1, max = nrow(files$Photos.csv))
im_up <- rep(NA, nrow(files$Photos.csv))

for(i in 1:nrow(files$Photos.csv)){
  setTxtProgressBar(pb, i)
  tmp <- files$Photos.csv[i,]
  fullpath <- paste0(tmp$FilePath, tmp$FileName)
  if(nchar(fullpath)>200){
    stop("reduce filepath length")
  }
  tmp_qry <- paste0(
    "INSERT INTO Photos (photoName, photoDatetime, filepath,",
    " highlighted, visitID, areaID) VALUES ('",
    tmp$FileName, "', '", tmp$ImageDate , "', '",
    fullpath,"', ", tmp$Highlight, ", ",
    files$Visits$newID[
      files$Visits.csv$VisitID == tmp$VisitID
    ], ", ", aid$areaID,");"
  )
  response <- try(
    MODIFY(tmp_qry),
    silent = TRUE
  )
  if(class(response) == "try-error"){
    if(length(grep("Duplicate", response[1])) == 1){
      cat("\nduplicate image", i,"\n\n")
      im_up[i] <- "duplicate"
      next
    }
    if(length(grep("Incorrect datetime value: 'NA", response[1]))==1){
      cat("\n No datetime. Row", i, "skipped.\n\n")
      im_up[i] <- "no datetime"
      next
    }
    stop("investigate issue")
  }
  im_up[i] <- "uploaded"
}

# drop detections with no datetime
to_go <- which(im_up == "no datetime")
to_go <- files$Photos.csv$ImageID[to_go]


ph <- files$Photos.csv

test <- paste0(ph$FilePath, ph$FileName)

test <- gsub("\\\\\\\\", "//", test)
test <- gsub("\\\\", "/", test)


# check to see if images are in db
tmp_ph <- split(
  files$Photos.csv,
  factor(floor(1:nrow(files$Photos.csv)/ 1000))
)

ph_in <- vector("list", length(tmp_ph))

# fix duplicate photos

for(i in 1:length(ph_in)){
  tqry <- SELECT(
    paste0(
      "SELECT * FROM Photos ph\n",
      "WHERE ph.photoName IN ",
      sql_IN(tmp_ph[[i]]$FileName)
    )
  )
  ph_in[[i]] <- tqry
}
SELECT("SELECT ph.imageID FROM Photos ph Where ph.photoName = '04030001.JPG'")
# pb <- txtProgressBar(max = nrow(files$Photos.csv))
# for(i in 1:nrow(files$Photos.csv)){
#   tmp <- files$Photos.csv[i,]
#   to_up <- paste0(
#     "UPDATE Photos SET photoDateTime = ",
#     shQuote(tmp$ImageDate), " WHERE ",
#     "photoName = ", shQuote(tmp$FileName), ";"
#   )
#   MODIFY(to_up)
#   setTxtProgressBar(pb, i)
#
#
# }

