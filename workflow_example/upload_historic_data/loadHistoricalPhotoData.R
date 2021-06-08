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


my_path <- "../uwin-dataset/data/cnil/to_upload/"
areaabbr <- "URIL"

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
files$Visits.csv$newID <- visits_map
# update the datetime stuff
files$Photos.csv$ImageDate <- as.character(
  mdy_hms(files$Photos.csv$ImageDate)
)
pb <- txtProgressBar(max = nrow(files$Photos.csv))


for(i in 649:nrow(files$Photos.csv)){
  setTxtProgressBar(pb, i)
  tmp <- files$Photos.csv[i,]
  fullpath <- paste0(tmp$FilePath, tmp$FileName)
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
    if(grep("Duplicate", response[1]) == 1){
      cat("\nduplicate image", i,"\n\n")
      next
    }
    stop("investigate issue")
  }
}
