#################################
#
# Auto-generate occupancy reports
#
# Written by: M. Fidino
#
################################# start on line 490

library(uwinutils)
library(lubridate)
library(dplyr)
connect2db()

# little function we need for this:
data_source <- function(x){
  # We first want to determine if it's cloud data
  #  this shoud have 1) one file and 2) should start with
  #  "Seasons Legend:,Start Date,End Date"

  # first rule, determine number of files in folder
  files <- list.files(
    x,
    full.names = TRUE
  )

  nfile <- length(
    files
  )

  if(nfile == 1){
    # then check to see if the first line is
    firstline <-
      readLines(
        files,
        n = 1
      )
    if(firstline == "Seasons Legend:,Start Date,End Date"){
      return("CLOUD")
    }
  }

  # check to see if this was summarized from the uwin access db
  #  we can determine if the folder contains "site_locations.csv" and
  #  "observation_matrix.csv"
  has_site_locations <- grep(
    "site_locations.csv",
    files
  )
  has_observation_mat <- grep(
    "observation_matrix.csv",
    files
  )
  if(
    all(
      c(
        length(has_site_locations) == 1,
        length(has_observation_mat) == 1
      )
    )
  ){
    return("ACCESS")
  }
  # MAKS has some seasons generated differently
  if(
    length(grep("*\\.xls", files)) > 0 &
    !all(
      c(
        length(has_site_locations) == 1,
        length(has_observation_mat) == 1
      )
    ) &
    nfile > 1
  ){
    return("CPW_WAREHOUSE")
  }

  if(
    length(grep("scut|sewa|lbca", x)) == 1 &
    all(
      c(
        length(has_site_locations) == 0,
        length(has_observation_mat) == 0
      )
    ) &
    nfile > 1
  ){
    return("PRECLEANED")
  }

  # if neither of these are true then the data are from some other source
  return("UNKNOWN")
}


# the folder to save all the data in.
data_dest <- paste0(
  "../uwin-dataset/data_", Sys.Date()
)
if(!file.exists(data_dest)){
  dir.create(data_dest)
}
years <- 2016:2021

# The unique season codes I'll need for making
# the correct folders
unq_se <- paste0(
  c("JA", "AP", "JU", "OC"),
  rep(years - 2000, each = 4)
)

# Step 1. Get the names of each city that has detection data
qry <- "
SELECT DISTINCT sa.areaAbbr, sa.areaID FROM StudyAreas sa
INNER JOIN CameraLocations cl on cl.areaID = sa.areaID
INNER JOIN Visits vi ON vi.locationID = cl.locationID
INNER JOIN Photos ph ON ph.visitID = vi.visitID
INNER JOIN Detections de ON de.photoName = ph.photoName
WHERE NOT sa.areaAbbr IN('CHMF', 'FAKE')"
cities <- SELECT(qry)

# Step 2. Get the species we want to query

# this is a curated list to drop out all the birds and
#  the like.
my_species <- read.csv(
  "../uwin-dataset/species_to_pull.csv",
  header = FALSE
)

species <- SELECT(
  paste0(
    "SELECT * FROM Species sp\n",
    "WHERE sp.commonName IN ", sql_IN(my_species[,1]),";"
  )
)
species <- species[order(species$commonName),]

# This starts setting up the sampling windows
seasons <- expand.grid(
  years,
  c(1,4,7,10),
  1
  ) %>% apply(
    .,
    1,
    paste,
    collapse = "-"
  ) %>% ymd %>% sort

buffers <- data.frame(
  lower = seasons - 14,
  upper = ceiling_date(seasons, "month") + 13
)



for(city in 1:nrow(cities)){
  # Pull all the locations, visits, etc.

  vis_loc <- SELECT(
    paste0(
      "SELECT v.visitID, v.activeStart, v.activeEnd, v.firstPhotoDate, v.lastPhotoDate, cl.utmEast, cl.utmNorth, cl.utmZone, cl.locationAbbr, cl.defaultTimeZone FROM Visits v\n",
      "INNER JOIN CameraLocations cl ON cl.locationID = v.locationID\n",
      "AND cl.areaID = ", cities$areaID[city]
    )
  )

  # drop any without photo data
  if(any(is.na(vis_loc$firstPhotoDate))){
    vis_loc <- vis_loc[!is.na(vis_loc$firstPhotoDate),]
  }

  # The date time stuff are all screwy, we need to fix them
  vis_loc$activeStart <- b2utc(vis_loc$activeStart)
  vis_loc$activeEnd <- b2utc(vis_loc$activeEnd)
  vis_loc$firstPhotoDate <- b2utc(vis_loc$firstPhotoDate)
  vis_loc$lastPhotoDate <- b2utc(vis_loc$lastPhotoDate)

  # And now convert them to the appropriate timezone
  vis_loc$activeStart <- with_tz(
    vis_loc$activeStart,
    unique(vis_loc$defaultTimeZone)
  )
  vis_loc$activeEnd <- with_tz(
    vis_loc$activeEnd,
    unique(vis_loc$defaultTimeZone)
  )
  vis_loc$firstPhotoDate <- with_tz(
    vis_loc$firstPhotoDate,
    unique(vis_loc$defaultTimeZone)
  )
  vis_loc$lastPhotoDate<- with_tz(
    vis_loc$lastPhotoDate,
    unique(vis_loc$defaultTimeZone)
  )
  # Pull in all of the detection data
  response <- try(
    SELECT(
      paste0(
        "SELECT DISTINCT s.commonName, v.locationID, v.visitID, c.locationAbbr, p.photoDatetime, d.valStatID FROM Photos p \n",
        "INNER JOIN Visits v ON p.visitID=v.visitID AND p.photoDateTime >= v.activeStart AND p.photoDateTime <= v.activeEnd\n",
        "INNER JOIN CameraLocations c ON (v.locationID=c.locationID)\n",
        "INNER JOIN Detections d ON (p.photoName=d.photoName)",
        "INNER JOIN DetectionSpecies ds ON (d.detectionID=ds.detectionID)\n",
        "INNER JOIN Species s ON (ds.speciesID=s.speciesID)\n",
        "WHERE c.areaID = ", cities$areaID[city],"\n",
        #"AND s.speciesID IN ", sql_IN(species$speciesID, FALSE),
        "\n AND d.valStatID IN (1,2)"
      )
    ),
    silent = TRUE
  )
  # Correct the datetime data
  response$photoDatetime <- b2utc(response$photoDatetime)
  response$photoDatetime <- lubridate::with_tz(
    response$photoDatetime,
    unique(vis_loc$defaultTimeZone)
  )
  response$photoDatetime <- ymd_hms(
    response$photoDatetime,
    tz = unique(vis_loc$defaultTimeZone)
  )
  # get the species from this city
  city_species <- unique(
    response$commonName[
      response$commonName %in% species$commonName
    ]
  ) %>%
    sort
  # also get the utms

 # This list will store the detection matrix
  det_list <- vector("list", length = nrow(buffers))
 for(season in 1:nrow(buffers)){

   # Pull data from a given season
   tmp <- response[
     response$photoDatetime >= ymd_hms(
       paste(
         as.character(buffers$lower[season]), "00:00:00",
         tz = unique(vis_loc$defaultTimeZone)
        )
      ),]
   tmp <- tmp[
     tmp$photoDatetime <= ymd_hms(
       paste(
         as.character(buffers$upper[season]), "23:59:59",
         tz = unique(vis_loc$defaultTimeZone)
       )
     ),
   ]
   # If there is no data, then there is no data!
   if(nrow(tmp) == 0){
     det_list[[season]] <- NA
     next
   }

   # Get min and max date for each visitID
   samps <- tmp %>% group_by(visitID) %>%
     summarise(
       min = as.Date(min(photoDatetime)),
       max = as.Date(max(photoDatetime))
     )
   # reduce tmp down to the species we are interested in
   tmp <- tmp[tmp$commonName %in% species$commonName,]

   # make the detection array. To do so we need to determine
   #  the number of sites and days sampled.
   sites <- sort(
     unique(
       vis_loc$locationAbbr
     )
   )
   # While we are here we may as well grab the UTMs
   site_utms <- suppressWarnings(dplyr::left_join(
     data.frame(locationAbbr = sites),
     vis_loc[,c("locationAbbr", "utmNorth", "utmEast", "utmZone")],
     by = "locationAbbr"
   )) %>%
     distinct
   nsite <- length(
     unique(
       sites
       )
   )
   days <- seq(
     buffers$lower[season],
     buffers$upper[season],
     by = "1 day"
   )
   nday <- length(
     days
   )
   # The detection matrix
   det_mat <- matrix(
     NA,
     ncol = nday,
     nrow = nsite
   )
   # Filling it in with zeroes
   for(i in 1:nrow(samps)){
     # map visitID to site
     which_row <- which(
       sites %in%
       vis_loc$locationAbbr[vis_loc$visitID == samps$visitID[i]]
    )
     # get the appropriate dat
     which_col <- which(
       days %in% seq(samps$min[i], samps$max[i], by = "1 day")
     )
     # add some zeroes in there
     det_mat[which_row, which_col] <- 0
   }
   colnames(det_mat) <- paste0("Day_", 1:nday)
   # Make the data.frame to store all the species detection data
   det_df <- data.frame(
     Species = rep(city_species, each = nrow(det_mat)),
     Season = 1,
     Site = rep(site_utms$locationAbbr, length(city_species)),
     UTMNorth = rep(site_utms$utmNorth, length(city_species)),
     UTMEast  = rep(site_utms$utmEast, length(city_species)),
     UTMZone  = rep(site_utms$utmZone, length(city_species))
   )
   to_add <- ncol(det_df)
   # Combine the detection matrix
   det_df <- cbind(
     det_df,
     det_mat[rep(1:nrow(det_mat), length(city_species)),]
  )
   # And now we go trough and add 1's for when a species was
   #  detected.
   for(i in 1:nrow(tmp)){
     # Which site and species is the record?
     which_row <- which(
       det_df$Species == tmp$commonName[i] &
       det_df$Site == tmp$locationAbbr[i]
     )
     if(length(which_row)>1){
       stop()
     }
     # And what day was it seen?
     which_col <- which(
       days %in% as.Date(tmp$photoDatetime[i])
     ) + to_add
     if(length(which_col)>1){
       stop()
     }
     # With that info we can put a 1 in this matrix
     det_df[which_row, which_col] <- 1
   }
   det_list[[season]] <- det_df
 }
# check to see where we have seasons of data that we just pulled
#   from the data.base.
# null dimensions mean there is no data. the opposite means there is.
  has_data <- unq_se[sapply(det_list, function(x) !is.null(dim(x)))]
  if(length(has_data) == 0){
    next
  }
  # order it like it should be
  has_data <- factor(
    unq_se[unq_se %in% has_data],
    levels = unq_se[unq_se %in% has_data]
  ) %>%
    as.character

# The tabulated season folders all start with a leading zero
  season_paths <- list.files(
    paste0(
      "../uwin-dataset/data/",
      tolower(cities$areaAbbr[city])
    ),
    "0[0-9]?[1-9]_",
    full.names = TRUE
  )
# and get just the names in case we need to rename them
  season_folders <- list.files(
    paste0(
      "../uwin-dataset/data/",
      tolower(cities$areaAbbr[city])
    ),
    "0[0-9]?[1-9]_"
  )
# Drop the leading numbers
  if(length(season_folders)>0){
  just_seasons <- sapply(
    strsplit(
      season_folders,
      "_"
    ),
    "[",
    2
  )
# determine data source for each season
  came_from <- sapply(
    season_paths,
    data_source
  )
  names(came_from) <- just_seasons
# Now check to see where the overlap is at.
  has_overlap <- has_data[which(has_data %in% just_seasons)]
# check if any of them are not cloud data, and make
#  the det_list NA if that is the case.
  if(length(has_overlap)>0){
    to_drop <- came_from[
      names(came_from) %in% has_overlap &
        came_from != "CLOUD"]
      if(length(to_drop)>0){
        det_list[which(unq_se %in% names(to_drop))] <- NA
        has_data <- has_data[-which(has_data %in% names(to_drop))]
      }
  }
  } else {
    # make just_seasons NA in the event that there
    #  is no data
  just_seasons <- NA
  came_from <- rep(NA, length(has_data))
  names(came_from) <- has_data
}
  # this specifies the number of seasons of data we have
  #  for a given city
  folders_to_make <- unique(c(just_seasons, has_data))
  # order them appropriately
  folders_to_make <- folders_to_make %>%
    factor(., levels = unq_se) %>%
    sort %>%
    as.character

  # make this a data.frame now.
  folders_to_make <- data.frame(
    season = folders_to_make,
    folder = paste0(
      stringr::str_pad(
        1:length(folders_to_make),
        3,
        pad = "0"
      ),
      "_",
      folders_to_make
    ),
    path = paste0(
      "../uwin-dataset/data/",
      tolower(cities$areaAbbr[city]),"/"
    ),
    stringsAsFactors = FALSE
  )
  # tack on the source of the data.
  folders_to_make <- suppressWarnings(
    dplyr::left_join(
      folders_to_make,
      data.frame(
        season = names(came_from),
        source = unname(came_from),
        stringsAsFactors = FALSE
      ),
      by = "season"
    )
  )
  # # make any NA be CLOUD
  folders_to_make$source[is.na(folders_to_make$source)] <- "CLOUD"

  # and check to see if the number of the folder needs to change
  #  basically we could have a new 1
  if(all(!is.na(just_seasons))){
  folders_to_make <- suppressWarnings(
    dplyr::left_join(
      folders_to_make,
      data.frame(
        season = just_seasons,
        og_folder = season_folders,
        stringsAsFactors = FALSE
      ),
      by = "season"
    )
  )
  }
  # start by moving everything but the cloud data over.
  if(
    !file.exists(
      paste0(data_dest,"/", tolower(cities$areaAbbr[city]))
    )
  ){
    dir.create(
      paste0(data_dest,"/", tolower(cities$areaAbbr[city]))
    )
  }
  # start making the folders now!
  for(i in 1:nrow(folders_to_make)){
    # need to move things differently if the og folders
    #  differ from the new folders
    flag <- folders_to_make$folder[i] != folders_to_make$og_folder[i] &
      !is.na(folders_to_make$og_folder[i])
    if(
      file.exists(
        paste0(
          folders_to_make$path[i],
          ifelse(
            flag,
            folders_to_make$og_folder[i],
            folders_to_make$folder[i]
           )
          )
      ) &
      folders_to_make$source[i] != "CLOUD"
    ){
      dir.create(
        paste0(
          data_dest,
          "/",tolower(cities$areaAbbr[city]),"/",
          folders_to_make$folder[i]
        )
      )
      if(flag){
        # We need to list all the files in that folder
        to_ship <-
          list.files(
            paste0(
              folders_to_make$path[i],
              folders_to_make$og_folder[i]
            ),
            full.names = TRUE
          )
        file.copy(
          to_ship,
          paste0(
            data_dest,
            "/",tolower(cities$areaAbbr[city]),
            "/",folders_to_make$folder[i]
          )
        )
      } else {

        file.copy(
          paste0(
            folders_to_make$path[i],
            folders_to_make$folder[i]
          )
          ,
          paste0(
            data_dest,
            "/",tolower(cities$areaAbbr[city])
          ),
          recursive = TRUE
        )
      }

    }
  }
  #  Deal with cloud data. If we have the data in det_list
  #  it needs to be formatted and saved in the correct way. If we
  #  don't (for some reason) we just copy over the old data.
  for(i in 1:nrow(folders_to_make)){
    if(folders_to_make$source[i] != "CLOUD"){
      next
    }
    # locate where in det_list the season of data lives.
    my_season <- which(unq_se == folders_to_make$season[i])
    # modify the dates for the sampling window.
    tmp <- format(
      c(
        buffers$lower[my_season],
        buffers$upper[my_season]
        ),
        format = "%m/%d/%y"
      )
    # drop leading zero from months
    tmp <- gsub("^0", "", tmp)
    tmp <- gsub("/0", "/", tmp)

    # make the first two rows of the file
    first_two <- matrix(
      c("Seasons Legend:", "Start Date", "End Date",
        "Season 1", tmp),
      ncol = 3, nrow = 2, byrow = TRUE
    )
    fpath <-paste0(
      data_dest,
      "/",tolower(cities$areaAbbr[city]),"/",
      folders_to_make$folder[i]
    )
    fname <- paste0(
      "/",
      tolower(cities$areaAbbr[city]),
      "_",
      tolower(folders_to_make$season[i]),
      ".csv"
    )
    if(!file.exists(fpath)){
      dir.create(fpath)
    }
    # write the first three rows of data.
    write.table(
      first_two,
      paste0(fpath,fname),
      quote = FALSE,
      row.names = FALSE,
      col.names = FALSE,
      sep = ","
    )
    # add a blank line
     write.table(
       "\n",
       paste0(fpath, fname),
       quote = FALSE,
       row.names = FALSE,
       col.names = FALSE,
       append = TRUE,
       eol = ""
     )
    # and then the detection data. Supress warnings
    # because it justs tells us it's appending column names
    # to the file, which is what we want.
     suppressWarnings(
  write.table(
    det_list[[my_season]],
    paste0(fpath, fname),
    quote = FALSE,
    row.names = FALSE,
    col.names = TRUE,
    append = TRUE,
    sep = ","
  )
     )

  }

}
# check to see if there are any cities with data that were not
#  brought over.

# get folder names in uwin-dataset
fnames_uwds <- list.files(
  "../uwin-dataset/data/",
  "^\\w\\w\\w\\w$"
)

# and the folder names in new dataset
fnames_new <- list.files(
  data_dest,
  "^\\w\\w\\w\\w$"
)
to_add <- fnames_uwds[which(!fnames_uwds %in% fnames_new)]

if(length(to_add) > 0){
  for(i in 1:length(to_add)){
    dir.create(
      paste0(data_dest,"/", to_add[i])
    )
    # check how many folders we need to recreate
    to_make <- list.files(
      paste0("../uwin-dataset/data/",to_add[i]),
      "^0"
    )
    for(j in 1:length(to_make)){
      dir.create(
        paste0(data_dest,"/",to_add[i],"/",to_make[j])
      )
      file.copy(
        paste0("../uwin-dataset/data/",to_add[i],"/",to_make[j]),
        paste0(data_dest,"/",to_add[i]),
        recursive = TRUE
      )
    }
  }

}

