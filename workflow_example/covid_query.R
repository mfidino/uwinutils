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

# the folder to save all the data in.
data_dest <- paste0(
  "../uwin-dataset/covid_", Sys.Date()
)
if(!file.exists(data_dest)){
  dir.create(data_dest)
}
years <- 2019:2020

# The unique season codes I'll need for making
# the correct folders
unq_se <- paste0(
  c("AP", "JU"),
  rep(years - 2000, each = 2)
  #c("JA", "AP", "JU", "OC"),
  #rep(years - 2000, each = 4)
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

# reduce cities down

to_keep <- c("CHIL", "ATGA", "ININ", "OACA", "JAMS", "LBCA",
             "PACA", "IOIO", "RONY", "SLMO", "WIDE")

cities <- cities[cities$areaAbbr %in% to_keep,]
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
  c(4,7),
  #c(1,4,7,10),
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


city_data <- vector("list", nrow(cities))
for(city in 7:nrow(cities)){
  # Pull all the locations, visits, etc.
  cat("\n",cities$areaAbbr[city],"\n\n")
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
        "SELECT DISTINCT s.commonName, v.locationID, v.visitID, c.locationAbbr, c.utmEast, c.utmNorth, c.utmZone, p.photoDatetime, d.valStatID, d.userID FROM Photos p \n",
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
  if(cities$areaAbbr[city] == "WIDE"){
    experts <- c(138, 903)
    to_go <- which(!response$userID %in% experts & response$valStatID == 1)
    response <- response[-to_go,]
    response <- response[,-which(colnames(response) == "userID")]
    response <- dplyr::distinct(response)
  } else {
    response <- response[,-which(colnames(response) == "userID")]
    response <- dplyr::distinct(response)
  }
  # Correct the datetime data
  response$photoDatetime <- b2utc(response$photoDatetime)
  response$photoDatetime <- lubridate::with_tz(
    response$photoDatetime,
    unique(vis_loc$defaultTimeZone)
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
   citytime <- as.numeric(
     tmp$photoDatetime - floor_date(tmp$photoDatetime, "1 day"),
     unit = "secs"
   ) / 86400
   cityradians <- citytime * 2 * pi
   tmp$timeRadians <- cityradians
   # convert time back to UTC
   tmp$city <- cities$areaAbbr[city]
   tmp$cityTZ <- tz(tmp$photoDatetime)
   tmp$photoDatetime <- with_tz(tmp$photoDatetime, "UTC")
   tmp$season <- unq_se[season]

   det_list[[season]] <- tmp
 }
# combine data for that city
  if(any(sapply(det_list, class) == "logical")){
    to_go <- which(sapply(det_list, class) == "logical")
    det_list <- det_list[-to_go]
  }
  city_data[[city]] <- dplyr::bind_rows(det_list)

}

all_data <- dplyr::bind_rows(city_data)

write.csv(
  all_data,
  "../uwin-dataset/covid_2021-06-17/all_data.csv",
  row.names = FALSE
)
