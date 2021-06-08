
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

hm <- SELECT(
  paste0(
    "SELECT * FROM CameraLocations cl WHERE cl.areaID = ",
    aid$areaID, ";"
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
