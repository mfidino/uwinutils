library(uwinutils)


#### This is a useful script if you need to just pull all the detection data
# from a specific city

connect2db()

my_cities <- c("LBCA")


SELECT("SELECT * FROM CameraLocations LIMIT 1")

q1  <- paste0(
  "SELECT DISTINCT tmp.areaAbbr, tmp.locationAbbr, tmp.utmNorth, tmp.utmEast, tmp.utmZone, tmp.visitID, tmp.visitDatetime, tmp.photoDateTime, tmp.timeZone, de.photoName, sp.commonName AS species, ds.numIndividuals, de.valStatID\n",
  "FROM Detections de\n",
  "INNER JOIN (SELECT ph.photoName, ph.photoDateTime, pg.photoGroupID, sa.areaAbbr, cl.locationAbbr, cl.utmNorth, cl.utmEast, cl.utmZone, cl.defaultTimeZone AS timeZone, vi.visitID, vi.visitDateTime, apg.userID FROM Photos ph\n",
  "INNER JOIN Visits vi ON vi.visitID = ph.visitID\n",
  "INNER JOIN CameraLocations cl ON cl.locationID = vi.locationID\n",
  "INNER JOIN StudyAreas sa ON sa.areaID = cl.areaID\n",
  "INNER JOIN PhotoGroup pg ON pg.photoGroupID = ph.photoGroupID\n",
  "INNER JOIN AssignedPhotoGroup apg ON apg.photoGroupID = pg.photoGroupID\n",
  "WHERE sa.areaAbbr IN ", sql_IN(my_cities)," \n",
  "AND apg.completed = 1) AS tmp ON tmp.photoName = de.photoName AND tmp.userID = de.userID\n",
  "INNER JOIN DetectionSpecies ds ON ds.detectionID = de.detectionID\n",
  "INNER JOIN Species sp ON sp.speciesID = ds.speciesID\n",
  "WHERE de.valStatID != 3;"
)

test <- SELECT(q1)


test$visitDateTime <- lubridate::with_tz(
  test$visitDateTime,
  "UTC"
)

test$photoDateTime <- lubridate::with_tz(
  test$photoDateTime,
  "UTC"
)

# now convert them to the correct time zone
the_zone <- unique(test$timeZone)

if(length(the_zone)>1){
  stop("Multiple time zones.")
}

test$visitDateTime <- lubridate::with_tz(
  test$visitDateTime,
  the_zone
)

test$photoDateTime <- lubridate::with_tz(
  test$photoDateTime,
  the_zone
)

test <- test[order(
  test$areaAbbr, test$locationAbbr, test$visitDateTime, test$photoDateTime
),
]

column_info <- data.frame(
  column = colnames(test),
  type = c("character", "character", "integer", "integer", "character",
           "integer", "datetime", "datetime", "character", "character", "character",
           "integer", "integer"),
  description = c(
    "Unique abbrevitation for a city",
    "Unique abbrevitation for a site within a city",
    "Northing UTM coordinates. Every once in a while a city puts latitude here instead.",
    "Easting UTM coordinates. Every once in a while a city puts longitude here instead.",
    "The utm zone for the site. A common mistake here is people forget to input the full UTM zone.",
    "Unique number for a visit to a site",
    "The date a visit occurred in the time zone of the timeZone column",
    "The date a photo happened in the time zone of the timeZone column",
    "The time zone for this image",
    "Unique identifier for each image. If there are multiple species in one image this ID will be duplicated across all species records for the image (i.e., rows of the csv)",
    "A species record tied to the image.",
    "The number of individuals of species in the photo",
    "validation status. 1 = pending complete. Only one user has seen the image. 2 = full complete. Multiple users have seen the image and there is agreement on what is in the image."
  )
)


write.csv(test, "lbca_records.csv", row.names = FALSE)
write.csv(column_info, "lbca_records_metadata.csv", row.names = FALSE)
# convert datetime
