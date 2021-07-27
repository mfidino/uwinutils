#######################################
#
# Pull data for Asia Murphy
#
#
#######################################

library(uwinutils)

connect2db()

cities <- c("CHIL", "BOMA", "WIDE", "OACA")

# pull visit data for each city, plus lure info

SELECT("SELECT * FROM VisitLures LIMIT 1")

visis <- SELECT(
  paste0(
    "SELECT DISTINCT sa.areaName, cl.locationAbbr, cl.utmEast, cl.utmNorth, cl.utmZone,\n",
    "cl.defaultTimeZone, vi.activeStart, vi.activeEnd, llu.lureName, vi.visitID FROM Visits vi\n",
    "INNER JOIN CameraLocations cl ON cl.locationID = vi.locationID\n",
    "INNER JOIN VisitLures vl ON vl.visitID = vi.visitID\n",
    "INNER JOIN LureLookup llu ON llu.lureID = vl.lureID\n",
    "INNER JOIN StudyAreas sa ON sa.areaID = cl.areaID\n",
    "INNER JOIN Photos ph ON ph.visitID = vi.visitID\n",
    "INNER JOIN Detections de ON de.photoName = ph.photoName\n",
    "WHERE vi.visitTypeID != 3\n",
    "AND sa.areaAbbr IN ", sql_IN(cities),";"
  )
)

visis <- visis[order(visis$areaName, visis$activeStart),]

# pull in bobcat and coyote data

sp <- c("Coyote", "Bobcat")

my_areas <- SELECT(
  paste0("SELECT areaID FROM StudyAreas sa WHERE
  sa.areaAbbr IN ", sql_IN(cities))
))
my_areas <- unlist(my_areas)

photos <- SELECT(
  paste0(
    "SELECT DISTINCT sp.commonName, ph.photoDateTime, cl.locationAbbr, vi.visitID, de.valStatID FROM Photos ph\n",
    "INNER JOIN Detections de ON ph.photoName = de.photoName\n",
    "AND ph.areaID IN", sql_IN(my_areas, FALSE),"\n",
    "AND de.valStatID IN (1,2)\n",
    "INNER JOIN DetectionSpecies ds ON de.detectionID = ds.detectionID\n",
    "INNER JOIN Visits vi ON vi.visitID = ph.visitID\n",
    "INNER JOIN CameraLocations cl ON cl.locationID = vi.locationID\n",
    "INNER JOIN Species sp ON sp.speciesID = ds.speciesID\n",
    "WHERE sp.commonName IN ", sql_IN(sp),";"
  )
)

write.csv(visis, "UWIN_deployment_data.csv", row.names = FALSE)

write.csv(photos, "UWIN_photo_data.csv", row.names = FALSE)

