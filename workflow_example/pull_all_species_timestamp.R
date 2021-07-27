
library(uwinutils)
library(lubridate)
library(lutz) # lookup timezone!

connect2db()

tmp_qry <- "
SELECT DISTINCT cl.locationAbbr, vi.activeStart, vi.activeEnd, cl.utmEast, cl.utmNorth, cl.utmZone, ph.photoDateTime, sa.defaultTimeZone, ph.photoName, sp.commonName, ds.numIndividuals, de.valStatID FROM Photos ph
INNER JOIN Visits vi ON vi.visitID = ph.visitID
INNER JOIN CameraLocations cl ON cl.locationID = vi.locationID
INNER JOIN StudyAreas sa ON sa.areaID = cl.areaID
INNER JOIN Detections de ON de.photoName = ph.photoName
INNER JOIN DetectionSpecies ds ON ds.detectionID = de.detectionID
INNER JOIN Species sp ON sp.speciesID = ds.speciesID
WHERE sa.areaAbbr = 'OACA'
AND de.valStatID IN (1,2);
"

paca <- SELECT(tmp_qry)

paca$photoDateTime <- lubridate::with_tz(
  paca$photoDateTime,
  "UTC"
)

paca$photoDateTime <- lubridate::with_tz(
  paca$photoDateTime,
  unique(paca$defaultTimeZone)
)

paca <- paca[order(paca$locationAbbr, paca$photoDateTime),]

write.csv(paca, "OACA_TIMESTAMPS.csv", row.names = FALSE)

