library(uwinutils)

connect2db()

city <- "CHIL"
myear <- 2021


tmp_qry <- "
SELECT DISTINCT sa.areaName, lol.landOwnerName, cl.fullName, sp.commonName, year(vi.visitDateTime) as 'year', ph.photoName, ph.photoDateTime
FROM Detections de
INNER JOIN DetectionSpecies ds ON de.detectionID = ds.detectionID
INNER JOIN Photos ph ON ph.photoName = de.photoName
INNER JOIN Visits vi ON vi.visitID = ph.visitID
INNER JOIN CameraLocations cl ON cl.locationID = vi.locationID
INNER JOIN StudyAreas sa ON sa.areaID = cl.areaID
INNER JOIN LandOwnerLookup lol ON lol.landOwnerID = cl.landOwnerID
INNER JOIN Species sp ON sp.speciesID = ds.speciesID
WHERE year(vi.visitDateTime) IN (2018, 2019, 2020, 2021)
AND de.valStatID IN (1,2)
AND sa.areaAbbr = 'NACA'
ORDER BY sa.areaName, lol.landOwnerName, year, cl.fullName, sp.commonName
"

SELECT("SELECT * FROM LandOwnerLookup LIMIT 1")

ch_dat <- SELECT(tmp_qry)

tmp <- lubridate::ymd_hms(ch_dat$photoDateTime)
tmp <- lubridate::with_tz(tmp, tz = "US/Eastern")
ch_dat$photoDateTime <- as.character(tmp)
# for 2020
ch_2020 <- ch_dat[ch_dat$year == 2021,]

ch_2020 <- ch_2020[-which(ch_2020$landOwnerName %in% c("Lake County", "Dupage County")),]

# sort by year, landownder, site name, then common name of species

ch_2020 <- ch_2020[order(ch_2020$landOwnerName, ch_2020$fullName, ch_2020$commonName),]

ch_long <- ch_dat[which(ch_dat$landOwnerName %in% c("Lake County", "Dupage County")),]


unique(ch_2020$commonName)

to_go <- c("Empty", "Unknown", "B&W squirrel", "Squirrel (cannot ID)",
           "Lawn mower", "Golf cart ", "Mouse", "Bird", "Human")
ch_2020 <- ch_2020[-which(ch_2020$commonName %in% to_go),]

library(dplyr)

ch_2020 <- ch_2020 %>%
  group_by(areaName, landOwnerName,fullName,commonName,year) %>%
  summarise(nPhotos = length(photoName))


write.csv(ch_2020, "Chicago_2021_only.csv", row.names = FALSE)



unique(ch_long$commonName)

to_go <- c("Empty", "Unknown", "B&W squirrel", "Squirrel (cannot ID)",
           "Lawn mower", "Golf cart", "Mouse", "Rat spp.", "American robin",
           "Mallard Duck", "Project staff (human)", "Snake", "Bird")

ch_long <- ch_long[-which(ch_long$commonName %in% to_go),]

ch_long <- ch_long[order(ch_long$landOwnerName, ch_long$areaName, ch_long$fullName, ch_long$commonName),]

write.csv(ch_long, "dupage_lake.csv", row.names = FALSE)


ch_dat <- ch_dat[order(ch_dat$landOwnerName, ch_dat$fullName, ch_dat$fullName),]


write.csv(ch_dat, "NACA_all_data_time.csv", row.names = FALSE)
SELECT("SELECT * FROM LandOwnerLookup LIMIT 1")

SELECT("SELECT * FROM INFORMATION_SCHEMA.TABLES
WHERE Lower(TABLE_NAME) LIKE Lower('%land%')")

q1  <- paste0(
  "SELECT de.*, ds.speciesID, ds.detailID, ds.numIndividuals, tmp.photoGroupID, tmp.completed, tmp.numViewers\n",
  "FROM Detections de\n",
  "INNER JOIN (SELECT ph.photoName, pg.photoGroupID, apg.completed, apg.userID, sa.numViewers FROM Photos ph\n",
  "INNER JOIN Visits vi ON vi.visitID = ph.visitID\n",
  "INNER JOIN CameraLocations cl ON cl.locationID = vi.locationID\n",
  "INNER JOIN StudyAreas sa ON sa.areaID = cl.areaID\n",
  "INNER JOIN PhotoGroup pg ON pg.photoGroupID = ph.photoGroupID\n",
  "INNER JOIN AssignedPhotoGroup apg ON apg.photoGroupID = pg.photoGroupID\n",
  "WHERE sa.AreaAbbr = '", city,"'\n",
  "AND apg.completed = 1) AS tmp ON tmp.photoName = de.photoName AND tmp.userID = de.userID\n",
  "INNER JOIN DetectionSpecies ds ON ds.detectionID = de.detectionID\n",
  "WHERE de.valStatID = 1\n",
  "AND tmp.completed = 1;"
)
