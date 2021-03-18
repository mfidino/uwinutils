# The update species function on the UWIN API won't work if the
#  species you are updating to is already present in the record.

# Fixing the human-maintanance

# step 1. Get the double records

library(uwinutils)
connect2db()


ah <- SELECT("SELECT * FROM DetectionSpecies ds WHERE ds.detailID = 77;")


MODIFY("UPDATE DetectionSpecies SET detailID = 77 WHERE detailID = 122;", TRUE)

SELECT("SELECT * FROM SpeciesDetails")

MODIFY("DELETE FROM SpeciesDetails WHERE detailID = 77")
MODIFY("DELETE FROM DetailsLookup WHERE detailID = 77")


hm <- SELECT("SELECT * FROM SpeciesDetails sd INNER JOIN DetailsLookup dl ON dl.detailID = sd.detailID WHERE dl.detailText = 'collar'")



bad_id <- 667
good_id <- 1

tmp_qry <- paste0(
  "SELECT ds.detectionID, COUNT(ds.detectionID) as YO FROM DetectionSpecies ds
WHERE ds.speciesID IN ",
  sql_IN(c(bad_id, good_id), FALSE),
  " GROUP BY ds.detectionID HAVING YO > 1;"
)

my_data <- SELECT(tmp_qry)

# bring in all the data for these to see if this is what we need

det_query <- paste0(
  "SELECT * FROM DetectionSpecies ds WHERE ds.detectionID IN ",
  sql_IN(my_data$detectionID, FALSE)
)

humans <- SELECT(det_query)

# get the detectionID's that have the badID

to_keep <- humans$detectionID[humans$speciesID == bad_id]

humans <- humans[which(humans$detectionID %in% to_keep),]

SELECT("SELECT * FROM Species WHERE Species.speciesID = 1381")

SELECT("SELECT * FROM SpeciesDetails sd INNER JOIN DetailsLookup dl ON dl.detailID = sd.detailID WHERE sd.speciesID = 1")

# make the human records include the appropriate species detail
# mower = 115

# drop any detections that already have the correct detailID
to_go <- humans$detectionID[which(humans$speciesID==1 & humans$detailID == 115)]
humans <- humans[-which(humans$detectionID %in% to_go),]

unq_dets <- unique(humans$detectionID)



# add mower for now
tmp_mod <- paste0(
  "UPDATE DetectionSpecies\n",
  "SET detailID = 115\n",
  "WHERE detectionID IN ",
  sql_IN(unq_dets, FALSE),
  "\nAND speciesID = 1 AND detailID = 1;"
)

MODIFY(tmp_mod, TRUE)

rm(tmp_mod)

# drop the ones where we have a speciesID and detailID that is
#  correct

good <- humans[which(humans$speciesID ==1 & humans$detailID == 115),]

begone <- unique(good$detectionID)

# delete the records here with bad_id
tmp_mod <- paste0(
  "DELETE FROM DetectionSpecies\n",
  "WHERE detectionID IN ",
  sql_IN(begone, FALSE),
  "\n AND speciesID = ", bad_id,";"
)


for(i in 2242:length(begone)){
  tmp_mod <- paste0(
    "DELETE FROM DetectionSpecies\n",
    "WHERE detectionID IN ",
    sql_IN(begone[i], FALSE),
    "\n AND speciesID = ", bad_id,";"
  )
  MODIFY(tmp_mod, TRUE)
}

MODIFY(tmp_mod, TRUE)

# Now change badID to goodID, plus add the appropriate detailID
hmns <- SELECT(
  paste0(
    "SELECT * FROM DetectionSpecies ds\n",
    "WHERE ds.speciesID = ", bad_id
  )
)

hm2 <- SELECT(
  paste0(
    "SELECT * FROM DetectionSpecies ds\n",
    "WHERE ds.detectionID IN ",
    sql_IN(hmns$detectionID, FALSE)
  )
)

# drop all records that are correct now
d2g <- hm2$detectionID[which(hm2$speciesID ==1 & hm2$detailID == 115)]
hm2 <- hm2[-which(hm2$detectionID %in% d2g),]


tmp_mod <- paste0(
  "UPDATE DetectionSpecies\n",
  "SET speciesID = 1, detailID = 114\n",
  "WHERE speciesID = 1381"
)

MODIFY(tmp_mod, TRUE)

hm <- SELECT("SELECT * FROM DetectionSpecies ds WHERE ds.detectionID = 2632417")

MODIFY("DELETE FROM DetectionSpecies WHERE detectionID = 2632417")

hm2 <- SELECT(
  paste0(
  "SELECT * FROM DetectionSpecies ds WHERE ds.detectionID IN ",
  sql_IN(unique(hm$detectionID, FALSE))
  )
)


2632417
hm2 <- hm2[-which(hm2$detectionID == 2632417),]
u_di <- unique(hm2)
for(i in 1:lent)
tmp_del <- paste0(
  "DELETE FROM DetectionSpecies\n",
  "WHERE SpeciesID = 483\n",
  "AND DetectionID IN ",
  sql_IN(unique(hm2$detectionID, FALSE))
)

MODIFY(tmp_del, TRUE)

SELECT("SELECT * FROM Dete")

connect2db()
sp <- SELECT("SELECT * FROM Species")

sp <- sp[order(sp$genus, sp$species),]

hey <- which(duplicated( paste(sp$genus, sp$species)))

sp[hey,]

