#################################################
#
# Check to see which valStatID should be set to 2
#
#################################################

library(dplyr)
library(crayon)
source("connect_to_uwidb.R")

"\nTest: Checking valStatID in Detections table" %>%
  underline %>%
  bold %>%
  white %>%
  cat


query <- "SELECT Detections.*, DetectionSpecies.speciesID,
DetectionSpecies.detailID, DetectionSpecies.numIndividuals,
Photos.photoGroupID, Photos.highlighted, PhotoGroup.completed,
apg.completed AS apg_complete, apg.tagIndex
FROM Detections
INNER JOIN DetectionSpecies
ON DetectionSpecies.detectionID = Detections.detectionID
LEFT JOIN Photos ON Photos.photoName = Detections.photoName
LEFT JOIN PhotoGroup ON PhotoGroup.photoGroupID = Photos.photoGroupID
LEFT JOIN AssignedPhotoGroup apg ON apg.photoGroupID = PhotoGroup.photoGroupID
WHERE Detections.valStatID = 1
AND PhotoGroup.completed = 1;"

qry_send <- dbSendQuery(uwidb, query)
results <- dbFetch(qry_send)
dbClearResult(qry_send)





results <- results[-which(duplicated(results)),]

tmper <- results[,c("valStatID", "photoName", "userID", "speciesID", "detailID", "numIndividuals")]

tmper[duplicated(tmper),]

results[which(results$photoName == "VID573-00010.jpg"),]

orrr <- results

to_go <- results$detectionID[duplicated(tmper)]

# get only tags that have photos with > 2 users
photos_two_taggers <- results %>% group_by(photoName) %>%
  summarise(users_tagged = n_distinct(userID)) %>%
  filter(users_tagged > 1)



# paste together all the user inputs
tags <- results[results$photoName %in% photos_two_taggers$photoName,] %>%
  arrange(photoName, userID, speciesID) %>%
  mutate(full_tag = trimws(paste0(speciesID,"-",detailID,"-",numIndividuals)))

# figure out how many tags were made by each user

tags_collapse <- tags %>% group_by(photoName, userID) %>%
  summarise(full_tag = paste0(full_tag, collapse = "-"))


# if tags are identical then unq_tags = 1, else not equal
tag_summary <- tags_collapse %>% group_by(photoName) %>%
  summarise(unq_tags = n_distinct(full_tag))

my_phooots <- results[which(results$photoName %in% photos_to_correct$photoName),]

my_groups <- sort(unique(my_phooots$photoGroupID))


write.csv(my_groups, "Joes_groupies2.csv",
          row.names = FALSE, col.names = FALSE, quote = FALSE)
# to correct
photos_to_correct <- tag_summary[tag_summary$unq_tags == 1,]
if(nrow(photos_to_correct) > 0){
  "\n\tvalStatID needs to be updated for some images.\n\n" %>%
    bold %>%
    red %>%
    cat
  if(auto_run_update){
    "\t.....UPDATING....." %>%
      bold %>%
      red %>%
      cat

    #photos_to_correct$photoName <- paste0("'",photos_to_correct$photoName,"'")

    updt_qry <- "Update Detections SET valStatID = 2 WHERE Detections.photoName in"
    p_names <- paste0(photos_to_correct$photoName, collapse = ', ')
    updt_qry <- paste(updt_qry, paste0("(",p_names,");"))

    results <- RMariaDB::dbSendStatement(uwidb, updt_qry)
    paste0("\t", dbGetRowsAffected(results), " rows affected") %>%
      bold %>%
      red %>%
      cat
    dbClearResult(results)
  }
} else {
  "\n\tvalStatID up to date. No unnecessary photos sent to Validation flow.\n\n" %>%
    bold %>%
    green %>%
    cat
}

dbDisconnect(uwidb)



updt_qry <- "DELETE FROM DetectionSpecies WHERE DetectionSpecies.detectionID IN"
p_names <- paste0(to_go, collapse = ', ')
updt_qry <- paste(updt_qry, paste0("(",p_names,");"))

MODIFY(updt_qry)

