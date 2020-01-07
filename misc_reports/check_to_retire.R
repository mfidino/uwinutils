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


tmp_qry <- "SELECT Detections.*, DetectionSpecies.speciesID,
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

results <- SELECT(tmp_qry)

results <- results[-which(duplicated(results)),]

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

for(i in 1:length(my_groups)){

  with_config(verbose(),{
    httr::VERB(
      verb = "POST",
      url =
        modify_url("https://us-central1-urban-wildlife-app.cloudfunctions.net/updateDetectionValStats",
                   query = "authorization=eyJhbGciOiJSUzI1NiIsImtpZCI6ImNkMjM0OTg4ZTNhYWU2N2FmYmMwMmNiMWM0MTQwYjNjZjk2ODJjYWEiLCJ0eXAiOiJKV1QifQ.eyJpc3MiOiJhY2NvdW50cy5nb29nbGUuY29tIiwiYXpwIjoiODA5MzgxMTY2MzY5LXMxZXEwNTlnZmZmcnJiNWI1YXJydmVlNjY3aGk0YXN1LmFwcHMuZ29vZ2xldXNlcmNvbnRlbnQuY29tIiwiYXVkIjoiODA5MzgxMTY2MzY5LXMxZXEwNTlnZmZmcnJiNWI1YXJydmVlNjY3aGk0YXN1LmFwcHMuZ29vZ2xldXNlcmNvbnRlbnQuY29tIiwic3ViIjoiMTA2MDYzMjY2MjY2NjA1NDI1NTc1IiwiZW1haWwiOiJ1cmJhbi53aWxkbGlmZS5pbnN0aXR1dGVAZ21haWwuY29tIiwiZW1haWxfdmVyaWZpZWQiOnRydWUsImF0X2hhc2giOiJLQjhBM3dCYzlQYlRReTluem16ZHF3IiwibmFtZSI6IlVyYmFuIFdpbGRsaWZlIEluc3RpdHV0ZSIsInBpY3R1cmUiOiJodHRwczovL2xoMy5nb29nbGV1c2VyY29udGVudC5jb20vYS0vQUF1RTdtQUR1dlJ4ODhhN0ZoLVdTeUNENEFJX3VOTGpIZG9UcXZvTTR0OD1zOTYtYyIsImdpdmVuX25hbWUiOiJVcmJhbiBXaWxkbGlmZSIsImZhbWlseV9uYW1lIjoiSW5zdGl0dXRlIiwibG9jYWxlIjoiZW4iLCJpYXQiOjE1Nzg0MDc4NDIsImV4cCI6MTU3ODQxMTQ0MiwianRpIjoiMGI1MmY5NWQwMDVhYzcxYzA2OTU1YWY2ZjBhNWMwNWViNmJjZjM3NiJ9.KryTP0Kqyow2b9CtRpp4PpyyUCVJrrgn-lMwy-w3y3bmp36D59WuhaGpeiiSByI6ngw5_wg1d1Gs2sfl4zyhx533Sq6A8CtsYmWecBMtUcfWN4Cty4IZrJwDhi6AISsDjC4YE1G7QeT0o-TYEgnf5aGCWhXbZkmEzE7wk7viMkRVxbzj8dGfPuWxithi2vk7BV9x_oKuKu07ZliipeIjnQwjPyvkIBIG8nNSRm-kHOIMRDOrR_QABkxeJvnTftvlMIPIQeS_dVeE6YTg5EOKmNJsEMj239n10v88n-Fg73bho1s9Ml_Fa2dTiF7u4J7QhyqfPmoRvuaZV5WeoVf_gg"),
      add_headers(`Content-Type` = "application/json",
                  `cache-control` = "no-cache",
                  `Accept` = "application/json"),
      body= rjson::toJSON(list(photoGroupID = my_groups), indent = 1),
      encode = "json")
  })

}

