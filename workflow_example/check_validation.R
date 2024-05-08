###########################
#
# Script to check on validation status of images
#
# Written by M. Fidino
#
###########################

library(uwinutils)
library(dplyr)
library(httr)
library(rjson)

connect2db()

my_key <- "eyJhbGciOiJSUzI1NiIsImtpZCI6ImM2MjYzZDA5NzQ1YjUwMzJlNTdmYTZlMWQwNDFiNzdhNTQwNjZkYmQiLCJ0eXAiOiJKV1QifQ.eyJpc3MiOiJodHRwczovL2FjY291bnRzLmdvb2dsZS5jb20iLCJhenAiOiI4MDkzODExNjYzNjktczFlcTA1OWdmZmZycmI1YjVhcnJ2ZWU2NjdoaTRhc3UuYXBwcy5nb29nbGV1c2VyY29udGVudC5jb20iLCJhdWQiOiI4MDkzODExNjYzNjktczFlcTA1OWdmZmZycmI1YjVhcnJ2ZWU2NjdoaTRhc3UuYXBwcy5nb29nbGV1c2VyY29udGVudC5jb20iLCJzdWIiOiIxMTMyNTY1ODI1MjUwODYyNTg0NjUiLCJlbWFpbCI6Im1hc29uZmlkaW5vQGdtYWlsLmNvbSIsImVtYWlsX3ZlcmlmaWVkIjp0cnVlLCJhdF9oYXNoIjoiXzVZeEhKMWFCWUtrdnlHcjByaDZFZyIsIm5hbWUiOiJNYXNvbiBGaWRpbm8iLCJwaWN0dXJlIjoiaHR0cHM6Ly9saDMuZ29vZ2xldXNlcmNvbnRlbnQuY29tL2EvQUNnOG9jSlRueUFHMk5penE5UlQtVERfaVpsdV83RWh1dkFpZ3ZJbndnWURQMV9IZW9zPXM5Ni1jIiwiZ2l2ZW5fbmFtZSI6Ik1hc29uIiwiZmFtaWx5X25hbWUiOiJGaWRpbm8iLCJsb2NhbGUiOiJlbiIsImlhdCI6MTY5NzE0MDE5NCwiZXhwIjoxNjk3MTQzNzk0fQ.XDmNbn85s7tQRBDO2v2ll1u4mja9q2OxdZ9qX8Xx8QsUZBqFRmeZgWvNXPCVix4bDULjkkASg9kMNdK1iQCb1RfQWnobRGXj76cJ6z9GLH-iCfD2s6fkdmLxlipVyCJUrdW1HkJppDrdg_JCcFC_vnENNs0WYNPqF-b-7urhlg1UQiocit3bJ4lbRaY9LOobaUugA5bUEH4BMTmSZYDEb93HWDUAt5HWPgC7bbIhZwtXo-w0SXrSCoC_Z__18uLAhTG1dcALITU9ogDrflaqqQSpizmOuRJ5X9hsASEP9kEnjC8eQ_YcHIKUT3gFUmEKivZFxNnpPwRr5XCBvkttvg"

# Step 1. Get the names of each city that has detection data
qry <- "
SELECT DISTINCT sa.areaAbbr FROM StudyAreas sa
INNER JOIN Locations cl on cl.areaID = sa.areaID
INNER JOIN Visits vi ON vi.locationID = cl.locationID
INNER JOIN Photos ph ON ph.visitID = vi.visitID
INNER JOIN Detections de ON de.photoName = ph.photoName"
cities <- SELECT(qry)

# For each of these we are going to check and see if there
#  are images that need to get validated.

pid_list <- vector(
  "list",
  length = nrow(cities)
)
photo_count <- rep(
  0,
  length(pid_list)
)
names(photo_count) <- cities$areaAbbr
for(i in 1:length(pid_list)){
  print(cities$areaAbbr[i])
q1  <- paste0(
  "SELECT de.*, ds.speciesID, ds.detailID, ds.numIndividuals, tmp.photoGroupID, tmp.completed, tmp.numViewers\n",
  "FROM Detections de\n",
  "STRAIGHT_JOIN (SELECT ph.photoName, pg.photoGroupID, apg.completed, apg.userID, sa.numViewers FROM Photos ph\n",
  "INNER JOIN Visits vi ON vi.visitID = ph.visitID\n",
 "INNER JOIN Locations cl ON cl.locationID = vi.locationID\n",
 "INNER JOIN StudyAreas sa ON sa.areaID = cl.areaID\n",
 "INNER JOIN PhotoGroup pg ON pg.photoGroupID = ph.photoGroupID\n",
 "INNER JOIN AssignedPhotoGroup apg ON apg.photoGroupID = pg.photoGroupID\n",
 "WHERE sa.AreaAbbr = '", cities$areaAbbr[i],"'\n",
 "AND apg.completed = 1) AS tmp ON tmp.photoName = de.photoName AND tmp.userID = de.userID\n",
 "INNER JOIN DetectionSpecies ds ON ds.detectionID = de.detectionID\n",
 "WHERE de.valStatID = 1\n",
 "AND tmp.completed = 1;"
)

# fixes times when there are no records
results <- try(
  SELECT(q1),
  silent = TRUE
)

if(class(results) == 'try-error'){
  next
}

# get only tags that have photos with > 2 users
photos_two_taggers <- results %>%
  dplyr::group_by(photoName) %>%
  dplyr::summarise(users_tagged = dplyr::n_distinct(userID),
                   numViewers = unique(numViewers)) %>%
  dplyr::filter(users_tagged >= numViewers)

if(nrow(photos_two_taggers) == 0){
  next
}

# paste together all the user inputs
tags <- results[results$photoName %in% photos_two_taggers$photoName,] %>%
  dplyr::arrange(
    photoName, userID, speciesID
  ) %>%
  dplyr::mutate(
    full_tag = trimws(
      paste0(
        speciesID,"-",detailID,"-",numIndividuals
      )
    )
  )

# figure out how many tags were made by each user
tags_collapse <- tags %>%
  dplyr::group_by(
    photoName, userID
  ) %>%
  dplyr::summarise(
    full_tag = paste0(
      full_tag, collapse = "-"
    )
  )

# if tags are identical then unq_tags = 1, else not equal
tag_summary <- tags_collapse %>%
  dplyr::group_by(photoName) %>%
  dplyr::summarise(unq_tags = dplyr::n_distinct(full_tag))

# to correct
photos_to_correct <- tag_summary[tag_summary$unq_tags == 1,]

# get the unique photoGroupID's to send through update detection val
# stats

pid_list[[i]] <- unique(
  results$photoGroupID[which(results$photoName %in% photos_to_correct$photoName)]
)
photo_count[i] <- dplyr::n_distinct(photos_to_correct$photoName)

}

# combine all of them
pid_vec <-
  unlist(
    pid_list
  )



for(i in 1:length(pid_vec)){

  cat("\n",i,"of", length(pid_vec),"\n")

tmp_response <- httr::with_config(httr::verbose(),{
  httr::VERB(
    verb = "POST",
    url =
      modify_url("https://us-central1-urban-wildlife-app.cloudfunctions.net/updateDetectionValStats",
                 query = paste0("authorization=Bearer%20",my_key)),
    add_headers(`Content-Type` = "application/json",
                `Cache-Control` = "no-cache",
                `Accept` = "application/json"),
    body= rjson::toJSON(list(photoGroupID = pid_vec[i]), indent = 1),
    encode = "json")

})

if(tmp_response$status_code != 200){
  httr::with_config(httr::verbose(),{
    httr::VERB(
      verb = "POST",
      url =
        modify_url("https://us-central1-urban-wildlife-app.cloudfunctions.net/updateDetectionValStats",
                   query = paste0("authorization=Bearer%20",my_key)),
      add_headers(`Content-Type` = "application/json",
                  `cache-control` = "no-cache",
                  `Accept` = "application/json"),
      body= rjson::toJSON(list(photoGroupID = pid_vec[i]), indent = 1),
      encode = "json")
  })
}
}


