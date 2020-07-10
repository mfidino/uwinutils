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

# Step 1. Get the names of each city that has detection data
qry <- "
SELECT DISTINCT sa.areaAbbr FROM StudyAreas sa
INNER JOIN CameraLocations cl on cl.areaID = sa.areaID
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
  "INNER JOIN (SELECT ph.photoName, pg.photoGroupID, apg.completed, apg.userID, sa.numViewers FROM Photos ph\n",
  "INNER JOIN Visits vi ON vi.visitID = ph.visitID\n",
 "INNER JOIN CameraLocations cl ON cl.locationID = vi.locationID\n",
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
  dplyr::filter(users_tagged > numViewers)

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
  cat(i,"of", length(pid_vec))
tmp_response <- httr::with_config(httr::verbose(),{
  httr::VERB(
    verb = "POST",
    url =
      modify_url("https://us-central1-urban-wildlife-app.cloudfunctions.net/updateDetectionValStats",
                 query = "authorization=eyJhbGciOiJSUzI1NiIsImtpZCI6IjY1YjNmZWFhZDlkYjBmMzhiMWI0YWI5NDU1M2ZmMTdkZTRkZDRkNDkiLCJ0eXAiOiJKV1QifQ.eyJpc3MiOiJhY2NvdW50cy5nb29nbGUuY29tIiwiYXpwIjoiODA5MzgxMTY2MzY5LXMxZXEwNTlnZmZmcnJiNWI1YXJydmVlNjY3aGk0YXN1LmFwcHMuZ29vZ2xldXNlcmNvbnRlbnQuY29tIiwiYXVkIjoiODA5MzgxMTY2MzY5LXMxZXEwNTlnZmZmcnJiNWI1YXJydmVlNjY3aGk0YXN1LmFwcHMuZ29vZ2xldXNlcmNvbnRlbnQuY29tIiwic3ViIjoiMTA2MDYzMjY2MjY2NjA1NDI1NTc1IiwiZW1haWwiOiJ1cmJhbi53aWxkbGlmZS5pbnN0aXR1dGVAZ21haWwuY29tIiwiZW1haWxfdmVyaWZpZWQiOnRydWUsImF0X2hhc2giOiJqUTNsSUotMmNEX0otRmlyOUs3dzBBIiwibmFtZSI6IlVyYmFuIFdpbGRsaWZlIEluc3RpdHV0ZSIsInBpY3R1cmUiOiJodHRwczovL2xoMy5nb29nbGV1c2VyY29udGVudC5jb20vYS0vQU9oMTRHaTQ1YmpPZDF3TGJDVHJpRlZBSUNyRVplSndjb3d4QlZ6dTUxUT1zOTYtYyIsImdpdmVuX25hbWUiOiJVcmJhbiBXaWxkbGlmZSIsImZhbWlseV9uYW1lIjoiSW5zdGl0dXRlIiwibG9jYWxlIjoiZW4iLCJpYXQiOjE1OTQzMzA4MzgsImV4cCI6MTU5NDMzNDQzOCwianRpIjoiNDZhMGM3NTcxNDExZDRkYTYwMjZiMzZhMzhjNzBkZjU1MWJiM2ZlZSJ9.LDBvJAkmJccMF5zhe1ilhVwBfRAqa1vhPYju4cgNczBq1EDLLyTn_0nza1cKFWoOS5XoapuDLvjQWEgzcueCPrSZyCeXR7SYd1nzC3BOlGbbRhGf480MDik8TwzGWqxf6fQvHf0NTTTcOgYZpoCPU02RxpEc-NE9zQyYKb-X5Dw--EoJ9v0JxFH-l4FG3rvb-PmIixfveH-I7vn3dCFaIm-oHo1NfQHNincauNurSgpkPst8njS1Lyorj5x8L6uEPk5Ov4O6qup4sFyQ9usdm48sQDFrbRm8X4kRNgETSrhLz_u_enzNkYRT5IX5LdOlguWV1_VyjnPul1IX8nhLeg"),
    add_headers(`Content-Type` = "application/json",
                `cache-control` = "no-cache",
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
                   query = "authorization=eyJhbGciOiJSUzI1NiIsImtpZCI6IjY1YjNmZWFhZDlkYjBmMzhiMWI0YWI5NDU1M2ZmMTdkZTRkZDRkNDkiLCJ0eXAiOiJKV1QifQ.eyJpc3MiOiJhY2NvdW50cy5nb29nbGUuY29tIiwiYXpwIjoiODA5MzgxMTY2MzY5LXMxZXEwNTlnZmZmcnJiNWI1YXJydmVlNjY3aGk0YXN1LmFwcHMuZ29vZ2xldXNlcmNvbnRlbnQuY29tIiwiYXVkIjoiODA5MzgxMTY2MzY5LXMxZXEwNTlnZmZmcnJiNWI1YXJydmVlNjY3aGk0YXN1LmFwcHMuZ29vZ2xldXNlcmNvbnRlbnQuY29tIiwic3ViIjoiMTA2MDYzMjY2MjY2NjA1NDI1NTc1IiwiZW1haWwiOiJ1cmJhbi53aWxkbGlmZS5pbnN0aXR1dGVAZ21haWwuY29tIiwiZW1haWxfdmVyaWZpZWQiOnRydWUsImF0X2hhc2giOiJqUTNsSUotMmNEX0otRmlyOUs3dzBBIiwibmFtZSI6IlVyYmFuIFdpbGRsaWZlIEluc3RpdHV0ZSIsInBpY3R1cmUiOiJodHRwczovL2xoMy5nb29nbGV1c2VyY29udGVudC5jb20vYS0vQU9oMTRHaTQ1YmpPZDF3TGJDVHJpRlZBSUNyRVplSndjb3d4QlZ6dTUxUT1zOTYtYyIsImdpdmVuX25hbWUiOiJVcmJhbiBXaWxkbGlmZSIsImZhbWlseV9uYW1lIjoiSW5zdGl0dXRlIiwibG9jYWxlIjoiZW4iLCJpYXQiOjE1OTQzMzA4MzgsImV4cCI6MTU5NDMzNDQzOCwianRpIjoiNDZhMGM3NTcxNDExZDRkYTYwMjZiMzZhMzhjNzBkZjU1MWJiM2ZlZSJ9.LDBvJAkmJccMF5zhe1ilhVwBfRAqa1vhPYju4cgNczBq1EDLLyTn_0nza1cKFWoOS5XoapuDLvjQWEgzcueCPrSZyCeXR7SYd1nzC3BOlGbbRhGf480MDik8TwzGWqxf6fQvHf0NTTTcOgYZpoCPU02RxpEc-NE9zQyYKb-X5Dw--EoJ9v0JxFH-l4FG3rvb-PmIixfveH-I7vn3dCFaIm-oHo1NfQHNincauNurSgpkPst8njS1Lyorj5x8L6uEPk5Ov4O6qup4sFyQ9usdm48sQDFrbRm8X4kRNgETSrhLz_u_enzNkYRT5IX5LdOlguWV1_VyjnPul1IX8nhLeg"),
      add_headers(`Content-Type` = "application/json",
                  `cache-control` = "no-cache",
                  `Accept` = "application/json"),
      body= rjson::toJSON(list(photoGroupID = pid_vec[i]), indent = 1),
      encode = "json")
  })
}
}


