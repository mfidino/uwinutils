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

my_key <- "eyJhbGciOiJSUzI1NiIsImtpZCI6IjYzMWZhZTliNTk0MGEyZDFmYmZmYjAwNDAzZDRjZjgwYTIxYmUwNGUiLCJ0eXAiOiJKV1QifQ.eyJpc3MiOiJhY2NvdW50cy5nb29nbGUuY29tIiwiYXpwIjoiODA5MzgxMTY2MzY5LXMxZXEwNTlnZmZmcnJiNWI1YXJydmVlNjY3aGk0YXN1LmFwcHMuZ29vZ2xldXNlcmNvbnRlbnQuY29tIiwiYXVkIjoiODA5MzgxMTY2MzY5LXMxZXEwNTlnZmZmcnJiNWI1YXJydmVlNjY3aGk0YXN1LmFwcHMuZ29vZ2xldXNlcmNvbnRlbnQuY29tIiwic3ViIjoiMTEzMjU2NTgyNTI1MDg2MjU4NDY1IiwiZW1haWwiOiJtYXNvbmZpZGlub0BnbWFpbC5jb20iLCJlbWFpbF92ZXJpZmllZCI6dHJ1ZSwiYXRfaGFzaCI6InE5dFhVQW5oLXR1X0JCNDFQMjdjOFEiLCJuYW1lIjoiTWFzb24gRmlkaW5vIiwicGljdHVyZSI6Imh0dHBzOi8vbGgzLmdvb2dsZXVzZXJjb250ZW50LmNvbS9hL0FJdGJ2bWszZVZhREZMY2pQRnlnR2E4X2Y1V2xrYUhLYkFCVndRZER0U2tHPXM5Ni1jIiwiZ2l2ZW5fbmFtZSI6Ik1hc29uIiwiZmFtaWx5X25hbWUiOiJGaWRpbm8iLCJsb2NhbGUiOiJlbiIsImlhdCI6MTY1NzgzMjM2MSwiZXhwIjoxNjU3ODM1OTYxLCJqdGkiOiIxNDMwY2NhYWVlZmY4YTEzNDA0YzNiMTc3N2EyMWMyMDlmYjM4MzQzIn0.Zd0K4rUTlFRKm6DzdiQtcQGUbjOE5TtyQiOhDIQDEmRzCV7u_-TkgPK4xpj0ZTQYY6AyzQIBtjcEefkBICqME7TrE9v-fogw-80bsjMpRfgtkWeiFf4g9-YFIY4y_cuOpGDQhP3omn7a97QV15GAZBbZW5WYZlTs7ipsGOVe2gzZwZyCy0xgy2h0zLFXWCTLNxv8jgXiEPsrYhPLy7uZcB89hETxHHtbMggqZ8Sy3IgQJwX4sqA00pFYz8h8pyUGS3dzYq5EljUGlVpLcuP5mYHdn6ndSDf9vjyhsg0thIDe3mGV2ytc5z2Z3GYBFB9qnFuA-ZsI3OgU8RKgAz_gog"

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
  "STRAIGHT_JOIN (SELECT ph.photoName, pg.photoGroupID, apg.completed, apg.userID, sa.numViewers FROM Photos ph\n",
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



for(i in 2:length(pid_vec)){

  cat("\n",i,"of", length(pid_vec),"\n")

tmp_response <- httr::with_config(httr::verbose(),{
  httr::VERB(
    verb = "POST",
    url =
      modify_url("https://us-central1-urban-wildlife-app.cloudfunctions.net/updateDetectionValStats",
                 query = paste0("authorization=",my_key)),
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
                   query = paste0("authorization=",my_key)),
      add_headers(`Content-Type` = "application/json",
                  `cache-control` = "no-cache",
                  `Accept` = "application/json"),
      body= rjson::toJSON(list(photoGroupID = pid_vec[i]), indent = 1),
      encode = "json")
  })
}
}


