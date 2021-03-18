##################################
#
# A script to convert unknown tags to the correct species
#
# Written by M. Fidino 2021/03/18 ymd
#
##################################

# The first thing we need to do is check the csv file
#  to ensure that it has correct species names, site
#  names, etc. To do that we need to pull the species
#  data and the like.

tmp <- try(
  SELECT("SELECT * FROM StudyAreas LIMIT 1"),
  silent = TRUE
)
if(class(tmp) == "try-error"){
  connect2db()
}


# Get the species
sp <- SELECT("SELECT * FROM Species")

# also get the camera locations
cl <- SELECT("SELECT * FROM CameraLocations")
# If you know the csv path then put it here, otherwise
#  create this null object and use the next line
#  to manually locate the file. If it is in your current
#  working directory you just need to create a character
#  object of the file name (e.g., "my_file.csv")
csv_path <- NULL

# Open a GUI to select file if you prefer that
if(is.null(csv_path)){
  cat("SELECT csv to apply updates to uwin db\n")
  csv_path <- file.choose()
}

# load in the file
my_data <- read.csv(
  csv_path,
  stringsAsFactors = FALSE
)

# Some QAQC!
#  drop trailing or leading whitespace,
for(i in 1:ncol(my_data)){
  my_data[,i] <- trimws(my_data[,i])
}

 cat("Starting QAQC ON...\n\n", csv_path)

 # check to make sure the column names are correct
 the_columns <- c(
  "locationName","photoName",
  "commonName","numIndividuals",
  "updateCommonName", "updateNumIndividuals",
  "Month"
 )
 if(!all(colnames(my_data) == the_columns)){
   to_report <-
     paste0(
       "\nThe column headers should be labeled this\n",
       "and be in this order:\n\n",
       paste0(the_columns, collapse = ", "),
       "\n\nThey are currently:\n\n",
       paste0(colnames(my_data), collapse = ", ")

     )
 } else {
   rm(the_columns)
 }

 # Check to make sure species names are in the DB
 species_in <- my_data$updateCommonName %in% sp$commonName

 if(all(species_in)){
   cat("\n", "All commonNames in UWIN DB...")
   rm(species_in)
 } else {
   which_err <- unique(my_data$commonName[!species_in])
   which_rows <- which(!species_in)
   to_report <- paste0(
     "\nThere are ", length(which_rows), " records\n",
     "that have incorrect species names.\n",
     "The species names are:\n\n",
     paste0(which_err, collapse = ", "),
     "\nErrors are on these rows:\n\n",
     paste0(which_rows, collapse = ", ")
   )
   rm(which_err)
   rm(which_rows)
   stop(to_report)
 }

 # check to make sure Location name is in as well
 locs_in <- my_data$locationName %in% cl$fullName

 if(all(locs_in)){
   cat("\n", "All locationNames in UWIN DB...")
   rm(locs_in)
 } else {
   which_err <- unique(my_data$locationName[!locs_in])
   which_rows <- which(!locs_in)
   to_report <- paste0(
     "\nThere are ", length(which_rows), " records\n",
     "that have incorrect location names.\n",
     "The location names are:\n\n",
     paste0(which_err, collapse = ", "),
     "\nErrors are on these rows:\n\n",
     paste0(which_rows, collapse = ", ")
   )
   rm(which_err)
   rm(which_rows)
   stop(to_report)
 }

 # And finally photo names. Little trickier as I don't
 #  want to pull all the photodata in so we will query
 #  it instead.

 photos_in <- SELECT(
   paste0(
     "SELECT DISTINCT photoName FROM Photos\n",
     "WHERE Photos.photoName IN ",
     sql_IN(my_data$photoName),";"
   )
 )

 if(nrow(photos_in) == length(unique(my_data$photoName))){
   cat("\n", "All PhotoNames in UWIN DB...")
   rm(photos_in)
 } else {
   which_err <- unique(
     my_data$photoName[
       which(!my_data$photoName %in% photos_in$photoName)
     ]
   )
   which_rows <- which(!my_data$photoName %in% photos_in$photoName)

   to_report <- paste0(
     "\nThere are ", length(which_rows), " records\n",
     "that have incorrect photo names.\n",
     "The photo names are:\n\n",
     paste0(which_err, collapse = ", "),
     "\nErrors are on these rows:\n\n",
     paste0(which_rows, collapse = ", ")
   )
   rm(which_err)
   rm(which_rows)
   stop(to_report)
 }
cat("\nQAQC complete")
cat("\nUpdating DetectionSpecies...\n")
Sys.sleep(2.5)

# get the speciesID's that we need for updating.
spids <- sp[sp$commonName %in% my_data$updateCommonName,]

unknown <- sp$speciesID[sp$commonName=="Unknown"]

tmp_qry <-   paste0(
  "SELECT de.photoName, de.userID, ds.detectionID,\n",
  "ds.speciesID, ds.numIndividuals, de.valStatID FROM Detections de\n",
  "INNER JOIN DetectionSpecies ds ON de.detectionID = ",
  "ds.detectionID AND\n de.photoName IN \n",
  sql_IN(unique(my_data$photoName)), " AND\n",
  "ds.speciesID = ", unknown, " AND\n",
  "de.valStatID IN (1,2);"
)

unk <- SELECT(
tmp_qry
)

to_update <- dplyr::inner_join(
  unk,
  my_data[,c("photoName", "updateCommonName", "updateNumIndividuals")],
  by = "photoName"
)

# Pull all the detections
my_pb <- txtProgressBar(0, max = nrow(to_update))
for(i in 83:nrow(to_update)){
  tr <- to_update[i,]
  new_id <- spids$speciesID[spids$commonName == tr$updateCommonName]
  new_count <- tr$updateNumIndividuals
  upqry <- paste0(
    "UPDATE DetectionSpecies\n",
    "SET speciesID = ", new_id, ",\n",
    "numIndividuals = ", new_count, "\n",
    "WHERE detectionID = ", tr$detectionID,
    "\nAND speciesID = ", unknown,
    "\nAND numIndividuals = ", tr$numIndividuals,";"
  )
  my_att <- try(MODIFY(upqry), silent = TRUE)
  # check if it's a duplicate entry error
  if(class(my_att) == "try-error"){
    if(length(grep("Duplicate entry", my_att[1])) == 1){
      delqry <- paste0(
        "DELETE FROM DetectionSpecies\n",
        "WHERE detectionID = ", tr$detectionID,
        "\nAND speciesID = ", unknown,
        "\nAND numIndividuals = ", tr$numIndividuals,";"
      )
      MODIFY(delqry)
      rm(delqry)
    }
  }
  rm(my_att)
  rm(upqry)
  setTxtProgressBar(my_pb, i)
}




