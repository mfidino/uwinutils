library(uwinutils)
library(lubridate)

connect2db()

my_path <- "../uwin-api/csv-data/mawi"
areaabbr <- "MAWI"

file_paths <- list.files(
  my_path,
  "*.csv",
  full.names = TRUE
)

file_names <- list.files(
  my_path,
  "*.csv"
)
# read in and name the files
files <- lapply(
  file_paths,
  function(x) read.csv(x, stringsAsFactors = FALSE)
)
names(files) <- file_names

# get the study area ID
aid <- SELECT(
  paste0(
    "SELECT * FROM StudyAreas sa WHERE sa.areaAbbr = '",areaabbr,"';"
  )
)

# insert landowners
ll_upload <- rep(NA, nrow(files$Landowners.csv))
for(i in 1:nrow(files$Landowners.csv)){
  sql1 <- try(
    SELECT(
      paste0(
        "SELECT landOwnerID, areaID FROM LandOwnerLookup lol WHERE ",
        "lol.landOwnerAbbr = '", files$Landowners.csv$LandownerAbbrev[i],
        "';"
      )
    ),
    silent = TRUE
  )
  # quick check to see if landownerAbbr used by another city
  if(class(sql1) != "try-error"){
    if(sql1$areaID != aid$areaID){
      stop("landOwnerAbbr already used by different area")
    }
  }
  # if we have a try-error then there is no record, time to add
  # it.

  if(class(sql1) == "try-error"){
    response <- try(
      MODIFY(
        paste0(
          "INSERT INTO LandOwnerLookup (",
          "landOwnerName, landOwnerAbbr, areaID) VALUES (",
          shQuote(
            files$Landowners.csv$Landowner[i]
          ), ", ",
          shQuote(
            files$Landowners.csv$LandownerAbbrev[i]
          ), ", ",
          aid$areaID, ");"
        )
      ),
      silent = TRUE
    )
    if(class(response) == "try-error"){
    if(grep("Duplicate", response) == 1){
      cat(response[1])
      ll_upload[i] <- "duplicate"
      next
    }}
    if(class(response) != "try-error"){
      ll_upload[i] <- "uploaded"
    } else if(class(response) == "try-error"){
      ll_upload[i] <- "failure"
    }
  }
}
# check for any failures to upload
if(any(!is.na(ll_upload))){
if(any(ll_upload == "failure")){
  flocs <- which(ll_upload == "failure")
  stop(
    paste0(
      "Some landowners did not upload, rows in file are: ",
      paste(flocs, collapse = ",")
    )
  )
}
}
# now we map the landowners
landowner_map <- rep(NA, nrow(files$Landowners.csv))
for(i in 1:length(landowner_map)){
  response <- try(
    SELECT(
      paste0(
        "SELECT landOwnerID FROM LandOwnerLookup lol WHERE ",
        "lol.landOwnerAbbr = ",
        shQuote(files$Landowners.csv$LandownerAbbrev[i]), ";"
      )
    ),
    silent = TRUE
  )
  if(is.data.frame(response)){
    landowner_map[i] <- response$landOwnerID
  }
}
if(any(is.na(landowner_map))){
  stop("Missing landowner, check landowner_map")
} else {
  files$Landowners.csv$newID = landowner_map
}

# UPLOAD CAMERA LOCATIONS

# Check length of fullName
ncount <- nchar(files$CameraLocations.csv$FullLocationName)
if(any(ncount>40)){
  to_short <- which(ncount > 40)
  files$CameraLocations.csv$FullLocationName[to_short] <-
    trimws( substr(
      files$CameraLocations.csv$FullLocationName[to_short],
      1, 40
    ))
}

cc_upload <- rep(NA, nrow(files$CameraLocations.csv))
for(i in 1:nrow(files$CameraLocations.csv)){
  sql1 <- try(
    SELECT(
      paste0(
        "SELECT locationID, areaID FROM CameraLocations cl",
        " WHERE",
        " cl.locationAbbr = ", shQuote(files$CameraLocations.csv$LocationName[i])
      )
    ),
    silent = TRUE
  )
  # quick check to see if landownerAbbr used by another city
  if(class(sql1) != "try-error"){
    if(sql1$areaID != aid$areaID){
      stop("CameraLocation name already used by different area")
    }
  }
  # if we have a try-error then there is no record, time to add
  # it.
  if(class(sql1) == "try-error"){
    response <- try(
      MODIFY(
        paste0(
          "INSERT INTO CameraLocations (",
          "fullName, utmEast, utmNorth, utmZone, locationAbbr,
           areaID, landOwnerID, defaultTimeZone) VALUES (",
          shQuote(
            files$CameraLocations.csv$FullLocationName[i]
          ), ", ",
          files$CameraLocations.csv$UTM_E[i], ", ",
          files$CameraLocations.csv$UTM_N[i], ", ",
          shQuote(
            files$CameraLocations.csv$UTMZone[i]
          ), ", ",
          shQuote(
            files$CameraLocations.csv$LocationName[i]
          ), ", ",
          aid$areaID, ", ",
          files$Landowners.csv$newID[
            files$Landowners.csv$LandownerAbbrev  == files$CameraLocations.csv$Landowner[i]
          ], ", ",
          shQuote(
            aid$defaultTimeZone
          ), ");"
        )
      ),
      silent = TRUE
    )
    if(class(response) == "try-error"){
    if(length(grep("Duplicate", response)) == 1){
      cat(response[1])
      cc_upload[i] <- "duplicate"
      next
    }
    }
    if(class(response) != "try-error"){
      cc_upload[i] <- "uploaded"
    } else if(class(response) == "try-error"){
      cc_upload[i] <- "failure"
    }
  }else{
    cc_upload[i] <- "duplicate"
  }
}
# check for any failures to upload
if(any(cc_upload == "failure", na.rm = TRUE)){
  flocs <- which(ll_upload == "failure")
  stop(
    paste0(
      "Some cameras did not upload, rows in file are: ",
      paste(flocs, collapse = ",")
    )
  )
}
# now we map lures
lure_map <- rep(NA, nrow(files$Lures.csv))
for(i in 1:length(lure_map)){
  tmp_qry <- paste0(
    "SELECT lureID FROM LureLookup ll\n",
    "WHERE ll.lureName = '",
    files$Lures.csv$Lure[i],"';"
  )
  response <- try(
    SELECT(
      tmp_qry
    ),
    silent = TRUE
  )
  if(is.data.frame(response)){
    lure_map[i] <- response$lureID
  }
}
if(any(is.na(lure_map))){
  stop("Upload missing lures.")
}
files$Lures.csv$newID <- lure_map
landowner_map <- rep(NA, nrow(files$Landowners.csv))
for(i in 1:length(landowner_map)){
  response <- try(
    SELECT(
      paste0(
        "SELECT landOwnerID FROM LandOwnerLookup lol WHERE ",
        "lol.landOwnerAbbr = ",
        shQuote(files$Landowners.csv$LandownerAbbrev[i]), ";"
      )
    ),
    silent = TRUE
  )
  if(is.data.frame(response)){
    landowner_map[i] <- response$landOwnerID
  }
}
if(any(is.na(landowner_map))){
  stop("Missing landowner, check landowner_map")
} else {
  files$Landowners.csv$newID = landowner_map
}

action_map <- rep(NA, nrow(files$Actions.csv))
for(i in 1:length(action_map)){
  response <- try(
    SELECT(
      paste0(
        "SELECT actionID FROM ActionsLookup al WHERE ",
        "al.actionName = ",
        shQuote(files$Actions.csv$Action[i]), ";"
      )
    ),
    silent = TRUE
  )
  if(is.data.frame(response)){
    action_map[i] <- response$actionID
  }
}
if(any(is.na(action_map))){
  stop("Missing action, check action_map")
} else {
  files$Actions.csv$newID = action_map
}

# cameras
camera_map <- rep(NA, nrow(files$Cameras.csv))
for(i in 1:length(camera_map)){
  tmp_qry <- paste0(
    "SELECT cameraID FROM Cameras ca\n",
    "WHERE ca.cameraName = '",
    files$Cameras.csv$CameraID[i],"'",
    " AND ca.areaID = ", aid$areaID,";"
  )
  response <- try(
    SELECT(
      tmp_qry
    ),
    silent = TRUE
  )
  if(is.data.frame(response)){
    camera_map[i] <- response$cameraID
  }
}
if(any(is.na(camera_map))){
  stop("Upload missing camera types")
}
files$Cameras.csv$newID <- camera_map

# map camera types too
ctype_map <- rep(NA, nrow(files$CameraTypes.csv))
for(i in 1:length(ctype_map)){
  tmp_qry <- paste0(
    "SELECT cameraTypeID FROM CameraTypes ca\n",
    "WHERE ca.modelNumber = ",
    shQuote(
      files$CameraTypes.csv$ModelNumber[i]
    ), " AND ca.camType = ",
    shQuote(
      files$CameraTypes.csv$CameraType[i]
    )
    ,";"
  )
  response <- try(
    SELECT(
      tmp_qry
    ),
    silent = TRUE
  )
  if(is.data.frame(response)){
    ctype_map[i] <- response$cameraTypeID
  }
}
if(any(is.na(ctype_map))){
  stop("Upload missing camera types")
}
files$CameraTypes.csv$newID <- ctype_map

# Species map
species_map <- rep(NA, nrow(files$Species.csv))
for(i in 1:length(species_map)){
  tmp_qry <- paste0(
    "SELECT speciesID FROM Species sp WHERE\n",
    "sp.commonName = '", files$Species.csv$CommonName[i],"';"
  )
  response <- try(
    SELECT(tmp_qry),
    silent = TRUE
  )
  if(is.data.frame(response)){
    species_map[i] <- response$speciesID
  }
}
if(any(is.na(species_map))){
  stop("add new species")
} else {
  files$Species.csv$newID <- species_map
}

# Check to make sure species are in the regional study area as well
all_in <- SELECT(
  paste0(
    "select * from SpeciesStudyArea ssa where\n",
    " ssa.speciesID IN ", sql_IN(species_map),
    " AND ssa.areaID = ", aid$areaID
  )
)

if(nrow(all_in) != length(species_map)){
  to_add <- species_map[which(!species_map %in% all_in$speciesID)]
  tmp_qry <- paste0(
    "INSERT INTO SpeciesStudyArea (speciesID, areaID) VALUES ",
    paste0("(", to_add, ",", aid$areaID,")", collapse = ", ")
  )
  MODIFY(tmp_qry, TRUE)
}

# Map species details
details_map <- rep(NA, nrow(files$SpeciesDetails.csv))
for(i in 1:length(details_map)){
  tmp_qry <- paste0(
    "SELECT * FROM DetailsLookup dl WHERE ",
    "dl.DetailText = ",
    shQuote(
      files$SpeciesDetails.csv$DetailText[i]
    )
  )
  response <- try(
    SELECT(tmp_qry),
    silent = TRUE
  )
  if(is.data.frame(response)){
    details_map[i] <- response$detailID
  }
}
if(any(is.na(details_map))){
  stop("Detail missing. Fix this.")
}
files$SpeciesDetails.csv$newID <- details_map

# and now map details to species
sdetails_ll <- rep(NA, nrow(files$SpeciesDetails.csv))

for(i in 1:length(sdetails_ll)){
  tmp_qry <- paste0(
    "SELECT * FROM SpeciesDetails sd WHERE ",
    "sd.speciesID = ",
    files$Species.csv$newID[
      files$Species.csv$SpeciesID == files$SpeciesDetails.csv$SpeciesID[i]
    ], " AND sd.detailID = ",
    files$SpeciesDetails.csv$newID[i], ";"
  )
  response <- try(
    SELECT(tmp_qry),
    silent = TRUE
  )
  if(is.data.frame(response)){
    sdetails_ll[i] <- TRUE
  } else {
    sdetails_ll[i] <- FALSE
  }
}
# if any false we need to upload some
nfix <- sum(!sdetails_ll)
where_fix <- which(!sdetails_ll)
added <- rep(NA, nfix)
for(i in 1:nfix){
  tmp <- files$DetectionDetails.csv[where_fix[i],]
  tmp_qry <- paste0(
    "INSERT INTO SpeciesDetails (detailID, speciesID) ",
    "VALUES (",
    tmp$newID, ", ",
    files$Species.csv$newID[
      files$Species.csv$SpeciesID == tmp$SpeciesID
    ], ");"
  )
  response <- try(
    MODIFY(tmp_qry,TRUE),
    silent = TRUE
  )
  if(response == 1){
    added[i] <- TRUE
  }
}

# and finally checking the roles
roles_map <- rep(NA, nrow(files$Users.csv))
for(i in 1:length(roles_map)){
  response <- try(
    SELECT(
      paste0(
        "SELECT userID, areaID FROM Users u ",
        "WHERE u.email = ",
        shQuote(
          files$Users.csv$Email[i]
        )
      )
    ),
    silent = TRUE
  )
  if(is.data.frame(response)){
    if(response$areaID != aid$areaID){
      stop("User tied to another area")
    }
    roles_map[i] <- response$userID
  }
  # also check first and last name
  response2 <- try(
    SELECT(
      paste0(
        "SELECT userID, areaID FROM Users u ",
        "WHERE u.firstName = ",
        shQuote(
          files$Users.csv$FirstName[i]
        ), " AND u.lastName = ",
        shQuote(
          files$Users.csv$LastName[i]
        ), " AND u.areaID = ", aid$areaID, ";"

      )
    ),
    silent = TRUE
  )
  if(class(response) == "try-error" & is.data.frame(response2)){
    roles_map[i] <- -1
  }

}
# add the missing observers. Map the roleID
unq_roles <- unique(files$Users.csv$Role)
roleID <- SELECT(
  paste0(
    "SELECT * FROM RolesLookup rl WHERE rl.roleName IN ",
    sql_IN(unq_roles)
  )
)
files$Users.csv <- dplyr::left_join(
  files$Users.csv,
  roleID,
  by = c("Role" = "roleName")
)
files$Users.csv <- data.frame(files$Users.csv)

to_add <- which(is.na(roles_map))
added <- rep(NA, length(to_add))
for(i in 1:length(to_add)){
  tmp <- files$Users.csv[to_add[i],]
  tmp_qry <- paste0(
    "INSERT INTO Users (firstName, lastName, email, roleID,",
    " statusID, areaID) VALUES (",
    shQuote(tmp$FirstName), ", ",
    shQuote(tmp$LastName), ", ",
    shQuote(tmp$Email), ", ",
    tmp$roleID, ", ",
    1, ", ",
    aid$areaID,");"
  )
  response <- try(
    MODIFY(
      tmp_qry,
      TRUE
    ),
    silent = TRUE
  )
  if(response == 1){
    added[i] <- TRUE
  } else {
    added[i] <- FALSE
  }
}









