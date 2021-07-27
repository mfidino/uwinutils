library(uwinutils)

connect2db()


my_path <- "../uwin-api/csv-data/mawi/"
areaabbr <- "MAWI"

#files$Species.csv[13,] <- c(13, "Empty", NA, NA, "empty")
file_paths <- list.files(
  my_path,
  full.names = TRUE
)

file_names <- list.files(
  my_path
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

#map species
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

# DetectionDetails map
dd_map <- rep(NA, nrow(files$SpeciesDetails.csv))
for(i in 1:length(dd_map)){
  tmp_qry <- paste0(
    "SELECT detailID FROM DetailsLookup dl WHERE\n",
    "dl.detailText = '", files$SpeciesDetails.csv$DetailText[i],"';"
  )
  response <- try(
    SELECT(tmp_qry),
    silent = TRUE
  )
  if(is.data.frame(response)){
    dd_map[i] <- response$detailID
  }
}
if(any(is.na(dd_map))){
  stop("add new detection details")
} else {
  files$SpeciesDetails.csv$newID <- dd_map
}

# map users
user_map <- rep(NA, nrow(files$Users.csv))
for(i in 1:length(user_map)){
  tmp_qry <- paste0(
    "SELECT userID FROM Users us WHERE\n",
    "us.email = '", files$Users.csv$Email[i],"'"
  )
  response <- try(
    SELECT(tmp_qry),
    silent = TRUE
  )
  if(is.data.frame(response)){
    user_map[i] <- response$userID
  }
}
if(any(is.na(user_map))){
  stop("add users")
} else {
  files$Users.csv$newID <- user_map
}

if(any(files$Detections.csv$StatusID == 3)){
  to_go <- which(
    files$Detections.csv$StatusID == 3
  )
  files$Detections.csv <- files$Detections.csv[-to_go,]
}

# start uploading detections
pb <- txtProgressBar(max = nrow(files$Detections.csv))
upped <- rep(NA, length(nrow(files$Detections.csv)))

for(i in 2:nrow(files$Detections.csv)){
  setTxtProgressBar(pb, i)
  #strip comment stuff
  tmp <- files$Detections.csv[i,]
  if(!is.na(tmp$Comments)){
    weird_encoding <- try(nchar(tmp$Comments), silent = TRUE)
    if(class(weird_encoding) == "try-error"){
      tmp$Comments <- enc2utf8(tmp$Comments)
    }
    tmp$Comments <- gsub("'|,|;", "", tmp$Comments)
    if(nchar(tmp$Comments)>200){
      tmp$Comments <- substr(tmp$Comments, 1,200)
    }
  } else {
    tmp$Comments <- ""
  }
  tmp_qry <- paste0(
    "INSERT INTO Detections (comments, valStatID, photoName,",
    " userID) VALUES ('",
    tmp$Comments,"', ", tmp$StatusID, ", '",
    files$Photos.csv$FileName[
      files$Photos.csv$ImageID == tmp$ImageID
    ], "', ",
    files$Users.csv$newID[
      files$Users.csv$ObserverID == tmp$ObsID
    ],");"
  )
  response <- try(
    MODIFY(tmp_qry),
    silent = TRUE
  )
  # return the new detection ID
  if(!class(response) == "try-error"){
    upped[i] <- "uploaded"
    newdetID <- SELECT(
      paste0(
        "SELECT detectionID FROM Detections de WHERE",
        " de.photoName = '",
        files$Photos.csv$FileName[
          files$Photos.csv$ImageID == tmp$ImageID
          ], "'"
      )
    )
    newdetID <- max(newdetID)
  }

  # Now insert speciesDetections
  if(is.na(tmp$DetailID)){
    newID <- 1
  } else {
    whichID <- which(
      files$SpeciesDetails.csv$DetailID ==
        tmp$DetailID &
        files$SpeciesDetails.csv$SpeciesID ==
        tmp$SpeciesID
    )
    if(length(whichID) == 0){
      stop("You screwed up the details query. fix it.")
    }else{
      newID <- files$SpeciesDetails.csv$newID[whichID]
      if(length(newID)>1){
      if(length(unique(newID))==1){
        newID <- unique(newID)
      } else {
        stop("got 2 IDs, that is no good")
      }
      }
    }
  }
  tmp_qry <- paste0(
    "INSERT INTO DetectionSpecies (detectionID, speciesID,",
    " detailID, numIndividuals) VALUES (",
    newdetID,", ",
    files$Species.csv$newID[
      files$Species.csv$SpeciesID == tmp$SpeciesID
    ],", ",
    newID,", ",
    tmp$Individuals,");"
  )

  response <- try(
    MODIFY(tmp_qry),
    silent = TRUE
  )
  if(class(response) == "try-error"){
    if(grep("Duplicate", response[1]) == 1){
      cat("\nduplicate image", i,"\n\n")
      next
    }
    stop("investigate the issue")
  }
  rm(newID)
}
