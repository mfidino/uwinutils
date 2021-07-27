
######################################################### CSV COLUMN FORMATTING ASSUMPTIONS #################################################################
#                                                                                                                                                           #
# CameraLocations:      LocationID, StudyAreaID, LocationName,  UTM_E,   UTM_N,      UTMZone,    Landowner Abbrev,   FullLocationName                       #
#                       line[0]     line[1]      line[2]        line[3]  line[4]     line[5]     line[6]             line[7]                                #
# LandOwnerLookup:      ID,         Landowner,  LandownerAbbrev,    StudyAreaID                                                                             #
#                       line[0]     line[1]     line[2]             line[3]                                                                                 #
# Cameras:              ID,         CameraID,   ModelNumber,    Retired,    Mfg                                                                             #
#                       line[0]     line[1]     line[2]         line[3]     line[4]                                                                         #
# Species:              SpeciesID,  CommonName,     Genus,      Species,    ShortName                                                                       #
#                       line[0]     line[1]         line[2]     line[3]     line[4]                                                                         #
# SpeciesDetails:       SpeciesShortName,   DetailText                                                                                                      #
#                       line[0]             line[1]                                                                                                         #
# Users:                ObserverID,     LastName,   FirstName,  Role,       Email,      UserAccountStatus                                                   #
#                       line[0]         line[1]     line[2]     line[3]     line[4]     line[6]     line[7]                                                 #                                                                                 #
############################################################ INSTRUCTIONS TO RUN SCRIPT #####################################################################

library(uwinutils)
library(lubridate)

connect2db()


my_path <- "../uwin-api/csv-data/mawi/"
areaabbr <- "MAWI"

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

# Map old locationID to new locationID
location_map <- rep(NA, nrow(files$CameraLocations.csv))
for(i in 1:length(location_map)){
  # put together query for a given location
  tmp_qry <- paste0(
    "SELECT locationID FROM CameraLocations cl\n",
    "WHERE cl.locationAbbr = '",
    files$CameraLocations.csv$LocationName[i],"';"
  )
  response <- try(
    SELECT(tmp_qry),
    silent = TRUE
  )
  # Provide locationID so long as we have it in
  #  the database
  if(is.data.frame(response)){
    location_map[i] <- response$locationID
  }
}
if(any(is.na(location_map))){
  stop("You need to upload new locations")
}
files$CameraLocations.csv$newID <- location_map

# map camera IDs
camera_map <- rep(NA, nrow(files$Cameras.csv))
for(i in 1:length(camera_map)){
  tmp_qry <- paste0(
    "SELECT cameraID FROM Cameras ca\n",
    "WHERE ca.cameraName = '",
    files$Cameras.csv$CameraID[i],"';"
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

# Now map the action ID's
action_map <- rep(NA, nrow(files$Actions.csv))
for(i in 1:length(action_map)){
  tmp_qry <- paste0(
    "SELECT actionID FROM ActionsLookup al\n",
    "WHERE al.actionName = '",
    files$Actions.csv$Action[i],"';"
  )
  response <- try(
    SELECT(
      tmp_qry
    ),
    silent = TRUE
  )
  if(is.data.frame(response)){
    action_map[i] <- response$actionID
  }
}
if(any(is.na(action_map))){
  stop("Upload missing actions.")
}
files$Actions.csv$newID <- action_map
# now the lure map
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
# store whether the data was uploaded
vi_up <- rep(NA, nrow(files$Visits.csv))
# Upload the visits
pb <- txtProgressBar(min = 1, max = nrow(files$Visits.csv))
for(i in 2:nrow(files$Visits.csv)){
  setTxtProgressBar(pb, i)
  tmp <- files$Visits.csv[i,]
  # convert 0 to NA if they are there in actions.
  tmp[,grep("Action", colnames(tmp))] <-
    as.numeric(gsub("^0$", NA, tmp[,grep("Action", colnames(tmp))]))
  VisitID = 0
  # make sure comments are under 200 characters, don't have
  #  commas, etc.
  if(!is.na(tmp$Comments)){
    # drop commas, apostrophes, etc.
    # futz with encoding issues
    weird_encoding <- try(nchar(tmp$Comments), silent = TRUE)
    if(class(weird_encoding) == "try-error"){
      tmp$Comments <- enc2utf8(tmp$Comments)
    }
    tmp$Comments <- gsub("'|,|~|\"", "", tmp$Comments)
    if(nchar(tmp$Comments) > 200){
      # under 200
      tmp$Comments <- substr(tmp$Comments,1,200)
    }
  }
  if(is.na(tmp$Comments)){
    tmp$Comments <- ''
  }
    # format dates correctly YMD HMS. First up, combiine
    #  visit date and time
    vdt <- paste(
      strsplit(tmp$VisitDate, " ")[[1]][1],
      strsplit(tmp$VisitTime, " ")[[1]][2]
    )
    # format as ymd hms, convert back to character
    vdt <- as.character(mdy_hms(vdt))
    # if we don't have active start and end then we
    # just upload the data.
    if(all(is.na(tmp[,c("ActiveStart", "ActiveEnd")]))){
      tmp_up <- paste0(
        "INSERT INTO Visits ( visitDatetime, comments,",
        " visitTypeID, conditionID, locationID,",
        " cameraID, sensitivityID ) VALUES ( ",
        paste0(
          "'",vdt,"', '", tmp$Comments,"', ", tmp$VisitTypeID,", ",
          tmp$CameraConditionID,", ",
          files$CameraLocations.csv$newID[
            files$CameraLocations.csv$LocationID == tmp$LocationID
          ],", ",
          files$Cameras.csv$newID[
            files$Cameras.csv$CameraID == tmp$CameraID
          ],", ",
          tmp$SensitivityID," );"
              )

      )
      response <- try(
        MODIFY(tmp_up,report = TRUE),
        silent = TRUE
      )
    }
    # otherwise activedates are there and we need to add those
    if(all(!is.na(tmp[,c("ActiveStart", "ActiveEnd")]))){
      # format active start and end
      newActiveStart <- as.character(
        mdy_hms(tmp$ActiveStart)
      )
      newActiveEnd <- as.character(
        mdy_hms(tmp$ActiveEnd)
      )
      tmp_up <- paste0(
        "INSERT INTO Visits ( visitDatetime, comments,",
        " activeStart, activeEnd,",
        " visitTypeID, conditionID, locationID,",
        " cameraID, sensitivityID ) VALUES ( ",
        paste0(
          "'",vdt,"', '", tmp$Comments,"', '",newActiveStart,"', '",
          newActiveEnd, "', ",
          tmp$VisitTypeID,", ",
          tmp$CameraConditionID,", ",
          files$CameraLocations.csv$newID[
            files$CameraLocations.csv$LocationID == tmp$LocationID
            ],", ",
          files$Cameras.csv$newID[
            files$Cameras.csv$CameraID == tmp$CameraID
            ],", ",
          tmp$SensitivityID," );"
        )

      )
      response <- try(
        MODIFY(tmp_up,report = TRUE),
        silent = TRUE
      )

    }
    if(class(response) == "try-error"){
      # Check if it is a duplicate.
      has_dupe <- grep("Duplicate", response)
      if(length(has_dupe) == 1){
        vi_up[i] <- "duplicate"
        next
      }
      stop("Issue with upload, start writing exceptions")
    }
    #Assuming it went through...
    cquery <- paste0(
      "SELECT visitID FROM Visits vi\n",
      "where vi.locationID = ", files$CameraLocations.csv$newID[
        files$CameraLocations.csv$LocationID == tmp$LocationID
        ], " AND vi.visitDatetime = '", vdt,"';"
    )
    new_visitID <- SELECT(cquery)
    if(is.data.frame(new_visitID)){
      visitID <- new_visitID$visitID
    }
    # add on the actions now
    if(visitID != 0){
      # try action 1 if available
      if(!is.na(tmp$Action1ID)){
        a1 <- files$Actions.csv$newID[
          files$Actions.csv$ID == tmp$Action1ID
        ]
        atmp <- paste0(
          "INSERT INTO VisitActions (visitID, actionID)",
          " VALUES (", visitID, ", ", a1,");"
        )
        response <- try(
          MODIFY(
            atmp
          )
        )
      }
      # try action 2 if avilable
      if(!is.na(tmp$Action2ID)){
        a2 <- files$Actions.csv$newID[
          files$Actions.csv$ID == tmp$Action2ID
          ]
        atmp <- paste0(
          "INSERT INTO VisitActions (visitID, actionID)",
          " VALUES (", visitID, ", ", a2,");"
        )
        response <- try(
          MODIFY(
            atmp
          )
        )
      }
      # and then action 3
      if(!is.na(tmp$Action3ID)){
        a3 <- files$Actions.csv$newID[
          files$Actions.csv$ID == tmp$Action3ID
          ]
        atmp <- paste0(
          "INSERT INTO VisitActions (visitID, actionID)",
          " VALUES (", visitID, ", ", a3,");"
        )
        response <- try(
          MODIFY(
            atmp
          )
        )
      }
      # and finally the lure
      l1 <- files$Lures.csv$newID[
        files$Lures.csv$LureID == tmp$LureID
      ]
      lure_query <- paste0(
        "INSERT INTO VisitLures (visitID, lureID)",
        " VALUES (", visitID, ", ", l1,");"
      )
      response <- try(
        MODIFY(lure_query),
        silent = TRUE
      )
    }
    vi_up[i] <- "upload"
  }

