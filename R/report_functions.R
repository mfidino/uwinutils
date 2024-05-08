#' Images of a species at a given study area
#'
#' \code{images_of} queries the UWIN database for a selected species and
#'   study area.
#'
#' @param species A character vector of the species to be queried. If left as
#'   \code{NULL} then you can select the species you want in a pop up list.
#'   Multiple species can be selected by holding CTRL when clicking on them.
#'
#' @param studyArea The four letter capitalized study area abbreviation for a
#'   city. If left as \code{NULL} you can select the study area from a pop up
#'   list. Only one study area may be selected at a time.
#'
#' @param db The MySQL connection to the UWIN database. Defaults to 'uwidb'
#'
#' @return a data.frame with the following columns:
#' - filepath: the location of the image on google cloud
#' - locationName: the site name the image was taken
#' - photoName: the name of the image
#' - commonName: the species tagged in the images
#' - numIndividuals: The number of individuals of the tagged species
#'
#'
#' @importFrom rstudioapi askForPassword
#' @importFrom utils select.list
#' @importFrom dplyr distinct
#'
#' @examples
#' \dontrun{
#' my_images <- images_of()
#' }
#' @export
images_of <- function(species = NULL,
                      studyArea = NULL,
                      db = uwidb){
  if(class(db) != 'MySQLConnection'){
    stop('db is not the correct class, please connect to database with connect2db().')
  }
  # pull the species
  if(is.null(species)){
    species_df <- SELECT('SELECT * FROM Species ORDER BY commonName;', uwidb)

    species <- select.list( as.character(species_df$commonName),
                            multiple = TRUE, graphics = TRUE,
                            title = 'Select species:')

  }
  # make sure the species are in the table
  tmp_sql <- paste(
    'SELECT speciesID , commonName FROM Species\n',
    'WHERE CommonNAME IN (',
    paste(paste0("'",species,"'") , collapse = ", "),
    ');')
  if(nrow(SELECT(tmp_sql)) != length(species)){
    stop('You have manually input an incorrect species. Check spelling.')
  } else {
    tmp_species <- SELECT(tmp_sql)
  }
  # collect the appropriate study area, open selection if no input
  #  otherwise try the sql statement
  if(is.null(studyArea)){
    studyArea_df <- SELECT('SELECT areaID, areaAbbr FROM StudyAreas ORDER BY areaName;')
    studyArea <- select.list( as.character(studyArea_df$areaAbbr), graphics = TRUE,
                              title = 'Select one study area:')
    # Convert to areaID
    studyArea <- studyArea_df$areaID[studyArea_df$areaAbbr == studyArea]
  } else {
    tmp_sql <- paste(
      'SELECT areaID FROM StudyAreas\n',
      'WHERE areaAbbr = ', paste0("'",studyArea, "'"),';')
    studyArea <- SELECT(tmp_sql)
  }
  # Query the images that are in the specific study area and that are
  #  of these species.
  tmp_sql <- paste(
    "SELECT ph.filepath, cl.fullName AS 'locationName', ph.photoName, sp.commonName, ds.numIndividuals  FROM Photos ph\n",
    'INNER JOIN Detections de ON de.photoName = ph.photoName\n',
    'INNER JOIN DetectionSpecies ds on ds.detectionID = de.detectionID\n',
    'INNER JOIN Species sp ON sp.speciesID = ds.speciesID\n',
    'INNER JOIN Visits vi ON vi.visitID = ph.visitID\n',
    'INNER JOIN Locations cl ON cl.locationID = vi.locationID\n',
    'WHERE de.valStatID = 2\n',
    'AND ds.speciesID IN(', paste(tmp_species$speciesID, collapse = ", "), ")\n",
    'AND ph.areaID =', as.numeric(studyArea), "\n",
    'ORDER BY ph.photoName;'
  )
  to_return <- SELECT(tmp_sql)
  # drop all images that are not stored on google drive
  to_return <- to_return[grep("^gs://urban-wildlife", to_return$filepath),]
  return(dplyr::distinct(to_return))
}


#' The progress made for each season's worth of data
#'
#' \code{progress_of} queries the UWIN database for a selected study area.
#'
#'
#' @param studyArea The four letter capitalized study area abbreviation for a
#'   city. If left as \code{NULL} you can select the study area from a pop up
#'   list. Only one study area may be selected at a time.
#'
#' @param db The MySQL connection to the UWIN database. Defaults to 'uwidb'
#'
#' @return a list with the following elements:\cr\cr
#'  - \code{assignedIncomplete} (data.frame): This is a report on photo groups
#'      that have been assigned but have yet to be finished.
#'      The columns in \code{assingedIncomplete} are:
#'      \itemize{
#'        \item User: The name of the person who has photo groups to classify.
#'        \item email: The users email.
#'        \item yearMonth: The year and month of visits in the database that have photos.r
#'        \item countAssignedIncomplete: The number of photo groups assigned to a user
#'        per yearMonth that have not been completed.
#'        }
#'  - \code{fullComplete} (data.frame): This is a progress report on the images that have been
#'      classified as 'complete' in the cloud database, which depends on how many
#'      users are necessary to consider an image 'complete' (either one or two,
#'      depending on a study area). The columns in \code{fullComplete} are:
#'      \itemize{
#'      \item yearMonth: The year and month of visits in the database that have photos.
#'      \item percentComplete: The percent of images in a yearMonth that
#'        are considered 100% complete (i.e., double validated).
#'      \item TotalPhotos: The number of photos tied to a yearMonth.
#'      \item NotAssigned: Photos in a yearMonth that have not been assigned
#'        to a user to tag.
#'      \item ToValidate: The number of photos that need to go through Validation
#'        per yearMonth. The Valdidation is on the UWIN cloud db.
#'      }
#'  - \code{pendingComplete} (data.frame): This is a progress report on the images that have
#'      been tagged by at least one user on the cloud db, which could be used
#'      to generate an occupancy query (assuming that 1 viewer is okay).
#'      The columns in \code{pendingComplete} are:
#'      \itemize{
#'      \item yearMonth: The year and month of visits in the database that have photos.
#'      \item percentComplete: The percent of images in a yearMonth that
#'        have been tagged at least once.
#'      \item TotalPhotos: The number of photos tied to a yearMonth.
#'      \item NotAssigned: Photos in a yearMonth that have not been assigned
#'        to a user to tag.
#'      }
#'
#' @importFrom utils select.list
#' @importFrom dplyr distinct group_by  arrange  desc  summarise  left_join
#' @importFrom lubridate month  year
#' @importFrom stringr str_pad
#'
#' @examples
#' \dontrun{
#' my_images <- progress_of("CHIL")
#' }
#' @export
#'
progress_of <- function(
  studyArea = NULL,
  db = uwidb
){
  # allow for selection if left null
  if(is.null(studyArea)){
  studyArea_df <- SELECT('SELECT areaAbbr, areaName FROM StudyAreas ORDER BY areaName;')
  studyArea <- select.list( as.character(studyArea_df$areaName), graphics = TRUE,
                            title = 'Select one study area:')
  studyArea <- studyArea_df$areaAbbr[which(studyArea_df$areaName == studyArea)]
  }

  my_cols <- paste(
    "cl.fullName AS siteName",
    "Date(vi.visitDateTime) AS visitDate",
    "Concat(Users.firstName, ' ', Users.lastName) AS User",
    "Users.email",
    "apg.tagIndex > 0 AS started",
    "Count(ph.photoName) AS photosInGroup",
    sep = ",\n")

  # this is a query to see what photo groups have been assinged
  #  but have not been finished.
  tmp_qry <- paste0(
    "SELECT DISTINCT\n", my_cols, " FROM Visits vi\n",
    "INNER JOIN Photos ph ON vi.visitID = ph.visitID\n",
    "LEFT JOIN PhotoGroup pg ON pg.photoGroupID = ph.photoGroupID\n",
    "LEFT JOIN Locations cl ON cl.locationID = vi.locationID\n",
    "LEFT JOIN AssignedPhotoGroup apg on pg.photoGroupID = apg.photoGroupID\n",
    "LEFT JOIN Users ON Users.userID = apg.userID\n",
    "LEFT JOIN StudyAreas sa ON cl.areaID = sa.areaID\n",
    "WHERE sa.AreaAbbr = '", studyArea, "'\n",
    "AND pg.completed = 0\n",
    "AND apg.completed = 0\n",
    "GROUP BY pg.photoGroupID, vi.visitID, Users.userID, apg.completed, apg.tagIndex"
  )

  # grab the data
  q1 <- try(SELECT(tmp_qry), silent = TRUE)
  if(class(q1) != "try-error"){
  # convert started to yes / no
  q1$started <- ifelse(is.na(q1$started), "No", "Yes")

  q1$yearMonth <- paste(
    lubridate::year(
      q1$visitDate
    ),
    stringr::str_pad(
      lubridate::month(
        q1$visitDate
      ),
      2,
      pad = "0"
    ),
    sep = "-"
  )


  # second, the count of users that have groups to complete
  q2 <- q1 %>% dplyr::group_by(User, email, yearMonth) %>%
          dplyr::summarise(countAssignedIncomplete = length(User)) %>%
          dplyr::arrange(yearMonth)

  # reorder q1 and drop the email
  q1 <- q1[
    order(q1$visitDate),
    -which(colnames(q1) == "email")
  ]

  rm(my_cols)
  rm(tmp_qry)
  } else {
    q2 <- "No photogroups currently assigned"
  }
  # Next thing is to check progress of all photos by month

  my_cols <- paste(
    "cl.fullName AS siteName",
    "Date(vi.visitDateTime) AS visitDate",
    "ph.photoName",
    "de.detectionID",
    "pg.completed",
    "de.valStatID",
    sep = ",\n")

  tmp_qry <- paste0(
    "SELECT ", my_cols, " FROM Photos ph\n",
    "LEFT JOIN Detections de ON de.photoName = ph.photoName\n",
    "LEFT JOIN Visits vi on vi.visitID = ph.visitID\n",
    "LEFT JOIN Locations cl ON cl.locationID = vi.locationID\n",
    "LEFT JOIN StudyAreas sa ON sa.areaID = cl.areaID\n",
    "LEFT JOIN PhotoGroup pg ON pg.photoGroupID = ph.photoGroupID\n",
    "WHERE sa.AreaAbbr = '", studyArea, "';"
  )

 q3 <- SELECT(tmp_qry)
  if(any(q3$valStatID == 3, na.rm = TRUE)){
    q3 <- q3[-which(q3$valStatID == 3),]
  }
q3$yearMonth  <- paste(
  lubridate::year(
    q3$visitDate
  ),
  stringr::str_pad(
    lubridate::month(
      q3$visitDate
    ),
    2,
    pad = "0"
  ),
  sep = "-"
)
# need to reduce down to just images, whether it's been compled
#  and the yearmonth
q_fullcomplete <- q3[,c("photoName", "completed", "yearMonth", "detectionID")]
q_fullcomplete$not_assigned <- 0
q_fullcomplete$not_assigned[which(is.na(q_fullcomplete$detectionID))] <- 1
# turn that does not start with VID into 0
if(length(grep("^VID", q_fullcomplete$photoName)) != nrow(q_fullcomplete)){
  q_fullcomplete$not_assigned[-grep("^VID", q_fullcomplete$photoName)] <- 0
  q3$valStatID[-grep("^VID", q3$photoName)] <- 2
  q3$completed[-grep("^VID", q3$photoName)] <- 1
  q3$detectionID[-grep("^VID", q3$photoName)] <- -1
}

q_fullcomplete <-
  q_fullcomplete[,
                 c(
                   "photoName",
                   "completed",
                   "yearMonth",
                   "not_assigned"
                  )
  ] %>%
  dplyr::distinct(.)

 q_fullcomplete <- q_fullcomplete %>% dplyr::group_by(yearMonth) %>%
   dplyr::summarise(percentcomplete = sum(
     completed, na.rm = TRUE) / length(completed),
     TotalPhotos = length(unique(photoName)),
     NotAssigned = sum(not_assigned)
     ) %>%
   dplyr::arrange(yearMonth)


 if(any(q3$completed == 1 & q3$valStatID ==1, na.rm=TRUE)){
   q4 <- q3
   to_go <- unique(q3$photoName[which(q3$valStatID == 2)])
   q4 <- q4[-which(q4$photoName %in% to_go),]
   q4 <- q4[q4$completed == 1,]
   to_validate <- q4[which(q4$completed == 1 & q4$valStatID == 1),] %>%
     dplyr::select(photoName, yearMonth, completed) %>%
     dplyr::distinct(.) %>%
     dplyr::group_by(yearMonth) %>%
     dplyr::summarise(ToValidate = length(completed))

   q_fullcomplete <- dplyr::left_join(
     q_fullcomplete,
     to_validate,
     by = "yearMonth"
   )
 }else{
   q_fullcomplete$ToValidate <- NA
 }


 one_check <- q3[-which(duplicated(q3$photoName)),]
 one_check <- one_check %>% dplyr::group_by(yearMonth) %>%
   dplyr::summarise(
     percentComplete = sum(
       valStatID %in% c(1,2), na.rm = TRUE
       ) / length(valStatID),
     TotalPhotos = length(valStatID),
     NotAssigned = sum(is.na(detectionID))
   )


 to_return <- list(
   assignedIncomplete = q2,
   fullComplete = q_fullcomplete,
   pendingComplete = one_check

  )
 return(to_return)

  }


