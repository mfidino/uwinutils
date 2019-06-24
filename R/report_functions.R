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
#' @param db The MariaDB connection to the UWIN database. Defaults to 'uwidb'
#'
#' @return a data.frame with the following columns:
#' - filepath: the location of the image on google cloud
#' - locationName: the site name the image was taken
#' - photoName: the name of the image
#' - commonName: the species tagged in the images
#' - numIndividuals: The number of individuals of the tagged species
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
  if(class(db) != 'MariaDBConnection'){
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
    'INNER JOIN CameraLocations cl ON cl.locationID = vi.locationID\n',
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


