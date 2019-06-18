
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
    'ORDER BY sp.commonName, ph.photoName;'
  )
  to_return <- SELECT(tmp_sql)
  # drop all images that are not stored on google drive
  to_return <- to_return[grep("^gs://urban-wildlife", to_return$filepath),]
  return(dplyr::distinct(to_return))
}

gsutil_copy <- function(images_to_copy = NULL,
                        output_folder = NULL,
                        ncore = 2){
  # add a wildlcard on the filepath to look in the archive as well
  images_to_copy$filepath <- gsub('(urban-wildlife-\\w+)/',
                                  '\\1*/',
                                  images_to_copy$filepath)

  # create folder if it does not exist
  if(!file.exists(output_folder)){
    dir.create(output_folder)
  }
  cl <- makeCluster(ncore)
  registerDoSNOW(cl)
  cat('Copying images to', output_folder)
  pb <- progress_bar$new(
    format = "Images complete [:bar] :elapsed | eta: :eta",
    total = nrow(images_to_copy),
    width = 60
  )
  progress <- function(n){
    pb$tick()
  }
  opts <- list(progress = progress)

  foreach(i = 1:nrow(images_to_copy), .options.snow = opts) %dopar% {
    system(paste('gsutil cp',images_to_copy$filepath[i], output_folder))
  }
  stopCluster(cl)
  # prepare the csv to go along with those images
  to_save <- images_to_copy[,-grep('filepath', colnames(images_to_copy))]
  to_save$updateCommonName <- ""
  to_save$updateNumIndividuals <- ""
  write.csv(to_save, paste0(output_folder,"/detections_to_update_",Sys.Date(),".csv"))
  cat('Images and detection data copied to', output_folder)
}


