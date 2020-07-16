#' Copy images from google cloud
#'
#' \code{gsutil_copy} takes in the object from \code{images_of} and copies
#'   them to a specified folder.
#'
#' @param images_to_copy The output data.frame from \code{images_of}.
#'
#' @param output_to_folder A character vector that denotes the full path of
#'   a folder to copy the images to.
#'   If the folder does not exist it will be created. A final slash cannot
#'   be placed at the end of the character. For example, \code{"C:/users/data"}
#'   works but \code{"C:/users/data/"} does not.
#'
#' @param ncore a numeric scalar. The number of cores to use to copy images
#'   from google cloud. It defaults to 2.
#'
#' @importFrom foreach foreach
#' @importFrom progress progress_bar
#'
#'@export
gsutil_copy <- function(images_to_copy = NULL,
                        output_folder = NULL,
                        ncore = 2){
  # Check to see if google-cloud-sdk is in path variable
  if(length(grep('google-cloud-sdk', Sys.getenv('PATH'))) != 1 ){
    stop('google-cloud-sdk is not in your PATH environment variable.',
         '\nTo add it, see readME on www.github.com/mfidino/uwinutils')
  }

  # add a wildlcard on the filepath to look in the archive as well
  images_to_copy$filepath <- gsub('(urban-wildlife-\\w+)/',
                                  '\\1*/',
                                  images_to_copy$filepath)

  # create folder if it does not exist
  if(!file.exists(output_folder)){
    dir.create(output_folder)
  }
  cl <- snow::makeCluster(ncore)
  doSNOW::registerDoSNOW(cl)
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
  snow::stopCluster(cl)
  # prepare the csv to go along with those images
  to_save <- images_to_copy[,-grep('filepath', colnames(images_to_copy))]
  to_save$updateCommonName <- ""
  to_save$updateNumIndividuals <- ""
  write.csv(to_save, paste0(output_folder,"/detections_to_update_",Sys.Date(),".csv"))
  cat('Images and detection data copied to', output_folder)
}


