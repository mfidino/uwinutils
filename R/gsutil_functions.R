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


#' Delete images from google cloud
#'
#' \code{gsutil_delete} takes in the object from \code{images_of} and deletes them
#' from the google cloud.
#'
#' @param images_to_delete a data.frame with a column titled 'filepath'.
#'         the filepath is where on the cloud these photos should be
#'         deleted from
#'
#'
#' @param ncore a numeric scalar. The number of cores to use to copy images
#'   from google cloud. It defaults to 2.
#'
#' @param all Delete all photos in a given folder? Defaults to FALSE.
#'
#' @importFrom foreach foreach
#' @importFrom progress progress_bar
#'
#'@export
gsutil_delete <- function(images_to_delete = NULL,
                        ncore = 2, all = FALSE){
  # Check to see if google-cloud-sdk is in path variable
  if(length(grep('google-cloud-sdk', Sys.getenv('PATH'))) != 1 ){
    stop('google-cloud-sdk is not in your PATH environment variable.',
         '\nTo add it, see readME on www.github.com/mfidino/uwinutils')
  }

  # add a wildlcard on the filepath to look in the archive as well
  if(all){
    the_folder <- strsplit(images_to_delete$filepath, "/")
    the_folder <- lapply(
      the_folder, function(x) x[1:(length(x)-1)]
    )
    the_folder <- lapply(
      the_folder, function(x) paste0(x, collapse = "/")
    )
    the_folder <- sapply(
      the_folder, function(x) paste0(x, "/**")
    )
    the_folder <- unique(the_folder)
    images_to_delete <- data.frame(
      filepath = the_folder
    )
  }else{

    images_to_delete$filepath <- gsub(
      '(urban-wildlife-\\w+)/',
      '\\1*/',
      images_to_delete$filepath
    )
  }
  if(all){
  cl <- snow::makeCluster(ncore)
  doSNOW::registerDoSNOW(cl)
  pb <- progress_bar$new(
    format = "Images complete [:bar] :elapsed | eta: :eta",
    total = nrow(images_to_delete),
    width = 60
  )
  progress <- function(n){
    pb$tick()
  }
  opts <- list(progress = progress)
  # go through each folder and drop, otherwise go through
  #  selective images

    foreach(i = 1:nrow(images_to_delete), .options.snow = opts) %dopar% {
      system(paste('gsutil rm',images_to_delete$filepath[i]))
    }
    snow::stopCluster(cl)
  } else {
    # make a temporary file
    my_tmp <- tempdir()
    # get temp filepath
    my_fp <- paste0(my_tmp,"/","to_delete.txt")
    write.table(
      images_to_delete$filepath,
      my_fp,
      sep = "/t",
      row.names = FALSE,
      col.names = FALSE
    )
    system(paste('cat', my_fp, "| xargs gsutil rm "))
    file.remove(my_fp)
  }
}



#' Move images between folders on Google cloud
#'
#' \code{gsutil_move} requires paths \code{from} (where the images
#' are located) and \code{to} where the images should go on google cloud.
#'
#' @param from paths of files or folders to move
#'
#' @param to paths of where files should go. If \code{from} and \code{to} are
#'   folders, this will move all files between them. If files,
#'   then the file will be moved (which can be used to rename images if needed).
#'
#'
#'@export
gsutil_move <- function(from = NULL,to = NULL){
  # Check to see if google-cloud-sdk is in path variable
  if(length(grep('google-cloud-sdk', Sys.getenv('PATH'))) != 1 ){
    stop('google-cloud-sdk is not in your PATH environment variable.',
         '\nTo add it, see readME on www.github.com/mfidino/uwinutils')
  }

    # go through each folder and drop, otherwise go through
    #  selective images
    for(i in 1:length(from)){
      system(paste('gsutil mv', from[i], to[i]))
    }
}


