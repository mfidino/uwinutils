#' Formatting different data outputs for occupancy analyses
#'
#' \code{check_format} inspects the first line of a data file to determine if
#'   it came from emammal, the distributed UWIN Access DB, or the UWIN cloud
#'   DB.
#'
#' @param file The file path of the data you want to read in as a character.
#'   This file should be a summarised output from emammal,
#'   the distributed UWIN Access DB, or the UWIN cloud DB.
#'
#' @return a data.frame of the data. The format of the data is written
#'   in the \code{\link{attributes}} of the object and is either
#'   \code{'emammal'},\code{'access'}, or \code{'uwin-app'}.
#'
#'
#' @importFrom data.table fread
#'
#' @examples
#' \dontrun{
#' my_path <- "path_to_occupancy_data.csv"
#' my_data <- check_format(my_path)
#'
#' # Check the format
#' attributes(my_data)$format
#' }
#' @export
check_format <- function(file){
  # Ensure it is a character
  if(!is.character(file)){
    err <- paste0("invalid 'type' (", class(file),") of argument")
    stop(err)
  }
  # read in the first line of data, and drop UTF-encoding character if it
  #  exists.
  tmp_line <- data.table::fread(
    file = file,
    nrows = 1,
    data.table = FALSE
  ) %>%
    colnames

  # Check if 'Clumping(days)' is a column name. If so this
  #  is eMammal data.
  tmp_clump <- grep("Clumping\\(days\\)|Clumping\\.days\\.", tmp_line)
  step_1 <- length(tmp_clump) == 1

  if(step_1){
    data <- data.table::fread(
      file = file,
      data.table = FALSE
    )
    attributes(data)$format <- "emammal"
  }

  # Check if the column names are non-existent. This would come from the
  #  summary of the Access data.
  access_compare1 <- paste0("V", c(1, 1:(length(tmp_line) -1)))
  access_compare2 <- paste0("V", 1:length(tmp_line))

 if(identical(tmp_line, access_compare1) |
              identical(tmp_line, access_compare2)) {
    data <- data.table::fread(
      file = file,
      data.table = FALSE
    )
    attributes(data)$format <- "access"
  }

  # Check if they come from the uwin app
  uwin_compare <- c("Seasons Legend:", "Start Date", "End Date")
  if(identical(uwin_compare, tmp_line)){
    data <- data.table::fread(
      file = file,
      data.table = FALSE,
      fill = TRUE
    )
    attributes(data)$format <- "uwin-app"
  }

  if(!exists("data")){
    err <- paste0("These data don't follow a specific format.\n",
                  "\t-emammal data contains 'Clumping (days)' as a column.\n",
                  "\t-UWIN Access data do not contain column names.\n",
                  "\t-UWIN cloud data starts with 'Seasons Legend:'"
    )
    stop(err)
  }

  return(data)

}


