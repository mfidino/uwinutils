#' Formatting different data outputs for occupancy analyses
#'
#' \code{check_format} inspects the first line of a file to determine if
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

format_emammal <- function(data, start_date = NULL, end_date = NULL,
                           occ_data = NULL){
  # Quick check to make sure the data come from emammal. It could
  #  be that someone reads in the emammal data via read.csv
  #  or something or the other.
  tmp_line <- colnames(data)
  tmp_clump <- grep("Clumping\\(days\\)|Clumping\\.days\\.", tmp_line)

  if(length(tmp_clump) > 0 & is.null(attributes(data)$format)){
    attributes(data)$format <- "emammal"
  }

  if(attributes(data)$format != "emammal"){
    err <- "This is not emammal data (i.e., does not contain a 'Clumping(days)' column)."
    stop(err)
  }

  # get dates
  if(is.null(start_date)){
    start_date_vec <- data$date.out
  } else if(is.numeric(start_date)){
    start_date_vec <- data[,start_date]
  } else if(is.character(start_date)){
    start_date_vec <- data[[start_date]]
  }
  start_date_vec <- lubridate::mdy(start_date_vec)
  if(is.null(end_date)){
    end_date_vec <- data$date.in
  } else if(is.numeric(end_date)){
    end_date_vec <- data[,end_date]
  } else if(is.character(end_date)){
    end_date_vec <- data[[end_date]]
  }
  end_date_vec <- lubridate::mdy(end_date_vec)

  # check that all end dates are after start dates
  if(!all(end_date_vec > start_date_vec)){

  }


  if(attributes(data)$format != "emammal"){
    err <- "This"

  }

}


