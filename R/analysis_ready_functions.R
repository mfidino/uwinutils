#' Connect to UWIN database
#'
#' \code{data_source} checks the current files in the uwin-dataset
#' folder and let's you know if they were generated from an access
#' db, the UWIN web app, or if it's precleaned.
#'
#' #' @param x The filepath to a given folder of data within a city.
#'
#' @return A character that says where the data came from.
#'
#' @importFrom rstudioapi askForPassword
#'
#' @examples
#' \dontrun{
#' data_source(x = "./chil/1_JA16/")
#' }
#'
data_source <- function(x){
# We first want to determine if it's cloud data
#  this shoud have 1) one file and 2) should start with
#  "Seasons Legend:,Start Date,End Date"

# first rule, determine number of files in folder
files <- list.files(
  x,
  full.names = TRUE
)

nfile <- length(
  files
)

if(nfile == 1){
  # then check to see if the first line is
  firstline <-
    readLines(
      files,
      n = 1
    )
  if(firstline == "Seasons Legend:,Start Date,End Date"){
    return("CLOUD")
  }
}

# check to see if this was summarized from the uwin access db
#  we can determine if the folder contains "site_locations.csv" and
#  "observation_matrix.csv"
has_site_locations <- grep(
  "site_locations.csv",
  files
)
has_observation_mat <- grep(
  "observation_matrix.csv",
  files
)
if(
  all(
    c(
      length(has_site_locations) == 1,
      length(has_observation_mat) == 1
    )
  )
){
  return("ACCESS")
}
# MAKS has some seasons generated differently
if(
  length(grep("*\\.xls", files)) > 0 &
  !all(
    c(
      length(has_site_locations) == 1,
      length(has_observation_mat) == 1
    )
  ) &
  nfile > 1
){
  return("CPW_WAREHOUSE")
}

if(
  length(grep("scut|sewa|lbca", x)) == 1 &
  all(
    c(
      length(has_site_locations) == 0,
      length(has_observation_mat) == 0
    )
  ) &
  nfile > 1
){
  return("PRECLEANED")
}

# if neither of these are true then the data are from some other source
return("UNKNOWN")
}
