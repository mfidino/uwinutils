# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


#' Connect to UWIN database
#'
#' \code{connect2db} connects to the UWIN camera trapping database. It takes
#' no arguments but will request for the password to the UWIN database when
#' a connection is attempted.
#'
#' @return If the password is correctly input \code{connect2db} will return
#' a \code{MariaDBConnection} called \code{uwidb.}
#'
#' @example
#'
#' \dontrun{
#' connect2db()
#' }
connect2db <- function(){
  uwidb <- dbConnect(MariaDB(),
            user = 'root',
            password = rstudioapi::askForPassword(prompt = 'Input UWIDB password'),
            host = '35.188.69.174',
            port = 3306,
            database = 'UWIDB',
            group = 'UWIDB')
  assign('uwidb', uwidb, envir = globalenv())
}

