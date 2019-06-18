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
#' @importFrom rstudioapi askForPassword
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
            dbname = 'UWIDB')
  assign('uwidb', uwidb, envir = globalenv())
}

#' Apply a select query to the UWIN database.
#'
#' \code{SELECT} will send SELECT queries to the UWIN database and return
#' the output.
#'
#' @param sql A SQL statement input as a character vector to be sent to the
#'   UWIN database.
#'
#' @param db The MariaDB connection to the UWIN database. Defaults to 'uwidb'
#'
#' @return A data.frame with the output from the SQL statement.

#'
#' @example
#'
#' \dontrun{
#' my_sql <- 'SELECT * FROM Visits'
#' SELECT(my_sql)
#' }

# helper functions
SELECT <- function(sql = NULL, db = uwidb){
  if(!is.character(sql)){
    stop('sql must be a character object')
  }
  if(class(db) != 'MariaDBConnection'){
    stop('db is not the correct class, please connect to database with connect2db().')
  }
  qry <- dbSendQuery(db, sql)
  result <- dbFetch(qry)
  dbClearResult(qry)
  if(nrow(result) == 0){
    stop(paste0("The query returned 0 records\n\n", sql))
  }
  return(result)
}

update_qry <- function(sql = NULL, report = FALSE, db = uwidb){
  qry <- RMariaDB::dbSendStatement(uwidb, sql)
  if(report){
    to_report <- dbGetRowsAffected(qry)
    dbClearResult(qry)
    return(to_report)
  }
  dbClearResult(qry)
}

