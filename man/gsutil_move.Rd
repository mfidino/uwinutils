% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gsutil_functions.R
\name{gsutil_move}
\alias{gsutil_move}
\title{Move images between folders on Google cloud}
\usage{
gsutil_move(from = NULL, to = NULL)
}
\arguments{
\item{from}{paths of files or folders to move}

\item{to}{paths of where files should go. If \code{from} and \code{to} are
folders, this will move all files between them. If files,
then the file will be moved (which can be used to rename images if needed).}
}
\description{
\code{gsutil_move} requires paths \code{from} (where the images
are located) and \code{to} where the images should go on google cloud.
}
