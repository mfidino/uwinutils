% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/report_functions.R
\name{progress_of}
\alias{progress_of}
\title{The progress made for each season's worth of data}
\usage{
progress_of(studyArea = NULL, db = uwidb)
}
\arguments{
\item{studyArea}{The four letter capitalized study area abbreviation for a
city. If left as \code{NULL} you can select the study area from a pop up
list. Only one study area may be selected at a time.}

\item{db}{The MySQL connection to the UWIN database. Defaults to 'uwidb'}
}
\value{
a list with the following elements:\cr\cr
 - \code{assignedIncomplete} (data.frame): This is a report on photo groups
     that have been assigned but have yet to be finished.
     The columns in \code{assingedIncomplete} are:
     \itemize{
       \item User: The name of the person who has photo groups to classify.
       \item email: The users email.
       \item yearMonth: The year and month of visits in the database that have photos.r
       \item countAssignedIncomplete: The number of photo groups assigned to a user
       per yearMonth that have not been completed.
       }
 - \code{fullComplete} (data.frame): This is a progress report on the images that have been
     classified as 'complete' in the cloud database, which depends on how many
     users are necessary to consider an image 'complete' (either one or two,
     depending on a study area). The columns in \code{fullComplete} are:
     \itemize{
     \item yearMonth: The year and month of visits in the database that have photos.
     \item percentComplete: The percent of images in a yearMonth that
       are considered 100% complete (i.e., double validated).
     \item TotalPhotos: The number of photos tied to a yearMonth.
     \item NotAssigned: Photos in a yearMonth that have not been assigned
       to a user to tag.
     \item ToValidate: The number of photos that need to go through Validation
       per yearMonth. The Valdidation is on the UWIN cloud db.
     }
 - \code{pendingComplete} (data.frame): This is a progress report on the images that have
     been tagged by at least one user on the cloud db, which could be used
     to generate an occupancy query (assuming that 1 viewer is okay).
     The columns in \code{pendingComplete} are:
     \itemize{
     \item yearMonth: The year and month of visits in the database that have photos.
     \item percentComplete: The percent of images in a yearMonth that
       have been tagged at least once.
     \item TotalPhotos: The number of photos tied to a yearMonth.
     \item NotAssigned: Photos in a yearMonth that have not been assigned
       to a user to tag.
     }
}
\description{
\code{progress_of} queries the UWIN database for a selected study area.
}
\examples{
\dontrun{
my_images <- progress_of("CHIL")
}
}
