library(uwinutils)

connect2db()

# for right now we are just going to compare to the 'last' time
#  as this is the current request.
city <- "WIDE"
date_to_compare <- "2020-04-01 00:00:00"


# first off we need to check for any duplicate entries in
#  the VisitLures table.

ogvl <- SELECT("SELECT * FROM VisitLures")

# check for duplicates

weirdness <- ogvl[which(duplicated(ogvl$visitID)),]

# This will pull the odd duplicated issues
vi_err <- paste0(
  "SELECT * FROM VisitLures vl\n",
  "INNER JOIN Visits vi ON vi.visitID = vl.visitID\n",
  "INNER JOIN CameraLocations cl ON cl.locationID = vi.locationID\n",
  "INNER JOIN StudyAreas sa ON sa.areaID = cl.areaID\n",
  "WHERE vi.visitID IN", sql_IN(weirdness$visitID, FALSE)
)

longshot <- try(SELECT(vi_err), TRUE)

if(!class(longshot) == 'try-error'){
  stop("You need to fix duplicate entry stuff in lure table.")
# get just the columns we need
longshot <- longshot[,c("areaAbbr", "visitID", "lureID", "visitDatetime")]

# split by the area abbreviation
longshot <- split(longshot, factor(longshot$areaAbbr))
# Check to see what no lure is

SELECT("SELECT * FROM LureLookup")

# 2 is 'No Lure'

ogvl <- SELECT("SELECT * FROM VisitLures")
# do the update

# check for duplicates

weirdness <- ogvl[which(duplicated(ogvl$visitID)),]


# get extra info for these
to_fix <- data.frame(
  abbr = c("AUTX", "CHIL", "JAMS", "ATGA", "LBCA", "SLMO", "HOTE", "ININ"),
  lure = c(F, F, F, T, T, T, F, F),
  dtime = c(
    "2019-10-01 00:00:00",
    "2019-10-01 00:00:00",
    "2010-01-01 00:00:00",
    NA,
    NA,
    NA,
    "2020-01-01 00:00:00",
    "2019-12-01 00:00:00"
  ),
  stringsAsFactors = FALSE
)

for(i in 1:nrow(to_fix)){
  tmp <- to_fix[i,]
  # if lure stopped, remove those after dtime
  if(!tmp$lure){
    tq <- paste0(
      "DELETE vl\n",
      "FROM VisitLures vl\n",
      "INNER JOIN Visits vi ON vi.visitID = vl.visitID\n",
      "INNER JOIN CameraLocations cl ON cl.locationID = vi.locationID\n",
      "INNER JOIN StudyAreas sa ON sa.areaID = cl.areaID\n",
      "WHERE sa.areaAbbr = '", tmp$abbr,"'\n",
      "AND vi.visitDatetime > '", tmp$dtime,"'\n",
      "AND vl.lureID = 1\n",
      "AND vl.visitID IN ",
      sql_IN(
        unique(longshot[[tmp$abbr]]$visitID),
        FALSE
        ), ";")

    MODIFY(tq, TRUE)

    # then drop the no lure ones from before that time
    tq2 <- paste0(
      "DELETE vl\n",
      "FROM VisitLures vl\n",
      "INNER JOIN Visits vi ON vi.visitID = vl.visitID\n",
      "INNER JOIN CameraLocations cl ON cl.locationID = vi.locationID\n",
      "INNER JOIN StudyAreas sa ON sa.areaID = cl.areaID\n",
      "WHERE sa.areaAbbr = '", tmp$abbr,"'\n",
      "AND vi.visitDatetime < '", tmp$dtime,"'\n",
      "AND vl.lureID IN (2)\n",
      "AND vl.visitID IN ",
      sql_IN(
        unique(longshot[[tmp$abbr]]$visitID),
        FALSE
      ), ";")

    MODIFY(tq2, TRUE)
  }

  # If lure is always deployed then go ahead and remove the
  #  ones that are no lure
  if(tmp$lure){
    tq3 <- paste0(
      "DELETE vl\n",
      "FROM VisitLures vl\n",
      "INNER JOIN Visits vi ON vi.visitID = vl.visitID\n",
      "INNER JOIN CameraLocations cl ON cl.locationID = vi.locationID\n",
      "INNER JOIN StudyAreas sa ON sa.areaID = cl.areaID\n",
      "WHERE sa.areaAbbr = '", tmp$abbr,"'\n",
      "AND vl.lureID IN (2)\n",
      "AND vl.visitID IN ",
      sql_IN(
        unique(longshot[[tmp$abbr]]$visitID),
        FALSE
      ), ";")

    MODIFY(tq3, TRUE)
  }
}
}

# now that this is fixed it should be pretty easy to update
#  the lures


to_update <- paste0(
  "UPDATE VisitLures \n",
  "INNER JOIN Visits vi ON vi.visitID = VisitLures.visitID\n",
  "INNER JOIN CameraLocations cl ON cl.locationID = vi.locationID\n",
  "INNER JOIN StudyAreas sa ON sa.areaID = cl.areaID\n",
  "SET lureID = 2\n",
  "WHERE sa.areaAbbr = '", city,"'\n",
  "AND vi.visitDatetime > '", date_to_compare,"';"
)

cat(to_update)

MODIFY(to_update, TRUE)



