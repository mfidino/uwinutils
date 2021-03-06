library(uwinutils)
library(lubridate)
library(lutz) # lookup timezone!

connect2db()

# Check what the UTC time should be
#  This is needed if you do not have the dates in UTC
#  Check to see what the time zone should be. You can use
#  the UWIN web app for that.
b2utc(
  lubridate::ymd_hms(
    "2018-09-27 17:03:00",
    tz = "US/Eastern"
  )
)


# whether you already know what the time should be in UTC
have_as_utc <- TRUE
new_datetime <- "2020-03-24 20:47:00" # PUT THE TIME HERE as ymd_hms
#my_visitid <- 13385

# Change from first photo (FALSE) or last photo (TRUE)
from_last_photo <- TRUE

# SOME NOTES. You'll want to figure out the time difference in the tz
#  of the location. You then add that to the UTC time to get the
#  correct time difference. If you do it all in UTC it appears that
#  the computer will force a UTC shift in time.

# This code is wonky, you'll need to go through it the next time this occurs.

# You just need the visitID and the area abbreviation. you can
#  of course add more here if you want.
tmp_qry <- "
SELECT cl.locationAbbr, ph.photoDateTime, ph.photoName, sa.defaultTimeZone, vi.visitDateTime FROM Photos ph
INNER JOIN Visits vi ON vi.visitID = ph.visitID
INNER JOIN CameraLocations cl ON cl.locationID = vi.locationID
INNER JOIN StudyAreas sa ON sa.areaID = cl.areaID
WHERE sa.areaAbbr = 'PACA'
AND ph.photoName BETWEEN 'VID10286-00006.jpg'  AND 'VID10286-00089.jpg'  "

occ <- uwinutils::SELECT(tmp_qry)

# the output from above assumes a timezone associated to your
#  device, which is not very great. We change everything
#  BACK to UTC here.
occ$photoDateTime <- lubridate::with_tz(
  occ$photoDateTime,
  "UTC"
)
# collect just the date
occ$date <- as.Date(
  occ$photoDateTime,
  format = "%Y-%M-%d"
)
# convert visitDateTime as well
occ$visitDateTime <- lubridate::with_tz(
  occ$visitDateTime,
  "UTC"
)

# QUERY DOWN TO THE IMAGES YOU NEED CHANGED.
# If not need to do further queries create an object
#  call to_change
to_change <- occ

if(!have_as_utc){
# Here is our trick to figure out what the correct end time should
#  be for the LAST Photo. First make sure there is one
#  default time zone
  if(
    length(
      unique(
        occ$defaultTimeZone
      )
    ) != 1){
    stop("More than one default time zone.")
  }
  # this a date time object
  tmp_date <- lubridate::ymd_hms(
    new_datetime
  )
  # apply time zone
  photo_time <- lubridate::with_tz(
    tmp_date,
    unique(occ$defaultTimeZone)
  )

# Force it to be UTC without allowing the offset
photo_time_utc <- lubridate::force_tz(
  photo_time,
  "UTC"
)

} else {
  photo_time_utc <- lubridate::with_tz(
      lubridate::ymd_hms(
        new_datetime
      ),
    "UTC"
  )
}

diff_loc <- ifelse(
  from_last_photo,
  nrow(to_change),
  1
)
# How many minutes do we add?
to_add <- difftime(
  to_change$photoDateTime[diff_loc],
  photo_time_utc,
  units = "mins") %>%
  as.integer %>%
  abs

# check if we add or subtract
if(photo_time_utc < to_change$photoDateTime[diff_loc]){
  to_add <- to_add * -1
}

# do a little test to make sure it is correct
to_change$time_update <- to_change$photoDateTime +
  lubridate::minutes(to_add)

# this is just a little smoketest to make sure things are right
big_test <- paste0(
  "SELECT ph.photoName, ph.photoDateTime, DATE_ADD(ph.photoDateTime, INTERVAL ", to_add, " MINUTE ) AS newdate FROM Photos ph\n",
  "WHERE ph.photoName = '", to_change$photoName[diff_loc],"'"
)

smoketest <- uwinutils::SELECT(
  big_test
)

smoketest$newdate <-  lubridate::with_tz(
  smoketest$newdate,
  "UTC"
)
# take a little look to make sure it is okay
smoketest
new_datetime

# if we are good move on to do the sql update. This sets up our
#  IN SQL statement
in_qry <- uwinutils::sql_IN(
  to_change$photoName
)

# and here is the query
to_mod <- paste0(
  "UPDATE Photos\n",
  "SET photoDateTime = DATE_ADD(photoDateTime, INTERVAL ", to_add, " MINUTE)\n",
  "WHERE photoName IN ", in_qry, ";"
  )

uwinutils::MODIFY(
  to_mod,
  TRUE
)

rm(new_datetime)
# check to see if you need to update the first and last photo
#  date
y <- SELECT(
  paste0(
    "SELECT * FROM Photos ph WHERE ph.visitID = ", my_visitid,";"
  )
)

y$photoDateTime <- lubridate::with_tz(
  y$photoDateTime,
  "UTC"
)

new_range <- range(y$photoDateTime)

to_comp <- SELECT(
  paste0(
    "SELECT * FROM Visits vi WHERE vi.visitID = ", my_visitid,";"
  )
)
to_comp$visitDatetime <- lubridate::with_tz(
  to_comp$visitDatetime,
  "UTC"
)
if(to_comp$firstPhotoDate != new_range[1] ){
  to_up <- paste0(
    "UPDATE Visits\n",
    "SET firstPhotoDate = '", new_range[1],"'\n",
    "WHERE visitID = ", my_visitid,";"
  )
  MODIFY(to_up, TRUE)
}

if(to_comp$lastPhotoDate != new_range[2] ){
  to_up2 <- paste0(
    "UPDATE Visits\n",
    "SET lastPhotoDate = '", new_range[2],"'\n",
    "WHERE visitID = ", my_visitid,";"
  )
  MODIFY(to_up2, TRUE)
}

