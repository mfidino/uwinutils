library(uwinutils)
library(lubridate)
library(lutz) # lookup timezone!

connect2db()

# whether you already know what the time should be in UTC
have_as_utc <- FALSE
new_datetime <- # PUT THE TIME HERE as ymd_hms

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
WHERE sa.areaAbbr = 'SLMO'
AND vi.visitID = 10984
AND ph.photoName BETWEEN 'VID10984-00006.jpg'  AND 'VID10984-0009.jpg'  "

occ <- uwinutils::SELECT(tmp_qry)

# the output from above assumes a timezone associated to your
#  device, which is not very great. We change everything
#  BACK to UTZ here.
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
to_change <- occ[which(year(occ$date) == 2017),]

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

# How many minutes do we add?
to_add <- difftime(
  to_change$photoDateTime[nrow(to_change)],
  photo_time_utc,
  units = "mins") %>%
  as.integer %>%
  abs

# check if we add or subtract
if(photo_time_utc < to_change$photoDateTime[nrow(to_change)]){
  to_add <- to_add * -1
}

# do a little test to make sure it is correct
to_change$time_update <- to_change$photoDateTime +
  lubridate::minutes(to_add)

# this is just a little smoketest to make sure things are right
big_test <- paste0(
  "SELECT ph.photoName, ph.photoDateTime, DATE_ADD(ph.photoDateTime, INTERVAL ", to_add, " MINUTE ) AS newdate FROM Photos ph\n",
  "WHERE ph.photoName = '", to_change$photoName[nrow(to_change)],"'"
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

