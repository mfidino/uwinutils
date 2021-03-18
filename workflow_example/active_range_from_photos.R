############################
#
# This script takes some visitIDs and then will collect
#  the date range of Photos from the Photos table and
#  input them as the activeStart and activeEnd in the visits
#  table. It also updates the first and last images columns
#  in the visits table as well.
#
# This should get used when the active date range is not fully
#  reflecting reality (there are images that should be queried
#  when for some reason they are not).
############################

my_ids <- c(
# put ID's here
)


for(i in 1:length(my_ids)){
  # get current visits
  tmp1 <- SELECT(
    paste0(
      "SELECT * FROM Visits vi WHERE vi.visitID = ", my_ids[i],";"
    )
  )
  # get photos
  tmp2 <- SELECT(
      paste0(
        "SELECT * FROM Photos ph WHERE ph.visitID = ", my_ids[i],";"
    )
  )
  # convert to UTC
  tmp2$photoDateTime <- b2utc(tmp2$photoDateTime)
  # get range of photodates
  my_range <- range(tmp2$photoDateTime)
  to_up <- paste0(
    "UPDATE Visits\n",
    "SET activeStart = '", my_range[1], "', ",
    "activeEnd = '", my_range[2], "', ",
    "firstPhotoDate = '", my_range[1], "', ",
    "lastPhotoDate = '", my_range[2], "'\n",
    "WHERE visitID = ", my_ids[i]
  )
  MODIFY(to_up, TRUE)
  rm(my_range)
  rm(to_up)
}
