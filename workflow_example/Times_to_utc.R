library(uwinutils)

connect2db()

# the study area and where the data is at.
sa <- "LBCA"
the_files <- "../uwin-api/csv-data/lbca/"

the_tz <- SELECT(
  paste0(
    "SELECT defaultTimeZone FROM StudyAreas\n",
    "WHERE StudyAreas.areaAbbr = ",
    "'",sa,"';"
  )
)


fps <- list.files(the_files, full.names = TRUE)
fps2 <- list.files(the_files)
tmp <- lapply(fps, read.csv, stringsAsFactors = FALSE)
names(tmp) <- fps2

# update visits stuff
vi_dt <- paste(
  tmp$Visits.csv$VisitDate,
  tmp$Visits.csv$VisitTime
)

vi_dt <- lubridate::mdy_hms(vi_dt, tz = the_tz$defaultTimeZone)

# convert to utc

vi_utc <- b2utc(vi_dt)

vi_date <- as.Date(vi_utc)

# change format back to what it was
ack <- sapply(
  strsplit(as.character(vi_date), "-"),
  function(x) paste0(
    as.numeric(x[2]),
    "/",
    as.numeric(x[3]),
    "/",
    x[1]
  )
)

vi_time <-   format(vi_utc, format = "%H:%M:%S")

# add them back in
tmp$Visits.csv$VisitDate <- ack
tmp$Visits.csv$VisitTime <- vi_time

# now do active start and end
astart <- lubridate::mdy_hm(
  tmp$Visits.csv$ActiveStart,
  tz = the_tz$defaultTimeZone
)


astart <- b2utc(astart)
atime <- format(astart, format = "%H:%M:%S")
aday <- as.Date(astart)
aday <- sapply(
  strsplit(as.character(aday), "-"),
  function(x) paste0(
    as.numeric(x[2]),
    "/",
    as.numeric(x[3]),
    "/",
    x[1]
  )
)

astart <- paste(aday, atime)

astart[grep("NA", astart)] <- NA

aend <- lubridate::mdy_hm(
  tmp$Visits.csv$ActiveEnd,
  tz = the_tz$defaultTimeZone
)

aend <- b2utc(aend)
etime <- format(aend, format = "%H:%M:%S")
eday <- as.Date(aend)
eday <- sapply(
  strsplit(as.character(eday), "-"),
  function(x) paste0(
    as.numeric(x[2]),
    "/",
    as.numeric(x[3]),
    "/",
    x[1]
  )
)

aend <- paste(eday, etime)

aend[grep("NA", aend)] <- NA



tmp$Visits.csv$ActiveStart[!is.na(astart)] <- astart[!is.na(astart)]
tmp$Visits.csv$ActiveEnd[!is.na(aend)] <- aend[!is.na(aend)]


# Now do photos

pdt <- lubridate::mdy_hm(
  tmp$Photos.csv$ImageDate,
  tz = the_tz$defaultTimeZone
)

pdt <- b2utc(pdt)

pday <- as.Date(pdt)
ptime <- format(pdt, "%H:%M:%S")

pday <- sapply(
  strsplit(as.character(pday), "-"),
  function(x) paste0(
    as.numeric(x[2]),
    "/",
    as.numeric(x[3]),
    "/",
    x[1]
  )
)

pdt <- paste(pday, ptime)

tmp$Photos.csv$ImageDate <- pdt

write.csv(
  tmp$Visits.csv,
  fps[grep("Visits.csv", fps)],
  row.names = FALSE
)

write.csv(
  tmp$Photos.csv,
  fps[grep("Photos.csv", fps)],
  row.names = FALSE
)

