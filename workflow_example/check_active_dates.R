library(uwinutils)
library(lubridate)
connect2db()

# step 1. pull first and last image from each visitID
ph <- SELECT(
  "SELECT DISTINCT ph.visitID, MIN(ph.photoDateTime) AS min_date, MAX(ph.photoDateTime) AS max_date FROM Photos ph GROUP BY ph.visitID "
)

ph$min_date <- lubridate::with_tz(
  ph$min_date,
  "UTC"
)
ph$max_date <- lubridate::with_tz(
  ph$max_date,
  "UTC"
)

# Step 2. pull in visits Table
vi <- SELECT(
  "SELECT * FROM Visits WHERE Visits.activeStart is not NULL"
)

# convert visitDateTime as well
vi$visitDatetime <- lubridate::with_tz(
  vi$visitDatetime,
  "UTC"
)
vi$activeStart <- lubridate::with_tz(
  vi$activeStart,
  "UTC"
)
vi$activeEnd <- lubridate::with_tz(
  vi$activeEnd,
  "UTC"
)
vi$firstPhotoDate <- lubridate::with_tz(
  vi$firstPhotoDate,
  "UTC"
)
vi$lastPhotoDate <- lubridate::with_tz(
  vi$lastPhotoDate,
  "UTC"
)

# reduce down to the photo stuff
vi <- vi[which(vi$visitID %in% ph$visitID),]

# convert everything down to date instead of datetime
vi2 <- vi
str(vi)
for(i in 1:ncol(vi2)){
  if(is.POSIXct(vi2[,i])){
    vi2[,i] <- as.Date(
      vi2[,i],
      format = "%Y-%M-%d"
    )
  }
}


ph2 <- ph
for(i in 1:ncol(ph2)){
  if(is.POSIXct(ph2[,i])){
    ph2[,i] <- as.Date(
      ph2[,i],
      format = "%Y-%M-%d"
    )
  }
}

# combine these
phvi <- dplyr::inner_join(
  vi2,
  ph2,
  by = "visitID"
)

# visits where there are images before activeStart
before_as <- phvi[
  which(phvi$min_date < phvi$activeStart),
]

head(before_as[,c("activeStart", "min_date")])

# visits where there are images after activeEnd
after_ae <- phvi[
  which(phvi$max_date > phvi$activeEnd),
]

head(after_ae[,c("activeEnd", "max_date")])

# figure out what study area they are tied to

tmp_qry <- paste0(
  "SELECT DISTINCT * FROM CameraLocations cl\n",
  "INNER JOIN StudyAreas sa ON sa.areaID = cl.areaID\n",
  "WHERE cl.locationID IN ", sql_IN(unique(c(before_as$locationID, after_ae$locationID)), FALSE)
)

locs <- SELECT(tmp_qry)

ncity <- dplyr::n_distinct(locs$areaID)

locs <- split(locs, factor(locs$areaAbbr))

# make a data.frame with these columns
#SiteName, VisitDate, FirstPhotoDate, ActiveStart,
#LastPhotoDate, ActiveEnd, nphoto
yo <- dplyr::inner_join(
  ph,
  vi,
  by = "visitID"
)

yo_before <- yo[as.numeric(row.names(before_as)),]
yo_after <- yo[as.numeric(row.names(after_ae)),]
# make a data.frame with these columns
#SiteName, VisitDate, FirstPhotoDate, ActiveStart,
#LastPhotoDate, ActiveEnd, nphoto
for(i in 1:length(locs)){
  tmp <- locs[[i]]
  if(any(tmp$locationID %in% yo_before$locationID)){
    tmp2 <- dplyr::inner_join(
      tmp[,c("locationID","areaAbbr", "locationAbbr" )],
      yo_before[,c("locationID", "min_date", "activeStart", "max_date", "activeEnd", "visitID")],
      by = "locationID"
    )
   if(nrow(tmp2)>0){
     write.csv(
       tmp2,
       paste0(names(locs)[i],"_images_before_activeStart.csv"),
       row.names = FALSE
     )
   }
    rm(tmp2)
  }
  if(any(tmp$locationID %in% yo_after$locationID)){
    tmp3 <- dplyr::inner_join(
      tmp[,c("locationID","areaAbbr", "locationAbbr")],
      yo_before[,c("locationID", "min_date", "activeEnd", "max_date", "activeEnd", "visitID")],
      by = "locationID"
    )
    if(nrow(tmp3)>0){
      write.csv(
        tmp3,
        paste0(names(locs)[i],"_images_after_activeEnd.csv"),
        row.names = FALSE
      )
    }
    rm(tmp3)
  }
}



sample(
  c("cria", "seth", "mason", "kim", "jackie", "maureen")
)

