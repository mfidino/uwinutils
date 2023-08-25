library(uwinutils)
library(lubridate)
connect2db()

# step 1. pull first and last image from each visitID
ph <- SELECT(
  "SELECT DISTINCT ph.visitID, MIN(ph.photoDateTime) AS min_date, MAX(ph.photoDateTime) AS max_date FROM Photos ph GROUP BY ph.visitID "
)

ph_areas <- SELECT(
  "
  select distinct ph.visitID, ph.areaID, sa.areaAbbr from Photos ph
  inner join StudyAreas sa on sa.areaID = ph.areaID
  "
)
ph <- dplyr::inner_join(
  ph,
  ph_areas,
  by = "visitID"
)


ph$min_date <- lubridate::with_tz(
  ph$min_date,
  "UTC"
)
ph$max_date <- lubridate::with_tz(
  ph$max_date,
  "UTC"
)

# check distance between min and max date to weed them out.
time_diff <- ph$max_date - ph$min_date
units(time_diff) <- "weeks"

ph$time_diff <- as.numeric(time_diff)
hist(as.numeric(time_diff))

weird <- ph[time_diff>100,]
weird[order(weird$time_diff),]

more_dat <- SELECT(
  paste0(
    "select cl.locationID, cl.locationAbbr, vi.visitID, vi.visitDatetime, vi.activeStart, vi.activeEnd from CameraLocations cl\n",
    "inner join Visits vi ON vi.locationID = cl.locationID\n",
    "where vi.visitID IN ", sql_IN(weird$visitID)
  )
)

weird <- dplyr::inner_join(
  weird,
  more_dat,
  by = "visitID"
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
      yo_before[,c("locationID","visitDatetime",  "min_date", "activeStart", "max_date", "activeEnd", "visitID")],
      by = "locationID"
    )
   if(nrow(tmp2)>0){
     tmp2$issue <- "min_date earlier than activeStart"
     tmp2$weeks_between_min_max_date <- tmp2$max_date -
       tmp2$min_date
     units(tmp2$weeks_between_min_max_date) <- "weeks"
   }
  } else {
    tmp2 <- matrix(nrow = 0, ncol = 1)
  }
  if(any(tmp$locationID %in% yo_after$locationID)){
    tmp3 <- dplyr::inner_join(
      tmp[,c("locationID","areaAbbr", "locationAbbr")],
      yo_after[,c("locationID", "visitDatetime","min_date", "activeStart", "max_date", "activeEnd", "visitID")],
      by = "locationID"
    )
    if(nrow(tmp3)>0){
      tmp3$weeks_between_min_max_date <- tmp3$max_date -
        tmp3$min_date
      units(tmp3$weeks_between_min_max_date) <- "weeks"
    }
    if(nrow(tmp2)>0){
      if(any(tmp3$visitID %in% tmp2$visitID)){
        to_update <- which(tmp2$visitID %in% tmp3$visitID)
        tmp2$issue[to_update] <- paste0(
          tmp2$issue[to_update], " & max_date later than activeEnd"
        )
        tmp3 <- tmp3[-which(tmp3$visitID %in% tmp2$visitID),]
      }
    }
    if(nrow(tmp3)>0){
      tmp3$issue <- "max_date later than activeEnd"
    }
  }else{
    tmp3 <- matrix(nrow = 0, ncol =1)
  }
    if(nrow(tmp3)>0 & nrow(tmp2)>0){
      tmp_both <- dplyr::bind_rows(
        list(tmp2, tmp3)
      )
    }
    if(nrow(tmp2)>0 & nrow(tmp3)==0){
      tmp_both <- tmp2
  }
  if(nrow(tmp3)>0 & nrow(tmp2)==0){
    tmp_both <- tmp3
  }
  if(any(tmp_both$areaAbbr %in% weird$areaAbbr)){
    tmp_weird <- weird[weird$areaAbbr %in% tmp_both$areaAbbr,]
    if(nrow(tmp_both)>0){
      if(any(tmp_weird$visitID %in% tmp_both$visitID)){
      to_update <- which(tmp_both$visitID %in% tmp_weird$visitID)
      tmp_both$issue[to_update] <- paste0(
        tmp_both$issue[to_update], " & camera deployed for > 100 weeks"
      )
      tmp_weird <- tmp_weird[-which(tmp_weird$visitID %in% tmp_both$visitID),]
      }
    }
    if(nrow(tmp_weird)>0){
      tmp_weird$issue <- "camera deployed for > 100 weeks"
      colnames(tmp_weird) <- gsub(
        "time_diff",
        "weeks_between_min_max_date",
        colnames(tmp_weird)
      )
      tmp_weird <- tmp_weird[,colnames(tmp_both)]
      tmp_both$weeks_between_min_max_date <- as.numeric(
        tmp_both$weeks_between_min_max_date
      )
      tmp_both <- dplyr::bind_rows(
        list(tmp_both, tmp_weird)
      )
    }
  }
    if(nrow(tmp_both)>0){
      write.csv(
        tmp_both,
        paste0(
          "./active_date_check/",
          names(locs[i]), ".csv"
        ),
        row.names = FALSE
      )
    }
  if(exists("tmp2")){
    rm(tmp2)
  }
  if(exists("tmp3")){
    rm(tmp3)
  }
  if(exists("tmp_both")){
    rm(tmp_both)
  }
  if(exists("tmp_weird")){
    rm(tmp_weird)
  }
}


