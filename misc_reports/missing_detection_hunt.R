# hunting out the missing detections

library(uwinutils)

connect2db()

# step 1. Check which detections are missing from species

dsl <- SELECT(
  paste0(
    "SELECT DISTINCT dsl.*, sp.commonName, apg.timeCompleted, pg.*, ph.photoName, de.userID\n",
    "FROM DetectionSpeciesLog dsl\n",
    "LEFT JOIN Species sp ON sp.speciesID = dsl.speciesID\n",
    "LEFT JOIN Detections de ON de.detectionID = dsl.detectionID\n",
    "LEFT JOIN Photos ph ON ph.photoName = de.photoName\n",
    "LEFT JOIN PhotoGroup pg ON pg.photoGroupID = ph.photoGroupID\n",
    "LEFT JOIN AssignedPhotoGroup apg ON apg.photoGroupID = pg.photoGroupID AND apg.userID = de.userID\n",
    "ORDER BY dsl.logDateTime DESC;"
    )
)


# step 2. check if the time completed lines up with the logDateTime

linedup <- which(dsl$logDateTime == dsl$timeCompleted)

# check if it is always before

dsl$logDateTime <- lubridate::ymd_hms(dsl$logDateTime)
dsl$timeCompleted <- lubridate::ymd_hms(dsl$timeCompleted)

before_comp <- which(dsl$logDateTime < dsl$timeCompleted)

length(before_comp) / nrow(dsl)

tail(dsl[before_comp,])

# check how much is after
after_comp <- which(dsl$logDateTime > dsl$timeCompleted)

length(after_comp) / nrow(dsl)

det <- SELECT(paste0("SELECT de.detectionID, de.valStatID, de.photoName,\n",
"ds.speciesID, ds.detailID, ds.numIndividuals, us.email\n",
"FROM Detections de\n",
"LEFT JOIN DetectionSpecies ds ON de.detectionID = ds.detectionID\n",
"LEFT JOIN Users us ON us.userID = de.userID"))

detna <- det[which(is.na(det$speciesID)),]


detna_dsl <- which(dsl$detectionID %in% detna$detectionID)

# step 2. Start inspecting why these values are missing
dsl_gone <- dsl[detna_dsl,]

dsl_gone <- dplyr::left_join(
  dsl_gone,
  detna[,c("detectionID", "valStatID", "email")],
  by = "detectionID"
)

# check if all the losses are from the online data
length(grep("-", dsl_gone$photoName)) == nrow(dsl_gone)

# check where the losses are occuring

# split on "-"
tmp <- strsplit(
  dsl_gone$photoName,
  "-"
)
# grab second piece
tmp <- sapply(
  tmp,
  "[[",
  2
)

# drop the .jpg stuff, turn to numeric
tmp <- gsub(
  "\\.jpg",
  "",
  tmp
)

tmp <- as.numeric(tmp)

dsl_gone$number <- tmp
hist(tmp)

backlog <- dsl_gone[which(is.na(tmp)),]

photos <- SELECT("SELECT * FROM Photos")
tmp2 <- strsplit(
  photos$photoName,
  "-"
)
# grab second piece
tmp2 <- sapply(
  tmp2,
  "[[",
  2
)

# drop the .jpg stuff, turn to numeric
tmp2 <- gsub(
  "\\.jpg",
  "",
  tmp2
)

tmp2 <- as.numeric(tmp2)
photos$number <- tmp2

# get only the photo groups that are in missing
photos <- photos[which(photos$photoGroupID %in% dsl_gone$photoGroupID),]
photos <- photos[-which(is.na(photos$number)),]

library(dplyr)
gsize <- photos %>% group_by(photoGroupID) %>%
  summarise(min_p = min(number),
            max_p = max(number))


dsl_gone <- left_join(dsl_gone, gsize, by = "photoGroupID")

distance_from <- dsl_gone$number - dsl_gone$min_p
distance_from <- distance_from / dsl_gone$max_p
distance_from <- round(distance_from, 2)

distance_from <- sort(table(distance_from), decreasing = TRUE)

plot(as.numeric(distance_from) ~ as.numeric(names(distance_from)), type = 'o',
     xlab = "Proportional location within a photo group", ylab = "Count of missing",
     bty = "l")

dets <- SELECT(
  paste0(
    "SELECT de.detectionID, sp.commonName FROM DetectionSpecies ds\n",
    "LEFT JOIN Detections de ON de.detectionID = ds.detectionID\n",
    "LEFT JOIN Species sp ON sp.speciesID = ds.speciesID"
  )
)
not_lost <- data.frame(t(t(sort(table(dets$commonName), decreasing = TRUE))))[,-2]
lost <- data.frame(t(t(sort(table(dsl_gone$commonName), decreasing = TRUE))))[,-2]
colnames(lost)[2] <- "lost"

lost <- left_join(not_lost, lost, by = "Var1")

lost$total_count <- rowSums(lost[,-1], na.rm = TRUE)

lost$prop_lost <- lost$lost / lost$total_count
lost$prop_lost[is.na(lost$prop_lost)] <- 0

lost <- lost[order(lost$prop_lost, decreasing = TRUE),]
lost[1:5,c(1,5)]

pg_done <- SELECT(
  paste0(
    "SELECT DISTINCT apg.photoGroupID, apg.tagIndex FROM AssignedPhotoGroup apg\n",
    "WHERE apg.completed = 1;"
  )
)
pg_done$photoGroupID <- as.character(pg_done$photoGroupID)

pg_count <- dsl_gone %>% group_by(photoGroupID, userID) %>%
  summarise(pg_count = length(photoGroupName))


pg_count$photoGroupID <- as.character(pg_count$photoGroupID)
pg_done <- left_join(pg_done, pg_count, by = "photoGroupID")

pg_done$prop_lost <- pg_done$pg_count / pg_done$tagIndex

# compare the time when something gets deleted relative to
dt_count <- data.frame(t(t(sort(table(dsl_gone$logDateTime), decreasing = TRUE))))
dt_count$Var2 <- gl(
  nrow(dt_count),
  1
)
colnames(dt_count) <- c("logDateTime", "ranked_loss", "amount_lost")

tmp <- dsl_gone
tmp$logDateTime <- as.character(tmp$logDateTime)
test <- left_join(
  tmp,
  dt_count,
  by = "logDateTime"
)

head(test[order(test$ranked_loss),])
test$logDateTime <- ymd_hms(test$logDateTime)

test$dac <- test$logDateTime > test$timeCompleted
test <- test[order(test$ranked_loss),]

big1 <- test[which(test$ranked_loss == "6"),]

big1 <- big1[order(big1$detectionID, na.last = NA),]

tail(big1[, c(
  "logDateTime",
  "timeCompleted",
  "dac",
  "detectionID",
  "commonName",
  "number",
  "photoGroupID")
)

test %>%

dsl_gone$dac <- dsl_gone$logDateTime > dsl_gone$timeCompleted

table(dsl_gone$commonName, dsl_gone$dac)

yo <- test %>% group_by(ranked_loss) %>%
  filter(detectionID == max(detectionID))

yo$photoGroupID[1:11]

{1896, 1945, 1032, 2010,  605, 1280, 1609, 1940, 1954, 2010, 1172}

big_issues <- yo$photoGroupID[1:11]


hm <- SELECT(
  paste0(
    "SELECT de.valStatID, de.photoName, de.userID, ph.photoGroupID, ds.* FROM Detections de\n",
    "LEFT JOIN Photos ph ON ph.photoName = de.photoName\n",
    "LEFT JOIN DetectionSpecies ds ON ds.detectionID = de.detectionID\n",
    "WHERE ph.photoGroupID IN (", paste(big_issues, collapse = ", "),");"
  )
)

edit(hm[order(hm$photoGroupID),])
