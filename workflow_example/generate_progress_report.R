library(uwinutils)

connect2db()


test <- SELECT("SELECT * FROM StudyAreas")
# do ININ

the_city <- "SLMO"

ci <- progress_of(the_city)

data.frame(ci$assignedIncomplete)
data.frame(ci$fullComplete)

data.frame(ci$pendingComplete)

write.csv(
  ci$assignedIncomplete,
  paste0(the_city, "_assignedIncomplete.csv"),
  row.names = FALSE
)

write.csv(
  ci$fullComplete,
  paste0(the_city, "_fullComplete.csv"),
  row.names = FALSE
)

write.csv(
  ci$pendingComplete,
  paste0(the_city, "_pendingComplete.csv"),
  row.names = FALSE
)
