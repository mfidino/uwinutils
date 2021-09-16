library(uwinutils)

cities_to_report <- c("JAMS")
connect2db()
for(report in 1:length(cities_to_report)){
  the_city <- cities_to_report[report]
  time_to_disconnect <- report == length(cities_to_report)
  rmarkdown::render(
    "./workflow_example/pretty_progress_report.Rmd",
    output_file = paste0(
      the_city,"_UWIN_PROGRESS_REPORT_",
      format(Sys.time(), '%d-%m-%Y'),".pdf"
    ),
    output_dir = "./workflow_example/progress_report"
  )
}




connect2db()


test <- SELECT("SELECT * FROM StudyAreas")
# do ININ

test[order(test$areaAbbr),]
the_city <- "ININ"


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
