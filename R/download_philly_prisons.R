# library(lubridate)
# library(here)
# setwd("C:/Users/user/Dropbox/R_project/jails/data/raw/philly_prison")
# files <- list.files()
# if (!paste0(lubridate::today()-1, "_philly_prison_census.pdf") %in% files) {
#   download.file("https://www.phila.gov/prisons/PDF/census.pdf",
#                 destfile = paste0(lubridate::today()-1, "_philly_prison_census.pdf"),
#                 mode = "wb")
#   Sys.sleep(10)
# }
