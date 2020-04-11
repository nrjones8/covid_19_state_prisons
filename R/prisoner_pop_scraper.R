source(here::here("R/utils.R"))
# Oregon ------------------------------------------------------------------

clean_oregon <- function() {
  setwd(here::here("data/raw_data"))
  # areas <- locate_areas("oregon_offender_population_trends.pdf",
  #                       pages  = 17)
  areas <- c(73.05675, 154.07434, 604.81409, 640.32096)
  names(areas) <- c("top", "left", "bottom", "right")
  areas <- list(areas)
  out <- extract_tables("oregon_offender_population_trends.pdf",
                        output = "data.frame",
                        method = "lattice",
                        pages = 6:17,
                        area = rep(areas, 12))
  for (i in 1:length(out)) {
    temp <- out[[i]]
    temp <-
      temp %>%
      mutate_all(as.character)
    out[[i]] <- temp
  }
  
  genders <- c("female", "male", "total")
  data <- do.call("bind_rows", out)
  names(data) <- c("date",
                   paste0("prison_", genders),
                   paste0("local_control_", genders),
                   paste0("parole_pps_", genders),
                   paste0("probation_", genders))
  data$date <- myd(paste0(data$date, "01"))
  data <- 
    data %>%
    mutate_if(is.character, readr::parse_number)
  
  return(data)
}



# California --------------------------------------------------------------
setwd(here::here("data/raw_data/california"))
files <- list.files()

for (file in files) {
   # areas <- locate_areas(files[1],
   #                       pages  = 2)
  areas <- c(166.38655, 33.12604, 648.90756, 575.54623)
  names(areas) <- c("top", "left", "bottom", "right")
  areas <- list(areas)
  out_page2 <- extract_tables(files[10],
                        output = "data.frame",
                        area = areas,
                        pages = 2)
  
  
  areas <- locate_areas(files[1],
                        pages  = 1)
  areas <- c(125.58568,  28.49612, 565.13555, 601.73406 )
  names(areas) <- c("top", "left", "bottom", "right")
  areas <- list(areas)
  out_page1 <- extract_tables(files[10],
                        output = "data.frame",
                        method = "stream",
                        area = areas, 
                        pages = 1)
}





# Iowa --------------------------------------------------------------------
page <-
  xml2::read_html("https://doc.iowa.gov/daily-statistics") %>%
  html_nodes("#block-system-main > table:nth-child(2) > tbody:nth-child(2)") %>%
  html_text()


