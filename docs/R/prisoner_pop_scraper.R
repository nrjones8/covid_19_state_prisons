source(here::here("R/utils.R"))


# Delaware ----------------------------------------------------------------
# https://data.delaware.gov/Public-Safety/Inmate-Population/vnau-c4rn/data
delaware <- fread("https://data.delaware.gov/api/views/vnau-c4rn/rows.csv")
names(delaware) <- tolower(names(delaware))
names(delaware) <- gsub(" ", "_", names(delaware))
delaware$month <- gsub(".. - ", "", delaware$month)
# TEMPORARY - DATA ACTUALLY FROM LAST DAY OF MONTH
delaware$date <- ymd(paste0(delaware$year, delaware$month, "01"))

delaware <-
  delaware %>%
  mutate_if(is.character, tolower)

z <-
  delaware %>%
  filter(type_of_institution %in% "prison") %>%
  group_by(date) %>%
  summarize(prisoners = sum(offender_count))


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






# Michigan --------------------------------------------------------------------

clean_michigan <- function(){
# from https://www.michigan.gov/documents/corrections/MDOC_2017_Statistical_Report_644556_7.pdf
fn <- here::here("data/raw_data/michigan/MDOC_2017_Statistical_Report_644556_7.pdf")

#Admissions
t <- extract_tables(file = fn, 
                    pages = 89,
                    output = "data.frame",
                    method = "lattice") %>% 
  as_tibble(.name_repair = "unique")
  

t$...1 %>%
  as.data.frame() %>% 
  mutate(year = as.integer(str_sub(PrisonNumericalPercent, 1, 4)),
         intakes = as.double(
           gsub("\\(|,", "", 
                str_sub(PrisonNumericalPercent, 5, 10)))
         ) %>% 
  filter(between(year, 2003, 2017)) %>%
  mutate(intakes = case_when(intakes == 88801 ~ 8880,
                             intakes == 92303 ~ 9230,
                             TRUE ~ intakes)) %>% 
  select(year, intakes) -> intakes

#Year end population
t <- extract_tables(file = fn, 
                    pages = 152,
                    output = "data.frame",
                    method = "stream") %>% 
  as_tibble(.name_repair = "unique")

t$...1 %>% as.data.frame() %>%
  select(X, Year.End) %>% 
  filter(!X == "Year") %>% 
  mutate(year = as.integer(X),
         year_end_population = as.double(
           gsub(",", "", Year.End))
  ) %>% 
  filter(between(year, 2003, 2017)) %>%
  select(year, year_end_population) -> year_end


# Discharges
t <- extract_tables(file = fn, 
                    pages = 227,
                    output = "data.frame",
                    method = "stream") %>% 
  as_tibble(.name_repair = "unique")

t$...1 %>% 
  as.data.frame() %>%
  select(X, `Actual`) %>% 
  filter(!X %in% c("", "Year")) %>% 
  mutate(year = as.integer(X),
         discharged_to_parole = as.double(
           gsub(",", "", Actual))
  ) %>% 
  select(year, discharged_to_parole) -> discharges


discharges %>% 
  left_join(., intakes, by = "year") %>% 
  left_join(., year_end, by = "year") %>% 
  select(year, intakes, discharged_to_parole, year_end_population) -> mi_pop_data

 return(mi_pop_data)

}


clean_michigan()



