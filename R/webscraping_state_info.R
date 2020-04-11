library(rvest)
library(glue)
library(tidyverse)
library(tesseract)
library(googlesheets4)
library(lubridate)
library(magick)
library(tabulizer)
library(tabulizer)
data_in_goog_sheet <- read_sheet("https://docs.google.com/spreadsheets/d/1CwD8aie_ib1wj3FtqACK3N2xssT0W_vX3d_WkKGpdOw/edit?ts=5e90b732#gid=0")
# create a safe function
safe_get <- safely(read_html)
# scraping of webpages happens heres
url_data <- data_in_goog_sheet %>%
  filter(!is.na(covid_link)) %>% 
  pull(covid_link) %>% 
  map(~safe_get(.))

# extract results in a compact way
data_for_use <- url_data %>% 
  map("result") %>% 
  compact()
# alaska data scraping
get_alaska_data <- function(alaska_doc) {
  alaska_tracker <- alaska_doc %>%
    html_nodes(".tracker_text") %>%
    html_text()
  tibble(category = alaska_tracker[seq(1, 8, 2)], people = alaska_tracker[seq(2, 8, 2)]) %>%
    mutate(state = "Alaska",
           scrape_date = today())
}

alaska_data <- get_alaska_data(data_for_use[[1]])

# get connecticut doc data
get_connecticut_covid_data <- function(ct_doc_path) {
   img_path <- data_for_use[[4]] %>%
     html_nodes("img") %>%
     `[`(3) %>%
     html_attr("src")
   table_text <- glue("https://portal.ct.gov{img_path}") %>%
     image_read() %>%
     image_scale("3000") %>%
     image_convert(type = "Grayscale") %>%
     image_trim(fuzz = 60)  %>%
     ocr() %>%
     glue() %>%
     str_extract_all(boundary("word")) %>%
     unlist()
   
   tibble(
     state = "Connecticut",
     staff_members_positive = table_text[90],
     inmates_tested  = table_text[91],
     inmates_positive = table_text[92],
     inmates_negative = table_text[93],
     scrape_date = today()
   )
}

get_connecticut_covid_data()
# extracting delaware
get_delaware_covid_data <- function(delaware_pdf_path) {
  # extract the tables from the pdf
  delaware_data <- tabulizer::extract_tables(delaware_pdf_path)
  # make the matrix into a tibble
  de_data <- delaware_data[[2]] %>%
    as_tibble()
  # change names of the tibble
  names(de_data) <- delaware_data[[1]]
  # this extracts the facilities and summarizes the data for now. 
  # currently, the facilities can be flattened. i just need to remember how to 
  # collapse the strings
  de_data %>%
    modify_at(2:4,  ~ as.numeric(.)) %>%
    summarise_at(2:4, list(total = ~ sum(., na.rm = T))) %>%
    mutate(state = "Delaware",
           scrape_date = today())
}

# use link in text format to get the delaware data  
del_data <- get_delaware_covid_data(data_in_goog_sheet[5,]$covid_link)

# get georgia data
get_georgia_covid_data <- function(georgia_doc_path) {
  georgia_text <- georgia_doc_path %>%
    html_nodes("table:nth-child(1) td") %>%
    html_text()
  # fix up the names of the data
  recovered_text <- paste(georgia_text[7:8], georgia_text[3])
  confirmed_text <- paste(georgia_text[5:6], georgia_text[2])
  names_of_df <-
    str_trim(c(georgia_text[4] , confirmed_text, recovered_text))
  georgia_data <- georgia_text[9:length(georgia_text)]
  # split into length 5 list of 20 elements each to make the 20x5 tibble
  georgia_data <- georgia_data %>%
    split(1:5)  %>%
    as_tibble()
  names(georgia_data) <- names_of_df
  georgia_data
}
get_georgia_covid_data(data_for_use[[6]])

# idaho data
idaho_text <- data_for_use[[7]] %>% 
  html_nodes(".covid-table:nth-child(2) td") %>% 
  html_text()
idaho_data <- as_tibble(split(idaho_text[5:8],1:4))
names(idaho_data) <- idaho_text[1:4]
idaho_data %>% 
  mutate(state = "Idaho",scrape_date = today())



















































