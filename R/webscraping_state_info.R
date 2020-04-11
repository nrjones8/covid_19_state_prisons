source(here::here("R","utils.R"))
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

get_connecticut_covid_data(data_for_use[[4]])
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
get_idaho_covid_data <- function(idaho_doc_path) {
  # extracts the text for idaho
  idaho_text <- idaho_doc_path %>%
    html_nodes(".covid-table:nth-child(2) td") %>%
    html_text()
  # make the numbers and split them so that they turn into 4 columns
  idaho_data <- as_tibble(split(idaho_text[5:8], 1:4))
  # overwrite the names of the tibble
  names(idaho_data) <- idaho_text[1:4]
  # get the labels in for the totals
  idaho_data %>%
    mutate(state = "Idaho", scrape_date = today())
}

get_idaho_covid_data(data_for_use[[7]])

# illinois covid data
get_illinois_covid_data <- function(il_doc_path) {
  illinois_text <- il_doc_path %>%
    html_nodes(
      ".soi-rteTable-1:nth-child(2) .soi-rteTableOddCol-1 , .soi-rteTable-1:nth-child(2) .soi-rteTableEvenCol-1 , .soi-rteTable-1:nth-child(2) .soi-rteTableHeaderOddCol-1 , .soi-rteTable-1:nth-child(2) .soi-rteTableHeaderEvenCol-1"
    ) %>%
    html_text()
  names_of_df <- illinois_text[1:3]
  illinois_data <- illinois_text[4:length(illinois_text)] %>%
    split(1:3) %>%
    as_tibble()
  names(illinois_data) <- names_of_df
  illinois_data %>% 
    modify_at(2:3,parse_number)
}

get_illinois_covid_data(data_for_use[[8]])
#pa data
#
get_pa_covid_data <- function(pa_covid_doc_path){
  pa_text <- pa_covid_doc_path %>%
    html_nodes(
      "p+ .ms-rteTable-default .ms-rteTableOddRow-default .ms-rteTableOddCol-default , .ms-rteTableOddRow-default strong , p+ .ms-rteTable-default .ms-rteTableOddRow-default .ms-rteTableEvenCol-default , p+ .ms-rteTable-default .ms-rteTableOddRow-default+ .ms-rteTableEvenRow-default .ms-rteTableOddCol-default , p+ .ms-rteTable-default .ms-rteTableOddRow-default+ .ms-rteTableEvenRow-default .ms-rteTableEvenCol-default"
    ) %>%
    html_text() %>%
    str_trim()
  #create a tibble from the data
  pa_data <- pa_text[11:length(pa_text)] %>%
    split(1:5) %>%
    as_tibble()
  # names of the dataframe
  names(pa_data) <-
    c(
      "locations",
      "employee_positive",
      "employee_negative",
      "inmate_positive",
      "inmate_negative"
    )
  pa_data <- pa_data %>%
    modify( ~ replace(., str_length(.) == 1, NA)) %>%
    mutate(scrape_date = today())
  # parse the columns
  pa_data %>%
    modify_at(2:5,  ~ parse_number(.))
}

