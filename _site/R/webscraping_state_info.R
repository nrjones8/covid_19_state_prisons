source(here::here("R", "utils.R"))
data_in_goog_sheet <-
  read_sheet(
    "https://docs.google.com/spreadsheets/d/1CwD8aie_ib1wj3FtqACK3N2xssT0W_vX3d_WkKGpdOw/edit?ts=5e90b732#gid=0"
  )
data_in_goog_sheet %>% 
  count(covid_scraped_binary)
# create a safe function
safe_get <- safely(read_html)

# scraping of webpages happens heres
url_data <- data_in_goog_sheet %>%
  filter(!is.na(covid_link)) %>%
  pull(covid_link) %>%
  map( ~ safe_get(.))
# extract results in a compact way
data_for_use <- url_data %>%
  map("result") %>%
  compact()


# Alaska ------------------------------------------------------------------

get_alaska_data <- function(alaska_doc) {
  alaska_tracker <- alaska_doc %>%
    html_nodes(".tracker_text") %>%
    html_text()
  tibble(category = alaska_tracker[seq(1, 8, 2)], people = alaska_tracker[seq(2, 8, 2)]) %>%
    mutate(state = "Alaska",
           scrape_date = today())
}
alaska_data <- get_alaska_data(data_for_use[[2]])
alaska_data
# Connecticut ------------------------------------------------------------

get_connecticut_covid_data <- function(ct_doc_path) {
  img_path <- ct_doc_path %>%
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
get_connecticut_covid_data(data_for_use[[7]])


# Delaware ----------------------------------------------------------------

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


del_data <-
  get_delaware_covid_data(data_in_goog_sheet[9, ]$covid_link)
del_data %>% 
  View()
# Georgia -----------------------------------------------------------------

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
  georgia_data %>%
    modify_at(2:3,  ~ parse_number(.)) %>%
    mutate(scrape_date = today())
}
get_georgia_covid_data(data_for_use[[10]]) %>% 
  View()




# Illinois ----------------------------------------------------------------
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
    modify_at(2:3, parse_number) %>%
    mutate(scrape_date = today())
}
get_illinois_covid_data(data_for_use[[13]])



# Pennsylvania ------------------------------------------------------------

get_pa_covid_data <- function(pa_covid_doc_path) {
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
    modify(~ replace(., str_length(.) == 1, NA)) %>%
    mutate(scrape_date = today())
  # parse the columns
  pa_data %>%
    modify_at(2:5,  ~ parse_number(.)) %>%
    mutate(scrape_date = today())
}

get_pa_covid_data(data_for_use[[37]])


# alabama -----------------------------------------------------------------
get_ala_covid_data <- function(ala_doc_path) {
  alabama_text <- ala_doc_path %>%
    html_nodes("td , th") %>%
    html_text()
  # split the text and make into a tibble
  alabama_data <- alabama_text[6:length(alabama_text)] %>%
    split(1:5) %>%
    as_tibble()
  # adjust the names of the data
  names(alabama_data) <- alabama_text[1:5]
  # tag the data as well as with the scrape date
  alabama_data %>%
    modify_at(2:4, ~parse_number(.)) %>%
    mutate(scrape_date = today())
}

# Arizona -----------------------------------------------------------------
get_arizona_covid_data <- function(az_doc_path) {
  az_text <- az_doc_path %>%
    # grabs two tables
    html_nodes("td") %>%
    html_text() %>%
    str_trim()
  #split the totals table up and rename
  az_total_data <- as_tibble(split(az_text[1:5], 1:5))
  names(az_total_data) <-
    c(
      "inmates_tested",
      "inmates_negative",
      "inmates_confirmed",
      "inmates_pending",
      "daily_total_population"
    )
  # split the facilities table up and rename
  az_facility_data <- az_text[6:length(az_text)] %>%
    split(1:6) %>%
    as_tibble() %>% 
    modify_at(2:4,~parse_number(.))
  names(az_facility_data) <-
    c(
      "Location",
      "inmates_tested",
      "inmates_negative",
      "inmates_confirmed",
      "inmates_pending",
      "daily_total_pop"
    )
  list(az_totals = az_total_data, az_facility = az_facility_data)
}


get_arizona_covid_data(data_for_use[[3]])

# Idaho -------------------------------------------------------------------
get_idaho_covid_data <- function(idaho_doc_path) {
  # extracts the text for idaho
  idaho_text <- idaho_doc_path %>%
    html_nodes(".covid-table:nth-child(2) td") %>%
    html_text()
  idaho_data <- as_tibble(split(idaho_text[5:8], 1:4))
  names(idaho_data) <- idaho_text[1:4]
  idaho_data %>%
    mutate(state = "Idaho", scrape_date = today())
}

get_idaho_covid_data(data_for_use[[12]])


# Michigan --------------------------------------------------------------------
get_mi_covid_data <- function(mi_covid_path){
  
  imgs <- mi_covid_path %>%
    html_nodes("img") 
  
  
  fn <- paste0("./data/raw_data/michigan/mi_doc_covid_", 
               format(Sys.time(), "%Y_%m_%d_%H_%M_%S")
               , ".png")
  
  imgs[8] %>%
    html_attr("src") %>%
    tibble() %>% 
    mutate(link = gsub("/48/", "/3314/", .)) %>%
    pull(link) %>% 
    download.file(., fn, mode = "wb")
  
  fn <- paste0("./data/raw_data/michigan/mi_doc_staff_covid_", 
               format(Sys.time(), "%Y_%m_%d_%H_%M_%S"),
               ".png")
  imgs[13] %>% 
    html_attr("src") %>% 
    download.file(., fn, mode = "wb")
}
get_mi_covid_data(data_for_use[[21]])



# Florida --------------------------------------------------------------
get_fl_covid_data <- function(fl_doc_path) {
  fl_data <- fl_doc_path %>%
    html_nodes("td") %>%
    html_text() %>%
    split(1:3) %>%
    as_tibble()
  names(fl_data) <-
    c("facility", "employee_positive", "inmate_positive")
  fl_data %>%
    modify_at(2:3,  ~ parse_number(.)) %>%
    mutate(scrape_date = today())
}
get_fl_covid_data(data_for_use[[9]])


# Iowa --------------------------------------------------------------------


# Kansas ------------------------------------------------------------------
get_ks_covid_data <- function(ks_doc_path) {
  data_for_ks <- ks_doc_path %>%
    html_nodes("td") %>%
    html_text() %>%
    split(1:3) %>%
    as_tibble() %>%
    modify_at(2:3,  ~ parse_number(.))

  names(data_for_ks) <-
    c("facility", "staff_positive", "inmate_positive")
  
  data_for_ks %>%
    mutate(scrape_date = today())
}
get_ks_covid_data(data_for_use[[16]])


# Louisiana ---------------------------------------------------------------

get_la_covid_data <- function(la_doc_path) {
  la_inmate_data <- la_doc_path %>%
    html_nodes(
      ".column-5 , .column-4 , #tablepress-5 .column-3 , #tablepress-5 .column-2 , #tablepress-5 .column-1"
    ) %>%
    html_text()
  column_names <- la_inmate_data[1:5]
  la_data <- la_inmate_data[6:length(la_inmate_data)] %>%
    split(1:5) %>%
    as_tibble() %>%
    modify_at(2:5,  ~ as.numeric(.))
  names(la_data) <- column_names
  la_data <- la_data %>%
    mutate(scrape_date = today())
  
  la_staff_text <- la_doc_path %>%
    html_nodes("#tablepress-4 .column-3 , #tablepress-4 .column-2 , #tablepress-4 .column-1") %>%
    html_text()
  col_staff_names <- la_staff_text[1:3]
  la_staff_data <- la_staff_text[4:length(la_staff_text)] %>%
    split(1:3) %>%
    as_tibble() %>%
    modify_at(2:3,  ~ as.numeric(.))
  names(la_staff_data) <- col_staff_names
  list(staff_data = la_staff_data, inmate_data = la_data)
}
get_la_covid_data( data_for_use[[18]])


# Minnesota ---------------------------------------------------------------

