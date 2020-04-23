source(here::here("R", "utils.R"))


# create a safe function
safe_get <- safely(read_html)
# this is somewhat useful to reduce replication
make_facility_table <- function(.data,splits,cols_to_turn_numeric){
  .data %>% 
    split(splits) %>% 
    as_tibble() %>% 
    modify_at(cols_to_turn_numeric,~parse_number(.))
}


# Alaska ------------------------------------------------------------------
get_alaska_covid_data <- function(alaska_doc) {
  alaska_tracker <- alaska_doc %>%
    html_nodes(".tracker_text") %>%
    html_text()
  tibble(inmates_tested = alaska_tracker[2],
         inmates_positive = alaska_tracker[4],
         inmates_negative = alaska_tracker[6],
         inmates_pending = alaska_tracker[8]) %>%
    mutate(state = "Alaska",
           scrape_date = today()) 
}
# Connecticut ------------------------------------------------------------
# ct is broken for now because of the difficulty in extracting the text from image
# as each new image offers new challenges
# get_connecticut_covid_data <- function(ct_doc_path) {
#   img_path <- ct_doc_path %>%
#     html_nodes("img") %>%
#     `[`(3) %>%
#     html_attr("src")
#   table_text <- glue("https://portal.ct.gov{img_path}") %>%
#     image_read() %>%
#     image_scale("3000") %>%
#     image_convert(type = "Grayscale") %>%
#     image_trim(fuzz = 60)  %>%
#     ocr() %>%
#     glue() %>%
#     str_extract_all(boundary("word")) %>%
#     unlist()
#   
#   tibble(
#     state = "Connecticut",
#     staff_members_positive = table_text[90],
#     inmates_tested  = table_text[91],
#     inmates_positive = table_text[92],
#     inmates_negative = table_text[93],
#     scrape_date = today()
#   )
# }

# Delaware ----------------------------------------------------------------
get_delaware_covid_data <- function(delaware_doc_path) {
  path_to_pdf <- delaware_doc_path %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    str_subset("\\.pdf") 
  delaware_pdf_path <- glue("https://doc.delaware.gov{path_to_pdf[2]}")
  
  # extract the tables from the pdf
  delaware_data <- tabulizer::extract_tables(delaware_pdf_path)
  # make the matrix into a tibble
  de_data <- delaware_data[[2]] %>%
    as_tibble(.name_repair = "minimal")
  # change names of the tibble
  names(de_data) <- delaware_data[[1]]
  # this extracts the facilities and summarizes the data for now.
  # currently, the facilities can be flattened. i just need to remember how to
  # collapse the strings
  de_data <-
    de_data %>%
    modify_at(2:4,  ~ as.numeric(.)) %>%
    rename_all(tolower) %>%
    mutate(state = "Delaware",
           scrape_date = today())
  
  names(de_data) <- gsub(" ", "_", names(de_data))
  de_data <-
    de_data %>%
    rename(facilities                = facility,
           staff_positive            = correctional_staff,
           contract_staff_positive   = contracted_staff,
           inmates_positive          = offenders)
  return(de_data)
}

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
    make_facility_table(1:5,2:5)
  names(georgia_data) <- c("facilities","staff_positive","inmates_positive","staff_recovered","inmates_recovered")
  
  georgia_data %>%
    mutate(scrape_date = today(),
           state = "Georgia")
}



# Illinois ----------------------------------------------------------------
get_illinois_covid_data <- function(il_doc_path) {
  illinois_text <- il_doc_path %>%
    html_nodes(
      ".soi-rteTable-1:nth-child(2) .soi-rteTableOddCol-1 , .soi-rteTable-1:nth-child(2) .soi-rteTableEvenCol-1 , .soi-rteTable-1:nth-child(2) .soi-rteTableHeaderOddCol-1 , .soi-rteTable-1:nth-child(2) .soi-rteTableHeaderEvenCol-1"
    ) %>%
    html_text()
  
  illinois_data <- illinois_text %>%
    make_facility_table(1:5,2:5)
  
  names(illinois_data) <- c("facilities","staff_positive","staff_recovered","inmates_positive","inmates_recovered")
  
  illinois_data %>%
    mutate(scrape_date = today(),
           state = "Illinois")
}


# Pennsylvania ------------------------------------------------------------
get_pa_covid_data <- function(pa_covid_doc_path) {
  pa_text <- pa_covid_doc_path %>%
    html_nodes("br~ .ms-rteTable-default .ms-rteTableOddRow-default+ .ms-rteTableEvenRow-default .ms-rteTableOddCol-default , br~ .ms-rteTable-default .ms-rteTableOddRow-default+ .ms-rteTableEvenRow-default .ms-rteTableEvenCol-default , br~ .ms-rteTable-default .ms-rteTableOddRow-default .ms-rteTableOddCol-default , br~ .ms-rteTable-default .ms-rteTableOddRow-default .ms-rteTableEvenCol-default"
    ) %>% 
    html_text() %>%
    str_trim()
  #create a tibble from the data
  pa_data <- pa_text %>%
    split(1:7) %>% 
    as_tibble() %>% 
    slice(2:nrow(.))
  # names of the dataframe
  names(pa_data) <-
    c(
      "facilities",
      "staff_positive",
      "staff_negative",
      "staff_deaths",
      "inmates_positive",
      "inmates_negative",
      "inmates_deaths"
    )
  pa_data <- pa_data %>%
    modify_at(2:7,  ~ parse_number(.)) %>% 
    mutate(scrape_date = today(),
           state = "Pennsylvania")
}



# alabama -----------------------------------------------------------------
get_ala_covid_data <- function(ala_doc_path) {
  alabama_data <- ala_doc_path %>%
    html_node("table ") %>%
    html_table() 
  # adjust the names of the data
  names(alabama_data) <- c("facilities","inmates_tested","inmates_pending","inmates_positive","inmates_deaths")
  # tag the data as well as with the scrape date
  alabama_data %>%
    as_tibble() %>% 
    mutate(scrape_date = today(),
           state = "Alabama") %>% 
    modify_at(2:5,~parse_number(.))
}

# Arizona -----------------------------------------------------------------
get_arizona_covid_data <- function(az_doc_path) {
  az_text <- az_doc_path %>%
    # grabs two tables
    html_nodes("td") %>%
    html_text() %>%
    str_trim()
  # split the facilities table up and rename
  az_facility_data <- az_text[6:length(az_text)] %>%
    make_facility_table(1:6,2:6) 
  names(az_facility_data) <-
    c(
      "facilities",
      "inmates_tested",
      "inmates_negative",
      "inmates_positive",
      "inmates_pending",
      "daily_total_pop"
    )
  az_facility_data %>% 
    mutate(.,scrape_date = today(),
           state = "Arizona")
}



# Idaho -------------------------------------------------------------------
get_idaho_covid_data <- function(idaho_doc_path) {
  # extracts the text for idaho
  idaho_text <- idaho_doc_path %>%
    html_nodes(".covid-table:nth-child(2) td") %>%
    html_text()
  idaho_data <- as_tibble(split(idaho_text[5:8], 1:4))
  names(idaho_data) <- idaho_text[1:4]
  idaho_data %>%
    modify_at(1:4,~parse_number(.)) %>% 
    mutate(state = "Idaho", scrape_date = today())
}





# Federal BOP -----------------------------------------------------------------

get_federal_data <- function(federal_bop_path){
  f <- federal_bop_path %>%
    html_text() %>% 
    jsonlite::fromJSON(.)
  
  list(offenders = f[[3]],
       reentry = f[[2]],
       overall_stats = f[[1]] %>% 
         modify_at(2:5,~parse_number(.))) %>% 
    map(~mutate(., scrape_date = today(),
                state = "Federal") %>% 
          as_tibble(.) %>% 
          clean_names())
  
}

# Federal BOP -----------------------------------------------------------------
get_federal_data <- function(federal_bop_path){
  f <- federal_bop_path %>%
    jsonlite::fromJSON(.)
  
  list(offenders = f[[3]],
       reentry = f[[2]],
       overall_stats = f[[1]]) %>% 
    map(~mutate(., scrape_date = today(),
                state = "Federal"))
    
}


# Michigan --------------------------------------------------------------------
# get_mi_covid_data <- function(mi_covid_path){
#   
#   imgs <- mi_covid_path %>%
#     html_nodes("img") 
#   
#   
#   fn <- paste0("./data/raw_data/michigan/mi_doc_covid_", 
#                format(Sys.time(), "%Y_%m_%d_%H_%M_%S")
#                , ".png")
#   
#   imgs[8] %>%
#     html_attr("src") %>%
#     tibble() %>% 
#     mutate(link = gsub("/48/", "/3314/", .)) %>%
#     pull(link) %>% 
#     download.file(., fn, mode = "wb")
#   
#   fn <- paste0("./data/raw_data/michigan/mi_doc_staff_covid_", 
#                format(Sys.time(), "%Y_%m_%d_%H_%M_%S"),
#                ".png")
#   imgs[13] %>% 
#     html_attr("src") %>% 
#     download.file(., fn, mode = "wb")
# }
# get_mi_covid_data(data_for_use[[21]])



# Florida --------------------------------------------------------------
get_fl_covid_data <- function(fl_doc_path) {
  fl_data <- fl_doc_path %>%
    html_nodes(".tableHead~ tr td") %>% 
    html_text() %>% 
    str_trim() %>% 
    make_facility_table(1:8,2:8)
  names(fl_data) <-
    c("facilities","security_quarantine","medical_quarantine",
      "medical_isolation","inmates_pending","inmates_negative","inmates_positive",
      "staff_positive")
  fl_data %>%
    mutate(scrape_date = today(),
           state = "Florida")
}




# Kansas ------------------------------------------------------------------
get_ks_covid_data <- function(ks_doc_path) {
  data_for_ks <- ks_doc_path %>%
    html_nodes("td") %>%
    html_text() %>%
    split(1:5) %>%
    as_tibble() %>%
    modify_at(2:5,  ~ parse_number(.))
  
  names(data_for_ks) <-
    c("facilities", "staff_positive", "inmates_positive","staff_recovered","inmates_recovered")
  
  data_for_ks %>%
    mutate(scrape_date = today(),
           state = "Kansas")
}
# Louisiana ---------------------------------------------------------------

get_la_covid_data <- function(la_doc_path) {
  la_inmate_data <- la_doc_path %>%
    html_nodes(
      "#tablepress-5 td"  ) %>%
    html_text()
  la_data <- la_inmate_data %>%
    split(1:8) %>%
    as_tibble() %>%
    modify_at(2:8,  ~ as.numeric(.))
  names(la_data) <-
    c(
      "facilities",
      "inmates_positive",
      "inmates_current_positive",
      "inmates_step_down",
      "inmates_recovered",
      "inmates_death_underlying_cond",
      "inmates_deaths",
      "total_deaths"
    )
  
  la_staff_text <- la_doc_path %>%
    html_nodes("#tablepress-4 td") %>%
    html_text()
  la_staff_data <- la_staff_text %>%
    split(1:4) %>%
    as_tibble() %>%
    modify_at(2:4,  ~ as.numeric(.))
  names(la_staff_data) <- c("facilities","staff_positive","staff_recovered","staff_deaths")
  list( inmate_data = la_data,staff_data = la_staff_data) %>% 
    map(~mutate(.,scrape_date = today(),
                state = "Louisiana")) %>% 
    reduce(left_join)
}

# New York ----------------------------------------------------------------
# reformat Aaron's code
get_nys_covid_data <- function(nys_doc_path) {
  new_york_text <- nys_doc_path %>%
    html_nodes("td") %>%
    html_text() %>%
    str_extract_all("[:number:]") %>%
    map( ~ str_flatten(., "")) %>%
    as_tibble(.name_repair = "minimal")
  names(new_york_text) <-
    c(
      "staff_positive",
      "incarcerated_positive",
      "parolees_positive",
      "staff_deaths",
      "incarcerated_deaths",
      "parolees_deaths"
    )
  new_york_text %>%
    mutate(state = "New York",
           scrape_date = today())
}


# Ohio --------------------------------------------------------------------
# more code reformatted from Aaron
not_all_empty_char <- function(x){
  !all(trimws(x) == "")
}

get_ohio_covid_data <- function(ohio_doc_path) {
  path_to_ohio_pdf <- ohio_doc_path %>%
    html_nodes("p a")  %>% 
    html_attr("href") %>% 
    pluck(2)
  path_to_ohio_pdf <- glue("https://drc.ohio.gov{path_to_ohio_pdf}")
  scrape_table <-
    extract_tables(path_to_ohio_pdf,method = "decide",output = "matrix")
  scrape_text <- extract_text(path_to_ohio_pdf)
  
  table_cleaning <- scrape_table %>%
    map( ~ as_tibble(.) ) %>% 
    map_at(
      1,
      ~ row_to_names(., row_number = 1) %>%
        rename(
          inmates_tested = Tested,
          inmates_pending = Pending,
          inmates_positive = Positive,
          inmates_negative = Negative
        )
    ) %>%
    map_at(3,~filter_at(.,.vars = vars("V1"),~!str_detect(.,"Total"))) %>% 
    map_at(2:3,
           ~  select_if(., not_all_empty_char)) 
  #rename the 2nd table with first table names
  names(table_cleaning[[2]]) <- names(table_cleaning[[3]])

  facility_ohio <- table_cleaning[2:3] %>%
    bind_rows() %>%
    rename(
      facilities = 1,
      staff_positive = 2,
      staff_deaths = 3,
      staff_recovered = 4,
      units_quarantine = 5,
      inmates_quarantine = 6,
      housing_type = 7,
      inmates_isolation = 8,
      inmates_positive = 9,
      inmates_deaths = 10,
      inmates_covid_deaths = 11
    ) %>%
    slice(12:39) %>% 
    modify_at(c(2:4, 8:11),  ~ parse_number(.)) %>%
    filter(facilities != "Totals") %>% 
    mutate(scrape_date = today(),
           state = "Ohio")
  list(ohio_facility = facility_ohio, ohio_totals = table_cleaning[[1]])
}
# New Jersey --------------------------------------------------------------
get_nj_covid_data <- function(nj_doc_path) {
  new_jersey_text <- nj_doc_path %>%
    html_nodes(".align-text-top td , th") %>%
    html_text()
  table_1 <- new_jersey_text[1:72]
  table_2 <- new_jersey_text[73:length(new_jersey_text)]
  table_1 <- table_1[5:length(table_1)] %>%
    make_facility_table(1:4,2:4)
  
  names(table_1) <- c("facilities","staff_positive","inmates_positive","inmates_deaths")
  
  table_2 <- table_2[4:length(table_2)] %>%
    make_facility_table(1:3,2:3)
  
  names(table_2) <- new_jersey_text[73:75]
  
  list(confirmed_nj_doc = table_1,
       confirmed_halfway_house_doc = table_2) %>% 
    map(~mutate(.,scrape_date = today(),
                state = "New Jersey"))
}

# North Carolina --------------------------------------------------------------
get_nc_covid_data <- function(nc_doc_path) {
  column_totals <- nc_doc_path %>%
    html_nodes('.DPSDITDOPCovidInformation') %>%
    html_nodes('.columntotal') %>%
    html_text() %>%
    str_trim()
  # There are 8 "column total" entries; the last three are Tests Performed, Positive, Negative
  total_tests <- as.integer(column_totals[6])
  total_positives <- as.integer(column_totals[7])
  total_negatives <- as.integer(column_totals[8])
  
  tibble(inmates_positive=total_positives, inmates_negative=total_negatives, inmates_tested=total_tests) %>%
    mutate(scrape_date = today(), state = 'North Carolina')
}

# North Dakota ------------------------------------------------------------
#this function is probably the most likely to break
get_nd_covid_data <- function(nd_doc_path) {
  nd_path <- nd_doc_path %>%
    html_nodes("img.align-center") %>%
    html_attr("src")
  nd_text <- glue("https://www.docr.nd.gov/{nd_path}") %>%
    image_read() %>%
    image_convert(type = "grayscale") %>%
    image_enhance() %>%
    ocr() %>%
    str_extract_all(": -|\\d+|O") %>%
    unlist()
  nd_data <- nd_text[7:length(nd_text)] %>%
    split(1:4) %>%
    as_tibble() %>%
    mutate(
      type = c(
        "inmates_positive",
        "inmates_negative",
        "persons_under_med_investigation",
        "inmates_recovered",
        "inmates_deaths"
      )
    )
  names(nd_data) <- c("ndsp", "jrcc", "mrcc", "ycc", "type")
  nd_data %>% 
    select(type,everything()) %>% 
    t() %>% 
    as_tibble() %>% 
    row_to_names(1) %>% 
    mutate(facilities = c("ndsp", "jrcc", "mrcc", "ycc")) %>% 
    modify_at(1:5,~parse_number(.)) %>% 
    mutate(scrape_date = today(),
           state = "North Dakota")
}

# Minnesota ------------------------------------------------------------

get_minnesota_covid_data <- function(minn_doc_path) {
  mn_img_src_relative <-  minn_doc_path %>%
    html_nodes('img[title="covid testing chart"]') %>%
    html_attr('src')
  
  mn_img_src_full <- paste('https://mn.gov', mn_img_src_relative, sep='')
  mn_img <- mn_img_src_full %>% image_read()
  
  mn_img_details <- image_info(mn_img)
  # See https://cran.r-project.org/web/packages/magick/vignettes/intro.html#cut_and_edit
  # for an example of what this should look like - from the docs:
  # image_crop(image, "100x150+50"): crop out width:100px and height:150px starting +50px from the left
  #
  # Basically we want the full width, but just the bottom 30px, which contain the totals. Trying to OCR
  # the entire table was failing miserably, but this works.
  crop_str <- sprintf('%sx30+0+%s', mn_img_details$width, mn_img_details$height - 30)
  cropped <- mn_img %>% image_crop(crop_str)
  
  # The bottom row is just numbers, so only look for digits (a "4" was being interpreted as an "a" without this)
  tesseract_digit_eng <- tesseract(options = list(tessedit_char_whitelist = "0123456789"))
  mn_ocr_data <- cropped %>% tesseract::ocr_data(engine=tesseract_digit_eng)
  ocred_data <- mn_ocr_data %>% mutate(as_ints = as.integer(word)) %>% pull(as_ints)
  tibble(
    inmates_tested=ocred_data[1],
    inmates_positive=ocred_data[2],
    inmates_negative=ocred_data[3],
    inmates_pending=ocred_data[4],
    inmates_presumed_positive=ocred_data[5],
    inmates_released_medical_isolation=ocred_data[6],
    inmates_hospital=ocred_data[7],
    inmates_deaths=ocred_data[8],
    
    scrape_date = today(),
    state = 'Minnesota'
  )
}


# Vermont --------------------------------------------------------------------
get_vermont_covid_data <- function(vermont_doc_path) {
  # As of 4/20/20, there are two imgs on the page - the first one contains data about incarcerated people, the
  # second about staff
  imgs <- vermont_doc_path %>%
    html_nodes("img") %>%
    html_attr("src")
  inmate_data_img_src <- imgs[1]
  staff_data_img_src <- imgs[2]
  
  # https://stackoverflow.com/questions/44349267/r-read-inline-base64-png-image-and-parse-text
  # First 23 characters are "data:image/png;base64," - which is not actually part of the image data
  img_data <- substring(inmate_data_img_src, 23)
  decoded_img <- base64enc::base64decode(img_data)
  # Write the decoded image to a tmp file
  fconn <- file(tf <- tempfile(fileext = ".png"), "wb")
  writeBin(decoded_img, fconn)
  close(fconn)
  
  # 3 cols, "word" contains OCR'd text
  ocr_inmate_data <- tesseract::ocr_data(tf)
  just_integer_fields <- ocr_inmate_data %>% filter(grepl("^[0-9]+$", word))
  
  # First 4 fields are total tests, pos, neg, pending
  total_tests <- as.integer(just_integer_fields$word[1])
  total_positives <- as.integer(just_integer_fields$word[2])
  total_negatives <- as.integer(just_integer_fields$word[3])
  pending_results <- as.integer(just_integer_fields$word[4])
  
  currently_incarcerated_positives <- as.integer(just_integer_fields$word[5])
  # "Inmates in Medical Isolation"
  inmates_medical_isolation <- as.integer(just_integer_fields$word[6])
  inmates_released_medical_isolation <- as.integer(just_integer_fields$word[7])
  inmates_hospital <- as.integer(just_integer_fields$word[8])
  
  ##
  ## Now do similar parsing for the second image, which contains data about staff testing
  # https://stackoverflow.com/questions/44349267/r-read-inline-base64-png-image-and-parse-text
  # First 23 characters are "data:image/png;base64," - which is not actually part of the image data
  staff_img_data <- substring(staff_data_img_src, 23)
  staff_decoded_img <- base64enc::base64decode(staff_img_data)
  # Write the decoded image to a tmp file
  fconn <- file(staff_tf <- tempfile(fileext = ".png"), "wb")
  writeBin(staff_decoded_img, fconn)
  close(fconn)
  
  ocr_staff_data <- tesseract::ocr_data(staff_tf)
  words <- ocr_staff_data$word
  # The image, when read left to right, has the word "Total" before the number of total staff who
  # have tested positive
  index_of_total <- which(words == 'Total')
  num_staff_positive <- as.integer(words[index_of_total + 1])
  
  tibble(inmates_positive=total_positives,
         inmates_negative=total_negatives,
         inmates_pending=pending_results,
         inmates_tested=total_tests,
         inmates_medical_isolation=inmates_medical_isolation,
         inmates_released_medical_isolation=inmates_released_medical_isolation,
         inmates_hospital=inmates_hospital,
         staff_positive=num_staff_positive) %>%
    mutate(scrape_date = today(), state = 'Vermont')
}

# South Carolina ----------------------------------------------------------
get_sc_covid_data <- function(sc_doc_path) {
  sc_text <- sc_doc_path %>%
    html_nodes("td") %>%
    html_text()
  
  table_headers <- sc_text[1:3]
  
  sc_data <- sc_text[4:length(sc_text)] %>%
    make_facility_table(1:3,2:3)
  
  names(sc_data) <- c("facilities","staff_positive","inmates_positive")
  sc_data %>% 
    mutate(scrape_date = today(),
           state = "South Carolina")
}



# Virginia ----------------------------------------------------------------
get_virginia_covid_data <- function(virginia_doc_path) {
  virginia_text <- virginia_doc_path %>%
    html_nodes("tbody td , tbody th") %>%
    html_text()
  virginia_data <- virginia_text[6:length(virginia_text)] %>%
    make_facility_table(1:5, 2:5)
  names(virginia_data) <- c("facilities","inmates_positive","inmates_hospital","inmates_deaths","staff_positive")
  virginia_data %>% 
    mutate(scrape_date = today(),
           state = "Virginia")
}


# Washington --------------------------------------------------------------
get_washington_covid_data <- function(wash_doc_path) {
  wash_text <- wash_doc_path %>%
    html_nodes(".default-top-border+ .default-top-border td") %>%
    html_text()
  text_table_1 <- wash_text
  table_1_data <- text_table_1 %>%
    make_facility_table(1:3, 2:3)  
  names(table_1_data) <- c("facilities","staff_positive","inmates_positive")
  table_1_data %>% 
    mutate(.,scrape_date = today(),state = "Washington")
}

# Texas -------------------------------------------------------------------

get_texas_covid_data <- function(tx_doc_path) {
  tx_text <-  tx_doc_path %>%
    html_nodes("img~ .div_for_table td") %>%
    html_text()
  # this divides the columns so that things stay even
  tx_length <- length(tx_text)/5 
  # subsetting everything since there are currently 107 facilities in tx
  table_1 <- tx_text[1:tx_length]
  table_2 <- tx_text[221:(tx_length * 2)]
  table_3 <- tx_text[(tx_length * 2 + 1):(tx_length * 3)]
  table_4 <- tx_text[(tx_length * 3 + 1):(tx_length*4)]
  table_5 <- tx_text[(tx_length * 4 + 1):(tx_length *5)]
  # reducing the tables to one.
  reduced_df <- list(table_1, table_2, table_3,table_4,table_5) %>%
    map(
      ~ split(., 1:2) %>%
        as_tibble(.) %>%
        rename("facilities" = 1) %>%
        mutate(facilities = stringr::str_squish(facilities))
    ) %>%
    reduce(left_join, by = "facilities") %>%
    rename(
      "inmates_pending" = 2,
      "inmates_negative" = 3,
      "inmates_positive" = 4,
      "inmates_medical_restriction" = 5,
      "inmates_medical_isolation" = 6
    )
  reduced_df %>% 
    modify_at(2:6,~as.numeric(.)) %>%
    filter(facilities != "No Longer in Custody") %>% 
    mutate(scrape_date  = today(),
           state = "Texas")
}


# California --------------------------------------------------------------
get_california_covid_data <- function(cali_doc_path) {
  cali_emp_text <-cali_doc_path %>% 
    html_nodes("tr+ tr td") %>%
    html_text() %>%
    split(1:2) %>%
    as_tibble()
  names(cali_emp_text) <- c("facilities", "staff_positive")
  cali_emp_text %>%
    filter(!str_detect(facilities, regex("Total", ignore_case = T))) %>%
    modify_at(2,  ~ as.numeric(.)) %>% 
    mutate(scrape_date= today(),
           state = "California")
}



# Montana -----------------------------------------------------------------
get_montana_covid_data <- function(montana_doc_path) {
  data <- montana_doc_path %>%
    html_nodes("td") %>%
    html_text()
  data <- matrix(data, ncol = 2, byrow = TRUE)
  data <- data.frame(data, stringsAsFactors = FALSE)
  names(data) <- c("facilities", "inmates_positive")
  data <- data[-1, ]
  data <-
    data %>%
    filter(stringr::str_trim(inmates_positive) != "",
           facilities != "Total confirmed cases") %>%
    mutate(inmates_positive = stringr::str_trim(inmates_positive),
           facilities         = stringr::str_trim(facilities),
           state            = "Montana",
           scrape_date      = lubridate::today(),
           inmates_positive = as.numeric(inmates_positive))
  
  return(data)
}


# Iowa --------------------------------------------------------------------
get_iowa_covid_data <- function(iowa_doc_path) {
  data <- iowa_doc_path %>%
    html_nodes("td") %>%
    html_text()
  data <- matrix(data, ncol = 4, byrow = TRUE)
  data <- data.frame(data, stringsAsFactors = FALSE)
  names(data) <- c("facilities", "inmates_tested", "inmates_positive", "staff_positive")
  data <- data[-1, ]
  
  data <-
    data %>%
    mutate_all(stringr::str_trim) %>%
    filter(facilities != "Total") %>%
    mutate(state            = "Iowa",
           scrape_date      = lubridate::today(),
           inmates_positive = as.numeric(inmates_positive),
           inmates_tested   = as.numeric(inmates_tested),
           staff_positive   = as.numeric(staff_positive)) 
  return(data)
}


# Utah --------------------------------------------------------------------
get_utah_covid_data <- function(ut_doc_path) {
  data <- ut_doc_path %>%
    html_nodes("p:nth-child(16) strong") %>%
    html_text()
  data <- tibble(
    state = "Utah",
    scrape_date = lubridate::today(),
    inmates_positive = data
  )
  data$inmates_positive <- gsub(".*: ", "", data$inmates_positive)
  data$inmates_positive <- as.numeric(data$inmates_positive)
  return(data)
}


# Indiana ---------------------------------------------------------------------
get_indiana_covid_data <- function(indiana_doc_path){
  in_off_data <- indiana_doc_path %>% 
    html_node("table") %>%
    html_table() %>% 
    rename(facilities = `Correctional Facility`,
           inmates_positive = `Offender Confirmed`,
           inmates_deaths = `Offender Death`) %>% 
    filter(!grepl("Total", facilities))
  
  
  in_staff_data <- indiana_doc_path %>%
    html_nodes(xpath = '//*[@id="main"]/div/div[2]/article/div/section/p[2]/span') %>% 
    html_text() %>% 
    tibble::enframe() %>% 
    mutate(value = parse_number(value)) %>% 
    select(-name,staff_positive = value)
  
  list(offenders = in_off_data,
       staff = in_staff_data) %>% 
    map(~mutate(.,scrape_date = today(),
                state = "Indiana"))
  
}


# oregon ------------------------------------------------------------------

get_oregon_covid_data <- function() {
  library(RSelenium)
  remDr <- RSelenium::remoteDriver(
    remoteServerAddr = "localhost",
    browser = "firefox",
    port = 4445L)
  remDr$open()
  remDr$navigate("https://www.oregon.gov/doc/covid19/Pages/covid19-tracking.aspx")
  Sys.sleep(15)
  
  
  # Get table
  data <-
    read_html(remDr$getPageSource()[[1]]) %>%
    html_nodes("td") %>%
    html_text()
  
  
  # Get column names
  column_names <-
    read_html(remDr$getPageSource()[[1]]) %>%
    html_nodes(".sorting_disabled") %>%
    html_text()
  remDr$quit()
  
  column_names <- tolower(column_names)
  column_names <- gsub(" ", "_", column_names)
  
  data <- matrix(data, ncol = 3, byrow = TRUE)
  data <- data.frame(data, stringsAsFactors = FALSE)
  names(data) <- column_names
  
  data <-
    data %>%
    rename(facilities         = location,
           staff_positive   = staff_confirmed,
           inmates_positive = adults_in_custody_confirmed) %>%
    mutate(state            = "Oregon",
           scrape_date      = lubridate::today(),
           inmates_positive = as.numeric(inmates_positive),
           staff_positive   = as.numeric(staff_positive)) 
  return(data)
}

# New Hampshire -----------------------------------------------------------
get_new_hampshire_covid_data <- function(nh_doc_path) {
  nh_text <- nh_doc_path %>%
    html_nodes(
      "tr:nth-child(6) p , tr:nth-child(5) p , td td td tr:nth-child(4) td , tr:nth-child(3) p , tr:nth-child(2) p"
    ) %>%
    html_text() %>%
    str_squish()
  nh_data <- nh_text %>%
    split(1:4) %>%
    as_tibble()
  names(nh_data) <-
    c("facilities",
      "staff_positive",
      "inmates_tested",
      "inmates_positive")
  nh_data %>%
    modify_at(2:4,  ~ parse_number(.)) %>%
    mutate(scrape_date = today(),
           state = "New Hampshire")
}


# Oklahoma --------------------------------------------------------------
get_oklahoma_covid_data <- function(ok_doc_path) {
  path_to_pdf <- ok_doc_path %>%
    html_nodes("h4 a") %>%
    html_attr("href")
  oklahoma_data <- tabulizer::extract_tables(path_to_pdf[2]) %>%
    map( ~ as_tibble(.)) %>%
    map_at(
      1,
      ~ row_to_names(., row_number = 1) %>%
        rename(
          inmates_tested = Tested,
          inmates_pending = Pending,
          inmates_positive = Positive,
          inmates_negative = Negative
        )
    ) %>%
    map_at(2,
           ~  select_if(., not_all_empty_char) %>%
             dplyr::slice(5:10))
  names(oklahoma_data[[2]]) <- names(oklahoma_data[[3]])
  facilities_data <- oklahoma_data[2:3] %>%
    reduce(bind_rows) %>%
    modify_at(c(2,6:7),  ~ parse_number(.))
  names(facilities_data) <-
    c(
      "facilities",
      "inmates_positive",
      "units_quarantine",
      "inmates_quarantine",
      "housing_type",
      "inmates_isolation",
      "staff_positive"
    )
  list(ok_facilities = facilities_data , ok_total = oklahoma_data[[1]]) %>% 
    map(~mutate(.,state = "Oklahoma",scrape_date = today()))
  
}



# Missouri ----------------------------------------------------------------
get_missouri_covid_data <- function(miss_doc_path) {
  data <- miss_doc_path %>% 
    html_nodes("h4+ ul li") %>%
    html_text()
  data <- stringr::str_split_fixed(data, ":", n = 2)
  data <- t(data)
  column_names <- gsub(":.*", "", data[1, ])
  column_names <- tolower(column_names)
  column_names <- gsub(" |-", "_", column_names)
  column_names <- gsub("prison.*staff", "prison_staff", column_names)
  
  data <- data.frame(data, stringsAsFactors = FALSE)
  names(data) <- column_names
  data <- data[-1, ]
  data <-
    data %>%
    dplyr::mutate_all(readr::parse_number) %>%
    dplyr::mutate(scrape_date      = lubridate::today(),
                  state            = "Missouri") %>%
    rename(inmates_positive        = positive_inmates,
           inmates_deaths          = inmate_deaths,
           contract_staff_positive = positive_non_prison_staff,
           staff_positive          = positive_prison_staff)
  
  
  return(data)  
}


# Maine -------------------------------------------------------------------

get_maine_covid_data <- function(maine_doc_path){
url <- "https://www.maine.gov/corrections/home/MDOC%20COVID19%20Web%20Dashboard%204-17-2020.pdf"
areas <- tabulizer::locate_areas(maine_doc_path,
                                 pages = 1)
areas <- c(198.5982, 281.4743, 238.0785, 544.6768)
names(areas) <- c("top", "left", "bottom", "right")
areas <- list(areas)

adult <- tabulizer::extract_tables(url, area = areas)
adult <- adult[[1]]
column_names <- adult[1,]
column_names <- column_names[column_names != ""]
column_names <- tolower(column_names)
column_names <- gsub(" ", "_", column_names)

values <- adult[2, ]
values <- values[values != ""]


adult <- data.frame(t(values), stringsAsFactors = FALSE)
names(adult) <- column_names
adult[] <- sapply(adult, readr::parse_number)

adult %>% 
  mutate(scrape_date = today(),
         state = "Maine")
}
# Wisconsin ---------------------------------------------------------------

# get_wisconsin_covid_data <- function(wisc_doc_path) {
#   
# data <- tabulizer::extract_tables(here::here("Testing_Table.pdf"),
#                                output = "data.frame",
#                                columns = list(4))
# 
# data <- pdftools::pdf_text("Testing_Table.pdf")
# data <- trimws(data)
# data <- strsplit(data, "\r\n")
# data <- data[[1]]
# update_date <- data[grep("Updated:", data)]
# update_date <- gsub(".*:", "", update_date)
# update_date <- lubridate::mdy(update_date)
# 
# data <- trimws(data)
# column_names <- data[grep("Completed Tests", data)]
# column_names <- gsub("Tests ", "Tests     ", column_names)
# column_names <- c("facilities", strsplit(column_names, " {2, }")[[1]])
# column_names <- tolower(column_names)
# column_names <- gsub(" ", "_", column_names)
# 
# data <- data[(grep("Completed Tests", data) + 1):length(data)]
# data <- data[-grep("Grand Total", data)]
# data <- stringr::str_split_fixed(data, " {2,}", n = 5)
# data <- data.frame(data, stringsAsFactors = FALSE)
# names(data) <- column_names
# data <-
#   data %>%
#   mutate_at(2:ncol(.), readr::parse_number) %>%
#   rename(inmates_positive = positive_tests,
#          inmates_negative = negative_tests,
#          inmates_pending  = pending_tests,
#          inmates_tested   = completed_tests) %>%
#   mutate(scrape_date = lubridate::today(),
#          update_date = update_date,
#          state = "Wisconsin")
# 
# return(data)
# }
# 
# get_wisconsin_covid_data("https://doc.wi.gov/Pages/COVID19(Coronavirus)/COVID19TestingDashboard.aspx")

# Massachusetts -----------------------------------------------------------

get_mass_covid_data <- function() {
  links <- 
    read_html("https://data.aclum.org/sjc-12926-tracker/") %>%
    html_nodes("a") %>%
    html_attr('href')
  
  
  download.file(
    "https://data.aclum.org/sjc-12926-tracker/session/b8c49700e11ae0df5334500957ec3b77/download/downloadData?w=",
    destfile = "test.xlsx"
  )
  mass_data <- read_xlsx("test.xlsx")
  unlink("test.xlsx")
  mass_data %>%
    clean_names() %>%
    rename(
      scrape_date = date,
      facilities = county,
      inmates_tested = n_positive_detainees_inmates,
      inmates_positive = n_positive_detainees_inmates,
      staff_tested = n_tested_staff,
      staff_positive = n_positive_staff,
      contract_staff_tested = n_tested_contractors,
      contract_staff_positive = n_positive_contractor
    ) %>% 
    mutate(state = "Massachusetts") %>% 
    modify_at(3:18,~as.numeric(.))
}
