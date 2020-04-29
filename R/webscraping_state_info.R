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
  
  #tracker

  alaska_tracker <- alaska_doc %>%
    html_nodes("#tracker") %>%
    html_text()
  
  alaska_tracker <- gsub("\r|\n|\t", " ", alaska_tracker)
  alaska_tracker <- trimws(alaska_tracker)
  
  alaska_tracker <- stringr::str_split_fixed(alaska_tracker, " {2,}", n = 8)
  column_names <- alaska_tracker[, c(TRUE, FALSE)]
  column_names <- tolower(column_names)
  
  alaska_tracker <- alaska_tracker[, c(FALSE, TRUE)]
  alaska_tracker <- data.frame(t(alaska_tracker), stringsAsFactors = FALSE)
  names(alaska_tracker) <- column_names
  alaska_tracker <-
    alaska_tracker %>%
    dplyr::rename(inmates_tested = tested,
                  inmates_positive = positive,
                  inmates_negative = negative,
                  inmates_pending = pending) %>%
    dplyr::mutate_at(c("inmates_tested",
                       "inmates_positive",
                       "inmates_negative",
                       "inmates_pending"),
                     readr::parse_number) %>%
    dplyr::mutate(state = "Alaska",
                  scrape_date = today())
  
  return(alaska_tracker)
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
get_delaware_covid_data <- function() {
  data <- pdftools::pdf_text("https://doc.delaware.gov/assets/documents/Confirmed_COVID_Cases.pdf")
  data <- gsub("\r\nPlummer\r\nCommunity Corrections\r\nCenter\r\n",
               " \r\nPlummer Community Corrections Center", data)
  data <- gsub("\r\nHoward R. Young\r\nCorrectional Institution\r\n",
               " \r\nHoward R. Young Correctional Institution", data)
  data <- gsub("\r\nHazel D. Plant\r\nWoman's Treatment\r\nFacility\r\n",
               " \r\nHazel D. Plant Woman's Treatment Facility", data)
  data <- gsub("\r\nDelores J. Baylor\r\nWoman's Correctional\r\nInstitution",
               " \r\nDelores J. Baylor Woman's Correctional Institution", data)
  data <- gsub("\r\nNew Castle Probation &\r\nParole & Day Reporting\r\nCenter \\(Hares Corner\\)\r\n",
               " \r\nNew Castle Probation & Parole & Day Reporting Center \\(Hares Corner\\)", data)
  data <- gsub("\r\nGeorgetown\r\nAdministrative Services\r\n",
               " \r\nGeorgetown Administrative Services", data)
  data <- gsub("\r\nGeorgetown Probation &",
               " \r\nGeorgetown Probation &", data)
  data <- gsub("\r\nSussex Correctiona",
               " \r\nSussex Correctiona", data)
  data <- gsub("\r\nSussex Community",
               " \r\nSussex Community", data)
  data <- gsub("\r\nNorthern New Castle\r\nCounty Adult Probation\r\n& Parole \\(Cherry Lane\\)\r\n",
               " \r\nNorthern New Castle County Adult Probation & Parole \\(Cherry Lane\\)", data)
  data <- gsub("([[:alpha:]])\\r", "\\1 \\\r", data)
  data <- gsub("Contracted Staff Offenders", "Contracted Staff   Offenders", data)
  data <- strsplit(data, split = " \\r\\n")
  
  data <- data[[1]]
  data <- data[-grep("DE DOC CONFIRMED|Updated", data)]
  data <- trimws(data)
  data <- stringr::str_split_fixed(data, " {2,}", n = 4)
  data <- data.frame(data, stringsAsFactors = FALSE)
  
  column_names <- as.character(as.vector(data[1, ]))
  data <- data[-1, ]
  column_names <- janitor::make_clean_names(column_names)
  names(data) <- column_names
  
  facility_name_part2 <- gsub("\r\n(.*)", "\\1", data$offenders)
  facility_name_part2 <- gsub("[0-9]+", "", facility_name_part2)
  
  data$facility <- paste(data$facility, facility_name_part2)
  data$offenders <- gsub("\r\n.*", "", data$offenders)
  
  data <-
    data %>%
    dplyr::rename(facilities = facility,
                  staff_positive = correctional_staff,
                  contract_staff_positive = contracted_staff,
                  inmates_positive = offenders) %>%
    dplyr::mutate_at(c("staff_positive",
                       "contract_staff_positive",
                       "inmates_positive"),
                     readr::parse_number) %>%
    dplyr::mutate(state = "Delaware",
                  scrape_date = lubridate::today())
  return(data)
}

# Georgia -----------------------------------------------------------------
get_georgia_covid_data <- function(georgia_doc_path) {
  public_facilities <- georgia_doc_path %>%
    html_nodes(".field-item > table:nth-child(1)") %>%
    html_table()
  public_facilities <- public_facilities[[1]]
  column_names <- as.character(as.vector(public_facilities[1, ]))
  column_names <- paste0(column_names, as.character(as.vector(public_facilities[2, ])))
  public_facilities <- public_facilities[-c(1:2), ]
  column_names <- gsub(" |\\*", "_", column_names)
  column_names <- tolower(column_names)
  column_names <- gsub("covid.*19", "covid_19", column_names)
  names(public_facilities) <- column_names
  
  public_facilities <-
    public_facilities %>%
    dplyr::rename(facilities        = gdc_facilities,
                  staff_positive    = confirmed_covid_19_casesstaff_,
                  inmates_positive  = confirmed_covid_19_casesoffenders,
                  staff_recovered   = recovered_casesstaff_,
                  inmates_recovered = recovered_casesoffenders) %>%
    dplyr::mutate(facilities = iconv(facilities, from = "UTF-8", to = "ASCII", "")) %>%
    dplyr::filter(tolower(facilities) != "total cases")
  
  private_facilities <- georgia_doc_path %>%
    html_nodes(".field-item > table:nth-child(3)") %>%
    html_table()
  private_facilities <- private_facilities[[1]]
  column_names <- as.character(as.vector(private_facilities[1, ]))
  column_names <- paste0(column_names, as.character(as.vector(private_facilities[2, ])))
  private_facilities <- private_facilities[-c(1:2), ]
  column_names <- gsub(" |\\*|\\/", "_", column_names)
  column_names <- tolower(column_names)
  column_names <- gsub("covid.*19", "covid_19", column_names)
  names(private_facilities) <- column_names
  
  private_facilities <-
    private_facilities %>%
    dplyr::rename_all(iconv, from = "UTF-8", to = "ASCII", "") %>%
    dplyr::rename(facilities        = county_private_facilities,
                  inmates_positive  = confirmed_covid_19_casesoffenders,
                  inmates_recovered = recovered_casesoffenders)
  
  deaths <- georgia_doc_path %>%
    html_nodes(".field-item > table:nth-child(6)") %>%
    html_table()
  deaths <- deaths[[1]]
  deaths <-
    deaths %>%
    janitor::row_to_names(row_number = 1) %>%
    dplyr::rename_all(tolower) %>%
    dplyr::rename_all(iconv, from = "UTF-8", to = "ASCII", "") %>%
    dplyr::rename(facilities     = locations,
                  staff_deaths   = staff,
                  inmates_deaths = offenders) %>%
    dplyr::mutate_at(c("staff_deaths",
                       "inmates_deaths"),
                     readr::parse_number) %>%
    dplyr::mutate_at(c("staff_deaths",
                       "inmates_deaths"),
                     na_to_0) 
  
  
  data <-
    public_facilities %>%
    dplyr::bind_rows(private_facilities) %>%
    dplyr::left_join(deaths, by = "facilities") %>%
    dplyr::mutate(state = "Georgia",
                  scrape_date = lubridate::today()) %>%
    dplyr::mutate_at(c("staff_positive",
                       "inmates_positive",
                       "staff_recovered",
                       "inmates_recovered"),
                     readr::parse_number)
  
  return(data)
}



# Illinois ----------------------------------------------------------------
get_illinois_covid_data <- function(il_doc_path) {
  data <- il_doc_path %>%
    html_nodes(".soi-rteTable-1") %>%
    html_table()
  data <- data[[1]]
  names(data) <- janitor::make_clean_names(names(data))
  data <-
    data %>%
    dplyr::mutate_all(iconv, from = "UTF-8", to = "ASCII", "") %>%
    dplyr::rename(facilities = locations,
                  staff_positive = staff_confirmed,
                  inmates_positive = incarcerated_individuals_confirmed,
                  inmates_recovered = incarcerated_individuals_recovered) %>%
    dplyr::mutate_at(c("staff_positive",
                       "inmates_positive",
                       "inmates_recovered",
                       "staff_recovered"),
                     readr::parse_number) %>%
    dplyr::mutate(state = "Illinois",
                  scrape_date = today()) %>%
    dplyr::filter(tolower(facilities) != "total")
  
  return(data)
}

na_to_0 <- function(x) {
  x[is.na(x)] <- 0
  return(x)
}

# Pennsylvania ------------------------------------------------------------
get_pa_covid_data <- function(pa_covid_doc_path) {
  data <- pa_covid_doc_path %>%
    html_nodes("table.ms-rteTable-default:nth-child(17)") %>%
    html_table()
  data <- data[[1]]
  
  column_names <- as.character(as.vector(data[1, ]))
  column_names <- paste0(column_names, as.character(as.vector(data[2, ])))
  data <- data[-c(1:2), ]
  column_names <- gsub(" ", "_", column_names)
  column_names <- tolower(column_names)
  column_names <- iconv(column_names, from = 'UTF-8', to = 'ASCII', "")
  names(data) <- column_names
  
  data <-
    data %>%
    dplyr::rename(facilities     = location,
                  staff_positive = employees_positive,
                  staff_negative = employees_negative,
                  staff_deaths   = employees_deaths) %>%
    dplyr::mutate_all(iconv, from = "UTF-8", to = "ASCII", "") %>%
    dplyr::mutate_at(c("staff_positive",
                       "staff_negative",
                       "staff_deaths",
                       "inmates_positive",
                       "inmates_negative",
                       "inmates_deaths"),
                     readr::parse_number) %>%
    dplyr::mutate_all(na_to_0) %>%
    dplyr::mutate(facilities = readr::parse_character(facilities),
                  state = "Pennsylvania",
                  scrape_date = today()) %>%
    dplyr::filter(tolower(facilities) != "total")
  
  return(data)
  
}

# alabama -----------------------------------------------------------------
get_ala_covid_data <- function(ala_doc_path) {
  data <- ala_doc_path %>%
    html_node("table ") %>%
    html_table() 
  # adjust the names of the data
  names(data) <- gsub(" |-|\\*", "_", names(data))
  data <-
    data %>%
    dplyr::rename(facilities       = Facility,
                  inmates_tested   = Inmates_Tested,
                  inmates_pending  = Tests_Results_Pending,
                  inmates_positive = Confirmed_Positive_,
                  inmates_deaths   = COVID_19_Related_Inmate_Deaths_) %>%
    dplyr::mutate(state = "Alabama",
                  scrape_date = today()) %>% 
    dplyr::mutate_at(c("inmates_tested",
                       "inmates_pending",
                       "inmates_positive",
                       "inmates_deaths"),
                     dplyr::na_if, "-") %>%
    dplyr::mutate_at(c("inmates_tested",
                       "inmates_pending",
                       "inmates_positive",
                       "inmates_deaths"), 
                     readr::parse_number) %>%
    dplyr::mutate_at(c("inmates_tested",
                       "inmates_pending",
                       "inmates_positive",
                       "inmates_deaths"), 
                     na_to_0) %>%
    dplyr::filter(tolower(facilities) != "total:")
  
  return(data)
}

# Arizona -----------------------------------------------------------------
get_arizona_covid_data <- function(az_doc_path) {
  data <- az_doc_path %>%
    html_nodes("#block-views-covid-19-data-table-block > div > div > table") %>%
    html_table()
  data <- data[[1]]
  names(data) <- janitor::make_clean_names(names(data))
  data <-
    data %>%
    dplyr::rename(facilities       = location,
                  inmates_positive = inmates_confirmed) %>%
    dplyr::mutate(state       = "Arizona",
                  scrape_date = lubridate::today())
  
  return(data)
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
    mutate(state = "Idaho",
           scrape_date = today()) %>%
    dplyr::rename(inmates_tested   = Tested,
                  inmates_pending  = Pending,
                  inmates_positive = Positive,
                  inmates_negative = Negative)
}





# Federal BOP -----------------------------------------------------------------

get_federal_data <- function(federal_bop_path){
  data <- federal_bop_path %>%
    html_text() %>% 
    jsonlite::fromJSON(.)
  
  overall_stats <- data$other
  reentry       <- data$rrcData
  private       <- data$privateData
  offenders     <- data$bopData
  
  column_names <- unique(c(names(reentry),
                           names(private),
                           names(offenders)))
  column_names <- sort(column_names)
  
  federal_columns_fix <- c("contractNum"       = "contract_number",
                           "id"                = "facilities",
                           "inmateDeathAmt"    = "inmates_deaths",
                           "inmatePositiveAmt" = "inmates_positive",
                           "inmateRecoveries"  = "inmates_recovered",
                           "staffDeathAmt"     = "staff_deaths",
                           "staffPositiveAmt"  = "staff_positive",
                           "staffRecoveries"   = "staff_recovered")
  
  data <- list(offenders     = data$bopData,
               reentry       = data$rrcData,
               overall_stats = data$other,
               private       = data$privateData) %>% 
    map(~mutate(., 
                state = "Federal",
                scrape_date = today())) %>% 
    map(~rename_all(., stringr::str_replace_all,  federal_columns_fix))
  
  return(data)
}

# Florida --------------------------------------------------------------
get_fl_covid_data <- function(fl_doc_path) {
  data <- fl_doc_path %>%
    html_nodes(".ciInfo > div:nth-child(2) > table:nth-child(10)") %>% 
    html_table()
  data <- data[[1]]
  data$`Staff Status Information`[1] <- "staff_positive"
  column_names <- as.character(as.vector(data[1, ]))
  column_names <- tolower(column_names)
  column_names <- gsub(" ", "_", column_names)
  data <- data[-1, ]
  names(data) <- column_names
  data <-
    data %>%
    dplyr::rename(facilities                  = facility,
                  inmates_security_quarantine = security_quarantine,
                  inmates_medical_quarantine  = medical_quarantine,
                  inmates_isolation           = medical_isolation,
                  inmates_pending             = pending_tests,
                  inmates_negative            = negative_tests,
                  inmates_positive            = positive_tests,
                  staff_positive              = staff_positive) %>%
    dplyr::mutate_all(na_if, "N/A") %>%
    dplyr::mutate_at(c("inmates_security_quarantine",
                       "inmates_medical_quarantine",
                       "inmates_isolation",
                       "inmates_pending",
                       "inmates_negative",
                       "inmates_positive",
                       "staff_positive"),
                     readr::parse_number) %>%
    dplyr::mutate(state = "Florida",
                  scrape_date = lubridate::today())
  
  return(data)
}




# Kansas ------------------------------------------------------------------
get_ks_covid_data <- function(ks_doc_path) {
  data <- ks_doc_path %>%
    html_nodes("table.plain") %>%
    html_table()
  data <- data[[1]]
  names(data) <- janitor::make_clean_names(names(data))
  
  data <-
    data %>%
    dplyr::rename(facilities        = facility,
                  staff_positive    = staff_confirmed,
                  inmates_positive  = residents_confirmed,
                  staff_recovered   = staff_returned_to_work,
                  inmates_recovered = residents_recovered,
                  inmates_deaths    = resident_deaths) %>%
    dplyr::mutate_all(na_if, "-") %>%
    dplyr::mutate_at(c("inmates_positive",
                       "staff_recovered",
                       "inmates_recovered"),
                     readr::parse_number) %>%
    dplyr::mutate(state       = "Kansas",
                  scrape_date = today())
  
  
  return(data)
}


# Louisiana ---------------------------------------------------------------

get_la_covid_data <- function(la_doc_path) {
  inmate_data <- la_doc_path %>%
    html_nodes("#tablepress-5") %>%
    html_table()
  inmate_data <- inmate_data[[1]]
  names(inmate_data) <- gsub("\n| |\\(|\\)", "_", names(inmate_data))
  names(inmate_data) <- tolower(names(inmate_data))
  names(inmate_data) <- gsub("covid.19", "covid_19", names(inmate_data))
  inmate_data <-
    inmate_data %>%
    dplyr::rename(facilities                    = prisons,
                  inmates_positive              = total_tested_positive,
                  inmates_positive_symptomatic  = tested_positive_symptomatic,
                  inmates_positive_asymptomatic = tested_positive_asymptomatic,
                  inmates_positive_current      = currently_positive,
                  inmates_step_down             = step_down,
                  inmates_recovered             = recovered,
                  inmates_covid_deaths_underlying_conditions = covid_19__deaths__underlying_medical___conditions_,
                  inmates_covid_deaths          = covid_19__deaths,
                  inmates_deaths                = total_deaths) %>%
    dplyr::filter(tolower(facilities) != "total")
  
  staff_data <- la_doc_path %>%
    html_nodes("#tablepress-4") %>%
    html_table()
  staff_data <- staff_data[[1]]
  names(staff_data) <- tolower(names(staff_data))
  staff_data <-
    staff_data %>%
    dplyr::rename(facilities      = prisons,
                  staff_positive  = positive,
                  staff_recovered = recovered,
                  staff_deaths    = deaths) %>%
    dplyr::filter(tolower(facilities) != "total")

  
  data <-
    inmate_data %>%
    dplyr::full_join(staff_data, by = "facilities") %>%
    dplyr::mutate_all(na_if, "-") %>%
    dplyr::mutate(state = "Louisiana",
                  scrape_date = lubridate::today())
  
  
  numeric_cols <- c("inmates_positive",
                    "inmates_positive_symptomatic",
                    "inmates_positive_asymptomatic",
                    "inmates_positive_current",
                    "inmates_step_down",
                    "inmates_recovered",
                    "inmates_covid_deaths_underlying_conditions",
                    "inmates_covid_deaths",
                    "inmates_deaths",
                    "staff_positive",
                    "staff_recovered",
                    "staff_deaths")
  numeric_cols <- numeric_cols[sapply(data[, numeric_cols], typeof) == "character"]
  if (length(numeric_cols) > 0) {
    data <-
      data %>%
      dplyr::mutate_at(numeric_cols,
                       readr::parse_number)
  }
  
  return(data)
}


# New York ----------------------------------------------------------------
# reformat Aaron's code
get_nys_covid_data <- function(nys_doc_path) {
  state_table1 <-
    nys_doc_path %>%
    html_nodes("#toc_7581 > div:nth-child(2) > div > table:nth-child(1)") %>%
    html_table()
  state_table1 <- state_table1[[1]]
  names(state_table1) <- janitor::make_clean_names(names(state_table1))
  state_table1 <-
    state_table1 %>%
    dplyr::rename(staff_positive    = staff,
                  inmates_positive  = incarcerated_population,
                  parolees_positive = parolees)
  
  state_table2 <-
    nys_doc_path %>%
    html_nodes("#toc_7581 > div:nth-child(2) > div > table:nth-child(6)") %>%
    html_table()
  state_table2 <- state_table2[[1]]
  names(state_table2) <- janitor::make_clean_names(names(state_table2))
  state_table2 <-
    state_table2 %>%
    dplyr::rename(staff_deaths    = staff,
                  inmates_deaths  = incarcerated_population,
                  parolees_deaths = parolees)
  state_data <- dplyr::bind_cols(state_table1, state_table2) %>%
    dplyr::mutate(state = "New York",
                  scrape_date = today())
  
  
  
  ny_pdf_path <- nys_doc_path %>% 
    html_nodes("a") %>% 
    html_attr("href") %>% 
    pluck(32)
  data <- extract_tables(glue("https://doccs.ny.gov/{ny_pdf_path}"))
  data <- data[[1]]
  data <- data[5:nrow(data)]
  
  data <- gsub("HOUSING FACILITY", "facilities", data)
  data <- data[-grep("^total ", data, ignore.case = TRUE)]
  column_names <- as.character(as.vector(data[1]))
  data <- data[-1]
  column_names <- strsplit(column_names, " ")[[1]]
  column_names <- tolower(column_names)
  data <- gsub("([[:alpha:]]) ([[:alpha:]])", "\\1_\\2", data)
  data <- stringr::str_split_fixed(data, " ", n = 6)
  data <- data.frame(data)
  names(data) <- column_names
  data$facilities <- gsub("_", " ", data$facilities)
  data <-
    data %>%
    dplyr::rename(inmates_recovered = recovered,
                  inmates_deaths    = deceased,
                  inmates_positive  = positive,
                  inmates_pending   = pending,
                  inmates_negative  = negative) %>%
    dplyr::mutate_at(c("inmates_recovered",
                       "inmates_deaths",
                       "inmates_positive",
                       "inmates_pending",
                       "inmates_negative"),
                     as.character) %>%
    dplyr::mutate_at(c("inmates_recovered",
                       "inmates_deaths",
                       "inmates_positive",
                       "inmates_pending",
                       "inmates_negative"),
                     readr::parse_number) %>%
    dplyr::mutate(state = "New York",
                  scrape_date = lubridate::today())
  
  
  list(totals = state_data,
       facilities = data)
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
   suppressMessages(extract_tables(path_to_ohio_pdf,
                   method = "decide",
                   output = "matrix"))
  
  state_data <-
    scrape_table[[1]]
  state_data <- data.frame(state_data, stringsAsFactors = FALSE)
  state_data <- 
    state_data %>%
    janitor::row_to_names(row_number = 1) %>%
    dplyr::rename(inmates_tested   = Tested,
                  inmates_pending  = Pending,
                  inmates_positive = Positive,
                  inmates_negative = Negative) %>%
    dplyr::mutate_all(na_if, "*") 
  
  
  facility_data <- scrape_table[[2]]
  facility_data <- data.frame(facility_data, stringsAsFactors = FALSE)
  
  column_names <- as.character(as.vector(facility_data[1, ]))
  column_names <- paste(column_names, as.character(as.vector(facility_data[2, ])))
  column_names <- paste(column_names, as.character(as.vector(facility_data[3, ])))
  column_names <- paste(column_names, as.character(as.vector(facility_data[4, ])))
  column_names <- janitor::make_clean_names(column_names)
  
  cols_to_remove <- grep("^x$|^x_[0-9]+$", column_names)
  
  facility_data <- facility_data[-c(1:4), ]
  facility_data <- facility_data[, -cols_to_remove]
  column_names <- column_names[-cols_to_remove]
  
  names(facility_data) <- column_names
  facility_data2 <- scrape_table[[3]]
  facility_data2 <- data.frame(facility_data2, stringsAsFactors = FALSE)
  facility_data2 <- facility_data2[-nrow(facility_data2), ]
  facility_data2 <- facility_data2 %>% mutate_all(na_if, "")
  facility_data2 <- janitor::remove_empty(facility_data2, which = "cols")
  names(facility_data2) <- column_names
  
  facility_data <-
    facility_data %>%
    dplyr::bind_rows(facility_data2) %>%
    dplyr::rename(facilities               = institution,
                  staff_positive           = number_of_staff_who_have_reported_positive_tests,
                  staff_deaths             = number_of_covid_19_related_staff_deaths,
                  staff_recovered          = number_of_staff_who_have_recover_ed,
                  units_in_quarantine      = units_in_quarantine,
                  inmates_quarantine       = number_of_inmates_in_quaranti_ne,
                  housing_type             = housing_type_cell_open_bay_combo,
                  inmates_isolation        = number_of_inmates_in_isolation,
                  inmates_positive         = number_of_inmates_currently_positive_for_covid_19,
                  inmates_deaths_probable  = number_of_probable_covid_19_related_inmate_deaths,
                  inmates_deaths_confirmed = number_of_confirmed_covid_19_related_inmate_deaths,
                  inmates_pending = number_of_inmates_who_have_pending_results,
                  inmates_recovered = number_of_inmates_who_have_recovered) %>%
    dplyr::filter(tolower(facilities) != "totals") %>%
    dplyr::mutate_at(c("staff_positive",
                       "staff_deaths",
                       "staff_recovered",
                       "inmates_quarantine",
                       "inmates_isolation",
                       "inmates_positive",
                       "inmates_deaths_probable",
                       "inmates_deaths_confirmed",
                       "inmates_pending",
                       "inmates_recovered"),
                     readr::parse_number) %>%
    dplyr::mutate_all(na_if, "") %>%
    janitor::remove_empty(which = "rows") %>%
    dplyr::mutate(state = "Ohio",
                  scrape_date = lubridate::today()) 
    
    
    
    list(ohio_facility = facility_data, ohio_totals = state_data)
}
# New Jersey --------------------------------------------------------------
get_nj_covid_data <- function(nj_doc_path) {
  prisons <- nj_doc_path %>%
    html_nodes("div.col-md-12:nth-child(1) > div:nth-child(1) > div:nth-child(2) > table:nth-child(2)") %>%
    html_table()
  prisons <- prisons[[1]]
  names(prisons) <- janitor::make_clean_names(names(prisons))
  prisons        <- dplyr::rename(prisons, 
                                  facilities = prisons_and_ancillary_locations)
  
  halfway_houses <- nj_doc_path %>%
    html_nodes("div.col-md-12:nth-child(2) > div:nth-child(1) > div:nth-child(2) > table:nth-child(2)") %>%
    html_table()
  halfway_houses <- halfway_houses[[1]]
  names(halfway_houses) <- janitor::make_clean_names(names(halfway_houses))
  halfway_houses <- dplyr::rename(halfway_houses, 
                                  facilities = residential_community_release_program)
  
  
  data <-
    prisons %>%
    dplyr::bind_rows(halfway_houses) %>%
    dplyr::rename(staff_positive = employees,
                  inmates_positive = inmates,
                  inmates_deaths = inmate_deaths) %>%
    dplyr::mutate(state = "New Jersey",
                  scrape_date = lubridate::today()) %>%
    dplyr::filter(tolower(facilities) != "totals")
  
  
  return(data)
}
# North Carolina --------------------------------------------------------------
get_nc_covid_data <- function(nc_doc_path) {
  
  data <-
    nc_doc_path %>%
    html_nodes(xpath = "/html/body/div[1]/div/table") %>%
    html_table()
  data <- data[[1]]
  data <-
    data %>%
    rename(facilities       = Facility,
           inmates_tested   = TestsPerformed,
           inmates_positive = Positive,
           inmates_negative = Negative) %>%
    mutate(inmates_tested   = readr::parse_number(inmates_tested),
           inmates_positive = readr::parse_number(inmates_positive),
           inmates_negative = readr::parse_number(inmates_negative),
           state = "North Carolina",
           scrape_date = lubridate::today()) 
  data <- data[-grep("Statewide Totals", data$facilities), ]
  
  return(data)
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
        "inmates_medical",
        "inmates_recovered",
        "inmates_deaths"
      )
    )
  names(nd_data) <- c("ndsp", "jrcc", "mrcc", "ycc", "type")
  nd_data <-
    nd_data %>% 
    select(type,
           everything()) %>% 
    t()
  nd_data <- data.frame(nd_data, stringsAsFactors = FALSE)
  nd_data <- 
    nd_data %>% 
    row_to_names(1) %>% 
    mutate(facilities = c("ndsp", "jrcc", "mrcc", "ycc")) %>% 
    dplyr::mutate(inmates_positive = gsub("O", "0", inmates_positive)) %>%
    modify_at(c("inmates_positive",
                "inmates_negative",
                "inmates_medical",
                "inmates_recovered",
                "inmates_deaths"),  na_if, ": -") %>% 
    modify_at(c("inmates_positive",
                "inmates_negative",
                "inmates_medical",
                "inmates_recovered",
                "inmates_deaths"), readr::parse_number) %>% 
    mutate(state = "North Dakota",
           scrape_date = today()) %>%
    dplyr::select(facilities,
                  everything())
  
  return(nd_data)
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
    inmates_recovered=ocred_data[6],
    inmates_hospital=ocred_data[7],
    inmates_deaths=ocred_data[8],
    state = 'Minnesota',
    scrape_date = today()
  )
}

get_kentucky_data <- function(kentucky_url) {
  covid_cases_img_path <- kentucky_url %>%
    read_html() %>%
    html_nodes("img") %>%
    html_attr("src") %>%
    tibble::enframe() %>%
    # There are a few images on their page - the one we care about is named something like
    # "/Facilities/AI/PublishingImages/COVID%20Cases%204-28-20.jpg"
    # so just grep for "Cases"
    filter(grepl('Cases', value)) %>%
    pull(value)
  img_url <- paste('https://corrections.ky.gov', covid_cases_img_path, sep='')
  img <- image_read(img_url)
  ocred_img <- tesseract::ocr_data(img)

  # The last row looks like "Total", "29", "0", "44", "2" - so find the index where "Total" appears, and
  # the data will come right after that. Their image is high quality!
  starting_row_for_totals_data <- which(ocred_img$word == 'Total') + 1
  data_row_for_totals <- ocred_img[starting_row_for_totals_data:nrow(ocred_img), ]

  tibble(
    staff_positive = as.integer(data_row_for_totals$word[1]),
    staff_deaths = as.integer(data_row_for_totals$word[2]),
    inmates_positive = as.integer(data_row_for_totals$word[3]),
    inmates_deaths = as.integer(data_row_for_totals$word[4]),
    scrape_date = today(),
    state = 'Kentucky'
  )
}

image_contains_regex <- function(image, search_regex) {
  raw_ocr_text <- image %>% tesseract::ocr()
  grepl(search_regex, raw_ocr_text)
}

# Michigan - the infamous Medium page --------------------------------------------------------------------
# This is a fragile one, OCR had trouble with the image if it's not cropped just right - likely getting tripped
# up on the border of the table cells, and thinking those are characters. Beware!
get_michigan_data <- function(michigan_medium_page) {
  all_img_paths <- michigan_medium_page %>%
    read_html() %>%
    html_nodes("img") %>%
    html_attr("src") %>%
    tibble::enframe() %>%
    filter(! is.na(value)) %>%
    pull(value)
  
  all_imgs <- all_img_paths %>% image_read()
  num_imgs <- all_imgs %>% length()
  header_regex <- '(Prisoners Tested|Prisoners Confirmed|Prisoners Negative|Prisoner Deaths)'
  
  # Iterate over all the images until we find one that looks like the table containing data about people
  # in prison - just OCR every image and look for some of the words in the header.
  for (i in 1:num_imgs) {
    has_prisoner_text <- image_contains_regex(all_imgs[i], header_regex)
    if (has_prisoner_text) {
      break
    }
  }
  prisoner_data_image <- all_imgs[i]
  prison_img_details <- image_info(prisoner_data_image)

  # But the bottom border lines confuse tesseract, so have to be careful where exactly it gets cropped to
  # See https://cran.r-project.org/web/packages/magick/vignettes/intro.html#cut_and_edit
  # We can divide the image into 32 "rows" - one row for the header and padding above, one "row" for each row in the table,
  # and one "row" for the padding below the table. Then, to grab the totals (which is the last row in the table), we grab the
  # second to last row of the image.
  # So image is basically:
  # 1 header row
  # 29 prison rows
  # 1 total row
  # 1 padding row at the bottom
  # = 32 in total
  num_rows_in_image <- 32
  from_bottom <- prison_img_details$height / num_rows_in_image
  crop_str <- sprintf('%sx%s+0+%s', prison_img_details$width, from_bottom, prison_img_details$height - (from_bottom*2))

  cropped_prisoner_data_img <- prisoner_data_image %>% image_crop(crop_str)
  tesseract_digit_eng <- tesseract(options = list(tessedit_char_whitelist = "0123456789"))
  ocred_bottom <- cropped_prisoner_data_img %>% tesseract::ocr_data(engine=tesseract_digit_eng)
  as_integers <- ocred_bottom %>% mutate(as_int = as.integer(word)) %>% pull(as_int)
  tibble(
    inmates_tested=as_integers[1],
    inmates_positive=as_integers[2],
    inmates_negative=as_integers[3],
    inmates_pending=as_integers[4],
    inmates_recovered=as_integers[6],
    inmates_deaths=as_integers[7],
    scrape_date = today(),
    state = 'Michigan'
  )
}


# Vermont --------------------------------------------------------------------
get_vermont_covid_data <- function(vermont_html) {
  # As of 4/20/20, there are two imgs on the page - the first one contains data about incarcerated people, the
  # second about staff
  imgs <- vermont_html %>%
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
         staff_positive=num_staff_positive) %>%
    mutate(scrape_date = today(), state = 'Vermont')
}

# South Carolina ----------------------------------------------------------
get_sc_covid_data <- function(sc_doc_path) {
  data <- sc_doc_path %>%
    html_nodes(".box > table:nth-child(2)") %>%
    html_table()
  data <- data[[1]]
  
  column_names <- as.character(as.vector(data[1, ]))
  data <- data[-1, ]
  column_names <- gsub(" |\\*", "_", column_names)
  column_names <- tolower(column_names)
  column_names <- iconv(column_names, from = 'UTF-8', to = 'ASCII', "")
  names(data) <- column_names
  
  data <-
    data %>%
    dplyr::rename(facilities = assigned_locations,
                  staff_positive = staff_,
                  inmates_positive = offenders) %>%
    dplyr::mutate_all(iconv, from = "UTF-8", to = "ASCII", "") %>%
    dplyr::mutate_at(c("staff_positive",
                       "inmates_positive"),
                     readr::parse_number) %>%
    dplyr::mutate(state = "South Carolina",
                  scrape_date = today()) %>%
    dplyr::filter(tolower(facilities) != "total confirmed cases")
  
  
  return(data)
}



# Virginia ----------------------------------------------------------------
get_virginia_covid_data <- function(virginia_doc_path) {
  data <- virginia_doc_path %>%
    html_nodes("#covid19numbers") %>%
    html_table()
  data <- data[[1]]
  names(data) <- tolower(names(data))
  names(data) <- gsub(" |\\(|\\)|,|&|\\-", "_", names(data))
  names(data) <- gsub("on.site", "on_site", names(data))
  names(data) <- gsub("includes.+recovered", "includes_recovered", names(data))
  names(data) <- gsub("covid.19", "covid_19", names(data))
  
  data <-
    data %>%
    dplyr::rename(facilities = location,
                  inmates_positive_on_site = offenders_on_site,
                  inmates_hospital = offenders_in_hospitals,
                  inmates_positive = total_positive_offenders_includes_recovered__deceased____released_offenders_,
                  inmates_deaths = death_of_covid_19_positive_offender,
                  staff_positive = staff__includes_both_employees___contractors_
    ) %>%
    dplyr::filter(tolower(facilities) != "totals") %>%
    dplyr::mutate_at(c("inmates_positive_on_site",
                       "inmates_hospital",
                       "inmates_positive"),
                     dplyr::na_if, "n/a") %>%
    dplyr::mutate_at(c("inmates_positive_on_site",
                       "inmates_hospital",
                       "inmates_positive"),
                     readr::parse_number) %>%
    dplyr::mutate(state = "Virginia",
                  scrape_date = lubridate::today())
  
  
  return(data)
}


# Washington --------------------------------------------------------------
get_washington_covid_data <- function(wash_doc_path) {
  data <- wash_doc_path %>%
    html_nodes("div.col-xxs-12:nth-child(25) > div:nth-child(1) > div:nth-child(1) > table:nth-child(2)") %>%
    html_table()
  data <- data[[1]]
  names(data) <- janitor::make_clean_names(names(data))
  data <-
    data %>%
    dplyr::rename(facilities       = location,
                  staff_positive   = staff_confirmed,
                  inmates_positive = incarcerated_individual_confirmed) %>%
    dplyr::mutate(inmates_positive = na_if(inmates_positive, "N/A"),
                  inmates_positive = readr::parse_number(inmates_positive),
                  state = "Washington",
                  scrape_date = lubridate::today()) %>%
    dplyr::filter(!tolower(facilities) %in% c("prisons",
                                              "other location*** (based on location where individual tested positive for covid-19)",
                                              "work release",
                                              "community corrections"))
  
  return(data)
}

# Texas -------------------------------------------------------------------

get_texas_covid_data <- function(tx_doc_path) {
  pending <- tx_doc_path %>%
    html_nodes("div.pending:nth-child(1) > div:nth-child(3) > table:nth-child(1)") %>%
    html_table()
  pending <- pending[[1]]
  names(pending) <- c("facilities", "inmates_pending")
  pending$facilities <- gsub(" +", " ", pending$facilities)
  pending$facilities <- gsub("\r\n", "", pending$facilities)
  
  negative <- tx_doc_path %>%
    html_nodes("div.div_container:nth-child(10) > div:nth-child(2) > div:nth-child(3) > table:nth-child(1)") %>%
    html_table()
  negative <- negative[[1]]
  names(negative) <- c("facilities", "inmates_negative")
  negative$facilities <- gsub(" +", " ", negative$facilities)
  negative$facilities <- gsub("\r\n", "", negative$facilities)
  
  positive <- tx_doc_path %>%
    html_nodes("div.div_container:nth-child(10) > div:nth-child(3) > div:nth-child(3) > table:nth-child(1)") %>%
    html_table()
  positive <- positive[[1]]
  names(positive) <- c("facilities", "inmates_positive")
  positive$facilities <- gsub(" +", " ", positive$facilities)
  positive$facilities <- gsub("\r\n", "", positive$facilities)
  
  medical_restriction <- tx_doc_path %>%
    html_nodes("div.med_res:nth-child(4) > div:nth-child(3) > table:nth-child(1)") %>%
    html_table()
  medical_restriction <- medical_restriction[[1]]
  names(medical_restriction) <- c("facilities", "inmates_medical_restriction")
  medical_restriction$facilities <- gsub(" +", " ", medical_restriction$facilities)
  medical_restriction$facilities <- gsub("\r\n", "", medical_restriction$facilities)
  
  medical_isolation <- tx_doc_path %>%
    html_nodes("div.med_iso:nth-child(5) > div:nth-child(3) > table:nth-child(1)") %>%
    html_table()
  medical_isolation <- medical_isolation[[1]]
  names(medical_isolation) <- c("facilities", "inmates_medical_isolation")
  medical_isolation$facilities <- gsub(" +", " ", medical_isolation$facilities)
  medical_isolation$facilities <- gsub("\r\n", "", medical_isolation$facilities)
  
  staff_positive <-
    tx_doc_path %>%
    html_nodes("br+ .indent td") %>%
    html_text()
  staff_positive <- data.frame(facilities = staff_positive[c(TRUE, FALSE)],
                               staff_positive = staff_positive[c(FALSE, TRUE)])
  staff_positive$facilities <- gsub(" +", " ", staff_positive$facilities)
  staff_positive$facilities <- gsub("\r\n", "", staff_positive$facilities)
  staff_positive$staff_positive <- as.character(staff_positive$staff_positive)
  staff_positive$staff_positive <- readr::parse_number(staff_positive$staff_positive)
  
  
  data <-
    pending %>%
    dplyr::left_join(negative, by = "facilities") %>%
    dplyr::left_join(positive, by = "facilities") %>%
    dplyr::left_join(medical_restriction, by = "facilities") %>%
    dplyr::left_join(medical_isolation, by = "facilities") %>%
    dplyr::left_join(staff_positive, by = "facilities") %>%
    dplyr::mutate(state = "Texas",
                  scrape_date = lubridate::today())
  
  return(data)
}

# California --------------------------------------------------------------
get_california_covid_data <- function(cali_doc_path) {
  data <-cali_doc_path %>% 
    html_nodes(".wp-block-table > table:nth-child(1)") %>%
    html_table()
  data <- data[[1]] %>%
    janitor::row_to_names(row_number = 1)
  names(data) <- janitor::make_clean_names(names(data))
  data <-
    data %>%
    dplyr::rename(facilities = locations,
                  staff_positive = staff_confirmed) %>%
    dplyr::mutate(staff_positive = readr::parse_number(staff_positive),
                  state = "California",
                  scrape_date = lubridate::today()) %>%
    dplyr::filter(tolower(facilities) !=  "statewide total")
  
  return(data)
}



# Montana -----------------------------------------------------------------
get_montana_covid_data <- function(montana_doc_path) {
  data <- montana_doc_path %>%
    html_nodes("#dnn_ctr93899_HtmlModule_lblContent > table:nth-child(11)") %>%
    html_table()
  data <- data[[1]]
  
  column_names <- as.character(as.vector(data[1, ]))
  data <- data[-1, ]
  column_names <- gsub(" ", "_", column_names)
  column_names <- tolower(column_names)
  names(data) <- column_names
  
  data <-
    data %>%
    dplyr::rename(facilities = location,
                  inmates_positive = inmate_confirmed) %>%
    dplyr::mutate(inmates_positive = readr::parse_number(inmates_positive),
                  state = "Montana",
                  scrape_date = lubridate::today()) %>%
    dplyr::filter(!tolower(facilities) %in% c("total confirmed cases",
                                              "secure - state",
                                              "secure - contracted",
                                              "community - contracted",
                                              ""))
  
  return(data)
}


# Iowa --------------------------------------------------------------------
get_iowa_covid_data <- function(iowa_doc_path) {
  data <- iowa_doc_path %>%
    html_nodes(".field-items > div:nth-child(1) > table:nth-child(6)") %>%
    html_table()
  data <- data[[1]]
  
  column_names <- as.character(as.vector(data[1, ]))
  data <- data[-1, ]
  column_names <- janitor::make_clean_names(column_names)
  names(data) <- column_names
  
  data <-
    data %>%
    dplyr::rename(facilities = prison) %>%
    dplyr::mutate_all(stringr::str_trim) %>%
    dplyr::filter(tolower(facilities) != "total") %>%
    dplyr::mutate(state            = "Iowa",
                  scrape_date      = lubridate::today(),
                  inmates_positive = readr::parse_number(inmates_positive),
                  inmates_tested   = readr::parse_number(inmates_tested),
                  staff_positive   = readr::parse_number(staff_positive)) 
  
  return(data)
}


# Utah --------------------------------------------------------------------
get_utah_covid_data <- function(ut_doc_path) {
  data <- ut_doc_path %>%
    html_nodes("p:nth-child(16) strong") %>%
    html_text()
  data <- tibble(
    inmates_positive = data,
    state = "Utah",
    scrape_date = lubridate::today()
  )
  data$inmates_positive <- gsub(".*: ", "", data$inmates_positive)
  data$inmates_positive <- as.numeric(data$inmates_positive)
  return(data)
}


# Indiana ---------------------------------------------------------------------

get_indiana_covid_data <- function(indiana_doc_path) {
  
  indi_img <- indiana_doc_path %>% 
    html_nodes("img") %>% 
    html_attr("src") %>% 
    pluck(3)
  image_to_scrape <- image_read(glue("https://www.in.gov{indi_img}")) %>% 
    image_convert(type = "Grayscale") %>% 
    image_enhance()
  
  info_image <- image_to_scrape %>% 
    image_info()
  crop_info <- sprintf('%sx30+0+%s',info_image$width, info_image$height - 30)
  totals_info <- image_to_scrape %>% 
    image_crop(crop_info) %>% 
    image_resize("2000x") %>% 
    ocr() %>% 
    str_extract_all(": -|\\d+|O")
  
  totals_info[[1]] %>% 
    split(1:length(.)) %>% 
    as_tibble() %>% 
    rename(staff_positive = 1,
           staff_deaths= 2,
           inmates_quarantine = 3,
           inmates_isolation = 4,
           inmates_positive = 5,
           inmates_probable_deaths = 6,
           inmates_deaths = 7) %>% 
    mutate(scrape_date = today(),
           state = "Indiana")
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
  
  data <- nh_doc_path %>%
    html_nodes("td td td td:nth-child(1) , td td td td:nth-child(2) p") %>%
    html_text()
  data <- data.frame(data[c(TRUE, FALSE)],
                     data[c(FALSE, TRUE)]) %>%
    janitor::row_to_names(row_number = 1) %>%
    janitor::clean_names() %>%
    dplyr::mutate_all(as.character) %>%
    dplyr::mutate_all(stringr::str_trim) %>%
    dplyr::filter(tolower(worksite) != "staff total")
  
  data2 <- nh_doc_path %>%
    html_nodes("td~ td+ td p") %>%
    html_text()
  data2 <- stringr::str_trim(data2)
  data2 <- data2[-grep("covid-19 testing|resident.*total|^$", data2, ignore.case = TRUE)]
  
  data2_temp <- data.frame(number_residents_tested = c(NA, NA),
                           number_residents_positive = c(NA, NA))
  
  data2 <- data.frame(data2[c(TRUE, FALSE)],
                      data2[c(FALSE, TRUE)]) %>%
    janitor::row_to_names(row_number = 1) %>%
    janitor::clean_names() %>%
    dplyr::mutate_all(as.character)
  # Drops the totals row
  data2 <- data2[-nrow(data2), ] %>%
    dplyr::bind_rows(data2_temp)
  
  data <-
    data %>%
    dplyr::bind_cols(data2) %>%
    dplyr::rename(facilities       = worksite,
                  staff_positive   = number_staff_positive,
                  inmates_tested   = number_residents_tested,
                  inmates_positive = number_residents_positive) %>%
    dplyr::mutate_at(c("staff_positive",
                       "inmates_tested",
                       "inmates_positive"),
                     readr::parse_number) %>%
    dplyr::mutate(state = "New Hampshire",
                  scrape_date = lubridate::today())
  
  
  return(data)
}


# Oklahoma --------------------------------------------------------------

get_oklahoma_covid_data <- function(ok_doc_path) {

  path_to_pdf <- ok_doc_path %>%
    html_nodes("h4 a") %>%
    html_attr("href")
  data <- tabulizer::extract_tables(path_to_pdf[2])
  state_data <- data[[1]] %>%
    data.frame(stringsAsFactors = FALSE) %>%
    janitor::row_to_names(row_number = 1) %>%
    dplyr::rename(inmates_tested   = Tested,
                  inmates_pending  = Pending,
                  inmates_positive = Positive,
                  inmates_negative = Negative) %>%
    dplyr::mutate_all(readr::parse_number) %>%
    dplyr::mutate(state = "Oklahoma",
                  scrape_date = lubridate::today())
  
  facility_data <- data[[2]]
  facility_data <- data.frame(facility_data, stringsAsFactors = FALSE)
  column_names  <- as.character(as.vector(facility_data[1, ]))
  column_names  <- paste(column_names, as.character(as.vector(facility_data[2, ])))
  column_names  <- paste(column_names, as.character(as.vector(facility_data[3, ])))
  column_names  <- paste(column_names, as.character(as.vector(facility_data[4, ])))
  column_names  <- janitor::make_clean_names(column_names)
  
  cols_to_remove       <- which(column_names == "x")
  column_names         <- column_names[-cols_to_remove]
  facility_data        <- facility_data[-c(1:4), -cols_to_remove]
  names(facility_data) <- column_names
  
  facility_data2        <- data[[3]]
  facility_data2        <- data.frame(facility_data2, stringsAsFactors = FALSE)
  names(facility_data2) <- column_names
  
  facility_data <-
    facility_data %>%
    dplyr::bind_rows(facility_data2) %>%
    dplyr::rename(facilities         = institution,
                  inmates_positive   = number_of_inmates_who_tested_positive,
                  inmates_quarantine = number_of_inmates_in_quarantine,
                  housing_type       = housing_type_cell_open_bay_combo,
                  inmates_isolation  = number_of_inmates_in_isolation,
                  staff_positive     = number_of_staff_who_have_reported_positive_tests) %>%
    dplyr::mutate_at(c("inmates_positive",
                       "inmates_quarantine",
                       "inmates_isolation",
                       "staff_positive",
                       "units_in_quarantine"),
                     na_if, "-") %>%
    dplyr::mutate_at(c("inmates_positive",
                       "inmates_quarantine",
                       "inmates_isolation",
                       "staff_positive",
                       "units_in_quarantine"),
                     readr::parse_number) %>%
    dplyr::mutate(state = "Oklahoma",
                  scrape_date = lubridate::today())
  
  
  list(ok_facilities = facility_data,
       ok_total = state_data)
  
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
    dplyr::mutate(state            = "Missouri",
                  scrape_date      = lubridate::today()) %>%
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
  
  download.file(
    "https://data.aclum.org/sjc-12926-tracker/session/9e2945e433500b9bda15509f105fbce6/download/downloadData?w=",
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
    modify_at(3:18,~as.numeric(.)) %>% 
    filter(scrape_date == today())
}
# D.C. --------------------------------------------------------------------
get_dc_covid_data <- function(dc_doc_path){
  text_data <- dc_doc_path %>%
    html_nodes("ul:nth-child(24) li") %>%
    html_text() %>%
    str_extract_all("\\d+") %>%
    unlist() 
  if(length(text_data[!duplicated(text_data)])!= 8){
    rlang::abort("D.C has changed their columns or they have duplicated fields")
  }
  text_data[!duplicated(text_data)] %>% 
    split(1:length(.)) %>% 
    as_tibble(.name_repair = "minimal")  %>%
    `[`(c(1:3,5:8)) %>% 
    rename(
      inmates_positive = `1`,
      inmates_positive_isolation = `2`,
      inmates_recovered = `3`,
      inmates_quarantine = `5`,
      inmates_positive_quarantine = `6`,
      inmates_return_gen_pop = `7`,
      inmates_deaths = `8`
    ) %>%
    mutate(state = "District of Columbia",
           scrape_date = today())
}


# Puerto Rico -------------------------------------------------------------

# read_html("http://dcr.pr.gov/covid-19/") %>% 
#   html_nodes("p.wp-block-pdfemb-pdf-embedder-viewer") %>% 
#   html_nodes("img")
# 
# image_to_test <- image_read("~/Downloads/download.png") %>% 
#   image_convert(type = "Grayscale") 
# image_to_test_info <- image_to_test %>% 
#   image_info()
# crop_str <- sprintf('%sx1000+10+%s', image_to_test_info$width, image_to_test_info$height - 10)
# image_to_test_info
# image_to_test %>% 
#   image_crop("1574x400+100-10")

# Tennessee ---------------------------------------------------------------

get_tn_covid_data <- function() {
  table_opt <-
    list(c(
      top = 128.27711,
      left = 16.04819,
      bottom =  465.28916,
      right =  562.55422
    ))
  test1 <-
    extract_tables(
      "https://www.tn.gov/content/dam/tn/correction/documents/TDOCInmatesCOVID19.pdf",
      area = table_opt
    )
  
  test1[[1]] %>%
    as_tibble() %>%
    select_if(not_all_empty_char) %>%
    rename(
      facilities = V1,
      inmates_tested = V3,
      inmates_posiitive = V5,
      inmates_negative = V7,
      inmates_pending = V9
    ) %>%
    filter(facilities != "", inmates_tested != "") %>%
    mutate(across(2:5, parse_number),
           scrape_date = today(),
           state = "Tennessee")
}
