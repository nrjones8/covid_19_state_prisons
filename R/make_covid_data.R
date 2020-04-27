source(here::here("R","webscraping_state_info.R"))


render_all_pages <- function() {
  #covid scraper for connecticut is broken and need a better way to automate from the image on
  # website to text. mass will be directly from aclu
  #read in the 
  google_prison_sheet <-
    read_sheet(
      "https://docs.google.com/spreadsheets/d/1CwD8aie_ib1wj3FtqACK3N2xssT0W_vX3d_WkKGpdOw/edit?ts=5e90b732#gid=0"
    )
  
  fns_list <-
    list(
      alabama = get_ala_covid_data,
      alaska = get_alaska_covid_data,
      arizona = get_arizona_covid_data,
      california = get_california_covid_data,
      district_columbia = get_dc_covid_data,
      delaware = get_delaware_covid_data,
      federal = get_federal_data,
      florida = get_fl_covid_data,
      #georgia = get_georgia_covid_data,
      idaho = get_idaho_covid_data,
      illinois = get_illinois_covid_data,
      #indiana = get_indiana_covid_data,
      iowa = get_iowa_covid_data,
      kansas = get_ks_covid_data,
      louisiana = get_la_covid_data,
      minnesota = get_minnesota_covid_data,
      missouri = get_missouri_covid_data,
      montana = get_montana_covid_data,
      new_hampshire = get_new_hampshire_covid_data,
      new_jersey = get_nj_covid_data,
      new_york = get_nys_covid_data,
      north_carolina = get_nc_covid_data,
      north_dakota = get_nd_covid_data,
      ohio = get_ohio_covid_data,
      oklahoma = get_oklahoma_covid_data,
      pennsylvania = get_pa_covid_data,
      south_carolina = get_sc_covid_data,
      texas = get_texas_covid_data,
      utah = get_utah_covid_data,
      vermont = get_vermont_covid_data,
      virginia = get_virginia_covid_data,
      washington = get_washington_covid_data
    )
  
  data_for_use <- google_prison_sheet %>%
    filter(
      scraped_binary == 1,
      !state %in% c(
        "Massachusetts",
        "Connecticut",
        "Oregon",
        "Georgia",
        "Indiana",
        "Tennessee"
      )
    ) %>%
    arrange(state) %>%
    pull(link) %>%
    map(~ read_html(.))
  # extract results in a compact way. idk if this is necessary anymore since the links should all technically pass
  # rename data for use
  names(data_for_use) <- names(fns_list)
  # this runs the functions against the list of urls in the order present to that it's a 1:1 match
  jails_data <<- map2(fns_list, data_for_use,  ~.x(.y))
  jails_data
}
#render_all_pages()
# create summaries and extract summaries for a variety of states with the needed fields

group_summary <- function(.data,...) {
  .data %>% 
    group_by(state,
             scrape_date) %>% 
    summarise(...)
}


write_facilities_data <-function(rendered_jail_data ,path_to_facilities_data) {
  # read in data from prior period
  past_period <- read_csv(path_to_facilities_data)
  # get data for states or feds which have facilities
  states_with_cc_facility <-
    rendered_jail_data[c(
      "alabama",
      "arizona",
      "florida",
      "california",
      "illinois",
      "louisiana",
      "kansas",
      "delaware",
      "new_hampshire",
      "north_dakota",
      "iowa",
      "new_jersey",
      "new_york",
      "ohio",
      "oklahoma",
      "pennsylvania",
      "south_carolina",
      "north_carolina",
      "texas",
      "montana",
      "virginia",
      "washington",
      "federal"
    )]
  # oregon data needs to be run separately from the above processes since it uses RSelenium
  # but ultimately produces facility level data
  oregon_data <- get_oregon_covid_data() 
  tn_data <- get_tn_covid_data()
  # indiana has two or more  sets of data and will need to be fixed up in the scraper somehow
  cc_facilities  <-
    states_with_cc_facility[!names(states_with_cc_facility) %in% c("ohio","new_jersey",
                                                                   "federal","new_york",
                                                                   "oklahoma")] %>%
    map(~as_tibble(.)) %>% 
    reduce(bind_rows) %>%
    select(facilities, state, scrape_date, everything()) 
  # modifying fed info
  fed_info <- states_with_cc_facility[["federal"]]$offenders %>% 
    rename_with(cols = vars(contains("_amt")),.fn = ~str_remove_all(.,"_amt")) %>% 
    rename_with(cols = vars(contains("inmates")),.fn = ~str_replace_all(.,"inmate","inmates")) %>% 
    rename_with(cols= vars(contains("death")),.fn = ~str_replace_all(.,"death","deaths")) %>% 
    select(facilities  = id,everything())
  # massachussets data
  mass_data <- get_mass_covid_data()    
  # join all confirmed facilities
  all_confirmed_facilities <-
    list(
      states_with_cc_facility[["ohio"]]$ohio_facility,
      states_with_cc_facility[["new_jersey"]]$confirmed_nj_doc,
      states_with_cc_facility[["oklahoma"]]$ok_facilities,
      states_with_cc_facility[["new_york"]]$facilities,
      oregon_data,
      cc_facilities,
      fed_info,
      mass_data,
      tn_data
    ) %>% 
    reduce(bind_rows) %>% 
    #this line of code is sacrosanct. remove it at your own risk
    filter(!str_detect(facilities, regex("Total"))) 
  
  list(all_confirmed_facilities %>% 
         modify_if(is.integer,~as.numeric(.)), 
       past_period)
  
} 

# write off the facilities level csv
write_facilities_csv <- function(jails_data,path_to_facilities_data ){
  
  # write and collapse the past and present data
  data_facilities <- write_facilities_data(rendered_jail_data = jails_data,path_to_facilities_data = path_to_facilities_data)  %>% 
    map(~as_tibble(.) %>% 
          modify_at(vars(contains("quarantine")),~as.character(.))) %>% 
    reduce(bind_rows) %>% 
    select(facilities,state,scrape_date,everything())  
  # write the new csv file for facilities out
  path_date <- glue("facilities_data_{year(today())}_0{month(today())}_{day(today())}.csv")
  data_facilities %>% 
    write_csv("data/daily/facilities_data_current.csv")
  
  data_facilities %>% 
    write_csv(glue("data/daily/{path_date}"))
}


# writing the state summaries
write_state_summaries <- function(data_facilities, jails_data,manual_entries ) {
  # making the facilities data more modular
  summaries_states_facilities <- data_facilities %>%
    filter(scrape_date == max(scrape_date,na.rm = T),state != "New York") %>%
    group_by(state, scrape_date) %>%
    summarise_at(vars(
      contains("positive"),
      
      contains("negative"),
      contains("pending"),
      contains("death")
    ),
    ~ sum(as.numeric(.), na.rm = T)) %>% 
    ungroup()
  # extracting alaska info
  alaska_summary <- jails_data$alaska %>%
    select(state, scrape_date, everything()) %>%
    modify_at(3:ncol(.),  ~ as.numeric(.)) 
  # extracting ny info
  new_york_totals <- jails_data$new_york$totals %>%
    select(state,
           scrape_date,
           inmates_positive = incarcerated_positive,
           staff_positive,
           staff_deaths) %>%
    modify_at(3:ncol(.),  ~ as.numeric(.))
  
  idaho_totals <- jails_data$idaho %>%
    select(
      state,
      scrape_date,
      inmates_tested ,
      inmates_pending ,
      inmates_positive ,
      inmates_negative 
    ) 
  
  # collapse data into one df
  reduced_data <- list(summaries_states_facilities,
                       alaska_summary,
                       idaho_totals,
                       new_york_totals,
                       jails_data$utah,
                       jails_data$minnesota,
                       jails_data$vermont,
                       jails_data$missouri,
                       manual_entries
  ) %>%
    reduce(bind_rows)
  
}

# writing out the summaries to csvs
write_summary_csv <- function(reduced_data){
  
  path_today_summary <- glue("data/daily/state_summaries_{year(today())}_0{month(today())}_{day(today())}.csv")
  
  reduced_data %>% 
    write_csv(path_today_summary)
  reduced_data %>% 
    write_csv("data/daily/state_summaries_current.csv")
}

