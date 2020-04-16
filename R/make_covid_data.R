source(here::here("R","webscraping_state_info.R"))

data_in_goog_sheet <-
  read_sheet(
    "https://docs.google.com/spreadsheets/d/1CwD8aie_ib1wj3FtqACK3N2xssT0W_vX3d_WkKGpdOw/edit?ts=5e90b732#gid=0"
  )

render_all_pages <- function(google_prison_sheet) {
  #covid scraper for connecticut is broken and need a better way to automate from the image on
  # website to text. mass will be directly from aclu
  fns_list <-
    list(
      alabama = get_ala_covid_data,
      alaska = get_alaska_covid_data,
      arizona = get_arizona_covid_data,
      delaware = get_delaware_covid_data,
      florida = get_fl_covid_data,
      georgia = get_georgia_covid_data,
      idaho = get_idaho_covid_data,
      illinois = get_illinois_covid_data,
      kansas = get_ks_covid_data,
      louisiana = get_la_covid_data,
      new_jersey = get_nj_covid_data,
      new_york = get_nys_covid_data,
      north_dakota = get_nd_covid_data,
      ohio = get_ohio_covid_data,
      pennsylvania = get_pa_covid_data,
      south_carolina = get_sc_covid_data,
      texas = get_texas_covid_data,
      virginia = get_virginia_covid_data,
      washington = get_washington_covid_data
    )
  urls_to_scrape <- data_in_goog_sheet %>%
    filter(scraped_binary == 1,
           !state %in% c("Massachusetts", "Connecticut")) %>%
    pull(link) %>%
    map( ~ safe_get(.))
  # extract results in a compact way. idk if this is necessary anymore since the links should all technically pass
  data_for_use <- urls_to_scrape %>%
    map("result") %>%
    compact()
  # rename data for use
  
  names(data_for_use) <- names(fns_list)
  # this runs the functions against the list of urls in the order present to that it's a 1:1 match
  jails_data <- map2(fns_list, data_for_use,  ~ .(.y))
  jails_data
}

# create summaries and extract summaries for a variety of states with the needed fields
group_summary <- function(.data,...){
  .data %>% 
    group_by(state,scrape_date) %>% 
    summarise(...)
}


write_facilities_data <-
  function(rendered_jail_data,
           path_to_facilities_data) {
    # read in data from prior period
    past_period <- read_csv(path_to_facilities_data)
    # get data for states which have facilities
    states_with_cc_facility <-
      rendered_jail_data_pages[c(
        "alabama",
        "arizona",
        "florida",
        "georgia",
        "illinois",
        "kansas",
        "pennsylvania",
        "south_carolina",
        "virginia",
        "washington",
        "louisiana",
        "texas"
      )]
    
    # alabama and washington have three rows to will need to be fixed up in the scraper
    cc_facilities  <-
      states_with_cc_facility[!names(states_with_cc_facility) %in% c("alabama", "washington")] %>%
      reduce(bind_rows) %>%
      select(facilities, state, scrape_date, everything()) 
    
    # join all confirmed facilities
    all_confirmed_facilities <-
      list(
        states_with_cc_facility[["washington"]]$confirmed_cases,
        states_with_cc_facility[["alabama"]],
        cc_facilities
      ) %>%
      reduce(bind_rows) %>%
      filter(!str_detect(facilities, regex("Total")))
    
}



summary_states<- states_with_cc_facility[!names(states_with_cc_facility) %in% c("alabama","washington")] %>%
  map(~select(.,state,facilities,inmates_positive,scrape_date)) %>% 
  reduce(bind_rows) %>% 
  filter(facilities != "Total") %>% 
  group_summary(total_inm_positive = sum(inmates_positive,na.rm = T)) 
washington_totals <- states_with_cc_facility$washington$confirmed_cases %>% 
  group_summary(total_inm_positive = sum(inmates_positive,na.rm = T))
alaska_totals <- jails_data$alaska %>% 
  select(state,scrape_date,total_inm_positive = inmates_positive) %>% 
  modify_at(3,~as.numeric(.))
new_jersey_totals <- jails_data$new_jersey$confirmed_nj_doc %>% 
  select(state,scrape_date,inmates_positive = INMATES,staff_positive = EMPLOYEES) %>% 
  group_summary(total_inm_positive = sum(inmates_positive,na.rm = T))
new_york_totals <- jails_data$new_york %>% 
  select(state,scrape_date,total_inm_positive = incarcerated_positive) %>% 
  modify_at(3,~as.numeric(.))

# collapse data into one df
reduced_data <- list(
  summary_states,
  washington_totals,
  alaska_totals,
  new_jersey_totals,
  new_york_totals
) %>%
  reduce(bind_rows) 
reduced_data %>% 
  write_csv("state_summaries_inm_pos.csv")



