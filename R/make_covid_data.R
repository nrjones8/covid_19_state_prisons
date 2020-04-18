source(here::here("R","webscraping_state_info.R"))
safe_get <- safely(read_html)
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
      california = get_california_covid_data,
      delaware = get_delaware_covid_data,
      florida = get_fl_covid_data,
      georgia = get_georgia_covid_data,
      idaho = get_idaho_covid_data,
      illinois = get_illinois_covid_data,
      indiana = get_indiana_covid_data,
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
  urls_to_scrape <- google_prison_sheet %>%
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
jails_data <- render_all_pages(data_in_goog_sheet)
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
      rendered_jail_data[c(
        "alabama",
        "arizona",
        "florida",
        "georgia",
        "indiana",
        "illinois",
        "kansas",
        "new_jersey",
        "pennsylvania",
        "south_carolina",
        "virginia",
        "washington",
        "louisiana",
        "texas",
        "north_dakota"
      )]
    
    # indiana has two or more  sets of data and will need to be fixed up in the scraper somehow
    cc_facilities  <-
      states_with_cc_facility[!names(states_with_cc_facility) %in% c("indiana","ohio","new_jersey")] %>%
      reduce(bind_rows) %>%
      select(facilities, state, scrape_date, everything()) 
    
    # join all confirmed facilities
    all_confirmed_facilities <-
      list(
        states_with_cc_facility[["indiana"]]$offenders,
        states_with_cc_facility[["ohio"]]$ohio_facility,
        states_with_cc_facility[["new_jersey"]]$confirmed_nj_doc,
        cc_facilities
      ) %>%
      reduce(bind_rows) %>%
      #this line of code is sacrosanct. remove it at your own risk
      filter(!str_detect(facilities, regex("Total")))
 
list( all_confirmed_facilities %>% 
   modify_if(is.integer,~as.numeric(.)), 
  past_period)
  } 
path_to_data <- glue("facilities_data_{year(today()-1)}_0{month(today()-1)}_{day(today()-1)}.csv")
path_to_facilities_data <- glue("data/daily/{path_to_data}")
data_facilities <- write_facilities_data(rendered_jail_data = jails_data,path_to_facilities_data = path_to_facilities_data)  %>% 
  map(~as_tibble(.)) %>% 
  bind_rows()

# write the new csv file
path_date <- glue("facilities_data_{year(today())}_0{month(today())}_{day(today())}.csv")
data_facilities %>% 
  write_csv(glue("data/daily/{path_date}"))


write_state_summaries <- function(data_facilities, jails_data) {
  # making the facilities data more modular
  summaries_states_facilities <- data_facilities %>%
    filter(scrape_date == max(scrape_date)) %>%
    group_by(state, scrape_date) %>%
    summarise_at(vars(
      contains("positive"),
      contains("negative"),
      contains("pending"),
      contains("death")
    ),
    ~ sum(., na.rm = T))
  # extracting alaska info
  alaska_summary <- jails_data$alaska %>%
    select(state, scrape_date, everything()) %>%
    modify_at(3:ncol(.),  ~ as.numeric(.)) %>%
    mutate(scrape_date = scrape_date - days(1))
  # extracting ny info
  new_york_totals <- jails_data$new_york %>%
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
      inmates_tests = Tested,
      inmates_pending = Pending,
      inmates_positive = Positive,
      inmates_negative = Negative
    )
  # collapse data into one df
  reduced_data <- list(summaries_states_facilities,
                       alaska_summary,
                       idaho_totals,
                       new_york_totals,
  ) %>%
    reduce(bind_rows)
}



reduced_data %>% 
  write_csv("data/daily/state_summaries_2020_04_16.csv")



