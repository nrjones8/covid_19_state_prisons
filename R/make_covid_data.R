source(here::here("R","webscraping_state_info.R"))
library(ggplot2)
library(patchwork)
library(forcats)
# create a safe function
safe_get <- safely(read_html)

data_in_goog_sheet <-
  read_sheet(
    "https://docs.google.com/spreadsheets/d/1CwD8aie_ib1wj3FtqACK3N2xssT0W_vX3d_WkKGpdOw/edit?ts=5e90b732#gid=0"
  )
#covid scraper for connecticut is broken and need a better way to automate from the image on
# website to text. mass will be from aclu and texas is unavailable
fns_list <- list(alabama = get_ala_covid_data,alaska = get_alaska_covid_data,
                 arizona = get_arizona_covid_data,delaware = get_delaware_covid_data,
                 florida = get_fl_covid_data,
                 georgia = get_georgia_covid_data,idaho = get_idaho_covid_data,
                 illinois = get_illinois_covid_data,kansas = get_ks_covid_data,
                 louisiana = get_la_covid_data,new_jersey = get_nj_covid_data,
                 new_york = get_nys_covid_data,north_dakota = get_nd_covid_data,
                 ohio = get_ohio_covid_data,pennsylvania = get_pa_covid_data,
                 south_carolina = get_sc_covid_data,virginia = get_virginia_covid_data,
                 washington = get_washington_covid_data)
urls_to_scrape<- data_in_goog_sheet %>% 
  filter(scraped_binary == 1,!state %in% c("Massachusetts","Texas","Connecticut")) %>%
  pull(link) %>% 
  map(~safe_get(.))
# extract results in a compact way
data_for_use <- urls_to_scrape %>%
  map("result") %>%
  compact()
# this runs the functions against the list of urls in the order present to that it's a 1:1 match
jails_data <- map2(fns_list,data_for_use,~.(.y))
# get data for facilities
states_with_cc_facility <- jails_data[c("alabama","arizona","florida","georgia","illinois","kansas","pennsylvania","south_carolina","virginia","washington","louisiana")]
# create summaries and extract summaries for a variety of states
summary_states <- states_with_cc_facility[!names(states_with_cc_facility) %in% c("alabama","arizona","washington","louisiana")] %>% 
  map(~select(.,state,facilities,inmates_positive,staff_positive)) %>% 
  reduce(bind_rows) %>% 
  filter(facilities != "Total") %>% 
  group_by(state) %>% 
  summarize(total_inm_positive = sum(inmates_positive,na.rm = T),
            total_staff_positive = sum(staff_positive ,na.rm = T)) 


lousiana_totals_inm <- states_with_cc_facility$louisiana$inmate_data %>% 
  group_by(state) %>% 
  summarize(total_inm_positive = sum(inmates_positive))
louisiana_totals_staff <- states_with_cc_facility$louisiana$staff_data %>% 
  group_by(state) %>% 
  summarize(total_staff_positive = sum(Positives))

louisiana_totals <- left_join(lousiana_totals_inm,louisiana_totals_staff)
washington_totals <- states_with_cc_facility$washington$confirmed_cases %>% 
  group_by(state) %>% 
  summarise(total_inm_positive = sum(inmates_positive,na.rm = T),
            total_staff_positive = sum(staff_positive,na.rm = T))
arizona_totals <- states_with_cc_facility$arizona$az_facility %>% 
  group_by(state) %>% 
  summarise(total_inm_positive = sum(inmates_positive,na.rm = T),
            total_staff_positive = sum(inmates_positive,na.rm = T))
alaska_totals <- jails_data$alaska %>% 
  select(state,total_inm_positive = inmates_positive) %>% 
  modify_at(2,~as.numeric(.))
new_jersey_totals <- jails_data$new_jersey$confirmed_nj_doc %>% 
  select(state,inmates_positive = INMATES,staff_positive = EMPLOYEES) %>% 
  group_by(state) %>% 
  summarize(total_inm_positive = sum(inmates_positive,na.rm = T),
            total_staff_positive = sum(staff_positive,na.rm = T))
new_york_totals <- jails_data$new_york %>% 
  select(state,total_inm_positive = incarcerated_positive,total_staff_positive = staff_positive) %>% 
  modify_at(2:3,~as.numeric(.))


# collapse data into one df
reduced_data <- list(
  summary_states,
  louisiana_totals,
  washington_totals,
  arizona_totals,
  alaska_totals,
  new_jersey_totals,
  new_york_totals
) %>%
  reduce(bind_rows) 
inc <- reduced_data %>%  
  ggplot(aes(total_inm_positive,fct_reorder(state,total_inm_positive)))+
  geom_col(fill = "lightblue",color = "black")+
  theme_minimal()+
  coord_cartesian(expand = FALSE)+
  labs(title = "Reported Positive COVID-19 Tests",
       subtitle = glue("Among State Prison Populations: 
    Total States: {length(reduced_data$state)}
    Total Positives: {sum(reduced_data$total_inm_positive)} "),
       x = "Number of People",
       y = "")+
  theme(plot.title = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold"))
reduced_filt <- reduced_data %>% 
  filter(!is.na(total_staff_positive)) 
correction_off <- reduced_filt %>%   
  ggplot(aes(total_staff_positive,fct_reorder(state,total_staff_positive)))+
  geom_col(color = "black",fill = "skyblue")+
  theme_minimal()+
  coord_cartesian(expand = FALSE)+
  labs(title = "Reported Positive COVID-19 Tests ",
       subtitle = glue("Among State Corrections Officials: 
      Totals States: {length(reduced_filt$state)}
      Total Positives: {sum(reduced_filt$total_staff_positive,na.rm = T)} "),
       x = "Number of People",
       y = "")+
  theme(plot.title = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold"))
inc + correction_off+
  plot_annotation(caption = "State departments of corrections differ in their testing and reporting practices. 
                  Data were collected from various State DOCs on 4/13.
                  See covidprisondata.com for more information coming soon.")

order_filt <- reduced_filt %>%
  pivot_longer(2:3) %>% 
  group_by(state) %>% 
  summarise(total = sum(value)) %>% 
  arrange(desc(total))
reduced_filt %>%
  mutate(total_inm_positive = total_inm_positive * -1,
         state = fct_reorder(as_factor(state), order_filt$state)) %>%
  pivot_longer(2:3) %>%
  ggplot(aes(x = fct_inorder(state), y = value , fill = name)) +
  geom_col(position = "identity", ) +
  coord_flip() +
  geom_hline(aes(yintercept = 0)) +
  theme_minimal() +
  scale_y_continuous(limits = c(-600, 600),
                     labels = c("600", "300", "0", "300", "600")) +
  geom_segment(aes(
    x = 13,
    y = 200,
    xend = 13,
    yend = 450
  ), arrow = arrow(length =  unit(.25, "cm"),type = "closed"),lty = "dashed") +
  geom_segment(aes(
    x = 13,
    y = -200,
    xend = 13,
    yend = -450
  ), arrow = arrow(length =  unit(.25, "cm"),type = "closed"),lty = "dashed")+
  labs(
    title = "People Who Have Tested Positive For COVID-19 ",
    subtitle = "in State Facilities",
    y = "Number of People",
    x = "",
    fill = "",
    caption = "State departments of corrections differ in their testing and reporting practices.
                  Data were collected from various State DOCs on 4/13.
                  See covidprisondata.com for more information coming soon."
  ) +
  scale_fill_manual(
    values = c("lightblue", "skyblue"),
    labels = c("Incarcerated People", "Corrections Officials")
  ) +
  theme(
    legend.position = "none",
    legend.key.height = unit(.35, "cm"),
    legend.key.width = unit(.35, "cm"),
    legend.key = element_rect(color = "black")
  )
