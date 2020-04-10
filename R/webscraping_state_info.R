library(tidyverse)
library(rtweet)
library(rvest)
timeline_oren <- get_timeline("everydayasoreng",)
text_of_interest <- timeline_oren %>% 
  filter(str_detect(text,str_flatten(state.name,"|"))) %>% 
  pull(text) 
str_view_all(text_of_interest[2:10],str_flatten(state.name,"|")) 
states_with_covid_info <- tibble(
  state = c("Wisconsin","Iowa","Montana",
            "South Dakota","Maine","Idaho",
            "Missouri","Nebraska","Oklahoma",
            "Alaska","Kansas","Michigan",
            "South Carolina","Vermont","Connecticut",
            "Virginia","Louisiana","Georgia",
            "Colorado","Utah","Illinois",
            "Oregon","Washington","Texas",
            "California","New Jersey","Delaware",
            "Ohio","Pennsylvania","North Carolina",
            "Minnesota"),
  links= c("https://t.co/u6vSnEVT9D","https://t.co/PGcKAcVw3J","https://t.co/hMPDlszXb8",
          "https://t.co/rC9jDdRHqn","https://t.co/ZkXOqM2wGw","https://t.co/Ne1jppp7VI",
          "https://t.co/lPq9F7aLQ6","https://t.co/EGiQQtdd2h","https://t.co/mnBKIxYdlm",
          "https://t.co/2GuahLDouo","https://t.co/0Wosq6c7a7","https://t.co/C5PEGDqEkz",
          "https://t.co/UmRg3LR28P","https://t.co/1XZJQTNTfU","https://t.co/HbM9YuCy2u",
          "https://t.co/91zoEqB3X2","https://t.co/OoI8T20zwI","https://t.co/CCarcR4TX1",
          "https://t.co/h8YUta693C","https://t.co/a6bJomAtle","https://t.co/jrWx6tXRwZ",
          "https://t.co/iNCj60ppsR","https://t.co/BKJKGmvzw7","https://t.co/a3UVlfGaxN",
          "https://t.co/LadSfMds24","https://t.co/7kZ07lMsEX","https://t.co/YODLzibxXI",
          "https://t.co/Jg23NR79rd","https://t.co/3WVVsohb2e","https://t.co/mlGrUAa0q1",
          "https://mn.gov/doc/about/covid-19-updates/")
)
states_with_covid_info %>% 
  write_csv("oren_findings.csv")
# create a safe function
safe_get <- safely(httr::GET)
# scraping of webpages happens heres
url_data <- states_with_covid_info$links %>% 
  map(~safe_get(.))
# extract results in a compact way
url_data %>% 
  pluck("result") %>% 
  compact()
