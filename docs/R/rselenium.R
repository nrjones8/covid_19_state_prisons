get_california_incarcerated_data <- function() {
library(RSelenium)
remDr <- RSelenium::remoteDriver(
  remoteServerAddr = "localhost",
  browser = "firefox",
  port = 13L)
remDr$open()
remDr$navigate("https://app.powerbigov.us/view?r=eyJrIjoiOWQ3YzQ0YjItYjIxMy00ZTVlLWIxODMtMjE4YzM2N2QzZmY4IiwidCI6IjA2NjI0NzdkLWZhMGMtNDU1Ni1hOGY1LWMzYmM2MmFhMGQ5YyJ9")


Sys.sleep(15)

library(rvest)
library(dplyr)
inconclusive <-
  read_html(remDr$getPageSource()[[1]]) %>%
  html_nodes("div:nth-child(5) .tablixAlignCenter") %>%
  html_text()
inconclusive <- inconclusive[-length(inconclusive)]
inconclusive <- inconclusive[-1]

negative <-
  read_html(remDr$getPageSource()[[1]]) %>%
  html_nodes("div:nth-child(6) .tablixAlignCenter") %>%
  html_text()
negative <- negative[1:(grep("State", negative) - 1)]
negative <- negative[-1]

positive <-
  read_html(remDr$getPageSource()[[1]]) %>%
  html_nodes("div:nth-child(7) .tablixAlignCenter") %>%
  html_text()
positive <- positive[-length(positive)]
positive <- positive[-1]

resolved <-
  read_html(remDr$getPageSource()[[1]]) %>%
  html_nodes("div:nth-child(8) .tablixAlignCenter") %>%
  html_text()
resolved <- resolved[-length(resolved)]
resolved <- resolved[-1]


resolved_in_previous_calendar_day <-
  read_html(remDr$getPageSource()[[1]]) %>%
  html_nodes("div:nth-child(9) .tablixAlignCenter") %>%
  html_text()
resolved_in_previous_calendar_day <- resolved_in_previous_calendar_day[-length(resolved_in_previous_calendar_day)]
resolved_in_previous_calendar_day <- resolved_in_previous_calendar_day[-1]

deaths <-
  read_html(remDr$getPageSource()[[1]]) %>%
  html_nodes("div:nth-child(10) .tablixAlignCenter") %>%
  html_text()
deaths <- deaths[-length(deaths)]
deaths <- deaths[-1]

prison <-
  read_html(remDr$getPageSource()[[1]]) %>%
  html_nodes("div:nth-child(1) .tablixAlignCenter") %>%
  html_text()
prison <- stringr::str_trim(prison)
prison <- prison[-grep("State", prison)]
prison <- prison[(grep("Deaths in Previous  Calendar Day", prison) + 1):length(prison)]
prison <- prison[-grep("[0-9]", prison)]



california_prison_level <- data.frame(state              = "California",
                                      scrape_date        = lubridate::today(),
                                      facility           = prison,
                                      inconclusive_tests = inconclusive,
                                      inmates_negative   = negative,
                                      inmates_positive   = positive,
                                      inmates_recovered  = resolved,
                                      inmates_recovered_in_previous_calendar_day = resolved_in_previous_calendar_day,
                                      inmates_deaths     = deaths,
                                      stringsAsFactors   = FALSE) 
california_prison_level <-
  california_prison_level %>%
  dplyr::mutate_at(4:9, readr::parse_number)

return(california_prison_level)
}


