data_in_goog_sheet <- read_sheet("https://docs.google.com/spreadsheets/d/1CwD8aie_ib1wj3FtqACK3N2xssT0W_vX3d_WkKGpdOw/edit?ts=5e90b732#gid=0")

safe_get <- safely(read_html)

urls_to_scrape <- data_in_goog_sheet %>%
  filter(scraped_binary == 1,
         !state %in% c("Massachusetts", "Connecticut")) %>%
  pull(link) %>%
  map( ~ safe_get(.))


z <- get_georgia_covid_data(data_in_goog_sheet$link[data_in_goog_sheet$state == "Georgia"])
