
make_choropleth_map <- function(data) {
  # If either of these columns is NA, treat as if it is 0!
  data$total_positive <- rowSums(data[, c("inmates_positive", "staff_positive")],
                                  na.rm = TRUE)
    
  
  
  
  counties <- sf::read_sf(here::here("data/shapefiles/cb_2018_us_county_20m.shp"))
  counties$cnty_fips <- paste0(counties$STATEFP, counties$COUNTYFP)
  county_hospitals <- readxl::read_excel(here::here("data/misc/KHN_ICU_bed_county_analysis_2.xlsx"))
  
  counties <-
    counties %>%
    left_join(county_hospitals)
  names(counties)[19:21] <- paste0("X", names(counties)[19:21])
  counties$X60plus_pct <- counties$X60plus_pct * 100
  counties$X60plus_pct_pretty <- paste0(counties$X60plus_pct, "%")
  
  
  counties$popup <-  paste0("<b>",
                            counties$cnty_name, ", ",
                            counties$state,
                            "</b>",
                            "<br>",
                            "Population Aged 60+: ",
                            counties$X60plus_pct_pretty,
                            "<br>",
                            "Total ICU Beds: ", counties$all_icu)
  county_labs <- as.list(counties$popup)
  
  
  states <- tigris::states()
  states <- sf::st_as_sf(states)
  states$state <- states$NAME
  states <-
    states %>%
    dplyr::left_join(data)
  states$popup <-  paste0("<b>",
                          states$NAME,
                          "</b>",
                          "<br>",
                          "Total Confirmed Positive: ", 
                          format(states$total_positive, big.mark = ","),
                          "<br>",
                          "Incarerated People Confirmed Positive: ", 
                          format(states$inmates_positive, big.mark = ","),
                          "<br>",
                          "Incarerated People Deaths: ", 
                          format(states$inmates_deaths, big.mark = ","),
                          "<br>",
                          "Corrections Staff Confirmed Positive: ",   
                          format(states$staff_positive, big.mark = ","),
                          "<br>",
                          "Corrections Staff Deaths: ",   
                          format(states$staff_deaths, big.mark = ","))
  labs <- as.list(states$popup)
  
  
  
  pal  <- leaflet::colorNumeric("OrRd", states$total_positive)
  
  county_pal  <- leaflet::colorNumeric("OrRd", counties$X60plus_pct)
  
  css_fix <- "div.info.legend.leaflet-control br {clear: both;}" # CSS to correct spacing
  html_fix <- htmltools::tags$style(type = "text/css", css_fix)  # Convert CSS to HTML
  
  leaflet::leaflet() %>%
    leaflet::addTiles('http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', 
                      attribution = '&copy; <a href="http://openstreetmap.org">
                OpenStreetMap</a> contributors') %>%
    leaflet::setView(-98.483330, 38.712046, zoom = 4) %>% 
    leaflet::addPolygons(data        = states,
                         group       = "states",
                         col         = "black",
                         weight      = 1,
                         fillColor   = pal(states$total_positive),
                         fillOpacity = 1,
                         label  = lapply(labs, htmltools::HTML),
                         popup  = states$popup) %>%
    leaflet::addPolygons(data   = counties,
                         group  = "counties",
                         col    = "black",
                         weight = 1,
                         fillOpacity = 1,
                         fillColor = county_pal(counties$X60plus_pct),
                         label  = lapply(county_labs, htmltools::HTML),
                         popup  = counties$popup) %>%
    leaflet::addLegend(pal      = pal, 
                       group    = "states",
                       values   = states$total_positive,
                       opacity  = 1,
                       na.label = "Data Not Available",
                       title    = paste0("Incarcerated People + 
                                  <br>Corrections Staff <br>Confirmed Positive
                                  <br>(",
                                         make_pretty_date(unique(data$scrape_date)),
                                         ")")) %>%
    leaflet::addLegend(pal      = county_pal, 
                       group    = "counties",
                       values   = counties$X60plus_pct,
                       opacity  = 1,
                       na.label = "Data Not Available",
                       title    = "Percent of Population <br>Aged 60+: ") %>%
    leaflet::groupOptions("states",       zoomLevels = 0:5) %>%
    leaflet::groupOptions("counties",     zoomLevels = 6:18) %>%
    htmlwidgets::prependContent(html_fix) 
  
}

