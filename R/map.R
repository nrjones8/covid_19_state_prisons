
make_state_choropleth_map <- function() {
  counties <- sf::read_sf(here::here("data/shapefiles/cb_2018_us_county_20m.shp"))
  counties$cnty_fips <- paste0(counties$STATEFP, counties$COUNTYFP)
  county_hospitals <- readxl::read_excel(here::here("data/misc/KHN_ICU_bed_county_analysis_2.xlsx"))
  
  counties <-
    counties %>%
    left_join(county_hospitals)
  names(counties)[19:21] <- paste0("X", names(counties)[19:21])
  counties$X60plus_pct <- counties$X60plus_pct * 100
  counties$X60plus_pct <- paste0(counties$X60plus_pct, "%")
  
  
  counties$popup <-  paste0("<b>",
                            counties$cnty_name, ", ",
                            counties$state,
                          "</b>",
                          "<br>",
                          "Population Aged 60+: ",
                          counties$X60plus_pct,
                          "<br>",
                          "Total ICU Beds: ", counties$all_icu)
  county_labs <- as.list(counties$popup)
  
  
  states             <- tigris::states()
  states$covid_death <- rnorm(56)
  states$popup       <-  paste0("<b>",
                          states$NAME,
                          "</b>",
                          "<br>",
                          "Prisoner Deaths: ", states$covid_death)
  labs <- as.list(states$popup)
  pal  <- leaflet::colorNumeric("OrRd", states$covid_death)
  
  leaflet::leaflet() %>%
    leaflet::addTiles('http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', 
                      attribution = '&copy; <a href="http://openstreetmap.org">
                OpenStreetMap</a> contributors') %>%
    leaflet::setView(-98.483330, 38.712046, zoom = 4) %>% 
    leaflet::addPolygons(data = states,
                         group = "states",
                         col = "black",
                         weight = 1,
                         fillColor = pal(states$covid_death),
                         fillOpacity = 1,
                         label = lapply(labs, htmltools::HTML),
                         popup = states$popup) %>%
    leaflet::addPolygons(data = counties,
                         group = "counties",
                         col = "black",
                         weight = 1,
                         ,
                         label = lapply(county_labs, htmltools::HTML),
                         popup = counties$popup) %>%
    leaflet::addLegend(pal = pal, 
                       group = "state_legend",
                       values = states$covid_death,
                       opacity = 1,
                       title = "Prisoner Deaths") %>%
    leaflet::groupOptions("states",       zoomLevels = 0:5) %>%
    leaflet::groupOptions("state_legend", zoomLevels = 0:5) %>%
    leaflet::groupOptions("counties",     zoomLevels = 6:18)
  
}

