
make_state_choropleth_map <- function() {
  states <- tigris::states()
  states$covid_death <- rnorm(56)
  states$popup <-  paste0("<b>",
                          states$NAME,
                          "</b>",
                          "<br>",
                          "Prisoner Deaths: ", states$covid_death)
  labs <- as.list(states$popup)
  pal <- leaflet::colorNumeric("OrRd", states$covid_death)
  
  leaflet::leaflet() %>%
    leaflet::addTiles('http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', 
                      attribution = '&copy; <a href="http://openstreetmap.org">
                OpenStreetMap</a> contributors') %>%
    leaflet::setView(-98.483330, 38.712046, zoom = 4) %>% 
    leaflet::addPolygons(data = states , 
                         col = "black",
                         weight = 1,
                         fillColor = pal(states$covid_death),
                         fillOpacity = 1,
                         label = lapply(labs, htmltools::HTML),
                         popup = states$popup) %>%
    leaflet::addLegend(pal = pal, 
                       values = states$covid_death,
                       opacity = 1,
                       title = "Prisoner Deaths")
    
}

