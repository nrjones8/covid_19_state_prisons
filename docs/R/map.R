library(tigris)
library(dplyr)
library(leaflet)
library(htmltools)

states <- tigris::states()
states$covid_death <- rnorm(56) * 100
states$popup <-  paste0(states$NAME,
                        "<br>",
                        "Prisoner Deaths: ", states$covid_death)
labs <- as.list(states$popup)
pal <- colorNumeric("OrRd", states$covid_death)

leaflet() %>%
  addTiles('http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', 
           attribution = '&copy; <a href="http://openstreetmap.org">
                OpenStreetMap</a> contributors') %>%
  setView(-98.483330, 38.712046, zoom = 4) %>% 
  addPolygons(data = states , 
              col = "black",
              weight = 1,
              fillColor = pal(states$covid_death),
              fillOpacity = 1,
              label = lapply(labs, HTML),
              popup = states$popup) %>%
  addLegend(pal = pal, 
            values = states$covid_death,
            opacity = 1,
            title = "Prisoner Deaths")
