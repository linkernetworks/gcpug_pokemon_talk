library(dplyr)
library(lubridate)
library(leaflet)
library(pmbq)
library(htmlwidgets)

sightings_grid_latlon_custom <- sightings_grid_latlon %>%
  mutate(log_avg_cnt = log10(avg_cnt))

pal <- colorNumeric(
  palette = "RdYlBu",
  domain = sightings_grid_latlon_custom$log_avg_cnt
)

map <- sightings_grid_latlon_custom %>%
  leaflet() %>%
  addCircleMarkers(
    lat=~y,
    lng=~x,
    radius = ~log_avg_cnt,
    color = ~pal(log_avg_cnt),
    stroke = F,
    fillOpacity = 0.8,
    popup=~as.character(round(avg_cnt))) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addLegend(
    "bottomright",
    title="Average Daily Sightings",
    values=~log_avg_cnt,
    pal=pal,
    labFormat = labelFormat(transform=function(x) round(10^x)))

saveWidget(map, "sightings.html")

snorlax_taipei <- snorlax %>%
  filter(
    longitude>121.415265 & latitude> 24.944514, 
    latitude<25.093436& longitude<121.618531,
    created<"2016-08-31") %>%
  mutate(date_created= as.factor(date(created)))

snorlax_icon <- makeIcon(
  iconUrl = "icons/143.png",
  iconWidth = 32,
  iconHeight = 32)

snorlax_map <- snorlax_taipei %>%
  leaflet() %>%
  addMarkers(
    lng=~longitude,
    lat=~latitude,
    popup=~as.character(created),
    icon = snorlax_icon
    ) %>%
  addTiles()%>%
  addProviderTiles("CartoDB.Positron") 

saveWidget(snorlax_map, "snorlax_map.html")

dragonite_taipei <- dragonite %>%
  filter(
    longitude>121.415265 & latitude> 24.944514, 
    latitude<25.093436& longitude<121.618531,
    date(created)<"2016-08-31") %>%
  mutate(date_created= as.factor(date(created)))

dragonite_icon <- makeIcon(
  iconUrl = "icons/149.png",
  iconWidth = 32,
  iconHeight = 32)

dragonite_map <- dragonite_taipei %>%
  leaflet() %>%
  addMarkers(
    lng=~longitude,
    lat=~latitude,
    popup=~as.character(created),
    icon = dragonite_icon
  ) %>%
  addTiles()%>%
  addProviderTiles("CartoDB.Positron") 

saveWidget(dragonite_map, "dragonite_map.html")
