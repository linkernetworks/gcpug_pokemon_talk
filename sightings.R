library(dplyr)
library(lubridate)
library(leaflet)
library(pmbq)
library(htmlwidgets)
library(pokemongor)

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

makeMap<- function(pokemon_df){
  pokemonId <- pokemon_df$pokemonId[1]
  pokemon_name <- pokemon_names$name[pokemonId] %>% tolower()
  taipei_df <- pokemon_df %>%
    filter(
      longitude>121.415265 & latitude> 24.944514, 
      latitude<25.093436& longitude<121.618531,
      created<"2016-08-31") %>%
    mutate(date_created= as.factor(date(created)))
  pokemon_icon <- makeIcon(
    iconUrl = paste0("icons/", pokemonId, ".png"),
    iconWidth = 32,
    iconHeight = 32)
  pokemon_map <- taipei_df %>%
    leaflet() %>%
    addMarkers(
      lng=~longitude,
      lat=~latitude,
      popup=~as.character(created),
      icon = pokemon_icon
    ) %>%
    addTiles()%>%
    addProviderTiles("CartoDB.Positron") 
  saveWidget(pokemon_map, paste0(pokemon_name, "_map.html"))
}

makeMap(lickitung)
makeMap(chansey)
makeMap(snorlax)
makeMap(dragonite)
