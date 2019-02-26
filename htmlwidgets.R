library(leaflet)
pal <- colorQuantile("YlOrRd", NULL, n = 8)
orstationc <- read.csv('orstationc.csv')
leaflet(orstationc) %>% 
  addTiles() %>%
  addCircleMarkers(color = ~pal(tann))

