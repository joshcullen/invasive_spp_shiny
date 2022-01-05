

library(terra)
library(sf)
library(tidyverse)
library(leaflet)

lulc<- rast("fake_lulc.tif")
values(lulc)[!is.na(values(lulc))]<- 0.5

#need to convert to web mercator projection for easy interaction w/ leaflet
empty.rast<- lulc %>%
  project(., "EPSG:3857", method = "bilinear")
values(empty.rast)[!is.na(values(empty.rast))]<- 0

i<- 0
repeat {

  plot(empty.rast)
  xy<- click(empty.rast, n = 1, xy = TRUE) %>%
    st_as_sf(., coords = c("x", "y"), crs = crs(empty.rast)) %>%
    st_buffer(., 20000)

  cell.buff.ind<- terra::extract(x = empty.rast, y = vect(xy), cells = TRUE, na.rm = TRUE) %>%
    drop_na()
  empty.rast[cell.buff.ind$cell]<- unlist(empty.rast[cell.buff.ind$cell] + 0.5)
  plot(empty.rast)

  i<- i + 1

  if (i > 10) {

  break  #end interactive updates
  }
}






### Leaflet map
pal<- viridis::inferno(n=100)
empty.rast2<- empty.rast %>%
  raster()

leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
  addProviderTiles(providers$Esri.OceanBasemap, group = "Ocean Basemap",
                   options = tileOptions(continuous_world = F)) %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery",
                   options = tileOptions(continuous_world = F)) %>%
  addProviderTiles(providers$OpenStreetMap, group = "Open Street Map",
                   options = tileOptions(continuous_world = F)) %>%
  setView(lng = -83, lat = 30, zoom = 6) %>%
  addMeasure(position = "topleft",
             primaryLengthUnit = "kilometers",
             primaryAreaUnit = "hectares",
             activeColor = "#3D535D",
             completedColor = "#7D4479") %>%
  addScaleBar() %>%
  addLayersControl(baseGroups = c("World Imagery", "Ocean Basemap", "Open Street Map"),
                   options = layersControlOptions(collapsed = TRUE)) %>%
  addRasterImage(empty.rast2, colors = pal, opacity = 0.8, project = FALSE) %>%
  addLegend(pal = pal, values = values(empty.rast2),
            title = "Occupancy")



