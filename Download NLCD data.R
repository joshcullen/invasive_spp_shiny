library(FedData)  #v3.0.1.90000; for NLCD data
library(sf)
library(raster)
# library(rgdal)
# library(readr)
library(rnaturalearth)
library(tidyverse)

crs <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

# se <- states(class = "sp") %>%
#   subset(NAME %in% c("Florida", "South Carolina", "Georgia", "Alabama")) %>%
#   spTransform(crs)
se <- ne_states(country = "United States of America", returnclass = "sf") %>%
  filter(name %in% c("Florida",
                     "South Carolina",
                     "Georgia",
                     "Alabama")) %>%
  st_transform(crs)

fact <- 6000/30

nlcd <- FedData::get_nlcd(template = se, year = 2019, label = "SECASC") %>%
  raster::aggregate(fact = fact, fun = "modal") %>%
  raster::crop(se) %>%
  raster::mask(se)

# nlcd[nlcd == 0] <- NA

nlcd_rat <- nlcd %>%
  ratify()

nlcd_levels <- levels(nlcd_rat)[[1]]

nlcd_data <- FedData::nlcd_colors() %>%
  filter(ID %in% nlcd_levels$ID)

levels(nlcd_rat) <- levels(nlcd_rat) %>%
  cbind(nlcd_data)

names(nlcd_rat) <- "nlcd"

ggplot() +
  geom_raster(data = as.data.frame(nlcd_rat, xy = T), aes(x = x, y = y, fill = factor(nlcd_ID))) +
  geom_sf(data = se, fill = NA, col = "grey40", size = 1) +
  scale_fill_manual(name = "Land cover",
                    values = nlcd_data$Color,
                    labels = nlcd_data$Class,
                    na.translate = FALSE) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "right") +
  coord_sf()



## Export data

# raster layer
raster::writeRaster(nlcd_rat, "invasive_expert_elicitation/NLCD_data.tif")

# raster metadata for classes
write.csv(nlcd_data, "invasive_expert_elicitation/NLCD_metadata.csv", row.names = FALSE)


##---------

#export Natural Earth state boundaries (due to issue with data from {rnaturalearthhires} on remote shinyapp.io server)

st_write(se, "invasive_expert_elicitation/state_bounds.shp", delete_dsn = TRUE)
