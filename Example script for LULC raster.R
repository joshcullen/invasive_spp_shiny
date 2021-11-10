

#install.packages("pacman)

# pacman::p_load(
#   "raster",
#   "tidyverse",
#   "tigris",
#   "elevatr",
#   "sp"
# )

library(raster)
library(tidyverse)
library(tigris)
library(elevatr)
library(sp)
library(sf)
library(rnaturalearth)
library(rnaturalearthhires)


# se <- states(class = "sp") %>%
#   subset(NAME %in%
#            c("Florida",
#              "South Carolina",
#              "Georgia",
#              "Alabama"))


se <- ne_states(country = "United States of America", returnclass = "sf") %>%
  filter(name %in% c("Florida",
                     "South Carolina",
                     "Georgia",
                     "Alabama"))

alt <- get_elev_raster(se, z = 8) %>%
  aggregate(fact = 24) %>%
  crop(se) %>%
  mask(se)

res(alt)
# 0.06028644

ncell(alt)
# 29028

plot(alt, axes = F, legend = F, main = "grain-size = 6 km")
plot(se$geometry, add = TRUE)
box(col = "white", lwd = 1)

blank <- alt > Inf

plot(blank, axes = F, legend = F, main = "grain-size = 6 km")
plot(se$geometry, add = TRUE)
box(col = "white", lwd = 1)


##---------

#create fake LU/LC layer
lulc<- alt

ind<- which(!is.na(values(lulc)))
values(lulc)[ind]<- sample(1:4,
                      size = length(ind),
                      replace = TRUE,
                      prob = c(0.5, 0.2, 0.15, 0.15))

plot(lulc)
plot(se$geometry, add = TRUE)


writeRaster(lulc, "invasive_expert_elicitation/fake_lulc.tif")
lulc<- raster('invasive_expert_elicitation/fake_lulc.tif')




##---------

#export Natural Earth state boundaries (due to issue with data from {rnaturalearthhires} on remote shinyapp.io server)

st_write(se, "invasive_expert_elicitation/state_bounds.shp")

