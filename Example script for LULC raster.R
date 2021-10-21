

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

se <- states(class = "sp") %>%
  subset(NAME %in%
           c("Florida",
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
# 29700

plot(alt, axes = F, legend = F, main = "grain-size = 6 km"); lines(se); box(col = "white", lwd = 1)

blank <- alt > Inf

plot(blank, axes = F, legend = F, main = "grain-size = 6 km"); lines(se); box(col = "white", lwd = 1)


##---------

#create fake LU/LC layer
lulc<- alt

ind<- which(!is.na(values(lulc)))
values(lulc)[ind]<- sample(1:4,
                      size = length(ind),
                      replace = TRUE,
                      prob = c(0.5, 0.2, 0.15, 0.15))

plot(lulc); lines(se)


writeRaster(lulc, "fake_lulc.tif")
lulc<- raster('fake_lulc.tif')
