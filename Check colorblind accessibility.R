

# Check for colorblind accessibility of plots in Shiny app

library(tidyverse)
library(MetBrewer)
library(terra)
library(sf)
library(colorblindr)

# Using screenshots taken from Shiny app
cvd_emulator(file = "LULC_map.png", overwrite = FALSE)
cvd_emulator(file = "Habitat_suitability_map.png", overwrite = FALSE)
cvd_emulator(file = "Prevalence_map.png", overwrite = FALSE)

