
## **Invasive Species Expert Elicitation App**

This tool was designed to support a workshop on the projected expansion of invasive species in the southeast United States (US). The workshop was hosted by project leads at the University of Florida and was supported by the Southeast Climate Adaption Science Center (SECASC). Given the general lack of information on the distributions and habitat preferences of invasive species, this tool sought to collect the best available information based on knowledge from experts in the field.

#### **References**

*Insert any relevant refs here*


### **Objectives**

The Invasive Species Expert Elicitation App was created to facilitate the collection of data during the SECASC Invasive Species Expert Elicitation Workshop (April 27-28, 2022). This workshop aimed to use expert elicitation to assess invasive species risk throughout the southeastern US. We have developed a purpose built web application that was used to collate expert knowledge to produce models of current species distributions and future distributions under climate change. The workshop focused on 13 invasive species that are currently, or have the potential to be, highly detrimental to ecosystems of the SE US. These species come from a previous meeting over Zoom in Autumn 2021 where species were collectively ranked among workshop participants. Thus, this workshop is quite focused on taxonomic expertise in distribution and ecological/trait data.

### **Tool Description**

As part of this expert elicitation tool, participants first consider the influence of land cover on invasive species habitat suitability in the southeastern US followed by an estimate of their relative geographic prevalence. Additionally, participants are asked to self-assess their confidence in the spatial prediction they have made for each species as well as their prior experience working with a given species. These results were stored in Google Drive and subsequently used to derive and critique estimates of each species' current distribution.

#### **About Tab**

Provides basic instructions for how the app should be used and provides images associated with each of the main invasive species to be evaluated.

##### **Step 1 Tab**

A set of widgets to be adjusted by the user are provided in the sidebar, including a radio button to evaluate the 13 'main' (or 13 'extra') species, a dropdown menu to select the species, and sliders for the user to adjust the expected habitat suitability per land cover type. Additionally, a slider is included at the bottom of the sidebar for the user to self-assess their confidence in their submission. Info buttons are listed next to each of the widgets and a map of land cover throughout the southeastern US is provided in the main panel of the tool as a reference.

1.  Start by selecting a species from the dropdown menu
2.  Go through and adjust each of the 15 land cover sliders that you think best reflect this species' preference
3.  Click the "Update Suitability" button to generate a map based on these selected inputs
4.  Based on your expert knowledge, either go back and adjust the sliders further to better reflect the predicted habitat suitability map, or proceed further
5.  Once satisfied with your selections, adjust the 'Confidence' slider
6.  Click the 'Submit Response' button
7.  Repeat steps 1-6 until all species have been completed

##### **Step 2 Tab**

A set of widgets to be adjusted by the user are provided in the sidebar, including a radio button to evaluate the 13 'main' (or 13 'extra') species, a dropdown menu to select the species, and sliders for the user to adjust the options when updating the interactive map in the main panel. Additionally, check boxes and a slider are included at the bottom of the sidebar for the user to report their experience with the selected species and to self-assess the confidence in their submission, respectively. Info buttons are listed next to some of the widgets to provide further details. The interactive map in the main panel is intended to be clicked on to manually define the relative prevalence of each species across the study area.

1.  Start by selecting a species from the dropdown menu
2.  Adjust the sliders named 'Raster Opacity', 'Buffer Radius Size', and 'Added Intensity Value' to determine how the overlaid raster on the interactive map appears, as well as the relative prevalence added to a set of pixels upon each mouse click
3.  Click across the raster layer in the interactive map to generate a relative prevalence surface, adjusting the 'Buffer Size' and 'Intensity' as needed; the 'Intensity' can also be made slightly negative if wanting to reduce values in certain places
4.  Check the created prevalence map: if satisfied, proceed further, otherwise either click the 'Clear Raster' button to start from scratch or continue modifying the layer
5.  Once the map has been completed, enter the appropriate responses related to user experience and confidence in their map submission
6.  Click the 'Submit Response' button
7.  Repeat steps 1-6 until all species have been completed


### **Contributors**

Joshua A. Cullen *joshcullen10@gmail.com* <br> Brett Scheffers *brett.scheffers@ufl.edu* <br> Luke Evans *lukeevans@ufl.edu* <br> Alex Baecher *jbaecher@ufl.edu* 

### **Software Dependencies**

| **Software** | **Version** | **OS bit** | **Reference** |
|------------|------------|------------|-----------------------------------|
| R | 4.4.2 | 64 | The R Project for Statistical Computing <https://www.r-project.org/>. |

### **Package Dependencies**

Packages dependencies can be found at the top of the [app.R](https://github.com/joshcullen/invasive_spp_shiny/blob/main/invasive_expert_elicitation/app.R) file.

### **Folder and file structure of the main repository**

-   **invasive_expert_elicitation**: folder with all files needed to run the app
    -   **rsconnect**: folder storing .dcf file for deploying tool on shinyapps.io website
    -   **www**: folder including image files for each of the 'main' invasive species
    -   `app.R`: code for running Shiny app
    -   `gtag.html`: file for using Google Analytics to evaluate number of users
    -   `NLCD_data.tif`: GeoTIFF file that provides rasterized land cover for use in Step 1 tab of app
    -   `NLCD_metadata.csv`: a data dictionary for each of the land cover types in the NLCD data
    -   `state_bounds.shp`: along with other files of same name but different extension, defines state boundaries in southeastern US via shapefile format
    -   `utils.R`: a script of utility functions to modularize and clean up code in `app.R`
-   `Check colorblind accessibility.R`: script outside of app to check that color palette is colorblind-friendly
-   `Download NLCD data.R`: script outside of app to download the NLCD dataset to be used as input for tool
-   `Example script for dynamically updated occupancy raster.R`: script outside of app to figure out how to create interactive raster layer used in Step 2 of tool
-   `Example script for LULC raster.R`: script outside of pp to rasterize the vector NLCD data
-   `fake_lulc.tif`: GeoTIFF layer to define rasterized landcover layer that also provides study area bounds for `Example script for LULC raster.R` and `Example script for dynamically updated occupancy raster.R`
-   `Habitat_suitability_map.png`: image demonstrating what the habitat suitability map might look like in Step 1 of tool
-   `LULC_map.png`: image of mapped land cover from the NLCD dataset
-   `Prevalence_map.png`: image demonstrating what the relative species prevalence map may look like when completing Step 2 of tool
-   `DISCLAIMER.md`: shows the standard USGS disclaimer.
-   `LICENSE.md`: shows the official CC0 license.
-   `README.md`: is this document.
-   `code.json`: provides metadata for the project repository.

### **Runtime**

Initial load time is approximately 5 seconds. This app is relatively lightweight, so is quick to load from a cold-start.

### **Disclaimers**

**Software Disclaimer**

This software is preliminary or provisional and is subject to revision. It is being provided to meet the need for timely best science. The software has not received final approval by the U.S. Geological Survey (USGS). No warranty, expressed or implied, is made by the USGS or the U.S. Government as to the functionality of the software and related material nor shall the fact of release constitute any such warranty. The software is provided on the condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from the authorized or unauthorized use of the software.

### **License**

Creative Commons Zero v1.0 Universal. (CC0;<https://creativecommons.org/public-domain/cc0/>)
