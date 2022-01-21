
#################################################################################################
### A Shiny web application that will be used as part of an expert elicitation workshop to    ###
### determine potential habitat suitability and future range shifts of invasive spp in the SE ### ### United States                                                                             ###
#################################################################################################



library(shiny)
library(terra)  #in place of {raster}
library(tidyverse)
library(leaflet)
library(leaflet.extras)  #do I still need this?
library(sf)
library(bslib)
library(googlesheets4)
library(googledrive)
library(raster)
library(rgdal)  #if not included, app doesn't load on shinyapps.io
library(janitor)

# load helper functions
source("utils.R")

options(shiny.trace=FALSE)
options(shiny.fullstacktrace=TRUE)


### Set up OAuth authorization to connect to Google Sheets
# drive_auth(cache = ".secrets")  #for the first time running the app in R to get the OAuth token
drive_auth(cache = ".secrets", email = TRUE)
gs4_auth(token = drive_token())

#get sheet id: as_sheets_id("https://docs.google.com/spreadsheets/d/1BBDaElkbc5hDWrp4WVTF-jkMwmSHNsN8m_UZkSAhdZc/edit?usp=sharing")
sheet_id <- "1BBDaElkbc5hDWrp4WVTF-jkMwmSHNsN8m_UZkSAhdZc"



### Load state boundaries for SE United States

# CRS for NLCD data and state boundaries
crs <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

se <- st_read("state_bounds.shp", crs = crs, quiet = TRUE)


### Load NLCD raster and metadata
lulc<- rast("NLCD_data.tif")
nlcd_data<- read.csv("NLCD_metadata.csv")

### Create new raster for displaying habitat suitability
hab_suit<- lulc
values(hab_suit)[!is.na(values(lulc))]<- 0.5


### Classify NLCD raster (as factor)
# levs<- left_join(terra::cats(lulc, 1), nlcd_data, by = 'ID')
# levels(lulc) <- levs
names(lulc) <- "nlcd"


spp_names<- c("Burmese python", "Argentine black and white tegu", "Nile monitor")


# Define sliders for habitat suitability map in UI
hab.id <- janitor::make_clean_names(nlcd_data$Class)
hab.label<- nlcd_data$Class
hab.sliders <- map2(hab.id, hab.label, sliderInput01)


# Define raster layer for interactively modifying as part of occupancy;
#need to convert to web mercator projection for easy interaction w/ leaflet
empty.rast<- lulc %>%
  terra::project(., "EPSG:3857", method = "bilinear")
terra::values(empty.rast)[!is.na(terra::values(empty.rast))]<- 0


# Define possible tile sources to choose from in Leaflet
tile.sources <- c("Esri.WorldImagery",
                  "Esri.OceanBasemap",
                  "OpenStreetMap")



# Define UI for application
ui <- navbarPage("Expert Elicitation of Invasive Species Habitat Suitability",
                 theme = bs_theme(bootswatch = "yeti", version = 5),


                 tabPanel("About",
                          fluidRow(
                              column(width = 8, offset = 2, h4(strong("How to use this app"))),
                              column(width = 8, offset = 2, p("As part of this expert elicitation, participants will first select the species on which they have expertise in the section below before proceeding to the next steps. These subsequent step will allow participants to first consider the influence of land cover on invasive species connectivity and habitat suitability. Additionally, participants will also be asked to self-assess their confidence in the spatial prediction they have made for each of the selected species before submitting their results. These results will then be used in the following days of the workshop.")
                                     ),  #close column for text paragraph,

                                     column(width = 8, offset = 2, h4(strong("Information on Invasive Species of Interest"))),

                                     column(width = 8, offset = 2, p(strong(spp_names[1])),
                                            img(src="burmese_python.jpg", align = "center",
                                                height = 225, width = 300),
                                            br(),
                                            br(),
                                            p("Burmese pythons ",em("(Python bivittatus)")," are large constrictors native to Southeast Asia. It’s native range is broad and whilst it can survive in a variety of habitats, it has preferences for wetland, swamp, and other aquatic systems. Despite this, as with other large constrictors, it is also arboreal. They reach in excess of 85m in length, meaning in their introduced range they encounter few natural predators. They are reported to have become established in the USA in 2000 (Harvey et al. 2016), but have been spotted intermittently for 20 years prior.  and  The total ecological impact of Burmese is difficult to characterize, however they are known to prey on a range of native fauna, including many native mammals (Dorcas et al. 2012)."),
                                            br(),
                                            br(),

                                            p(strong(spp_names[2])),
                                                   img(src="argentine_bw_tegu.jpg",
                                                       align = "center",
                                                       height = 225, width = 300),
                                                   br(),
                                                   br(),
                                                   p("The Argentine black and white tegu", em("(Salvator merianae)"), "is the largest member of the genus of tegus, Salvator. They are native to large parts of central and southern South America. Their broad habitat plasticity, and diverse diet allow make them to excellent at adapting  to new ecotypes and make them superior invaders. Behaviors such as burrowing mean that cold winters may not necessarily restrict invasive expansion across the US."),
                                            br(),
                                            br(),

                                            p(strong(spp_names[3])),
                                            img(src="nile_monitor.jpg",
                                                align = "center",
                                                height = 225, width = 300),
                                            br(),
                                            br(),
                                            p("Nile monitors", em("(Varanus niloticus)"), "native to Africa, are the largest species of the monitor lizard family, Varanidae family--, a family where body mass varies by five full orders of magnitude. It has a broad distribution, occurring across much of Africa, suggesting low . The specificity plasticity in ecosystem requirements, a hallmark of  means that it has a high invasive potential. In addition to this, Nile monitors have very broad diets, threatening a range of native fauna, within the US.")
                              ),  #close column() for text
                          )  #close fluidRow
                 ),  #close "About" tabPanel




                 tabPanel("Step 1",
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            h4("Expected Suitability of Land Cover"),
            selectInput("species_habsuit",
                        "Select a species",
                        choices = spp_names,
                        selected = spp_names[1]),
            br(),
            hab.sliders,  #habitat suitability sliders
            actionButton("update_button",
                         "Update Map",
                         class = "btn-primary"),
            br(), br(), br(), br(),
            sliderInput("conf_habsuit",
                        "Confidence in Habitat Suitability:",
                        min = 0,
                        max = 1,
                        value = 0.5,
                        step = 0.25),
            actionButton("habsuit_submit_button",
                         "Submit Response",
                         class = "btn-dark")
        ),  #close sidebarPanel

        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("lulcMap"),
           plotOutput("habitatMap")
        )  #close mainPanel
    )  #close sidebarLayout
                 ),  #close "Step 1" tabPanel




    tabPanel("Step 2",
             sidebarLayout(
               sidebarPanel(
                 h4("Likelihood of Occupancy"),
                 selectInput("species_occ",
                             "Select a species",
                             choices = spp_names,
                             selected = spp_names[1]),
                 selectInput("tiles",
                             "Basemap",
                             tile.sources),
                 radioButtons("radio",
                              "Time Period",
                              choices = c("Current", "Future"),
                              selected = "Current"),
                 sliderInput("alpha",
                             "Raster Opacity",
                             min = 0,
                             max = 1,
                             value = 0.5,
                             step = 0.1),
                 sliderInput("buff",
                             "Buffer Size (km)",
                             min = 5,
                             max = 100,
                             value = 20,
                             step = 1),
                 sliderInput("intensity",
                             "Added Intensity Value",
                             min = -2,
                             max = 10,
                             value = 1,
                             step = 0.5),
                 actionButton("clear_raster_button",
                              "Clear Raster",
                              class = "btn-dark"),
                 br(), br(), br(), br(),
                 sliderInput("conf_occ",
                             "Confidence in Occupancy Selection:",
                             min = 0,
                             max = 1,
                             value = 0.5,
                             step = 0.25),
                 actionButton("occ_submit_button",
                              "Submit Response",
                              class = "btn-dark")
               ),  #close sidebarPanel

               mainPanel(
                 leafletOutput("occmap", height = "100%", width = "100%")
               )  #close mainPanel
             )  #close sidebarLayout
             )  #close "Step 2" tabPanel
)  #close navbarPage




# Define server
server <- function(input, output) {


  ####################################
  ### Code for Habitat Suitability ###
  ####################################


  # Create modal pop-up for entering name
  popupModal <- function(failed = FALSE) {
    modalDialog(
      textInput("name", "Please enter your full name"),
      if (failed)
        div(tags$b("Your name is required", style = "color: red;")),

      footer = tagList(
        actionButton("ok", "OK")
      )
    )
  }

  # Create modal pop-up for confirmation of submission
  submitModal <- function() {
    modalDialog(
      h3("Your response has been submitted")
    )
  }

  # Show modal when button is clicked.
  showModal(popupModal())

  # Action for closing modal dialog box
  observeEvent(input$ok, {

    if (!is.null(input$name) && nzchar(input$name)) {
      removeModal()
    } else {
      showModal(popupModal(failed = TRUE))
    }
  })



  output$lulcMap <- renderPlot({

    # change to data frame for viz in ggplot
    lulc.df<- as.data.frame(lulc, xy = TRUE)


    # change col name and class in nlcd_data for df merge
    names(nlcd_data)[1]<- "nlcd"
    nlcd_data$nlcd<- factor(nlcd_data$nlcd)

    # Add land cover name to lulc.df
    lulc.df<- left_join(lulc.df, nlcd_data[,1:2], by = 'nlcd')
    # names(lulc.df)[3]<- "value"



    ggplot() +
      geom_tile(data = lulc.df, aes(x, y, fill = nlcd), na.rm = TRUE,
                  alpha = 0.8) +
      scale_fill_manual("LU/LC",
                        values = nlcd_data$Color,
                        labels = nlcd_data$Class) +
      geom_sf(data = se, size = 0.75, color = "black", fill = "transparent") +
      labs(x = "Longitude", y = "Latitude") +
      theme_bw() +
      theme(axis.title = element_text(size = 18),
            axis.text = element_text(size = 14),
            panel.grid = element_blank(),
            legend.title = element_text(size = 14, face = "bold"),
            legend.text = element_text(size = 12)) +
      coord_sf()
  })

    ### Adjust habitat suitability based on sliders


  hab_suit.update<- eventReactive(input$update_button, {  #create reactive update suitability raster

    # Use function to update all of these iteratively
    habsuit_update_fun(id = nlcd_data$ID, lab = hab.id, lulc = lulc, hab_suit = hab_suit,
                       input1 = input)

  })  #close eventReactive


        observeEvent(hab_suit.update(), {

        output$habitatMap <- renderPlot({

            # change to data frame for viz in ggplot
            hab_suit.df<- as.data.frame(hab_suit.update(), xy = TRUE)
            names(hab_suit.df)[3]<- "value"


            ggplot() +
                geom_tile(data = hab_suit.df, aes(x, y, fill = value), alpha = 0.8) +
                scale_fill_viridis_c("Suitability", option = "magma", limits = c(0,1),
                                     na.value = "transparent") +
                geom_sf(data = se, size = 0.75, color = "black", fill = "transparent") +
                labs(x = "Longitude", y = "Latitude") +
                theme_bw() +
                theme(axis.title = element_text(size = 18),
                      axis.text = element_text(size = 14),
                      panel.grid = element_blank(),
                      legend.title = element_text(size = 14, face = "bold"),
                      legend.text = element_text(size = 12)) +
                guides(fill = guide_colorbar(barheight = 20)) +
                coord_sf()
        })

    })  #close observeEvent



    ### Reset habitat suitability sliders/raster when changing species selection

        observeEvent(input$species_habsuit, {

          map(hab.id, updateSliderInput01)

        })



        ### Collect and export responses
        observeEvent(input$habsuit_submit_button, {

          habsuit_conf_export<- data.frame(
            Name = input$name,
            Species = input$species_habsuit,
            Suitability_confidence = input$conf_habsuit
          )

          #Export data to Google Sheets
          sheet_append(ss = sheet_id,
                       data = habsuit_conf_export,
                       sheet = "Suitability")

          #Put data on Google Drive
          fileName <- paste(gsub(pattern = " ", replacement = "", input$name),
                            as.Date.character(Sys.time()),
                            input$species_habsuit,
                            "habsuit.tif",
                            sep = "_")
          # Write the data to a temporary file locally
          filePath <- file.path(tempdir(), fileName)
          writeRaster(hab_suit.update(), filename = filePath, overwrite = TRUE)
          drive_upload(filePath,
                       path = "Invasive Spp Expert Elicitation Workshop/Habitat Suitability Rasters/")

          #Confirm submission


          # Show modal when button is clicked.
          showModal(submitModal())

        })  #close observeEvent







    #########################################
    ### Code for Species Occurrence Range ###
    #########################################

        # Need to convert to 'RasterLayer' to plot w/ Leaflet
        empty.rast2<- empty.rast %>%
          raster()

        pal <- leaflet::colorNumeric(viridis::viridis(100, option = 'magma'),
                                     raster::values(empty.rast2),
                                     na.color = "transparent")

        selectedTiles <- reactive({
          input$tiles
        })


    output$occmap <- renderLeaflet({
      leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
        addProviderTiles(selectedTiles(), options = tileOptions(continuous_world = F)) %>%
        # addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery",
        #                  options = tileOptions(continuous_world = F)) %>%
        # addProviderTiles(providers$OpenStreetMap, group = "Open Street Map",
        #                  options = tileOptions(continuous_world = F)) %>%
        setView(lng = -83, lat = 30, zoom = 6) %>%
        addMeasure(position = "topleft",
                   primaryLengthUnit = "kilometers",
                   primaryAreaUnit = "hectares",
                   activeColor = "#3D535D",
                   completedColor = "#7D4479") %>%
        addScaleBar() %>%
        # addLayersControl(baseGroups = c("World Imagery", "Ocean Basemap", "Open Street Map"),
        #                  options = layersControlOptions(collapsed = TRUE)) %>%
        addRasterImage(empty.rast2, opacity = input$alpha, project = FALSE)

    })




          # req(input$occ_startedit_button)

          click.df<- eventReactive(input$occmap_click, {

            data.frame(x = input$occmap_click$lng,
                       y = input$occmap_click$lat)

          })

          # Check that coordinates are updated properly
          output$Click_text<-renderText({
            print(unlist(click.df()))
          })


          # Update selected cells
          cell.ind<- eventReactive(click.df(), {

            xy<- click.df() %>%
              st_as_sf(., coords = c("x", "y"), crs = 4326) %>%
              st_transform(crs = crs(empty.rast)) %>%
              st_buffer(., input$buff * 1000)


            cell.buff.ind<- terra::extract(x = empty.rast, y = vect(xy), cells = TRUE,
                                           na.rm = TRUE) %>%
              drop_na()

          })



          # Update rast.vals each time values are added to new cells
          rast.vals<- reactiveValues(int = raster::values(empty.rast))

          observeEvent(cell.ind(), {

            rast.vals$int[cell.ind()$cell]<- rast.vals$int[cell.ind()$cell] + input$intensity

          })


          # Update raster layer w/ values from rast.vals
          occ.rast<- reactive({

            occ.rast1<- empty.rast %>%
              raster()
            raster::values(occ.rast1)<- rast.vals$int

            occ.rast1
          })


          # Allow raster to be cleared by action button or changing species selection
          observeEvent(list(input$clear_raster_button, input$species_occ), {

            rast.vals$int<- raster::values(empty.rast)

          })



            ## Update raster layer on map
            observe({
              req(occ.rast())
              occ.rast2<- occ.rast()

              pal <- leaflet::colorNumeric(viridis::viridis(100, option = 'magma'),
                                           raster::values(occ.rast2),
                                           na.color = "transparent")



            leafletProxy("occmap") %>%
              clearImages() %>%
              clearControls() %>%
              clearTiles() %>%
              addProviderTiles(selectedTiles(), options = tileOptions(continuous_world = F)) %>%
              addRasterImage(occ.rast2, colors = pal, opacity = input$alpha, project = FALSE) %>%
              addLegend_decreasing(pal = pal,
                        values = raster::values(occ.rast2),
                        title = "Intensity",
                        decreasing = TRUE)

        })


    #Collect and export responses
        observeEvent(input$occ_submit_button, {

          occ_conf_export<- data.frame(
            Name = input$name,
            Species = input$species_occ,
            Suitability_confidence = input$conf_occ,
            Time_Period = input$radio
          )

          #Export data to Google Sheets
          sheet_append(ss = sheet_id,
                       data = occ_conf_export,
                       sheet = "Range")

          #Put data on Google Drive
          fileName <- paste(gsub(pattern = " ", replacement = "", input$name),
                            as.Date.character(Sys.time()),
                            input$species_occ,
                            input$radio,
                            "occ.tif",
                            sep = "_")
          #Write the data to a temporary file locally
          filePath <- file.path(tempdir(), fileName)
          writeRaster(occ.rast(), filename = filePath, overwrite = TRUE)
          drive_upload(filePath,
                       path = "Invasive Spp Expert Elicitation Workshop/Occupancy Rasters/")

          #Confirm submission


          # Show modal when button is clicked.
          showModal(submitModal())

        })  #close observeEvent


}

# Run the application
shinyApp(ui = ui, server = server)

