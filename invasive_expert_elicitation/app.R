
#################################################################################################
### A Shiny web application that will be used as part of an expert elicitation workshop to    ###
### determine potential habitat suitability and future range shifts of invasive spp in the SE ### ### United States                                                                             ###
#################################################################################################



library(shiny)
library(terra)  #in place of {raster}
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(sf)
library(shinythemes)


### Load state boundaries for SE United States
se <- st_read("state_bounds.shp", crs = 4326, quiet = TRUE)


### Load fake LU/LC raster
lulc<- rast("fake_lulc.tif")

### Create new raster for displaying habitat suitability
hab_suit<- lulc
values(hab_suit)[!is.na(values(lulc))]<- 0.5


spp_names<- c("Burmese python", "Argentine black and white tegu", "Nile monitor")


# Define UI for application that draws a histogram
ui <- navbarPage("Expert Elicitation of Invasive Species Habitat Suitability",
                 theme = shinythemes::shinytheme("yeti"),


                 tabPanel("About",
                          fluidRow(
                              column(width = 8, offset = 2, h4(strong("How to use this app"))),
                              column(width = 8, offset = 2, p("As part of this expert elicitation, participants will first select the species on which they have expertise in the section below before proceeding to the next steps. These subsequent step will allow participants to first consider the influence of land cover on invasive species connectivity and habitat suitability. Additionally, participants will also be asked to self-assess their confidence in the spatial prediction they have made for each of the selected species before submitting their results. These results will then be used in the following days of the workshop.")
                                     ),  #close column for text paragraph,

                                     column(width = 8, offset = 2, h4(strong("Information on Invasive Species of Interest"))),

                                     column(width = 8, offset = 2, p(strong("Burmese python")),
                                            img(src="burmese_python.jpg", align = "center",
                                                height = 225, width = 300),
                                            br(),
                                            br(),
                                            p("Burmese pythons ",em("(Python bivittatus)")," are large constrictors native to Southeast Asia. It’s native range is broad and whilst it can survive in a variety of habitats, it has preferences for wetland, swamp, and other aquatic systems. Despite this, as with other large constrictors, it is also arboreal. They reach in excess of 85m in length, meaning in their introduced range they encounter few natural predators. They are reported to have become established in the USA in 2000 (Harvey et al. 2016), but have been spotted intermittently for 20 years prior.  and  The total ecological impact of Burmese is difficult to characterize, however they are known to prey on a range of native fauna, including many native mammals (Dorcas et al. 2012)."),
                                            br(),
                                            br(),

                                            p(strong("Argentine black and white tegu")),
                                                   img(src="argentine_bw_tegu.jpg",
                                                       align = "center",
                                                       height = 225, width = 300),
                                                   br(),
                                                   br(),
                                                   p("The Argentine black and white tegu", em("(Salvator merianae)"), "is the largest member of the genus of tegus, Salvator. They are native to large parts of central and southern South America. Their broad habitat plasticity, and diverse diet allow make them to excellent at adapting  to new ecotypes and make them superior invaders. Behaviors such as burrowing mean that cold winters may not necessarily restrict invasive expansion across the US."),
                                            br(),
                                            br(),

                                            p(strong("Nile monitor")),
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
            sliderInput("forest",
                        "Forest",
                        min = 0,
                        max = 1,
                        value = 0.5,
                        step = 0.05,
                        ),
            sliderInput("wet",
                        "Wetland",
                        min = 0,
                        max = 1,
                        value = 0.5,
                        step = 0.05),
            sliderInput("urban",
                        "Urban",
                        min = 0,
                        max = 1,
                        value = 0.5,
                        step = 0.05),
            sliderInput("grass",
                        "Grassland",
                        min = 0,
                        max = 1,
                        value = 0.5,
                        step = 0.05),
            actionButton("update_button",
                         "Update Map",
                         class = "btn-primary"),
            br(),
            br(),
            br(),
            br(),
            sliderInput("conf_habsuit",
                        "Confidence in Habitat Suitability:",
                        min = 0,
                        max = 1,
                        value = 0.5,
                        step = 0.25)
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
                 br(),
                 sliderInput("conf_habsuit",
                             "Confidence in Occupancy Selection:",
                             min = 0,
                             max = 1,
                             value = 0.5,
                             step = 0.25)
               ),  #close sidebarPanel

               mainPanel(
                 leafletOutput("occ_map")
               )  #close mainPanel
             )  #close sidebarLayout
             )  #close "Step 2" tabPanel
)  #close navbarPage




# Define server logic required to draw a histogram
server <- function(input, output) {

  output$lulcMap <- renderPlot({

    # change to data frame for viz in ggplot
    lulc.df<- as.data.frame(lulc, xy = TRUE)
    names(lulc.df)[3]<- "value"


    ggplot() +
      geom_tile(data = lulc.df, aes(x, y, fill = factor(value)), na.rm = TRUE,
                  alpha = 0.8) +
      scale_fill_manual("LU/LC",
                        values = c("darkgreen", "lightblue", "darkred", "lightgreen"),
                        labels = c("Forest", "Wetland", "Urban", "Grassland", "")) +
      geom_sf(data = se, size = 1, color = "black", fill = "transparent") +
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

    observeEvent(input$update_button, {

    #Forest
        ind.forest<- which(values(lulc) == 1)
        values(hab_suit)[ind.forest]<- input$forest

    #Wetland
        ind.wet<- which(values(lulc) == 2)
        values(hab_suit)[ind.wet]<- input$wet

    #Urban
        ind.urban<- which(values(lulc) == 3)
        values(hab_suit)[ind.urban]<- input$urban

    #Grassland
        ind.grass<- which(values(lulc) == 4)
        values(hab_suit)[ind.grass]<- input$grass



        output$habitatMap <- renderPlot({

            # change to data frame for viz in ggplot
            hab_suit.df<- as.data.frame(hab_suit, xy = TRUE)
            names(hab_suit.df)[3]<- "value"


            ggplot() +
                geom_tile(data = hab_suit.df, aes(x, y, fill = value), alpha = 0.8) +
                scale_fill_viridis_c("Suitability", option = "magma", limits = c(0,1),
                                     na.value = "transparent") +
                geom_sf(data = se, size = 1, color = "black", fill = "transparent") +
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



    output$occ_map <- renderLeaflet({
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
        leaflet.extras::addDrawToolbar(polylineOptions = FALSE,
                       circleOptions = FALSE,
                       polygonOptions = TRUE,
                       markerOptions = FALSE,
                       circleMarkerOptions = FALSE,
                       rectangleOptions = FALSE,
                       singleFeature = FALSE,
                       editOptions = editToolbarOptions())
    })


}

# Run the application
shinyApp(ui = ui, server = server)

