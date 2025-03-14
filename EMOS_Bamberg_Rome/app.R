#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(dplyr)

rf_model <- readRDS("rf_model_test.rds")



TimeFire <- list("2020-07-04",
                 "2020-07-04")
TimeCovariates <- list("2020-07-03",
                       "2020-07-03")
Region <- list(west=7.20,south=51.85,east=7.205,north=51.855)

topo_band_4 <- get_band(TimeCovariates, Region,"SENTINEL2_L2A", "B04" )
topo_band_8 <- get_band(TimeCovariates, Region, "SENTINEL2_L2A", "B08")
topo_band_table_4 <- convert_band_to_table(topo_band_4)
topo_band_table_8 <- convert_band_to_table(topo_band_8)

topo_band_table <- st_join(topo_band_table_4, topo_band_table_8)
topo_band_table$topo <- (topo_band_table$B08 - topo_band_table$B04)/
  (topo_band_table$B08 + topo_band_table$B04)

topo_band_table <- topo_band_table[, c("geometry", "topo")]


data_today <- st_drop_geometry(topo_band_table)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Fire Vulnerability Assessment Using EU Earth Observation Data"),
  
  h3("Analyzing the Impact of Environmental Factors on Fire Vulnerability"),
  
  p("To explore how different environmental variables influence fire probability, 
    adjust the values for Temperature, Moisture, Scene Classification, and the Blue Band. 
    This will help understand the potential impact of each factor on fire vulnerability, 
    using satellite data from the EU Earth Observation program."),
  
  # Sidebar with a slider input for Temperature
  sidebarLayout(
    sidebarPanel(
      numericInput("west", "West:", value = 7.20, min = -Inf,
                   max = Inf, step = 0.01),
      numericInput("south", "South:", value = 51.85, min = -Inf,
                   max = Inf, step = 0.01),
      numericInput("east", "East:", value = 7.205, min = -Inf, 
                   max = Inf, step = 0.01),
      numericInput("north", "North:", value = 51.855, min = -Inf, 
                   max = Inf, step = 0.01),
      actionButton("submit", "Submit"),
      
      
      sliderInput("LST_in",
                  "Temperature:",
                  min = -10,
                  max = 45,
                  value = 30),
      helpText("Temperature is measured in Celcius")
      ,
      
      sliderInput("moi_in",
                  "Moisture:",
                  min = -1,
                  max = 1,
                  value = 0), 
      helpText("Moisture Index renges from -1 to 1 and describes how wet the ground is: 
               -1: very dry areas with no vegetation
               0 to -0.2: dry areas with low vegetation
               0 to 0.4: vegetation is eperiencing water scarcity
               0.4 to 1: healthy vegetaion
               ")
      ,
      sliderInput("veg_in",
                  "Scene classification layer:",
                  min = 0,
                  max = 11,
                  value = 4,
                  step = 1),
      helpText("Possible values on the SCL band range from 0 to 11, 
               however no sensible predictions are possible with values 0, 1 and 7.
               Other values:
               2: topographic casted shadows
               3: cloud shadow
               4: vegetaion
               5: not vegetated
               6: water
               8: cloud medium probability
               9: cloud high probability
               10: thin cirrus
               11: snow/ice")
      ,
      
      # Sidebar with a slider input for Clorophyll 
      sliderInput("B03_in",
                  "Blue Band: Clorophyll and water",
                  min = 0,
                  max = 1,
                  value = 0.1), 
      helpText("Chlorophyll ranges from 0 to 1 and describes the blue reflection
               low values correspond to deep water bodies,
               dense vegetation, soil or shadows, 
               high values correspond to shallow or turbid water, 
               bare soil or sandy areas, urban or reflective surfaces
               ")
    ),
    
    mainPanel(
      plotOutput(outputId = "pred_plot"),
      plotOutput(outputId = "hist"),
      tableOutput(outputId = "table"), 
      downloadButton("download_map", "Download Map"),
      downloadButton("download_hist", "Download Histogram"),
      downloadButton("download_tab", "Download Table")
    )
  )
)

# Define server logic to calculate prediction
server <- function(input, output) {
  
  observeEvent(input$submit, {
    # Create the region list based on user input
    region <- list(
      west = input$west,
      south = input$south,
      east = input$east,
      north = input$north
    )
  
  L <- reactive(input$LST_in)
  B <- reactive(input$B03_in)
  m <- reactive(input$moi_in)
  S <- reactive(input$veg_in)
  
  data_today_new <- reactive(data.frame(data_today, LST= L(),
                               B03 = B(), moi = m(), SCL = S()))
  
  prediction <- reactive(predict(rf_model, data_today_new()))
  
  data_plots <- reactive(data.frame(topo_band_table, 
                                              pred = prediction()))
  #data_plotsun <- reactive(data.frame(topo_band_table, 
  #                                  pred = prediction()))
  
  #data_plots <- reactive(data_plotsun() %>%
  
  #group_by(SCL) %>% # Grouping by the 'group_id' column or any other relevant column
    
   # summarise(
      
    #  geometry = st_union(geometry)    # Merging polygons into one
      
    #))
 
  
  # Reactive expression to calculate prediction based on sliders
  output$pred_plot <- renderPlot({
    data_plots_sf <- st_as_sf(data_plots(), sf_column_name = "geometry")

    p <- ggplot(data = data_plots_sf) +
        geom_sf(aes(fill = pred)) +  # Fill polygons based on 'F1' values
        theme_minimal() +           # Minimal theme for clean look
      scale_fill_gradientn(
        colors = c("green", "yellow", "orange", "red"),  # Define custom color scale
        #limits = c(183.15, 183.75),
        name = "Fire index"  # Set the legend title
      ) +
        labs(title = "Area Graph for Fire Vulnerability", fill = "Fire index")
    
    p
  })
  
  # Download handler for the map (pred_plot)
  output$download_map <- downloadHandler(
    filename = function() {
      paste("fire_probability_map", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # Save the plot to the file
      ggsave(file, plot = last_plot(), device = "png")
    }
  )
  
   output$hist <- renderPlot({
     data_hist <- data_plots()
     ggplot(data_hist, aes(x = pred)) +
       geom_histogram(binwidth = 0.05, fill = "lightblue", color = "black") + 
       labs(title = "Histogram of FireProbs", x = "Probability", y = "Count") +
       theme_minimal()
   })
   
   output$table <- renderTable({
     data_hist <- data_plots()
     data.frame(Min = min(data_hist$pred), Max = max(data_hist$pred), 
                Mean = mean(data_hist$pred))
   })
   

   
   # Download handler for the histogram and table
   output$download_hist <- downloadHandler(
     filename = function() {
       paste("fire_histogram", Sys.Date(), ".png", sep = "")
     },
     content = function(file) {
       ggsave(file, plot = last_plot(), device = "png")
     }
   )
   
   # Download handler for the histogram and table
   output$download_tab <- downloadHandler(
     filename = function() {
       paste("fire_table", Sys.Date(), ".csv", sep = "")
     },
     content = function(file) {
       write.csv(table())
     }
   )
   
  })

}
# Run the application 
shinyApp(ui = ui, server = server)

