library(shiny)
library(dplyr)
library(leaflet)
library(rsconnect)

rat_sightings_data <- read.csv("datasets_clean/rat_sightings_data")

# Define UI
ui <- fluidPage(
  titlePanel("Rat Sighting App"),
  
  # UI components, including drop-down menus
  selectInput(
    inputId = "year_choice", 
    label = "Select Year",
    choices = NULL,  # Placeholder, choices will be updated dynamically
    selected = NULL  # Placeholder
  ),
  
  selectInput(
    inputId = "borough_choice", 
    label = "Select Borough",
    choices = NULL,  # Placeholder, choices will be updated dynamically
    selected = NULL  # Placeholder
  ),
  
  leafletOutput("map")  # Output container for the leaflet map
)

# Define server
server <- function(input, output, session) {
  # Read data
  rat_sightings_data <- read.csv("/Ratstical/datasets_clean/rat_sightings_data")
  
  # Update choices dynamically based on data
  updateSelectInput(session, "year_choice", choices = unique(rat_sightings_data$year_created))
  updateSelectInput(session, "borough_choice", choices = unique(rat_sightings_data$borough))
  
  # Server logic, including reactive data and rendering the leaflet map
  filtered_data <- reactive({
    rat_sightings_data %>%
      filter(
        year_created == input$year_choice,
        borough == input$borough_choice,
        !is.na(latitude),
        !is.na(longitude)
      )
  })
  
  output$map <- renderLeaflet({
    leaflet(filtered_data()) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = 5,
        popup = ~paste("Location Type", location_type, "<br>Date: ", paste(month_created, year_created, sep = "-"))
      ) %>%
      addLegend("bottomright", colors = "red", labels = paste("Rat Sightings - ", input$year_choice)) %>%
      addMarkers(
        clusterOptions = markerClusterOptions()
      ) %>%
      setView(lng = -73.97, lat = 40.78, zoom = 10)
  })
}

shinyApp(ui, server)
