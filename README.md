# ShinyApp1
UHI Shiny App assignment 11/12/23



# Load following packages
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(leaflet)

# Sample data not based on real data
airports_data <- data.frame(
  Airport = rep(c("Edinburgh", "Glasgow", "Aberdeen", "Inverness", "Dundee", "Lerwick"), each = 12),
  Month = rep(substr(month.name, 1, 3), times = 6),
  Year = rep(2022, each = 72),
  Passengers = sample(100:500, size = 72),
  Latitude = c(55.9500, 55.8652, 57.2000, 57.4778, 56.4637, 60.1545),
  Longitude = c(-3.3725, -4.2576, -2.2000, -4.2247, -2.9707, -1.1494)
)

# Defining Shiny UI
ui <- dashboardPage(
  dashboardHeader(title = "Scottish Airports Dashboard"),
  dashboardSidebar(
    selectInput("selectedAirport" , "Select Airport for Comparison", choices = unique(airports_data$Airport)),
    checkboxInput("compareAll", "Compare with All Airports", value = FALSE)
  ),
  dashboardBody(
    fluidRow(
      box(
        title = "Airport Selection",
        status = "primary",
        solidHeader = TRUE,
        selectInput("airport" , "Select Airport", choices = unique(airports_data$Airport), multiple = TRUE)
      )
    ),
    fluidRow(
      box(
        title = "Passenger Count Plot",
        status = "info",
        solidHeader = TRUE,
        plotOutput("passengerPlot")
      )
    ),
    fluidRow(
      box(
        title = "Airport Map",
        status = "success",
        solidHeader = TRUE,
        leafletOutput("airportMap")
      )
    ),
    
    # New tab for the report
    tabItem(
      tabName = "report",
      fluidRow(
        box(
          title = "Report",
          status = "warning",
          solidHeader = TRUE,
          width = 12,
          verbatimTextOutput("reportText")
        )
      )
    )
  )
)

# Defining the Server 
server <- function(input, output) {
  # Reactive expression for filtered data
  filtered_data <- reactive({
    airports_data %>%
      filter(Airport %in% input$airport)
  })
  
  # Generate the passenger plot output
  output$passengerPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = factor(Month, levels = substr(month.name, 1, 3)), y = Passengers, group = Airport, color = Airport)) +
      geom_line(size = 1.5, alpha = 0.8) +
      geom_smooth(aes(group = 1), method = "loess", color = "black", se = FALSE, linetype = "dashed") +
      scale_color_manual(values = c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#a6cee3")) +
      theme_minimal() +
      labs(title = paste("Number of People Traveling from", paste(input$airport, collapse = ", "), "Over a Year"),
           x = "Month", y = "Passenger Count")
  })
  
  # Leaflet map
  output$airportMap <- renderLeaflet({
    selected_airports <- airports_data %>% filter(Airport %in% input$airport)
    
    leaflet(data = selected_airports) %>%
      addTiles() %>%
      addMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        popup = ~Airport,
        label = ~Airport,
        group = "selectedAirports",
        labelOptions = labelOptions(direction = "auto")
      ) %>%
      setView(mean(selected_airports$Longitude), mean(selected_airports$Latitude), zoom = 6)
  })
  
  # Observe input$compareAll and update the map accordingly
  observe({
    if (input$compareAll) {
      selected_airports <- airports_data %>% filter(Airport %in% input$airport)
      updateMap(selected_airports)
    } else {
      selected_airport_data <- airports_data %>% filter(Airport == input$selectedAirport)
      updateMap(selected_airport_data)
    }
  })
  
  # Helper function to update map
  updateMap <- function(data) {
    leaflet(data = data) %>%
      addTiles() %>%
      addMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        popup = ~Airport,
        label = ~Airport,
        group = "selectedAirports",
        labelOptions = labelOptions(direction = "auto")
      ) %>%
      setView(mean(data$Longitude), mean(data$Latitude), zoom = 6)
  }
  
  # Server logic for the report
  output$reportText <- renderPrint({
    summary_data <- filtered_data()
    paste("Summary Report:",
          "\nSelected Airports: ", paste(input$airport, collapse = ", "),
          "\nTotal Passengers: ", sum(summary_data$Passengers),
          "\nAverage Passengers per Month: ", mean(summary_data$Passengers),
          "\nMaximum Passengers in a Month: ", max(summary_data$Passengers),
          "\nMinimum Passengers in a Month: ", min(summary_data$Passengers))
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
