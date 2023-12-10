# ShinyApp1
UHI Shiny App assignment 11/12/23



#load folloing packages
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(leaflet)

# Sample data not based on real data
airports_data <- data.frame(
  Airport = rep(c("Edinburgh", "Glasgow", "Aberdeen", "Inverness", "Dundee", "Lerwick"), each = 12),
  # Use first 3 letters of each month
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
        selectInput("airport" , "Select Airport", choices = unique(airports_data$Airport) , multiple = TRUE)
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
    )
  )
)
