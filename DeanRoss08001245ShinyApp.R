#Load packages required 
library(shiny)
library(ggplot2)
library(leaflet)
library(ggmap)
library(rmarkdown)
library(igraph)

#Set up the working directory
setwd("/tmp")  
#Setting the desired working directory path here
print(getwd())  
#Print the current working directory to verify

#Setting up the Google API Key
register_google(key = "YOUR_API_KEY")

#Defining the coordinates for all the cities used in the app, the reasons for their
#choice is to have each capitol city and I selected two random ones to generate fair data
#namely Southampton as it was deep south and Liverpool as it was right in the middle
city_coordinates <- data.frame(
  City = c("London", "Edinburgh", "Liverpool", "Cardiff", "Southampton"),
  Lat = c(51.5074, 55.9533, 53.4084, 51.4816, 50.9097), 
  Lon = c(-0.1278, -3.1883, -2.9916, -3.1791, -1.4044) 
)

#Defining the cities selected above distances in km rather than miles as I struggled to 
#get the miles to work correctly
city_distances <- matrix(c(0, 534, 365, 245, 109, 
                           534, 0, 289, 556, 603, 
                           365, 289, 0, 241, 212, 
                           245, 556, 241, 0, 337, 
                           109, 603, 212, 337, 0), 
                         nrow = 5, byrow = TRUE)
colnames(city_distances) <- rownames(city_distances) <- city_coordinates$City

#Defining the UI interface of the shiny app
ui <- fluidPage(
  titlePanel("Travel Time Comparisons Between Different Modes of Transport in the UK"),
  sidebarLayout(
    sidebarPanel(
      selectInput("origin", "Origin:",
                  choices = city_coordinates$City,
                  selected = "London"),
      selectInput("destination", "Destination:",
                  choices = city_coordinates$City,
                  selected = "London"),
      actionButton("generate_times", "Click here for Travel Comparison Report"),
      downloadButton("download_report", "Download Report"),
      downloadButton("download_report_full", "Download Full Report")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("travel_time_plot")),
        tabPanel("Comparison", plotOutput("comparison_plot")),
        tabPanel("Network Graph", plotOutput("network_plot")),
        tabPanel("Map", leafletOutput("map")),
        tabPanel("Markdown Report", verbatimTextOutput("report_text"))
      )
    )
  )
)

#Defining the server logic
server <- function(input, output) {
  
  #Function that will generate leaflet map
  generate_map <- function(origin, destination) {
    #Retrieve coordinates for origin and destination cities in the above
    origin_coords <- city_coordinates[city_coordinates$City == origin, c("Lat", "Lon")]
    destination_coords <- city_coordinates[city_coordinates$City == destination, c("Lat", "Lon")]
    
    #Create leaflet map markers for start and finish destination of the transit method
    #and when you hover over the marker it will say which is the start or end, it will
    #also be dynamic in the fact you can switch the start city and end city and the marker
    #will adjust in real time
    map <- leaflet() %>%
      addTiles() %>%
      addMarkers(lng = c(origin_coords$Lon, destination_coords$Lon),
                 lat = c(origin_coords$Lat, destination_coords$Lat),
                 label = c("Origin", "Destination")) %>%
      addPolylines(lng = c(origin_coords$Lon, destination_coords$Lon),
                   lat = c(origin_coords$Lat, destination_coords$Lat))
    
    return(map)
  }
  
  #Function to generate the travel times
  generate_travel_times <- function() {
    origin <- input$origin
    destination <- input$destination
    
    #I thought that ig the same city is selected that the start and finish destination, 
    #I'd set all times to zero as you'd clearly logically not travelled anywhere so no time to
    #compare for a journey
    if (origin == destination) {
      return(data.frame(Transport = c("Plane", "Car", "Bus", "Ship"),
                        Time = rep(0, 4),
                        Distance = rep(0, 4)))
    }
    
    #Estimated travel times based on distance and what would be assumed method of transport speed in each mode
    distance <- city_distances[origin, destination]
    plane_time <- max(1, distance / 800) * 60  
    #800 km/h assumed for plane (not taking into account take off landing speeds)
    car_time <- max(1, distance / 100) * 60    
    #100 km/h assumed for car (this is national speed limit on most UK roads)
    bus_time <- max(1, distance / 80) * 60 + 120  
    #80 km/h assumed for bus, increased by 2 hours which takes into account driver breaks/bus stops etc
    ship_time <- max(4, distance / 30) * 60   
    #30 km/h assumed for ship, I struggled with this one as its as the crow flies rather than correct route
    
    return(data.frame(Transport = c("Plane", "Car", "Bus", "Ship"),
                      Time = c(plane_time, car_time, bus_time, ship_time),
                      Distance = rep(distance, 4)))
  }
  
  #Plot the travel times for all the modes of transport, I opted for a simple bar graph as it visually
  #represents the quickest way to get the message across, where its clear to see the closer the journey 
  #the likelihood the car will be almost as quick.  I thought aboout adding dimensions to the plane, namely
  #the check in process, or train whilst waiting at platform but decided in end they would complicate the message
  output$travel_time_plot <- renderPlot({
    travel_times <- generate_travel_times()
    ggplot(travel_times, aes(x = Transport, y = Time / 60, fill = Transport)) +
      geom_bar(stat = "identity") +
      labs(title = paste("Travel Time from", input$origin, "to", input$destination),
           x = "Transportation",
           y = "Travel Time (hours)",
           fill = "Transportation") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  #Plot a comparison of the travel times
  output$comparison_plot <- renderPlot({
    travel_times <- generate_travel_times()
    
    #Calculating the average difference in time
    avg_time <- mean(travel_times$Time)
    travel_times$Time_Diff <- travel_times$Time - avg_time
    
    #Define colors based on the difference from average, if colour is green its above average and red below
    #average in the scatterplot
    travel_times$Color <- ifelse(travel_times$Time_Diff >= 0, "green", "red")
    
    ggplot(travel_times, aes(x = Transport, y = Time / 60, fill = Color)) +
      geom_violin(trim = FALSE) +
      geom_jitter(aes(color = Color), width = 0.2, size = 3, alpha = 0.7) +
      #I've added a dynamic average line here which will generate the average for each journey going up and down 
      geom_hline(yintercept = avg_time / 60, color = "black", linetype = "dashed") +
      geom_text(aes(label = "Average", x = as.numeric(Transport) + 0.5, y = avg_time / 60), 
                color = "black", vjust = -0.5, size = 5) +
      labs(title = "Comparison of Travel Times",
           x = "Transportation",
           y = "Travel Time (hours)",
           fill = "Difference from Average") +
      scale_color_manual(values = c("red" = "red", "green" = "green")) +
      theme_minimal() +
      theme(legend.position = "none",
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14))
  })
  
  #Plotting network graph, I discovered this whilst researching something totally different for the 
  #shiny app and decided to try and impliment something similar into my app, its not dynamic and a show
  #piece which I've placed before the map, as that way it correlates to show the user km distance from points 
  output$network_plot <- renderPlot({
    #Create graph object
    g <- graph_from_adjacency_matrix(city_distances, mode = "undirected", weighted = TRUE)
    
    #Plot network graph
    plot(g, layout = layout.circle(g), edge.arrow.size = 0.5,
         vertex.label = V(g)$name, vertex.size = 10, vertex.label.dist = 1,
         edge.label = E(g)$weight, edge.label.cex = 0.7,
         main = "Distance Between Cities")
  })
  
  #Render the leaflet map, I wanted to use a leaflet map and after feedback now am able to see where I 
  #went wrong previously and now have the way to get it working how my vision wanted
  output$map <- renderLeaflet({
    generate_map(input$origin, input$destination)
  })
  
  #Generate R Markdown report, this is dynamic and will change as you change the start and finish
  #destinations accordingly and will display different results as to which is selected. 
  observeEvent(input$generate_times, {
    travel_times <- generate_travel_times()
    output$report_text <- renderText({
      paste("Travel Time Comparison Report",
            "\n",
            "Origin:", input$origin,
            "\n",
            "Destination:", input$destination,
            "\n",
            "```{r, results='asis'}",
            knitr::kable(travel_times, 
                         col.names = c("Transportation", "Travel Time (minutes)", "Distance (km)"),
                         caption = "Travel Times by Transportation"),
            "```")
    })
  })
  
  #Download R Markdown report as Markdown
  output$download_report <- downloadHandler(
    filename = function() {
      paste("Travel_Time_Report", ".md", sep = "")
    },
    content = function(file) {
      travel_times <- generate_travel_times()
      rmarkdown::render(input = "report.Rmd", output_file = file,
                        params = list(travel_times = travel_times,
                                      origin = input$origin,
                                      destination = input$destination))
    }
  )
  
  #Download the R Markdown report as PDF, I had some issues with the Mac here so sometimes didn't work all time
  output$download_report_pdf <- downloadHandler(
    filename = function() {
      paste("Travel_Time_Report", ".pdf", sep = "")
    },
    content = function(file) {
      travel_times <- generate_travel_times()
      rmarkdown::render(input = "report.Rmd", output_file = file,
                        params = list(travel_times = travel_times,
                                      origin = input$origin,
                                      destination = input$destination),
                        output_format = "pdf_document")
    }
  )
  
  #Download full R Markdown report as PDF, again as above, hopefully it works on your machine
  output$download_report_full <- downloadHandler(
    filename = function() {
      paste("Full_Travel_Time_Report", ".pdf", sep = "")
    },
    content = function(file) {
      travel_times <- generate_travel_times()
      rmarkdown::render(input = "full_report.Rmd", output_file = file,
                        params = list(travel_times = travel_times,
                                      origin = input$origin,
                                      destination = input$destination),
                        output_format = "pdf_document")
    }
  )
}

#Run Shiny app
shinyApp(ui = ui, server = server)
