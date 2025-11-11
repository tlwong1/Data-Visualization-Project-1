library(shiny)
library(tidyverse)
library(igraph)
library(visNetwork)

# Load data once
uber_data <- read_csv("https://raw.githubusercontent.com/tlwong1/Data-Visualization-Project-1/refs/heads/main/Project%20Data/ncr_ride_bookings.csv")

# UI
ui <- fluidPage(
  titlePanel("Uber NCR Route Cancellation Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      h4("Filter Controls"),
      
      sliderInput("min_cancel_rate",
                  "Minimum Cancellation Rate:",
                  min = 5,
                  max = 40,
                  value = 5,
                  step = 5,
                  post = "%"),
      
      sliderInput("num_routes",
                  "Number of Routes to Display:",
                  min = 20,
                  max = 100,
                  value = 50,
                  step = 10),
      
      selectInput("vehicle_filter",
                  "Filter by Vehicle Type:",
                  choices = c("All Vehicle Types" = "all",
                              "Auto" = "Auto",
                              "Go Mini" = "Go Mini",
                              "Go Sedan" = "Go Sedan",
                              "Premier Sedan" = "Premier Sedan",
                              "UberXL" = "UberXL",
                              "eBike" = "eBike"),
                  selected = "all"),
      
      radioButtons("cancel_type",
                   "Show Cancellations:",
                   choices = c("All Cancellations" = "all",
                               "Customer Only" = "customer",
                               "Driver Only" = "driver"),
                   selected = "all"),
      
      hr(),
      
      h5("About This Visualization"),
      p("This network shows routes with high cancellation rates in NCR. 
        Node size indicates destination popularity. Edge color shows 
        cancellation severity (red = high, green = low)."),
      
      p(strong("Hover"), "over edges to see detailed statistics."),
      p(strong("Click"), "nodes to highlight connected routes.")
    ),
    
    mainPanel(
      width = 9,
      
      conditionalPanel(
        condition = "output.has_data == false",
        div(style = "text-align: center; padding: 100px; background-color: #f8f9fa; border: 2px dashed #ccc; border-radius: 10px;",
            h3(style = "color: #dc3545;", "⚠ No routes match the current filters"),
            p("Try adjusting the filters to see results:"),
            tags$ul(style = "text-align: left; display: inline-block;",
                    tags$li("Lower the minimum cancellation rate to 5%"),
                    tags$li("Increase the number of routes to 50+"),
                    tags$li("Select 'All Vehicle Types'"),
                    tags$li("Switch to 'All Cancellations'")
            )
        )
      ),
      
      # Debug output
      verbatimTextOutput("debug_info"),
      
      conditionalPanel(
        condition = "output.has_data == true",
        visNetworkOutput("network", height = "700px"),
        
        hr(),
        
        fluidRow(
          column(4,
                 h4(textOutput("total_routes_text")),
                 p("Routes displayed")
          ),
          column(4,
                 h4(textOutput("avg_cancel_text")),
                 p("Average cancellation rate")
          ),
          column(4,
                 h4(textOutput("worst_route_text")),
                 p("Worst route")
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive data filtering
  filtered_data <- reactive({
    
    # Filter by vehicle type if not "all"
    data_to_use <- uber_data
    if (input$vehicle_filter != "all") {
      data_to_use <- data_to_use %>%
        filter(`Vehicle Type` == input$vehicle_filter)
    }
    
    # Calculate cancellations based on type selected
    route_analysis <- data_to_use %>%
      filter(!is.na(`Pickup Location`), !is.na(`Drop Location`)) %>%
      group_by(pickup = `Pickup Location`, drop = `Drop Location`) %>%
      summarise(
        total_rides = n(),
        cancelled_customer = sum(`Booking Status` == "Cancelled by Customer", na.rm = TRUE),
        cancelled_driver = sum(`Booking Status` == "Cancelled by Driver", na.rm = TRUE),
        cancelled_total = sum(`Booking Status` %in% c("Cancelled by Customer", "Cancelled by Driver"), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        cancellation_rate = case_when(
          input$cancel_type == "customer" ~ (cancelled_customer / total_rides) * 100,
          input$cancel_type == "driver" ~ (cancelled_driver / total_rides) * 100,
          TRUE ~ (cancelled_total / total_rides) * 100
        )
      ) %>%
      filter(total_rides >= 1,
             cancellation_rate >= input$min_cancel_rate) %>%
      arrange(desc(total_rides)) %>%
      slice_head(n = input$num_routes)
    
    return(route_analysis)
  })
  
  # Check if we have data
  output$has_data <- reactive({
    nrow(filtered_data()) > 0
  })
  outputOptions(output, "has_data", suspendWhenHidden = FALSE)
  
  # Debug output
  output$debug_info <- renderPrint({
    cat("Rows in filtered data:", nrow(filtered_data()), "\n")
    cat("Filters:\n")
    cat("  - Min cancel rate:", input$min_cancel_rate, "%\n")
    cat("  - Num routes:", input$num_routes, "\n")
    cat("  - Vehicle:", input$vehicle_filter, "\n")
    cat("  - Cancel type:", input$cancel_type, "\n")
  })
  
  # Summary stats
  output$total_routes_text <- renderText({
    paste(nrow(filtered_data()))
  })
  
  output$avg_cancel_text <- renderText({
    if(nrow(filtered_data()) > 0) {
      paste0(round(mean(filtered_data()$cancellation_rate, na.rm = TRUE), 1), "%")
    } else {
      "N/A"
    }
  })
  
  output$worst_route_text <- renderText({
    if(nrow(filtered_data()) > 0) {
      worst <- filtered_data() %>%
        arrange(desc(cancellation_rate)) %>%
        slice(1)
      
      paste0(worst$pickup, " → ", worst$drop, " (", round(worst$cancellation_rate, 1), "%)")
    } else {
      "N/A"
    }
  })
  
  # Render network
  output$network <- renderVisNetwork({
    
    req(nrow(filtered_data()) > 0)
    
    problem_routes <- filtered_data()
    
    # Create edges
    edges_data <- problem_routes %>%
      rename(from = pickup, to = drop) %>%
      mutate(
        label = paste0(round(cancellation_rate, 1), "%"),
        title = paste0("<b>Route:</b> ", from, " → ", to,
                       "<br><b>Total Rides:</b> ", total_rides,
                       "<br><b>Cancelled (Customer):</b> ", cancelled_customer,
                       "<br><b>Cancelled (Driver):</b> ", cancelled_driver,
                       "<br><b>Cancellation Rate:</b> ", round(cancellation_rate, 1), "%"),
        color = case_when(
          cancellation_rate >= 40 ~ "#8B0000",
          cancellation_rate >= 30 ~ "#DC143C",
          cancellation_rate >= 20 ~ "#FF6347",
          cancellation_rate >= 10 ~ "#FFA500",
          TRUE ~ "#39FF14"
        ),
        width = sqrt(total_rides) / 2
      )
    
    # Get unique locations
    all_locations <- unique(c(edges_data$from, edges_data$to))
    
    # Get dropoff and pickup counts
    dropoff_counts <- uber_data %>%
      filter(!is.na(`Drop Location`)) %>%
      count(`Drop Location`, name = "dropoff_total")
    
    pickup_counts <- uber_data %>%
      filter(!is.na(`Pickup Location`)) %>%
      count(`Pickup Location`, name = "pickup_total")
    
    # Create nodes
    nodes_data <- data.frame(
      id = all_locations,
      label = all_locations
    ) %>%
      left_join(dropoff_counts, by = c("id" = "Drop Location")) %>%
      left_join(pickup_counts, by = c("id" = "Pickup Location")) %>%
      mutate(
        dropoff_total = replace_na(dropoff_total, 0),
        pickup_total = replace_na(pickup_total, 0),
        value = sqrt(dropoff_total) * 3,
        color = "#3498db",
        title = paste0("<b>", id, "</b><br>",
                       "Drop-offs: ", dropoff_total, "<br>",
                       "Pickups: ", pickup_total)
      )
    
    # Create network
    visNetwork(nodes_data, edges_data, height = "100%", width = "100%") %>%
      visNodes(
        shape = "dot",
        color = list(background = "#3498db", border = "#2980b9"),
        font = list(size = 13, color = "#000000", bold = TRUE),
        borderWidth = 2
      ) %>%
      visEdges(
        arrows = list(to = list(enabled = TRUE, scaleFactor = 0.5)),
        smooth = list(type = "curvedCW", roundness = 0.2),
        font = list(size = 12, color = "#000000", align = "middle")
      ) %>%
      visPhysics(
        solver = "forceAtlas2Based",
        forceAtlas2Based = list(gravitationalConstant = -50),
        stabilization = list(iterations = 100)
      ) %>%
      visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE)) %>%
      visInteraction(navigationButtons = TRUE)
  })
}

# Run the app
shinyApp(ui = ui, server = server)