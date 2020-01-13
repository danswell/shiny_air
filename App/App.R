##Air quality app

##This app pulls data from the ACT Government data.act.gov.au API for 1 hourly and 24 hour Pm2.5 and PM10 air quality data
## Basic charts are produced using ggplot2

#Libraries
library(dplyr)
library(RSocrata)
library(soql)
library(tidyr)
library(lubridate)
library(ggplot2)
library(leaflet)
library(shiny)
library(stringr)

##Read data


# Extract and process data from data.act.gov.au
# This code uses the RSocrate API. It can be used to extract both the data and the metadata
# Only a short timeframe is parsed, to speed up App load time.

#format(Sys.time(), "%Y-%m-%dT%H:%M:%S.000")

Sys.setenv(TZ="Australia/Sydney")

Datecall <- paste0("DateTime between '2019-12-01T09:00.000' and ","'",format(Sys.time(), "%Y-%m-%dT%H:%M:%S.000"),"'")

Query <- soql() %>%
  soql_add_endpoint("https://www.data.act.gov.au/resource/ufvu-jybu.json") %>%
  soql_where(Datecall) %>% #dynamic date using sys.Date()
  as.character()

My_data <- read.socrata(Query)

#Prep data
#GPS data

My_data <- My_data %>%
  separate(gps, into = c("Lat", "lng"), sep = ",")

My_data$Lat <- as.numeric(str_sub(My_data$Lat, 2, str_length(My_data$Lat)))
My_data$Lng <- as.numeric(str_sub(My_data$lng, end = -2))

My_data$station <- as.factor(My_data$station)

##Air quality data
My_data$pm10_1_hr <- as.numeric(My_data$pm10_1_hr)
My_data$pm2_5_1_hr <- as.numeric(My_data$pm2_5_1_hr)

My_data$pm10_24hr_rolling <- as.numeric(My_data$pm10_24hr_rolling)
My_data$pm2_5_24hr_rolling <- as.numeric(My_data$pm2_5_24hr_rolling)


#UI
ui <- fluidPage(titlePanel("ACT Air Quality Explorer"),
                sidebarLayout(
                  sidebarPanel(
                    
                    # Select site to plot
                    selectInput(inputId = "site", label = strong("Air quality monitoring station"),
                    choices = unique(My_data$station),
                    selected = "Civic"),
                    
                    # Select date range to be plotted
                    sliderInput("Date", strong("Date range"), min = min(My_data$datetime), max = max(My_data$datetime),
                    value = c(max(My_data$datetime-86400), max(My_data$datetime))),
                  
                  # Add leaflet map
                  leafletOutput("my_map")
                 ),
                
               
                
                # Output: Description, lineplot, and reference
                mainPanel(
                  tabsetPanel(type = c("pills"),
                      tabPanel("1 Hour", 
                              tableOutput(outputId = "my_table_1"),
                              plotOutput(outputId = "PM25_1", height = "400px"),
                              plotOutput(outputId = "PM10_1", height = "400px")),
                      tabPanel("24 Hour", 
                              tableOutput(outputId = "my_table_24"),
                              plotOutput(outputId = "PM25_24", height = "400px"),
                              plotOutput(outputId = "PM10_24", height = "400px")),
                      p("Note: This website is not to be used for medical advice. Data for this webapp can be found at", a("data.act.gov.au", href="http://www.data.act.gov.au"), ". Time is in Australian Eastern Standard Time (not Daylight Savings Time). For information on how to interpret these data see the ", a("ACT Health website", href = "https://www.health.act.gov.au/about-our-health-system/population-health/environmental-monitoring/monitoring-and-regulating-air"), "To contact the developer, please email danswell(dot)starrs(at)anu(dot)edu(dot)au")
                  )
              )
          )
)
                
                
server <- function(input, output, session) {
                  
                  # Subset data by site
                  selected_data <- reactive({
                    My_data %>%
                    filter(
                    station == input$site)
                    })
                  
                  #Update slider input to reflect site selected
                  observeEvent(input$site, {
                    updateSliderInput(session, "Date", value = c(max(selected_data()$datetime-86400), max(selected_data()$datetime)),
                    min = min(selected_data()$datetime), max = max(selected_data()$datetime))
                  })
                  
                  #Update selected site based on map click
                  observeEvent(input$my_map_marker_click, {
                    p <- input$my_map_marker_click
                    
                    #updateSelectInput(session, "site", selected = p$Siteid) 
                    updateSelectInput(session, "site", "Update my site", selected = p$id)
                  })
                  
                  
                  #subset data by selected daterange
                  selected_data2 <- reactive({
                    req(input$Date)
                    validate(need(!is.na(input$Date[1]) & !is.na(input$Date[2]), "Error: Please provide both a start and an end date."))
                    validate(need(input$Date < input$Date[2], "Error: Start date should be earlier than end date."))
                    selected_data() %>%
                    filter(datetime > input$Date[1] & datetime < input$Date[2])
                  })
                  
                  
                  
                  #  https://stackoverflow.com/questions/48633984/pass-a-dynamic-date-date-range-to-daterangeinput-in-r-shiny
                  
                  
                  output$my_map <- renderLeaflet({
                    leaflet() %>%
                    addTiles() %>%
                    addMarkers(data = My_data, lng = ~Lng, lat = ~Lat, layerId = ~station, popup = ~station, label = ~station) %>%
                    setView(lng = 149.1, lat = -35.3, zoom = 10)
                  })
                  
                  output$my_table_1 <- renderTable({
                    my_table_1 <- My_data %>%
                    filter(datetime == max(datetime)) %>%
                    select(datetime, station, pm2_5_1_hr, pm10_1_hr)
                  my_table_1$datetime <- format(my_table_1$datetime, format = "%Y-%m-%d %H:%M:%S")
                  colnames(my_table_1) <- c("Date/Time (AEST)", "Station", "PM2.5_1 Hour (ug/m3)", "PM10_1 Hour (ug/m3)")
                  return(my_table_1)
                  })
                  
                  output$my_table_24 <- renderTable({
                    my_table_24 <- My_data %>%
                    filter(datetime == max(datetime)) %>%
                    select(datetime, station, pm2_5_24hr_rolling, pm10_24hr_rolling)
                  my_table_24$datetime <- format(my_table_24$datetime, format = "%Y-%m-%d %H:%M:%S")
                  colnames(my_table_24) <- c("Date/Time (AEST)", "Station", "PM2.5_24 Hour (ug/m3)", "PM10_24 Hour (ug/m3)")
                  return(my_table_24)
                  })
                  
                  # Create scatterplot object the plotOutput function is expecting
                  output$PM25_1 <- renderPlot({
                    selected_data2() %>%
                      ggplot() + 
                        geom_col(aes(datetime, pm2_5_1_hr), color = "blue") +
                        scale_x_datetime(breaks = pretty_dates(selected_data2()$datetime, 24), limits = c(input$Date[1], input$Date[2])) +
                        labs(x = "Date/Time (AEST)", y = "1 hour average PM2.5 (ug/m3)", title = paste0("1 hour PM2.5 (ug/m3) at ", input$site)) +
                        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
                    })
                  
                  output$PM10_1 <- renderPlot({
                    selected_data2() %>%
                      ggplot() +
                        geom_col(aes(datetime, pm10_1_hr), color = "blue") +
                        scale_x_datetime(breaks = pretty_dates(selected_data2()$datetime, 24), limits = c(input$Date[1], input$Date[2])) +
                        labs(x = "Date/Time (AEST)", y = "1 hour average PM10 (ug/m3)", title = paste0("1 hour PM10 (ug/m3) at ", input$site)) +
                        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
                    })
                 
                  output$PM25_24 <- renderPlot({
                    selected_data2() %>%
                      ggplot() + 
                        geom_col(mapping = aes(datetime, pm2_5_24hr_rolling), color = "blue") +
                        scale_x_datetime(breaks = pretty_dates(selected_data2()$datetime, 24), limits = c(input$Date[1], input$Date[2])) + 
                        geom_hline(aes(yintercept = 177.9), color = "purple", linetype = "dashed") +
                        geom_text(aes(min(datetime),177.9),label = "Hazardous High", vjust = -1) +
                        geom_hline(aes(yintercept = 107), color = "red", linetype = "dashed") +
                        geom_text(aes(min(datetime),107),label = "Very unhealthy", vjust = -1) +
                        geom_hline(aes(yintercept = 40), color = "orange", linetype = "dashed") +
                        geom_text(aes(min(datetime),40),label = "Unhealthy", vjust = -1) +
                        geom_hline(aes(yintercept = 26), color = "Yellow", linetype = "dashed") +
                        geom_text(aes(min(datetime),26),label = "Unhealthy (sensitive)", vjust = -1) +
                        geom_hline(aes(yintercept = 9), color = "green", linetype = "dashed") +
                        geom_text(aes(min(datetime),9),label = "meets standard", vjust = -1) +
                        labs(x = "Date/Time (AEST)", y = "24 hour average PM2.5 (ug/m3)", title = paste0("PM2.5 (ug/m3) at ", input$site)) + 
                        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
                  })
                  
                output$PM10_24 <- renderPlot({
                  selected_data2() %>%
                  ggplot() + 
                    geom_col(mapping = aes(datetime, pm10_24hr_rolling), color = "blue") +
                    scale_x_datetime(breaks = pretty_dates(selected_data2()$datetime, 24), limits = c(input$Date[1], input$Date[2])) +
                    labs(x = "Date/Time (AEST)", y = "24 hour average PM10 (ug/m3)", title = paste0("PM10 (ug/m3) at ", input$site)) +
                    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
                })
}
# Create Shiny object
shinyApp(ui = ui, server = server)



