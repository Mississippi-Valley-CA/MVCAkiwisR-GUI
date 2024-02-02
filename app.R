# Prompt user for all needed inputs and then get values for the time series id.
# This Shiny App asks the user for details to narrow down on the timeseries and then returns time series values for given time series id and date range.
# Returns A visualization of the data and a data frame with the columns returned by the script ki_timeseries_values
# This package made possible thanks to Ryan Whaley's 'kiwisR' (<https://github.com/rywhale/kiwisR>).
# Author: Daniel Post, MVCA, dpost@mvc.on.ca
# Created: December 2023
# You can run this Shiny Web App clicking the 'Run App' button above

library("shiny")
library("data.table")
library("DT")
library("dplyr")
library("httr")
library("jsonlite")
library("lubridate")
library("purrr")
library("tibble")
library("ggplot2")
source("utils.R")
source("ki_timeseries_values.R")

# Increase the timeout due to the large amount of data requested from Quinte Conservation Authority's server
options(timeout=300)

# Download the complete list of MVCA timeseries
#data <- fread("https://waterdata.quinteconservation.ca/KiWIS/KiWIS?service=kisters&type=queryServices&request=getTimeseriesList&datasource=0&format=csv&csvdiv=,&timezone=GMT-5&dateformat=yyyy-MM-dd%20HH:mm:ss&site_no=2&station_name=*&returnfields=station_name,station_no,ts_id,ts_name,parametertype_name,stationparameter_name,coverage", sep=",")

load_data <- function() {
  # Load the complete list of MVCA timeseries
  data <- fread("tslist.csv", sep=",")

  # Remove any empty timeseries
  data <- subset(data, !is.na(data$from))

  # Build the parameters list and make it avaiable outside the function
  parameters <<- sort(unique(data$parametertype_name))

  return(data)
}

# Call function to load and cleanup the timeseries list
load_data()

# Define UI for app
ui <- fluidPage(
    p(HTML("<br>")),
    # MVCA Logo
    imageOutput("mvca_logo", width = 202, height = 40),
    # Application title
    titlePanel("MVCA WISKI-R"),
    # Button to Update the tslist from Quinte servers
    actionButton("update_tslist", "Update Timeseries List (takes approx 3 min)"),
    p(HTML("<br>")),
    # Sidebar with a slider input for number of bins
    sidebarLayout(
      # Sidebar panel for inputs
      sidebarPanel(
        helpText("Select parameter, station, timeseries type, and dates:"),

        # Select the parameter
        selectInput(
          inputId = "parameters",
          label = "1. Select a parameter:",
          choices = c("Choose", parameters),
        ),

        # Select the station
        selectInput(
          inputId = "stations",
          label = "2. Select a station:",
          # Initialize as NULL and update in the server function.
          choices = NULL,
        ),

        # Select the timeseries
        selectInput(
          inputId = "timeseries",
          label = "3. Select a timeseries:",
          # Initialize as NULL and update in the server function.
          choices = NULL,
        ),

        # Select the start date
        dateInput(
          inputId = "startDate",
          label = "4. Pick a start date:",
          # Initialize as NULL and update in the server function.
          min = NULL,
          max = NULL
        ),

        # Select the end data
        dateInput(
          inputId = "endDate",
          label = "5. Pick an end date:",
          # Initialize as NULL and update in the server function.
          min = NULL,
          max = NULL
        ),

        actionButton("loadTimeseriesData", "Load Timeseries Data"),
      ),

      # Main panel
      mainPanel(
        # CSS
        tags$style(type="text/css", "
          #load-message {
            position: fixed;
            top: 0px;
            left: 0px;
            width: 100%;
            padding: 15px 0px 15px 0px;
            text-align: center;
            font-weight: bold;
            font-size: 100%;
            color: #000000;
            background-color: #FFFF00;
            z-index: 999;
          }
        "),
        div(htmlOutput("codeblock")),
        br(),
        div(plotOutput("dataplot")),
        br(),
        div(DT::dataTableOutput("datatable")),
        # Loading message
        conditionalPanel(
          condition="$('html').hasClass('shiny-busy')",
          tags$div("Loading...", id="load-message")
        ),
      )
    )
)

# Define server logic #
# Include the optional "session" variable to the server function.
# This is necessary for Shiny applications which will have different behavior
# based upon user inputs. In this case, changing the presented timeseries
# choices based on previous choices.

server <- function(input, output, session) {
  data <- load_data()

  output$mvca_logo <- renderImage({
    filename <- normalizePath(file.path('./images','mvca-new-logo.jpg'))

    list(src = filename,
         contentType = 'image/jpg',
         width = 202,
         height = 40,
         alt = "the MVCA's new logo")
  }, deleteFile = FALSE)

  sentence <- "A sample Instructions sentence for demo"
  output$codeblock <- renderText(sentence)

  observeEvent(input$update_tslist, {
    updated_data <- fread("https://waterdata.quinteconservation.ca/KiWIS/KiWIS?service=kisters&type=queryServices&request=getTimeseriesList&datasource=0&format=csv&csvdiv=,&timezone=GMT-5&dateformat=yyyy-MM-dd%20HH:mm:ss&site_no=2&station_name=*&returnfields=station_name,station_no,ts_id,ts_name,parametertype_name,stationparameter_name,coverage", sep=",")
    write.csv(updated_data, file = "tslist.csv", row.names=F)
    load_data()
    session$reload()
    return()
  })

  tsDetails <- reactiveValues(startDate=NULL, endDate=NULL, tsId=NULL)
  # Observe functions will be triggered each time an argument undergoes a change in value
  observeEvent(input$parameters, {
    # Update stations input based on parameters
    dataAfterParameter <<- data[data$parametertype_name == input$parameters, ]
    stations <- sort(unique(dataAfterParameter$station_name))
    updateSelectizeInput(session, input = "stations", choices = c("Choose", stations))
  })

  observeEvent(input$stations, {
    # Update timeseries input based on stations
    dataAfterStation <<- dataAfterParameter[dataAfterParameter$station_name == input$stations, ]
    timeseries <- sort(unique(dataAfterStation$ts_name))
    updateSelectizeInput(session, input = "timeseries", choices = c("Choose", timeseries))
  })

  observeEvent(input$timeseries, {
    # Get the timeseries details to update the id, start date, and end date
    ts <- dataAfterStation[dataAfterStation$ts_name == input$timeseries, ]
    tsDetails$tsID <<- ts$ts_id
    earliestStartDate <<- format(ts$from, format="%Y-%m-%d", tzone = "GMT-5")
    latestEndDate <<- format(ts$to, format="%Y-%m-%d", tzone = "GMT-5")
    updateDateInput(session, "startDate", min = earliestStartDate, max = latestEndDate, value = earliestStartDate)
    updateDateInput(session, "endDate", min = earliestStartDate, max = latestEndDate, value = latestEndDate)
  })

  observeEvent(input$startDate, {
    tsDetails$startDate <- format(input$startDate, format="%Y-%m-%d", tzone = "GMT-5")
  })

  observeEvent(input$endDate, {
    tsDetails$endDate <- format(input$endDate, format="%Y-%m-%d", tzone = "GMT-5")
  })

  observeEvent(input$loadTimeseriesData, {
    # Get the timeseries values #
    values <- ki_timeseries_values(
      ts_id = tsDetails$tsID,
      start_date = tsDetails$startDate,
      end_date = tsDetails$endDate
    )

    parameterShef <- tolower(dataAfterStation[[6]][1])
    parameterShef <- sub('.*- ', '', parameterShef)
    parameterShef <- gsub(",", "", parameterShef)
    parameterShef <- gsub(" ", "_", parameterShef)
    stationName <- tolower(input$stations)
    stationName <- sub('.*- ', '', stationName)
    stationName <- gsub(",", "", stationName)
    stationName <- gsub(" ", "_", stationName)
    tsName <- paste(stationName, parameterShef, sep = "_")
    str0 <- paste0('<b>', 'R script code:', '</b>')
    str1 <- ''
    str2 <- '# Install pacman package manager from CRAN'
    str3 <- "if (!require('pacman')) install.packages('pacman')"
    str4 <- '# Use pacman to load kiwisR and add-on packages'
    str5 <- 'pacman::p_load(pacman, kiwisR, utils)'
    str6 <- ''
    str7 <- paste0('# Access WISKI data for ', tsName)
    str8 <- paste0(tsName, ' <- ki_timeseries_values(')
    str9 <- paste0('&nbsp;&nbsp;', 'hub = "quinte",')
    str10 <- paste0('&nbsp;&nbsp;', 'ts_id = "', tsDetails$tsID, '",')
    str11 <- paste0('&nbsp;&nbsp;', 'start_date = "', tsDetails$startDate, '",')
    str12 <- paste0('&nbsp;&nbsp;', 'end_date = "', tsDetails$endDate, '"')
    str13 <- ')'
    str14 <- ''

    output$codeblock <- renderText({
      paste(str0, str1, str2, str3, str4, str5, str6, str7, str8, str9, str10, str11, str12, str13, str14, sep = "<br/>")
    })

    output$datatable <- DT::renderDataTable(server = FALSE, {
      DT::datatable(
        values,
        extensions = c('Buttons', 'Scroller'),
        filter = 'top',
        rownames = FALSE,
        options = list(
          autoWidth = TRUE,
          columnDefs = list(list(visible = FALSE, targets = c("Timeseries", "TS ID"))),
          dom = 'Blfrtip',
          deferRender = TRUE,
          lengthMenu = list(c(10, 20, 50, 100, 200, 500),
                            c('10', '20', '50', '100', '200', '500')),
          pageLength = 100,
          buttons = list(
            list(extend = 'colvis', text = "Visible Columns", targets = 0, visible = FALSE),
            list(extend = 'csv', text = "CSV Current Page", filename = "data_page",
                exportOptions = list(
                  modifier = list(page = "current")
                )
            ),
            list(extend = "csv", text = "CSV All Results", filename = "data_all",
                exportOptions = list(
                  modifier = list(page = "all")
                )
            ),
            list(extend = "pdf", text = "PDF Current Page", filename = "data_page",
                exportOptions = list(
                  modifier = list(page = "current")
                )
            ),
            list(extend = "pdf", text = "PDF All Results", filename = "data_all",
                exportOptions = list(
                  modifier = list(page = "all")
                )
            )
          )
        )
      ) %>% formatDate("Timestamp", "toLocaleString")
    })

    output$dataplot <- renderPlot({
      ggplot(values) + geom_line(aes(Timestamp, Value))
    })
  })
}

# Run the application
shinyApp(ui, server)
