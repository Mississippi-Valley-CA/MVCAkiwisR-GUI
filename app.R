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

# Increase the timeout due to slow Quinte servers
options(timeout=300)

# Download the complete list of MVCAs timeseries
data <- fread("https://waterdata.quinteconservation.ca/KiWIS/KiWIS?format=csv&datasource=0&timezone=EST&dateformat=yyyy-MM-dd%20HH:mm:ss&service=kisters&type=queryServices&metadata=false&request=getTimeseriesList&station_no=MVCA1*,MVCA2*,MVCA3*,MVCA4*,MVCA5*,MVCA6*,MVCA7*,MVCA8*,MVCA9*,WISKI-0321,02KF01*,02KF001,02KF005,02KF006,02KF020&station_name=Gauge*&returnfields=station_name,station_no,ts_id,ts_name,parametertype_name,stationparameter_name,coverage")

# Build the parameters list
parameters <- sort(unique(data$parametertype_name))

# Define UI for app
ui <- fluidPage(
    # Application title
    titlePanel("MVCA WISKI Data"),

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

        actionButton("downloadButton", "Load Timeseries Data"),
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
            padding: 8px 0px 8px 0px;
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

  observeEvent(input$downloadButton, {
    # Get the timeseries values #
    values <- ki_timeseries_values(
      ts_id = tsDetails$tsID,
      start_date = tsDetails$startDate,
      end_date = tsDetails$endDate
    )

    parameterShef <- tolower(dataAfterStation[[6]][1])
    stationName <- tolower(input$stations)
    stationName <- sub('.*- ', '', stationName)
    stationName <- gsub(" ", "_", stationName)
    tsName <- paste(stationName, parameterShef, sep = "_")
    str0 <- paste0('<p><b>', 'R Script code:', '</b></p>')
    str1 <- '# Install kiwisR from CRAN'
    str2 <- "install.packages('kiwisR')"
    str3 <- '# Load the package kiwisR from the library'
    str4 <- 'library(kiwisR)'
    str5 <- ''
    str6 <- paste0('# Access WISKI data for ', tsName)
    str7 <- paste0(tsName, ' <- ki_timeseries_values(')
    str8 <- paste0('&nbsp;&nbsp;', 'hub = "quinte",')
    str9 <- paste0('&nbsp;&nbsp;', 'ts_id = "', tsDetails$tsID, '",')
    str10 <- paste0('&nbsp;&nbsp;', 'start_date = "', tsDetails$startDate, '",')
    str11 <- paste0('&nbsp;&nbsp;', 'end_date = "', tsDetails$endDate, '"')
    str12 <- ')'

    output$codeblock <- renderText({
      paste(str0, str1, str2, str3, str4, str5, str6, str7, str8, str9, str10, str11, str12, sep = "<br/>")
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
          deferRender = FALSE,
          pageLength = 20,
          lengthMenu = list(c(10, 20, 50, 100, 200, 500),
                            c('10', '20', '50', '100', '200', '500')),
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