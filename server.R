# server.R
# Make sure to have the "database.R" file in your app directory, and ensure it includes
# the get_telemetry_data() and calculate_metrics() functions as provided or adjusted.
# Adjust the code below to match your specific file paths or data requirements.

library(shiny)
library(dplyr)
library(highcharter)
library(RColorBrewer)
library(DBI)
library(RPostgres)



DASHBOARD_FONT <- "Inter, 'Segoe UI', sans-serif"

# Define colors used in the charts
PRIMARY_COLOR  <- "#4F46E5"
SUCCESS_COLOR  <- "#10B981"
WARNING_COLOR  <- "#F59E0B"
INFO_COLOR     <- "#3B82F6"

# Helper to convert date/time to JavaScript timestamps (milliseconds)
datetime_to_timestamp <- function(dt) {
  as.numeric(as.POSIXct(dt, tz = "UTC")) * 1000
}

# Helper function to calculate percentage change
calculate_percentage_change <- function(current, previous) {
  if (is.null(previous) || is.na(previous) || previous == 0) {
    return(100)
  }
  round((current - previous) / previous * 100)
}

server <- function(input, output, session) {
  # Current date and time for display
  current_datetime <- "2025-03-07 12:07:44"  # UTC
  current_user <- "DatViseR"
  
  # Reactive expression for the filtered data based on time range
  filteredData <- reactive({
    data <- get_telemetry_data()
    if (is.null(data)) {
      return(NULL)
    }
    
    # Apply time filter based on radio button selection
    if (input$timeRange == "7") {
      cutoff_date <- as.Date(Sys.Date()) - 7
      data <- data %>% filter(as.Date(session_start) >= cutoff_date)
    } else if (input$timeRange == "30") {
      cutoff_date <- as.Date(Sys.Date()) - 30
      data <- data %>% filter(as.Date(session_start) >= cutoff_date)
    } else if (input$timeRange == "custom") {
      start_date <- as.Date(input$customDateRange[1])
      end_date <- as.Date(input$customDateRange[2])
      end_datetime <- as.POSIXct(paste(end_date, "23:59:59"))
      data <- data %>% filter(session_start >= start_date & session_start <= end_datetime)
    }
    # else "all" -> no filtering
    data
  })
  
  # Reactive expression for metrics
  metrics <- reactive({
    data <- filteredData()
    if (is.null(data)) {
      return(NULL)
    }
    # calculate_metrics() is in database.R (adjust as needed)
    calculate_metrics(data)
  })
  
  # Comparison metrics
  comparisonMetrics <- reactive({
    # We'll use the unfiltered data from the database to compare current vs. previous
    data <- get_telemetry_data()
    if (is.null(data)) {
      return(NULL)
    }
    
    current_data <- filteredData()
    if (is.null(current_data) || nrow(current_data) == 0) {
      return(NULL)
    }
    
    # Determine previous period
    if (input$timeRange == "7") {
      previous_start <- as.Date(Sys.Date() - 14)
      previous_end   <- as.Date(Sys.Date() - 8)
      period_text    <- "vs previous 7 days"
    } else if (input$timeRange == "30") {
      previous_start <- as.Date(Sys.Date() - 60)
      previous_end   <- as.Date(Sys.Date() - 31)
      period_text    <- "vs previous 30 days"
    } else {
      # For custom or all time, use equal period before
      current_start <- as.Date(min(current_data$session_start))
      current_end   <- as.Date(max(current_data$session_start))
      date_range    <- as.integer(difftime(current_end, current_start, units = "days"))
      
      if (date_range < 1) date_range <- 1
      previous_end   <- current_start - 1
      previous_start <- previous_end - date_range
      period_text    <- "vs previous equal period"
    }
    
    previous_start_time <- as.POSIXct(paste(previous_start, "00:00:00"))
    previous_end_time   <- as.POSIXct(paste(previous_end, "23:59:59"))
    
    previous_data <- data %>%
      filter(session_start >= previous_start_time & session_start <= previous_end_time)
    
    # If previous data is empty, just return defaults
    if (nrow(previous_data) == 0) {
      return(list(
        sessions_pct = 100,
        visits_pct   = 100,
        uploads_pct  = 100,
        analyses_pct = 100,
        period_text  = period_text
      ))
    }
    
    current_metrics  <- calculate_metrics(current_data)
    previous_metrics <- calculate_metrics(previous_data)
    
    sessions_pct <- calculate_percentage_change(
      current_metrics$total_sessions, 
      previous_metrics$total_sessions
    )
    visits_pct <- calculate_percentage_change(
      current_metrics$first_time_visits, 
      previous_metrics$first_time_visits
    )
    uploads_pct <- calculate_percentage_change(
      current_metrics$total_uploads, 
      previous_metrics$total_uploads
    )
    analyses_pct <- calculate_percentage_change(
      (current_metrics$total_gsea + current_metrics$total_volcano), 
      (previous_metrics$total_gsea + previous_metrics$total_volcano)
    )
    
    list(
      sessions_pct = sessions_pct,
      visits_pct   = visits_pct,
      uploads_pct  = uploads_pct,
      analyses_pct = analyses_pct,
      period_text  = period_text
    )
  })
  
  # Last updated info
  output$lastUpdateInfo <- renderUI({
    HTML(paste("Last updated:", current_datetime, "UTC • by", current_user))
  })
  
  # Metric outputs
  output$totalSessionsValue <- renderUI({
    m <- metrics()
    if (is.null(m)) return(HTML("--"))
    HTML(format(m$total_sessions, big.mark = ","))
  })
  
  output$firstTimeVisitsValue <- renderUI({
    m <- metrics()
    if (is.null(m)) return(HTML("--"))
    HTML(format(m$first_time_visits, big.mark = ","))
  })
  
  output$totalUploadsValue <- renderUI({
    m <- metrics()
    if (is.null(m)) return(HTML("--"))
    HTML(format(m$total_uploads, big.mark = ","))
  })
  
  output$totalAnalysesValue <- renderUI({
    m <- metrics()
    if (is.null(m)) return(HTML("--"))
    total <- m$total_gsea + m$total_volcano
    HTML(format(total, big.mark = ","))
  })
  
  # Helper to create comparison text
  create_comparison_text <- function(pct, period_text) {
    if (pct > 0) {
      style <- "color: #28a745; font-size: 12px; margin-top: 5px;"
      icon_html <- icon("arrow-up")
      text <- paste0("+", pct, "% ", period_text)
    } else if (pct < 0) {
      style <- "color: #dc3545; font-size: 12px; margin-top: 5px;"
      icon_html <- icon("arrow-down")
      text <- paste0(pct, "% ", period_text)
    } else {
      style <- "color: #17a2b8; font-size: 12px; margin-top: 5px;"
      icon_html <- icon("equals")
      text <- paste("Same as", period_text)
    }
    div(style = style, icon_html, text)
  }
  
  # Comparison metrics outputs
  output$sessionsComparisonText <- renderUI({
    c <- comparisonMetrics()
    if (is.null(c)) return(NULL)
    create_comparison_text(c$sessions_pct, c$period_text)
  })
  
  output$visitsComparisonText <- renderUI({
    c <- comparisonMetrics()
    if (is.null(c)) return(NULL)
    create_comparison_text(c$visits_pct, c$period_text)
  })
  
  output$uploadsComparisonText <- renderUI({
    c <- comparisonMetrics()
    if (is.null(c)) return(NULL)
    create_comparison_text(c$uploads_pct, c$period_text)
  })
  
  output$analysesComparisonText <- renderUI({
    c <- comparisonMetrics()
    if (is.null(c)) return(NULL)
    create_comparison_text(c$analyses_pct, c$period_text)
  })
  
  # Sessions time series (Highcharter)
  output$sessionsTimeSeries <- renderHighchart({
    data <- filteredData()
    if (is.null(data) || nrow(data) == 0) return(NULL)
    
    # Aggregate by day
    daily_sessions <- data %>%
      mutate(date = as.Date(session_start)) %>%
      group_by(date) %>%
      summarise(
        sessions   = n(),
        first_time = sum(visit_count == 1),
        .groups    = "drop"
      )
    
    daily_sessions_hc <- daily_sessions %>%
      mutate(date_ms = datetime_to_timestamp(as.POSIXct(date, tz = "UTC")))
    
    highchart() %>%
      hc_chart(type = "spline") %>%
      hc_title(text = NULL) %>%
      hc_xAxis(type = "datetime", title = list(text = "Date")) %>%
      hc_yAxis(title = list(text = "Count")) %>%
      hc_tooltip(shared = TRUE) %>%
      hc_legend(
        align = "left",
        verticalAlign = "top",
        layout = "horizontal"
      ) %>%
      hc_add_series(
        name = "Total Sessions",
        data = list_parse2(daily_sessions_hc %>% transmute(x = date_ms, y = sessions))
      ) %>%
      hc_add_series(
        name = "First-Time Visitors",
        data = list_parse2(daily_sessions_hc %>% transmute(x = date_ms, y = first_time))
      ) %>%
      # Apply the theme at the end
    
      hc_add_theme(theme_vivid_dark)
  
  })

  
  # (Optional) Additional charts like uploadsTimeSeries, analysesTimeSeries, etc. can be handled likewise
}