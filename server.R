server <- function(input, output, session) {
  # Current date and time for display - using the updated values you provided
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
      # Use fixed integers with as.Date instead of days() function
      cutoff_date <- as.Date(Sys.Date()) - 7
      data <- data %>% filter(as.Date(session_start) >= cutoff_date)
    } else if (input$timeRange == "30") {
      # Use fixed integers with as.Date instead of days() function
      cutoff_date <- as.Date(Sys.Date()) - 30
      data <- data %>% filter(as.Date(session_start) >= cutoff_date)
    } else if (input$timeRange == "custom") {
      # Filter by custom date range - fixing the approach to avoid Period objects
      start_date <- as.Date(input$customDateRange[1])
      end_date <- as.Date(input$customDateRange[2])
      # Add time component to include the entire end day
      end_datetime <- as.POSIXct(paste(end_date, "23:59:59"))
      data <- data %>% filter(session_start >= start_date & session_start <= end_datetime)
    }
    
    return(data)
  })
  
  # Reactive expression for metrics
  metrics <- reactive({
    data <- filteredData()
    if (is.null(data)) {
      return(NULL)
    }
    
    calculate_metrics(data)
  })
  
  # Reactive function for calculating comparison metrics
  comparisonMetrics <- reactive({
    data <- get_telemetry_data()
    
    if (is.null(data)) {
      return(NULL)
    }
    
    # Current period
    current_data <- filteredData()
    if (is.null(current_data) || nrow(current_data) == 0) {
      return(NULL)
    }
    
    # Previous period - depends on selected time range
    if (input$timeRange == "7") {
      # Previous 7 days before current period - using Date arithmetic instead of Period
      previous_start <- as.Date(Sys.Date() - 14)
      previous_end <- as.Date(Sys.Date() - 8) # One day before current period starts
      period_text <- "vs previous 7 days"
    } else if (input$timeRange == "30") {
      # Previous 30 days before current period
      previous_start <- as.Date(Sys.Date() - 60)
      previous_end <- as.Date(Sys.Date() - 31) # One day before current period starts
      period_text <- "vs previous 30 days"
    } else {
      # For custom or all time, use equal period before
      current_start <- as.Date(min(current_data$session_start))
      current_end <- as.Date(max(current_data$session_start))
      date_range <- as.integer(difftime(current_end, current_start, units = "days"))
      
      # Ensure we have an integer number of days
      if (date_range < 1) date_range <- 1
      
      previous_end <- current_start - 1 # One day before current period starts
      previous_start <- previous_end - date_range
      
      period_text <- "vs previous equal period"
    }
    
    # Convert to POSIXct for filtering with proper time
    previous_start_time <- as.POSIXct(paste(previous_start, "00:00:00"))
    previous_end_time <- as.POSIXct(paste(previous_end, "23:59:59"))
    
    previous_data <- data %>%
      filter(session_start >= previous_start_time & session_start <= previous_end_time)
    
    # If previous data is empty, return default values
    if (nrow(previous_data) == 0) {
      return(list(
        sessions_pct = 100,
        visits_pct = 100,
        uploads_pct = 100,
        analyses_pct = 100,
        period_text = period_text
      ))
    }
    
    # Calculate metrics for both periods
    current_metrics <- calculate_metrics(current_data)
    previous_metrics <- calculate_metrics(previous_data)
    
    # Calculate percentage changes
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
    
    return(list(
      sessions_pct = sessions_pct,
      visits_pct = visits_pct,
      uploads_pct = uploads_pct,
      analyses_pct = analyses_pct,
      period_text = period_text
    ))
  })
  
  # Helper function to calculate percentage change
  calculate_percentage_change <- function(current, previous) {
    if (is.null(previous) || is.na(previous) || previous == 0) {
      return(100)
    }
    return(round((current - previous) / previous * 100))
  }
  
  # Last updated information
  output$lastUpdateInfo <- renderUI({
    HTML(paste("Last updated:", current_datetime, "UTC • by", current_user))
  })
  
  # Key metrics value outputs
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
  
  # Comparison metrics outputs
  output$sessionsComparisonText <- renderUI({
    c <- comparisonMetrics()
    if (is.null(c)) return(NULL)
    
    pct <- c$sessions_pct
    period_text <- c$period_text
    
    create_comparison_text(pct, period_text)
  })
  
  output$visitsComparisonText <- renderUI({
    c <- comparisonMetrics()
    if (is.null(c)) return(NULL)
    
    pct <- c$visits_pct
    period_text <- c$period_text
    
    create_comparison_text(pct, period_text)
  })
  
  output$uploadsComparisonText <- renderUI({
    c <- comparisonMetrics()
    if (is.null(c)) return(NULL)
    
    pct <- c$uploads_pct
    period_text <- c$period_text
    
    create_comparison_text(pct, period_text)
  })
  
  output$analysesComparisonText <- renderUI({
    c <- comparisonMetrics()
    if (is.null(c)) return(NULL)
    
    pct <- c$analyses_pct
    period_text <- c$period_text
    
    create_comparison_text(pct, period_text)
  })
  
  # Helper function to create comparison text with appropriate styling
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
  
  # Session duration histogram
  output$sessionDurationHist <- renderPlotly({
    data <- filteredData()
    if (is.null(data) || nrow(data) == 0) return(NULL)
    
    # Remove extreme outliers (more than 3 standard deviations)
    mean_duration <- mean(data$session_duration, na.rm = TRUE)
    sd_duration <- sd(data$session_duration, na.rm = TRUE)
    data_filtered <- data %>% 
      filter(session_duration > 0 & 
               session_duration < mean_duration + 3 * sd_duration)
    
    # Create histogram
    p <- plot_ly(
      x = data_filtered$session_duration,
      type = "histogram",
      marker = list(color = PRIMARY_COLOR,
                    line = list(color = "white", width = 0.5)),
      nbinsx = 20
    ) %>%
      layout(
        title = "", # Remove title as it's in the panel header now
        xaxis = list(title = "Duration (minutes)"),
        yaxis = list(title = "Count"),
        bargap = 0.1,
        margin = list(t = 10) # Reduce top margin since we have the panel header
      )
    
    return(p)
  })
  
  # Browser distribution pie chart
  output$browserPie <- renderPlotly({
    data <- filteredData()
    if (is.null(data) || nrow(data) == 0) return(NULL)
    
    browser_counts <- table(data$browser)
    
    colors <- brewer.pal(max(length(browser_counts), 3), "Set3")
    
    p <- plot_ly(
      labels = names(browser_counts),
      values = as.numeric(browser_counts),
      type = "pie",
      marker = list(colors = colors),
      textinfo = "label+percent",
      hoverinfo = "label+value+percent"
    ) %>%
      layout(
        title = "", # Remove title as it's in the panel header now
        margin = list(l = 20, r = 20, t = 10, b = 20) # Reduced top margin
      )
    
    return(p)
  })
  
  # Analysis metrics bar chart
  output$analysisMetricsBar <- renderPlotly({
    m <- metrics()
    if (is.null(m)) return(NULL)
    
    analysis_data <- data.frame(
      Type = c("GSEA Analyses", "Volcano Plots", "Data Uploads"),
      Count = c(m$total_gsea, m$total_volcano, m$total_uploads)
    )
    
    p <- plot_ly(
      data = analysis_data,
      x = ~Type,
      y = ~Count,
      type = "bar",
      marker = list(
        color = c(INFO_COLOR, WARNING_COLOR, SUCCESS_COLOR),
        line = list(color = "white", width = 0.5)
      )
    ) %>%
      layout(
        title = "", # Remove title as it's in the panel header now
        xaxis = list(title = ""),
        yaxis = list(title = "Count"),
        margin = list(t = 10) # Reduce top margin
      )
    
    return(p)
  })
  
  # Sessions time series
  output$sessionsTimeSeries <- renderPlotly({
    data <- filteredData()
    if (is.null(data) || nrow(data) == 0) return(NULL)
    
    # Aggregate by day
    daily_sessions <- data %>%
      mutate(date = as.Date(session_start)) %>%
      group_by(date) %>%
      summarise(
        sessions = n(),
        first_time = sum(visit_count == 1)
      )
    
    # Create time series plot
    p <- plot_ly() %>%
      add_trace(
        data = daily_sessions,
        x = ~date,
        y = ~sessions,
        type = "scatter",
        mode = "lines+markers",
        name = "Total Sessions",
        marker = list(color = PRIMARY_COLOR),
        line = list(color = PRIMARY_COLOR)
      ) %>%
      add_trace(
        data = daily_sessions,
        x = ~date,
        y = ~first_time,
        type = "scatter",
        mode = "lines+markers",
        name = "First-Time Visitors",
        marker = list(color = SUCCESS_COLOR),
        line = list(color = SUCCESS_COLOR)
      ) %>%
      layout(
        title = "", # Remove title as it's in the panel header now
        xaxis = list(title = "Date"),
        yaxis = list(title = "Count"),
        legend = list(x = 0.01, y = 0.99, bgcolor = "rgba(255,255,255,0.5)"),
        hovermode = "closest",
        margin = list(t = 10) # Reduce top margin
      )
    
    return(p)
  })
  
  # Uploads time series
  output$uploadsTimeSeries <- renderPlotly({
    data <- filteredData()
    if (is.null(data) || nrow(data) == 0) return(NULL)
    
    # Aggregate by day
    daily_uploads <- data %>%
      mutate(date = as.Date(session_start)) %>%
      group_by(date) %>%
      summarise(uploads = sum(upload_count))
    
    # Create time series plot
    p <- plot_ly(
      data = daily_uploads,
      x = ~date,
      y = ~uploads,
      type = "scatter",
      mode = "lines+markers",
      marker = list(color = WARNING_COLOR),
      line = list(color = WARNING_COLOR)
    ) %>%
      layout(
        title = "", # Remove title as it's in the panel header now
        xaxis = list(title = "Date"),
        yaxis = list(title = "Uploads"),
        hovermode = "closest",
        margin = list(t = 10) # Reduce top margin
      )
    
    return(p)
  })
  
  # Analyses time series
  output$analysesTimeSeries <- renderPlotly({
    data <- filteredData()
    if (is.null(data) || nrow(data) == 0) return(NULL)
    
    # Aggregate by day
    daily_analyses <- data %>%
      mutate(date = as.Date(session_start)) %>%
      group_by(date) %>%
      summarise(
        gsea = sum(gsea_count),
        volcano = sum(volcano_count)
      )
    
    # Create time series plot
    p <- plot_ly() %>%
      add_trace(
        data = daily_analyses,
        x = ~date,
        y = ~gsea,
        type = "scatter",
        mode = "lines+markers",
        name = "GSEA Analyses",
        marker = list(color = INFO_COLOR),
        line = list(color = INFO_COLOR)
      ) %>%
      add_trace(
        data = daily_analyses,
        x = ~date,
        y = ~volcano,
        type = "scatter",
        mode = "lines+markers",
        name = "Volcano Plots",
        marker = list(color = WARNING_COLOR),
        line = list(color = WARNING_COLOR)
      ) %>%
      layout(
        title = "", # Remove title as it's in the panel header now
        xaxis = list(title = "Date"),
        yaxis = list(title = "Count"),
        legend = list(x = 0.01, y = 0.99, bgcolor = "rgba(255,255,255,0.5)"),
        hovermode = "closest",
        margin = list(t = 10) # Reduce top margin
      )
    
    return(p)
  })
}

    
      
      
      
      
      
      