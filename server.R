server <- function(input, output, session) {
  # Reactive expression for the filtered data based on time range
  filteredData <- reactive({
    data <- get_telemetry_data()
    
    if (is.null(data)) {
      return(NULL)
    }
    
    # Apply time filter based on radio button selection
    if (input$timeRange == "7") {
      data <- filter_data_by_time(data, 7)
    } else if (input$timeRange == "30") {
      data <- filter_data_by_time(data, 30)
    } else if (input$timeRange == "custom") {
      # Filter by custom date range
      start_date <- as.POSIXct(input$customDateRange[1])
      end_date <- as.POSIXct(input$customDateRange[2]) + days(1) - seconds(1)
      data <- data %>% filter(session_start >= start_date & session_start <= end_date)
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
  
  # Value boxes
  output$totalSessions <- renderValueBox({
    m <- metrics()
    valueBox(
      m$total_sessions,
      "Total Sessions",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$firstTimeVisits <- renderValueBox({
    m <- metrics()
    valueBox(
      m$first_time_visits,
      "First-Time Visitors",
      icon = icon("user-plus"),
      color = "green"
    )
  })
  
  output$totalUploads <- renderValueBox({
    m <- metrics()
    valueBox(
      m$total_uploads,
      "Data Uploads",
      icon = icon("upload"),
      color = "purple"
    )
  })
  
  output$totalAnalyses <- renderValueBox({
    m <- metrics()
    gsea <- m$total_gsea
    volcano <- m$total_volcano
    total <- gsea + volcano
    
    valueBox(
      total,
      "Total Analyses",
      icon = icon("chart-bar"),
      color = "orange"
    )
  })
  
  # Session duration histogram
  output$sessionDurationHist <- renderPlotly({
    data <- filteredData()
    
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
        title = "Session Duration Distribution",
        xaxis = list(title = "Duration (minutes)"),
        yaxis = list(title = "Count"),
        bargap = 0.1
      )
    
    return(p)
  })
  
  # Browser distribution pie chart
  output$browserPie <- renderPlotly({
    data <- filteredData()
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
        title = "Browser Distribution",
        margin = list(l = 20, r = 20, t = 50, b = 20)
      )
    
    return(p)
  })
  
  # Analysis metrics bar chart
  output$analysisMetricsBar <- renderPlotly({
    m <- metrics()
    
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
        title = "Analysis Types",
        xaxis = list(title = ""),
        yaxis = list(title = "Count")
      )
    
    return(p)
  })
  
  # Sessions time series
  output$sessionsTimeSeries <- renderPlotly({
    data <- filteredData()
    
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
        title = "Sessions Over Time",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Count"),
        legend = list(x = 0.01, y = 0.99, bgcolor = "rgba(255,255,255,0.5)"),
        hovermode = "closest"
      )
    
    return(p)
  })
  
  # Uploads time series
  output$uploadsTimeSeries <- renderPlotly({
    data <- filteredData()
    
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
        title = "Daily Data Uploads",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Uploads"),
        hovermode = "closest"
      )
    
    return(p)
  })
  
  # Analyses time series
  output$analysesTimeSeries <- renderPlotly({
    data <- filteredData()
    
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
        marker = list(color = PRIMARY_COLOR),
        line = list(color = PRIMARY_COLOR)
      ) %>%
      layout(
        title = "Daily Analyses",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Count"),
        legend = list(x = 0.01, y = 0.99, bgcolor = "rgba(255,255,255,0.5)"),
        hovermode = "closest"
      )
    
    return(p)
  })
  
  # Refresh data periodically (every 5 minutes)
  observe({
    invalidateLater(300000)
    # This will cause reactive expressions to update
  })
}