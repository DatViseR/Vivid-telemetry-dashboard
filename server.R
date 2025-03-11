


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
  
  # Replace the previous observer with this one
  observeEvent(input$customDateRange, {
    # Use updateRadioGroupButtons for shinyWidgets::radioGroupButtons
    shinyWidgets::updateRadioGroupButtons(
      session = session,
      inputId = "timeRange",
      selected = "custom"
    )
    
    # For debugging - log to the console
    message("📅 Custom date range changed to: ", 
            format(input$customDateRange[1], "%Y-%m-%d"), " to ", 
            format(input$customDateRange[2], "%Y-%m-%d"))
    
    # Also output a notification to verify the event is triggering
    showNotification(
      paste("Date range selected:", 
            format(input$customDateRange[1], "%Y-%m-%d"), "to", 
            format(input$customDateRange[2], "%Y-%m-%d")),
      type = "message",
      duration = 3
    )
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

  
# Upload highchart ----

  output$uploadsTimeSeries <- renderHighchart({
    data <- filteredData()
    if (is.null(data) || nrow(data) == 0) {
      return(highchart() %>% 
               hc_title(text = "No data available") %>%
               hc_add_theme(theme_vivid_dark))
    }
    
    # Aggregate uploads data by day
    daily_uploads <- data %>%
      mutate(date = as.Date(session_start)) %>%
      group_by(date) %>%
      summarise(
        uploads = sum(upload_count, na.rm = TRUE)
           )
    
    # Convert dates to millisecond timestamps for highcharter
    daily_uploads_hc <- daily_uploads %>%
      mutate(date_ms = datetime_to_timestamp(as.POSIXct(date, tz = "UTC")))
    
    # Create the chart
    highchart() %>%
      hc_chart(type = "spline") %>%
      hc_title(text = NULL) %>%
      hc_xAxis(
        type = "datetime",
        title = list(text = "Date")
      ) %>%
      # Single Y-axis for uploads count
      hc_yAxis(
        title = list(text = "Number of Uploads")
      ) %>%
      hc_tooltip(
        shared = TRUE,
        formatter = JS("function() {
        var s = '<b>' + Highcharts.dateFormat('%Y-%m-%d', this.x) + '</b>';
        
        $.each(this.points, function () {
          s += '<br/><span style=\"color:' + this.series.color + '\">● </span>' + 
               this.series.name + ': ' + Highcharts.numberFormat(this.y, 0);
        });
        
        return s;
      }")
      ) %>%
      hc_legend(
        align = "left",
        verticalAlign = "top",
        layout = "horizontal"
      ) %>%
      hc_add_series(
        name = "File Uploads",
        data = list_parse2(daily_uploads_hc %>% transmute(x = date_ms, y = uploads)),
        color = WARNING_COLOR
      ) %>%
      # Add a column series type for uploads with some transparency
      hc_plotOptions(
        series = list(
          marker = list(
            enabled = TRUE,
            radius = 4
          )
        ),
        column = list(
          borderWidth = 0,
          opacity = 0.8
        )
      ) %>%
      # Apply the vivid dark theme
      hc_add_theme(theme_vivid_dark)
  }) 
  

# Analyses highchart ----
  output$analysesTimeSeries <- renderHighchart({
    data <- filteredData()
    if (is.null(data) || nrow(data) == 0) {
      return(highchart() %>% 
               hc_title(text = "No data available") %>%
               hc_add_theme(theme_vivid_dark))
    }
    
    # Aggregate analysis data by day
    daily_analyses <- data %>%
      mutate(date = as.Date(session_start)) %>%
      group_by(date) %>%
      summarise(
        gsea_analyses = sum(gsea_count, na.rm = TRUE),
        volcano_analyses = sum(volcano_count, na.rm = TRUE),
        total_analyses = sum(gsea_count + volcano_count, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Convert dates to millisecond timestamps for highcharter
    daily_analyses_hc <- daily_analyses %>%
      mutate(date_ms = datetime_to_timestamp(as.POSIXct(date, tz = "UTC")))
    
    # Create the chart with both types of analyses
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = NULL) %>%
      hc_xAxis(
        type = "datetime",
        title = list(text = "Date")
      ) %>%
      hc_yAxis(
        title = list(text = "Number of Analyses")
      ) %>%
      hc_tooltip(
        shared = TRUE,
        formatter = JS("function() {
        var s = '<b>' + Highcharts.dateFormat('%Y-%m-%d', this.x) + '</b>';
        var total = 0;
        
        $.each(this.points, function () {
          s += '<br/><span style=\"color:' + this.series.color + '\">● </span>' + 
               this.series.name + ': ' + Highcharts.numberFormat(this.y, 0);
          
          // Sum only the individual analysis types, not the total (which is the stacked area)
          if (this.series.name !== 'Total Analyses') {
            total += this.y;
          }
        });
        
        // Add total if we have both GSEA and Volcano points
        if (this.points.length > 1) {
          s += '<br/><hr style=\"margin: 4px 0\"/>';
          s += '<br/>Total: ' + Highcharts.numberFormat(total, 0);
        }
        
        return s;
      }")
      ) %>%
      hc_legend(
        align = "left",
        verticalAlign = "top",
        layout = "horizontal"
      ) %>%
      # Add GSEA analyses as columns
      hc_add_series(
        name = "GSEA Analyses",
        data = list_parse2(daily_analyses_hc %>% transmute(x = date_ms, y = gsea_analyses)),
        color = INFO_COLOR,
        type = "column"
      ) %>%
      # Add Volcano analyses as columns
      hc_add_series(
        name = "Volcano Analyses",
        data = list_parse2(daily_analyses_hc %>% transmute(x = date_ms, y = volcano_analyses)),
        color = SUCCESS_COLOR,
        type = "column"
      ) %>%
      # Add total analyses as a line
      hc_add_series(
        name = "Total Analyses",
        data = list_parse2(daily_analyses_hc %>% transmute(x = date_ms, y = total_analyses)),
        color = "#f472b6",
        type = "spline",
        lineWidth = 2,
        marker = list(
          lineWidth = 2,
          lineColor = "#ec4899",
          fillColor = "white"
        )
      ) %>%
      # Configure column stacking
      hc_plotOptions(
        column = list(
          stacking = "normal",
          borderWidth = 0,
          pointPadding = 0.1,
          groupPadding = 0.1
        ),
        series = list(
          marker = list(
            enabled = TRUE,
            radius = 3
          )
        )
      ) %>%
      # Apply the vivid dark theme
      hc_add_theme(theme_vivid_dark)
  }) 
  
# Session duration histogram ----
  
  output$sessionDurationHist <- renderHighchart({
    data <- filteredData()
    if (is.null(data) || nrow(data) == 0) {
      return(highchart() %>% 
               hc_title(text = "No data available") %>%
               hc_add_theme(theme_vivid_dark))
    }
    
    # Filter out unrealistic session durations (e.g., negative or extremely long)
    valid_sessions <- data %>%
      filter(!is.na(session_duration) & 
               session_duration >= 0 &
               session_duration <= 120)  # Cap at 2 hours for better visualization
    
    if (nrow(valid_sessions) == 0) {
      return(highchart() %>% 
               hc_title(text = "No valid session duration data available") %>%
               hc_add_theme(theme_vivid_dark))
    }
    
    # Calculate average session duration for reference line
    avg_duration <- mean(valid_sessions$session_duration, na.rm = TRUE)
    median_duration <- median(valid_sessions$session_duration, na.rm = TRUE)
    
    # Create histogram bins (1 minute intervals)
    bins <- seq(0, 120, by = 5)  # 5-minute bins up to 2 hours
    
    # Count sessions in each bin
    hist_data <- hist(valid_sessions$session_duration, 
                      breaks = bins, 
                      plot = FALSE)
    
    # Prepare data for highcharter
    histogram_data <- data.frame(
      x = hist_data$mids,
      y = hist_data$counts
    )
    
    # Create the histogram chart
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = NULL) %>%
      hc_xAxis(
        title = list(text = "Session Duration (minutes)"),
        min = 0,
        max = max(bins),
        tickInterval = 10,  # Show tick mark every 10 minutes
        labels = list(format = "{value} min")
      ) %>%
      hc_yAxis(
        title = list(text = "Number of Sessions"),
        allowDecimals = FALSE
      ) %>%
      hc_tooltip(
        headerFormat = "",
        pointFormat = "<b>{point.x:.0f}-{point.x+5:.0f} min</b>: {point.y} sessions"
      ) %>%
      # Add histogram columns
      hc_add_series(
        name = "Session Duration",
        data = list_parse2(histogram_data),
        color = PRIMARY_COLOR,
        groupPadding = 0.05,
        pointPadding = 0,
        borderWidth = 0
      ) %>%
      # Add average line
      hc_add_series(
        name = "Average Duration",
        data = list(list(x = avg_duration, y = 0), 
                    list(x = avg_duration, y = max(hist_data$counts) * 1.1)),
        color = WARNING_COLOR,
        type = "line",
        dashStyle = "shortdash",
        marker = list(enabled = FALSE),
        lineWidth = 2,
        states = list(hover = list(lineWidth = 2)),
        enableMouseTracking = FALSE
      ) %>%
      # Add median line
      hc_add_series(
        name = "Median Duration",
        data = list(list(x = median_duration, y = 0), 
                    list(x = median_duration, y = max(hist_data$counts) * 1.1)),
        color = SUCCESS_COLOR,
        type = "line",
        dashStyle = "solid",
        marker = list(enabled = F),
        lineWidth = 2,
        states = list(hover = list(lineWidth = 2)),
        enableMouseTracking = FALSE
      ) %>%
      # Add annotations for the lines
      hc_annotations(
        list(
          labels = list(
            list(
              point = list(x = avg_duration, y = max(hist_data$counts) * 1.05),
              text = paste0("Avg: ", round(avg_duration, 1), " min"),
              backgroundColor = "rgba(0, 0, 0, 0.3)",
              borderColor = WARNING_COLOR,
              borderWidth = 1,
              borderRadius = 3,
              padding = 3,
              style = list(color = "#FFF", fontSize= "10px")
            ),
            list(
              point = list(x = median_duration, y = max(hist_data$counts) * 0.95),
              text = paste0("Median: ", round(median_duration, 1), " min"),
              backgroundColor = "rgba(0, 0, 0, 0.3)",
              borderColor = SUCCESS_COLOR,
              borderWidth = 1,
              borderRadius = 3,
              padding = 3,
              style = list(color = "#FFF", fontSize= "10px")
            )
          )
        )
      ) %>%
      hc_legend(
        align = "left",
        verticalAlign = "top",
        layout = "horizontal"
      ) %>%
      hc_plotOptions(
        column = list(
          pointPadding = 0,
          borderWidth = 0,
          groupPadding = 0.1,
          shadow = FALSE
        )
      ) %>%
      # Apply the vivid dark theme
      hc_add_theme(theme_vivid_dark)
  })


# Browser Distribution Treemap ----
  output$browserTree <- renderHighchart({
    data <- filteredData()
    if (is.null(data) || nrow(data) == 0) {
      return(highchart() %>% 
               hc_title(text = "No data available") %>%
               hc_add_theme(theme_vivid_dark))
    }
    
    # Make sure we have browser data
    if (!"browser" %in% colnames(data)) {
      return(highchart() %>% 
               hc_title(text = "No browser data available") %>%
               hc_add_theme(theme_vivid_dark))
    }
    
    # Clean and prepare browser data
    browser_data <- data %>%
      # Clean browser strings
      mutate(
        browser = case_when(
          grepl("Chrome", browser, ignore.case = TRUE) & !grepl("Edge|Mobile", browser, ignore.case = TRUE) ~ "Chrome",
          grepl("Firefox", browser, ignore.case = TRUE) ~ "Firefox",
          grepl("Safari", browser, ignore.case = TRUE) & !grepl("Mobile", browser, ignore.case = TRUE) ~ "Safari",
          grepl("Edge", browser, ignore.case = TRUE) ~ "Edge",
          grepl("Opera", browser, ignore.case = TRUE) ~ "Opera",
          grepl("Mobile Safari|Android", browser, ignore.case = TRUE) ~ "Mobile",
          grepl("IE|Internet Explorer", browser, ignore.case = TRUE) ~ "IE",
          TRUE ~ "Other"
        )
      ) %>%
      # Group and count
      group_by(browser) %>%
      summarise(
        count = n(),
        .groups = "drop"
      ) %>%
      # Calculate percentage
      mutate(
        percentage = round(count / sum(count) * 100, 1)
      ) %>%
      # Sort by count descending
      arrange(desc(count))
    
    # Define colors for different browsers
    browser_colors <- list(
      "Chrome" = PRIMARY_COLOR,
      "Firefox" = "#FF9500",
      "Safari" = "#f472b6",
      "Edge" = "#1abc9c",
      "Opera" = "#FF1B2D",
      "Mobile" = WARNING_COLOR,
      "IE" = "#0076D7",
      "Other" = INFO_COLOR
    )
    
    # Create treemap data directly as a list
    treemap_data <- lapply(1:nrow(browser_data), function(i) {
      browser_name <- browser_data$browser[i]
      list(
        name = browser_name,
        value = browser_data$count[i],
        percentage = browser_data$percentage[i],
        color = if (browser_name %in% names(browser_colors)) 
          browser_colors[[browser_name]] 
        else 
          INFO_COLOR,
        dataLabels = list(
          enabled = TRUE,
          format = "{point.name}<br>{point.percentage:.1f}%"
        )
      )
    })
    
    # Create the treemap chart
    highchart() %>%
      hc_chart(type = "treemap") %>%
      hc_title(text = NULL) %>%
      hc_tooltip(
        pointFormat = "<b>{point.name}</b><br>Sessions: {point.value} ({point.percentage:.1f}%)"
      ) %>%
      hc_add_series(
        layoutAlgorithm = "squarified",
        name = "Browsers",
        data = treemap_data,
        allowDrillToNode = FALSE,
        levels = list(
          list(
            level = 1,
            dataLabels = list(
              enabled = TRUE,
              style = list(
                fontSize = "12px",
                textOutline = "none"
              )
            ),
            borderWidth = 3,
            borderColor = "#1b1b1b"
          )
        )
      ) %>%
      hc_plotOptions(
        treemap = list(
          layoutAlgorithm = "squarified",
          alternateStartingDirection = TRUE
        )
      ) %>%
      # Apply the vivid dark theme
      hc_add_theme(theme_vivid_dark)
  })
  
# Trend Analysis Plot ----
  output$Trend_plot <- renderHighchart({
    data <- filteredData()
    if (is.null(data) || nrow(data) == 0) {
      return(highchart() %>% 
               hc_title(text = "No data available") %>%
               hc_add_theme(theme_vivid_dark))
    }
    
    # Aggregate data by week (better granularity than daily for trend analysis)
    weekly_trend <- data %>%
      mutate(
        week_start = floor_date(session_start, unit = "week"),
        week_label = format(week_start, "%b %d")
      ) %>%
      group_by(week_start, week_label) %>%
      summarise(
        first_time_visitors = sum(visit_count == 1),
        gsea_analyses = sum(gsea_count, na.rm = TRUE),
        volcano_analyses = sum(volcano_count, na.rm = TRUE),
        total_analyses = sum(gsea_count + volcano_count, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(week_start)
    
    # Calculate percentage changes for annotations
    if (nrow(weekly_trend) > 1) {
      # Calculate overall change (first to last period)
      first_period <- weekly_trend[1, ]
      last_period <- weekly_trend[nrow(weekly_trend), ]
      
      visitors_change_pct <- round(
        (last_period$first_time_visitors - first_period$first_time_visitors) / 
          max(1, first_period$first_time_visitors) * 100
      )
      
      analyses_change_pct <- round(
        (last_period$total_analyses - first_period$total_analyses) / 
          max(1, first_period$total_analyses) * 100
      )
      
      # Determine trend directions
      visitors_trend <- if(visitors_change_pct > 0) "▲" else if(visitors_change_pct < 0) "▼" else "◆"
      analyses_trend <- if(analyses_change_pct > 0) "▲" else if(analyses_change_pct < 0) "▼" else "◆"
      
      # Format trend text
      visitors_trend_text <- paste0(visitors_trend, " ", abs(visitors_change_pct), "% ", 
                                    if(visitors_change_pct >= 0) "increase" else "decrease")
      analyses_trend_text <- paste0(analyses_trend, " ", abs(analyses_change_pct), "% ", 
                                    if(analyses_change_pct >= 0) "increase" else "decrease")
    } else {
      visitors_trend_text <- "Insufficient data for trend"
      analyses_trend_text <- "Insufficient data for trend"
    }
    
    # Convert dates for highcharter
    trend_data_hc <- weekly_trend %>%
      mutate(date_ms = datetime_to_timestamp(as.POSIXct(week_start)))
    
    # Determine maximum values for y-axis scaling
    max_visitors <- max(trend_data_hc$first_time_visitors)
    max_analyses <- max(trend_data_hc$total_analyses)
    
    # Create the chart
    hc <- highchart() %>%
      hc_chart(
        type = "line",
        zoomType = "xy"
      ) %>%
      hc_title(text = NULL) %>%
      hc_xAxis(
        categories = trend_data_hc$week_label,
        tickmarkPlacement = "on",
        title = list(text = "Time Period")
      ) %>%
      hc_yAxis_multiples(
        list(
          title = list(text = "First-time Visitors"),
          opposite = FALSE,
          max = max_visitors * 1.1  # Add 10% headroom
        ),
        list(
          title = list(text = "Analysis Count"),
          opposite = TRUE,
          max = max_analyses * 1.1  # Add 10% headroom
        )
      ) %>%
      hc_tooltip(
        shared = TRUE,
        crosshairs = TRUE
      ) %>%
      hc_legend(
        align = "left",
        verticalAlign = "top",
        layout = "horizontal"
      ) %>%
      # First time visitors (line with markers)
      hc_add_series(
        name = "First-time Visitors",
        data = trend_data_hc$first_time_visitors,
        type = "line",
        yAxis = 0,
        color = SUCCESS_COLOR,
        marker = list(
          enabled = TRUE,
          radius = 4,
          symbol = "circle"
        ),
        lineWidth = 3
      ) %>%
      # GSEA analyses (area)
      hc_add_series(
        name = "GSEA Analyses",
        data = trend_data_hc$gsea_analyses,
        type = "area",
        yAxis = 1,
        color = INFO_COLOR,
        fillOpacity = 0.3
      ) %>%
      # Volcano analyses (area)
      hc_add_series(
        name = "Volcano Analyses",
        data = trend_data_hc$volcano_analyses,
        type = "area",
        yAxis = 1,
        color = PRIMARY_COLOR,
        fillOpacity = 0.3
      ) %>%
      # Add trend annotations
      hc_annotations(
        list(
          labels = list(
            list(
              point = list(
                x = 0,
                y = 10,
                xAxis = 0,
                yAxis = 0
              ),
              text = paste("First-time Visitors:", visitors_trend_text),
              backgroundColor = "rgba(0, 0, 0, 0.6)",
              shape = "rect",
              style = list(
                color = "#FFFFFF",
                fontSize = "12px"
              ),
              borderWidth = 0,
              borderRadius = 5,
              padding = 8,
              shadow = TRUE,
              align = "left",
              verticalAlign = "bottom",
              x = 10,
              y = 15
            ),
            list(
              point = list(
                x = 0,
                y = 10,
                xAxis = 0,
                yAxis = 1
              ),
              text = paste("Analyses:", analyses_trend_text),
              backgroundColor = "rgba(0, 0, 0, 0.6)",
              shape = "rect",
              style = list(
                color = "#FFFFFF",
                fontSize = "12px"
              ),
              borderWidth = 0,
              borderRadius = 5,
              padding = 8,
              shadow = TRUE,
              align = "right",
              verticalAlign = "top",
              x = -10,
              y = 15
            )
          )
        )
      ) %>%
      # Add plot options for styling
      hc_plotOptions(
        series = list(
          marker = list(
            enabled = TRUE
          )
        ),
        area = list(
          stacking = "normal",
          lineWidth = 1,
          marker = list(
            enabled = FALSE
          )
        )
      ) %>%
      # Apply the vivid dark theme
      hc_add_theme(theme_vivid_dark)
    
    return(hc)
  })  
  
  
# Last database query time output----
  output$lastQueryTime <- renderText({
    # Get the current session start time or database query time
    query_time <- session_start_time()
    
    # Format the time nicely
    if (!is.null(query_time)) {
      format(query_time, "%Y-%m-%d %H:%M:%S")
    } else {
      format(Sys.time(), "%Y-%m-%d %H:%M:%S")  # Fallback to current time if not available
    }
  })
  
  # Function to get/store session start time
  session_start_time <- local({
    start_time <- NULL
    function() {
      # Initialize time if not set yet
      if (is.null(start_time)) {
        start_time <<- Sys.time()
      }
      return(start_time)
    }
  })
  
}