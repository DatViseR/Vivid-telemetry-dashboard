
# Configuration for connecting to Supabase via REST API

get_api_config <- function() {
  # Uses .Renviron file content - hidden variables
  list(
    supabase_url = Sys.getenv("SUPABASE_URL"),

    supabase_key = Sys.getenv("SUPABASE_KEY"),

    supabase_key = Sys.getenv("SUPABASE_API_KEY")

  )
}

# Function to fetch data from Supabase API

fetch_from_api <- function(endpoint, method = "GET", body = NULL, query_params = NULL) {
  config <- get_api_config()
  
  if (config$supabase_url == "" || config$supabase_key == "") {
    message("❌ Missing Supabase configuration. Check your .Renviron file.")
    return(NULL)
  }
  
  # Construct full URL
  url <- paste0(config$supabase_url, "/rest/v1/", endpoint)
  message("🔄 Connecting to Supabase API: ", endpoint)
  
  tryCatch({
    # Add headers for authentication and content type
    headers <- httr::add_headers(
      `apikey` = config$supabase_key,
      `Authorization` = paste("Bearer", config$supabase_key),
      `Content-Type` = "application/json",
      `Prefer` = "return=representation"
    )
    
    # Make the appropriate request
    response <- httr::GET(url, headers, query = query_params)
    
    # Check if the request was successful
    if (httr::status_code(response) < 300) {
      # Parse and return JSON response
      content_text <- httr::content(response, "text", encoding = "UTF-8")
      if (content_text == "" || content_text == "[]") {
        message("ℹ️ No data returned from API endpoint: ", endpoint)
        return(NULL)
      }
      message("✅ Successfully fetched data from Supabase API endpoint: ", endpoint)
      return(jsonlite::fromJSON(content_text))
    } else {
      message("❌ Supabase API error: ", httr::status_code(response), 
              " - ", httr::content(response, "text", encoding = "UTF-8"))
      return(NULL)
    }
  }, error = function(e) {
    message("❌ Error calling Supabase API: ", e$message)

fetch_from_api <- function(endpoint, query_params = NULL) {
  config <- get_api_config()
  
  if (config$supabase_url == "" || config$supabase_key == "") {
    message("Missing Supabase configuration. Check your .Renviron file.")
    return(NULL)
  }
  
  url <- paste0(config$supabase_url, "/rest/v1/", endpoint)
  
  tryCatch({
    response <- GET(
      url = url,
      add_headers(
        "apikey" = config$supabase_key,
        "Authorization" = paste("Bearer", config$supabase_key),
        "Content-Type" = "application/json"
      ),
      query = query_params
    )
    
    if (http_error(response)) {
      message("API request failed: ", content(response, "text"))
      return(NULL)
    }
    
    data <- fromJSON(content(response, "text", encoding = "UTF-8"))
    return(data)
    
  }, error = function(e) {
    message("Error in API request: ", e$message)

    return(NULL)
  })
}


# Get telemetry data from database (now using Supabase API)
get_telemetry_data <- function() {
  # Test Supabase connection
  message("🔄 Testing Supabase API connection...")
  
  config <- get_api_config()
  if (config$supabase_url == "" || config$supabase_key == "") {
    message("❌ Supabase credentials not found in .Renviron file")
    message("📝 Please add SUPABASE_URL and SUPABASE_KEY to your .Renviron file")
    message("⚠️ Using sample data as fallback")

# Get telemetry data from Supabase
get_telemetry_data <- function() {
  # Use sample data if .Renviron variables aren't set up
  if (Sys.getenv("SUPABASE_URL") == "" || Sys.getenv("SUPABASE_API_KEY") == "") {
    message("API credentials not found. Using sample data.")

    return(read.csv("sample_data.csv"))
  }
  
  # Fetch data from the app_usage_stats table

  message("🔄 Fetching telemetry data from Supabase API...")
  data <- fetch_from_api("app_usage_stats")
  
  if (is.null(data) || nrow(data) == 0) {
    message("⚠️ Failed to fetch data from Supabase API or received empty dataset")
    message("⚠️ Using sample data as fallback")
    return(read.csv("sample_data.csv"))
  }
  
  message(paste0("✅ Successfully retrieved ", nrow(data), " records from Supabase"))
  
  # Convert timestamps to POSIXct
  message("🔄 Processing timestamp data...")
  
  # Handle potential format differences in timestamps
  tryCatch({
    # Try to parse ISO8601 format (common in REST APIs)
    data$session_start <- as.POSIXct(data$session_start, format="%Y-%m-%dT%H:%M:%S", tz = "UTC")
    data$session_end <- as.POSIXct(data$session_end, format="%Y-%m-%dT%H:%M:%S", tz = "UTC")
    
    # Log some timestamp information for debugging
    if(nrow(data) > 0) {
      message("📅 Sample start time: ", as.character(data$session_start[1]))
      if(!is.na(data$session_end[1])) {
        message("📅 Sample end time: ", as.character(data$session_end[1]))
      }
    }
    
    # Calculate session duration in minutes - handle NULL and NA values properly
    message("🔄 Calculating session durations...")
    
    # Initialize session_duration column with NA values
    data$session_duration <- NA_real_
    
    # Only calculate duration for rows where both start and end times exist
    valid_rows <- !is.na(data$session_start) & !is.na(data$session_end)
    if(any(valid_rows)) {
      data$session_duration[valid_rows] <- as.numeric(
        difftime(data$session_end[valid_rows], 
                 data$session_start[valid_rows], 
                 units = "mins")
      )
      
      # Log duration stats
      valid_durations <- data$session_duration[!is.na(data$session_duration)]
      if(length(valid_durations) > 0) {
        message("⏱️ Duration stats - Min: ", round(min(valid_durations, na.rm=TRUE), 1),
                " Max: ", round(max(valid_durations, na.rm=TRUE), 1),
                " Avg: ", round(mean(valid_durations, na.rm=TRUE), 1))
      } else {
        message("⚠️ No valid session durations calculated")
      }
    } else {
      message("⚠️ No sessions with both start and end times found")
    }
    
  }, error = function(e) {
    message("❌ Error processing timestamps: ", e$message)
    message("⚠️ Session duration calculations may be affected")
  })
  
  # Ensure numeric columns are properly formatted
  message("🔄 Ensuring proper data types...")
  if("visit_count" %in% names(data)) data$visit_count <- as.integer(data$visit_count)
  if("upload_count" %in% names(data)) data$upload_count <- as.integer(data$upload_count)
  if("gsea_count" %in% names(data)) data$gsea_count <- as.integer(data$gsea_count)
  if("volcano_count" %in% names(data)) data$volcano_count <- as.integer(data$volcano_count)
  
  # Replace NAs with zeros in count columns
  data$visit_count[is.na(data$visit_count)] <- 0
  data$upload_count[is.na(data$upload_count)] <- 0
  data$gsea_count[is.na(data$gsea_count)] <- 0
  data$volcano_count[is.na(data$volcano_count)] <- 0
  
  message("✅ Data processing complete. Ready for dashboard display.")
  message(paste0("📊 Dataset summary: ", nrow(data), " sessions, spanning from ", 
                 as.character(min(data$session_start, na.rm = TRUE)), " to ", 
                 as.character(max(data$session_start, na.rm = TRUE))))
  
  # Count valid duration records
  valid_duration_count <- sum(!is.na(data$session_duration))
  message(paste0("⏱️ Valid session durations: ", valid_duration_count, " out of ", nrow(data), " records"))

  data <- fetch_from_api("app_usage_stats")
  
  if (is.null(data)) {
    # Return sample data if API request fails
    message("Failed to fetch data from API. Using sample data.")
    return(read.csv("sample_data.csv"))
  }
  
  # Convert timestamps to POSIXct
  data$session_start <- as.POSIXct(data$session_start, tz = "UTC")
  data$session_end <- as.POSIXct(data$session_end, tz = "UTC")
  
  # Calculate session duration in minutes
  data$session_duration <- as.numeric(difftime(data$session_end, 
                                               data$session_start, 
                                               units = "mins"))

  
  return(data)
}

# The following functions are kept exactly the same as in the original code

# Filter data for a specific time range (days_ago can be NULL for all time)
filter_data_by_time <- function(data, days_ago = NULL) {
  if (is.null(days_ago)) {
    return(data)
  }
  
  cutoff_date <- Sys.time() - days(days_ago)
  filtered_data <- data %>% 
    filter(session_start >= cutoff_date)
  
  message(paste0("📅 Filtered data to ", nrow(filtered_data), " records from past ", days_ago, " days"))
  return(filtered_data)
}

# Calculate metrics from data
calculate_metrics <- function(data) {
  metrics <- list(
    total_sessions = nrow(data),
    first_time_visits = sum(data$visit_count == 1, na.rm = TRUE),
    total_uploads = sum(data$upload_count, na.rm = TRUE),
    total_gsea = sum(data$gsea_count, na.rm = TRUE),
    total_volcano = sum(data$volcano_count, na.rm = TRUE),
    avg_duration = mean(data$session_duration, na.rm = TRUE),
    browsers = table(data$browser)
  )
  
  message("📊 Calculated metrics: ", 
          metrics$total_sessions, " sessions, ", 
          metrics$total_uploads, " uploads, ", 
          round(metrics$avg_duration, 1), " min avg. duration")
  
  return(metrics)
}

# Log current time and user - for debugging purposes
message(paste0("🕒 Telemetry dashboard loaded at: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC")))
