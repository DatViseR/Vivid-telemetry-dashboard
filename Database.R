
get_api_config <- function() {
  # Uses .Renviron file content - hidden variables
  list(
    supabase_url = Sys.getenv("SUPABASE_URL"),
    supabase_key = Sys.getenv("SUPABASE_API_KEY")
  )
}

# Function to fetch data from Supabase API
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

# Get telemetry data from Supabase
get_telemetry_data <- function() {
  # Use sample data if .Renviron variables aren't set up
  if (Sys.getenv("SUPABASE_URL") == "" || Sys.getenv("SUPABASE_API_KEY") == "") {
    message("API credentials not found. Using sample data.")
    return(read.csv("sample_data.csv"))
  }
  
  # Fetch data from the app_usage_stats table
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

# Filter data for a specific time range (days_ago can be NULL for all time)
filter_data_by_time <- function(data, days_ago = NULL) {
  if (is.null(days_ago)) {
    return(data)
  }
  
  cutoff_date <- Sys.time() - days(days_ago)
  filtered_data <- data %>% 
    filter(session_start >= cutoff_date)
  
  return(filtered_data)
}

# Calculate metrics from data
calculate_metrics <- function(data) {
  list(
    total_sessions = nrow(data),
    first_time_visits = sum(data$visit_count == 1),
    total_uploads = sum(data$upload_count),
    total_gsea = sum(data$gsea_count),
    total_volcano = sum(data$volcano_count),
    avg_duration = mean(data$session_duration, na.rm = TRUE),
    browsers = table(data$browser)
  )
}