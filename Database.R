# Database connection and data retrieval functions

# Configuration for connecting to Supabase PostgreSQL

get_db_config <- function() {
  # uses Renriron file content -  hidden variables
  list(
    dbname = Sys.getenv("TELEMETRY_DB_NAME"),
    host = Sys.getenv("TELEMETRY_DB_HOST"),
    port = Sys.getenv("TELEMETRY_DB_PORT", 5432),
    user = Sys.getenv("TELEMETRY_DB_USER"),
    password = Sys.getenv("TELEMETRY_DB_PASSWORD")
  )
}

# Connect to the database
connect_to_db <- function() {
  config <- get_db_config()
  
  tryCatch({
    conn <- DBI::dbConnect(
      RPostgres::Postgres(),
      dbname = config$dbname,
      host = config$host,
      port = config$port,
      user = config$user,
      password = config$password
    )
    return(conn)
  }, error = function(e) {
    message("Failed to connect to database: ", e$message)
    return(NULL)
  })
}

# Get telemetry data from database
get_telemetry_data <- function() {
  conn <- connect_to_db()
  
  if (is.null(conn)) {
    # Return sample data for development or if connection fails
    return(read.csv("sample_data.csv"))
  }
  
  tryCatch({
    # Adjust table name if needed
    query <- "SELECT * FROM app_usage_stats"
    data <- dbGetQuery(conn, query)
    
    # Convert timestamps to POSIXct
    data$session_start <- as.POSIXct(data$session_start, tz = "UTC")
    data$session_end <- as.POSIXct(data$session_end, tz = "UTC")
    
    # Calculate session duration in minutes
    data$session_duration <- as.numeric(difftime(data$session_end, 
                                                 data$session_start, 
                                                 units = "mins"))
    
    # Close connection
    dbDisconnect(conn)
    
    return(data)
  }, error = function(e) {
    message("Error fetching telemetry data: ", e$message)
    dbDisconnect(conn)
    return(NULL)
  })
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