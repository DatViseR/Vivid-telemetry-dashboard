

# Main application file for Vivid-telemetry-dashboard

# Load libraries and source files from R directory
source("global.R")
source("./R/Database.R")
source("./R/highcharter_theme.R")
source("ui.R")
source("server.R")

# Ensure CSS resources are properly available
# Check for www directory and create if needed
if (!dir.exists("www")) {
  dir.create("www", showWarnings = FALSE)
}

# Copy CSS files to www directory if needed
# Example: if CSS files were moved to R/css or similar
if (file.exists("./R/css/styles.css") && !file.exists("./www/styles.css")) {
  file.copy("./R/css/styles.css", "./www/styles.css", overwrite = TRUE)
}

# Run the application
shinyApp(ui = ui, server = server)