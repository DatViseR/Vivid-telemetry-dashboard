# Main application file

# Load libraries and source files
source("global.R")
source("Database.R")
source("highcharter_theme.R")
source("ui.R")
source("server.R")

# Run the application
shinyApp(ui = ui, server = server)