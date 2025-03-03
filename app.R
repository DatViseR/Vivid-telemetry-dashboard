# Main application file

# Load libraries and source files
source("global.R")
source("ui.R")
source("server.R")

# Run the application
shinyApp(ui = ui, server = server)