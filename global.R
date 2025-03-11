# Load required packages
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DBI)
library(RPostgres)
library(dplyr)
library(plotly)
library(lubridate)
library(RColorBrewer)
library(shinyjs)
library(shinycssloaders)
library(highcharter)
library(httr)
library(jsonlite)


# App metadata
APP_NAME <- "Vivid-Volcano Analytics"
GITHUB_URL <- "https://github.com/DatViseR/Vivid-Volcano"
LINKEDIN_URL <- "https://www.linkedin.com/in/tomasz-st%C4%99pkowski/"
DASHBOARD_CODE_URL <- "https://github.com/DatViseR/Vivid-telemetry-dashboard"
APP_URL <- "https://datviser-vivid-volcano.share.connect.posit.cloud/"

# Theme colors
PRIMARY_COLOR <- "#3498db"  
SECONDARY_COLOR <- "#2c3e50"
SUCCESS_COLOR <- "#2ecc71"
WARNING_COLOR <- "#f39c12"
INFO_COLOR <- "#1abc9c"