library(shiny)
library(shinythemes)
library(plotly)
library(shinyWidgets)
library(shinycssloaders)

ui <- navbarPage(
  title = tags$span(
    tags$img(src = "vividvolcano_logo.png", height = "25px", style = "margin-right: 10px;"),
    "VividVolcano Analytics"
  ),
  theme = shinytheme("flatly"),
  windowTitle = "VividVolcano Analytics Dashboard",
  
  # Main Dashboard Tab
  tabPanel("Dashboard",
           
           # Custom CSS for styling
           tags$head(
             tags$style(HTML("
        .nav-tabs {margin-bottom: 20px; background-color: #3c8dbc; border-bottom: none;}
        .nav-tabs>li>a {color: white !important;}
        .nav-tabs>li.active>a, .nav-tabs>li.active>a:focus, .nav-tabs>li.active>a:hover {
          background-color: #fff; 
          color: #3c8dbc !important;
        }
        .section-header {
          background-color: #f8f9fa;
          padding: 10px 15px;
          margin-bottom: 20px;
          font-weight: bold;
          font-size: 18px;
          border-left: 4px solid #3c8dbc;
          border-radius: 0 4px 4px 0;
        }
        .controls-panel {
          background-color: #f8f9fa;
          border-radius: 4px;
          padding: 15px;
          margin-bottom: 20px;
          box-shadow: 0 1px 3px rgba(0,0,0,0.1);
        }
        .time-controls {
          display: flex;
          flex-wrap: wrap;
          align-items: flex-end;
          gap: 15px;
        }
        .time-control-group {
          flex-grow: 1;
          min-width: 200px;
        }
        .action-buttons {
          display: flex;
          gap: 10px;
          margin-left: auto;
          align-items: flex-end;
        }
        .stat-box {
          background-color: #fff;
          border-radius: 4px;
          box-shadow: 0 2px 5px rgba(0,0,0,0.1);
          padding: 15px;
          margin-bottom: 20px;
          text-align: center;
          transition: transform 0.2s;
        }
        .stat-box:hover {
          transform: translateY(-2px);
          box-shadow: 0 4px 8px rgba(0,0,0,0.15);
        }
        .stat-value {
          font-size: 24px;
          font-weight: bold;
          color: #3c8dbc;
        }
        .stat-title {
          margin-top: 5px;
          color: #666;
        }
        .visualization-panel {
          background-color: #fff;
          border-radius: 4px;
          box-shadow: 0 2px 5px rgba(0,0,0,0.1);
          margin-bottom: 20px;
          overflow: hidden;
        }
        .panel-header {
          padding: 10px 15px;
          border-bottom: 1px solid #eee;
          background-color: #f8f9fa;
          font-weight: bold;
        }
        .panel-body {
          padding: 15px;
          min-height: 300px;
        }
        .last-updated {
          font-size: 12px;
          color: #666;
          margin-top: 5px;
          font-style: italic;
          text-align: right;
        }
      "))
           ),
           
           # Controls Panel
           div(class = "controls-panel",
               div(class = "time-controls",
                   div(class = "time-control-group",
                       h4("Time Range"),
                       radioGroupButtons(
                         inputId = "timeRange",
                         label = NULL,
                         choices = c("All Time" = "all", "Last 7 Days" = "7", "Last 30 Days" = "30"),
                         justified = TRUE,
                         status = "primary",
                         checkIcon = list(yes = icon("check"))
                       )
                   ),
                   div(class = "time-control-group",
                       h4("Custom Date Range"),
                       dateRangeInput(
                         "customDateRange",
                         label = NULL,
                         start = Sys.Date() - 30,
                         end = Sys.Date(),
                         width = "100%"
                       )
                   ),
                   div(class = "action-buttons",
                       actionButton("refreshBtn", "Refresh Data", icon = icon("sync"), class = "btn-primary"),
                       downloadButton("downloadBtn", "Export Data", class = "btn-default")
                   )
               ),
               div(class = "last-updated", 
                   paste("Last updated:", "2025-03-07 11:18:20", "UTC • by", "DatViseR")
               )
           ),
           
           # Key Metrics Section
           div(class = "section-header", "Key Metrics"),
           fluidRow(
             column(width = 3,
                    div(class = "stat-box",
                        div(class = "stat-value", uiOutput("totalSessionsValue")),
                        div(class = "stat-title", "Total Sessions"),
                        uiOutput("sessionsComparisonText")
                    )
             ),
             column(width = 3,
                    div(class = "stat-box",
                        div(class = "stat-value", uiOutput("firstTimeVisitsValue")),
                        div(class = "stat-title", "First-Time Visits"),
                        uiOutput("visitsComparisonText")
                    )
             ),
             column(width = 3,
                    div(class = "stat-box",
                        div(class = "stat-value", uiOutput("totalUploadsValue")),
                        div(class = "stat-title", "Total Uploads"),
                        uiOutput("uploadsComparisonText")
                    )
             ),
             column(width = 3,
                    div(class = "stat-box",
                        div(class = "stat-value", uiOutput("totalAnalysesValue")),
                        div(class = "stat-title", "Total Analyses"),
                        uiOutput("analysesComparisonText")
                    )
             )
           ),
           
           # Time Series Section
           div(class = "section-header", "Time Series Analytics"),
           fluidRow(
             # First row of visualizations in 3 columns
             column(width = 4,
                    div(class = "visualization-panel",
                        div(class = "panel-header", "Sessions Over Time"),
                        div(class = "panel-body",
                            plotlyOutput("sessionsTimeSeries") %>% withSpinner()
                        )
                    )
             ),
             column(width = 4,
                    div(class = "visualization-panel",
                        div(class = "panel-header", "Uploads Over Time"),
                        div(class = "panel-body",
                            plotlyOutput("uploadsTimeSeries") %>% withSpinner()
                        )
                    )
             ),
             column(width = 4,
                    div(class = "visualization-panel",
                        div(class = "panel-header", "Analyses Over Time"),
                        div(class = "panel-body",
                            plotlyOutput("analysesTimeSeries") %>% withSpinner()
                        )
                    )
             )
           ),
           
           # Distribution & Analysis Section
           div(class = "section-header", "Usage Distribution & Analysis"),
           fluidRow(
             # Second row of visualizations in 3 columns
             column(width = 4,
                    div(class = "visualization-panel",
                        div(class = "panel-header", "Session Duration Distribution"),
                        div(class = "panel-body",
                            plotlyOutput("sessionDurationHist") %>% withSpinner()
                        )
                    )
             ),
             column(width = 4,
                    div(class = "visualization-panel",
                        div(class = "panel-header", "Browser Distribution"),
                        div(class = "panel-body",
                            plotlyOutput("browserPie") %>% withSpinner()
                        )
                    )
             ),
             column(width = 4,
                    div(class = "visualization-panel",
                        div(class = "panel-header", "Analysis Metrics Breakdown"),
                        div(class = "panel-body",
                            plotlyOutput("analysisMetricsBar") %>% withSpinner()
                        )
                    )
             )
           )
  ),
  
  # About Tab
  tabPanel("About",
           fluidRow(
             column(width = 8, offset = 2,
                    div(class = "visualization-panel", style = "margin-top: 20px;",
                        div(class = "panel-header", "VividVolcano Analytics Dashboard"),
                        div(class = "panel-body",
                            h4("About This Dashboard"),
                            p("This dashboard provides analytics for the VividVolcano application, showing usage metrics and user behavior."),
                            p("The data is collected through a telemetry module and stored in a PostgreSQL database hosted on Supabase."),
                            h4("Features:"),
                            tags$ul(
                              tags$li("Real-time analytics of application usage"),
                              tags$li("Session tracking and visualization"),
                              tags$li("Analysis of user workflows and tool usage"),
                              tags$li("Time-based filtering options")
                            ),
                            h4("Technical Details:"),
                            p("Built with R Shiny, leveraging packages such as shinydashboard, plotly, and DBI for PostgreSQL connections."),
                            hr(),
                            div(
                              tags$a(href = APP_URL, target = "_blank", icon("desktop"), "Open App", class = "btn btn-default"),
                              tags$a(href = GITHUB_URL, target = "_blank", icon("github"), "GitHub Repository", class = "btn btn-default"),
                              tags$a(href = LINKEDIN_URL, target = "_blank", icon("linkedin"), "Developer LinkedIn", class = "btn btn-default"),
                              tags$a(href = DASHBOARD_CODE_URL, target = "_blank", icon("code"), "Dashboard Source", class = "btn btn-default")
                            )
                        )
                    )
             )
           )
  )
)