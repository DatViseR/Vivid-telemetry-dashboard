
library(shiny)
library(shinythemes)
library(plotly)
library(shinyWidgets)
library(shinycssloaders)
library(ggplot2)

ui <- navbarPage(
  title = tags$span(
    tags$img(src = "vividvolcano_logo.png", height = "25px", style = "margin-right: 10px;"),
    "VividVolcano Analytics"
  ),
  theme = shinytheme("flatly"),
  windowTitle = "VividVolcano Analytics Dashboard",
  
  # Include external CSS
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  # Main Dashboard Tab
  tabPanel("Dashboard",
           # First row with 4 equal sections
           fluidRow(
             # First element - Controls panel with 2x2 tiles grid below
             column(width = 3,
                    # Time controls section
                    div(class = "control-panel",
                        div(class = "time-range-group",
                            span(class = "time-range-label", "Time Range:"),
                            div(class = "time-range-controls",
                                radioGroupButtons(
                                  inputId = "timeRange",
                                  label = NULL,
                                  choices = c("All Time" = "all", "Last 7 Days" = "7", "Last 30 Days" = "30", "Custom" = "custom"),
                                  justified = TRUE,
                                  size = "sm",
                                  status = "primary",
                                  checkIcon = list(yes = icon("check"))
                                )
                            )
                        ),
                        div(class = "custom-date-group",
                            span(class = "custom-date-label", "Custom Date Range:"),
                            div(class = "custom-date-input",
                                dateRangeInput(
                                  "customDateRange",
                                  label = NULL,
                                  start = Sys.Date() - 30,
                                  end = Sys.Date(),
                                  width = "100%"
                                )
                            )
                        ),
                        div(class = "last-updated", 
                            paste("Last updated:", "2025-03-07 14:02:03", "UTC • by", "DatViseR")
                        )
                    ),
                    
                    # 2x2 Stats tiles grid
                    fluidRow(
                      # First row of tiles
                      column(width = 6, class = "tile-column",
                             div(class = "stat-tile",
                                 div(class = "stat-value", uiOutput("totalSessionsValue")),
                                 div(class = "stat-title", "Sessions"),
                                 div(class = "comparison-container", uiOutput("sessionsComparisonText"))
                             )
                      ),
                      column(width = 6, class = "tile-column",
                             div(class = "stat-tile",
                                 div(class = "stat-value", uiOutput("firstTimeVisitsValue")),
                                 div(class = "stat-title", "First-Time"),
                                 div(class = "comparison-container", uiOutput("visitsComparisonText"))
                             )
                      )
                    ),
                    fluidRow(
                      # Second row of tiles
                      column(width = 6, class = "tile-column",
                             div(class = "stat-tile",
                                 div(class = "stat-value", uiOutput("totalUploadsValue")),
                                 div(class = "stat-title", "Uploads"),
                                 div(class = "comparison-container", uiOutput("uploadsComparisonText"))
                             )
                      ),
                      column(width = 6, class = "tile-column",
                             div(class = "stat-tile",
                                 div(class = "stat-value", uiOutput("totalAnalysesValue")),
                                 div(class = "stat-title", "Analyses"),
                                 div(class = "comparison-container", uiOutput("analysesComparisonText"))
                             )
                      )
                    )
             ),
             
             # Second element - Session Duration Histogram
             column(width = 3,
                    div(class = "visualization-panel",
                        div(class = "panel-header", "Session Duration"),
                        div(class = "panel-body",
                            plotlyOutput("sessionDurationHist") %>% withSpinner()
                        )
                    )
             ),
             
             # Third element - Browser Distribution Pie Chart
             column(width = 3,
                    div(class = "visualization-panel",
                        div(class = "panel-header", "Browser Distribution"),
                        div(class = "panel-body",
                            plotlyOutput("browserPie") %>% withSpinner()
                        )
                    )
             ),
             
             # Fourth element - Analysis Metrics Bar Chart
             column(width = 3,
                    div(class = "visualization-panel",
                        div(class = "panel-header", "Analysis Metrics"),
                        div(class = "panel-body",
                            plotlyOutput("analysisMetricsBar") %>% withSpinner()
                        )
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