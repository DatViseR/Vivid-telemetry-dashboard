
# Define colors
PRIMARY_COLOR <- "#4F46E5"
SECONDARY_COLOR <- "#10B981" 
ACCENT_COLOR <- "#F59E0B"
TEXT_COLOR <- "#E5E7EB"
BG_COLOR <- "#111827"
CARD_BG <- "#1F2937"
DARK_ACCENT <- "#374151"

ui <- navbarPage(
  title = tags$span(
    tags$img(src = "Vivid_volcano_logo.png", height = "50px", style = "margin-right: 10px;"),
    "Vivid-Volcano Analytics"
  ),
  id = "navBar",
  theme = "custom_theme.css", # Will be provided in styles.css
  windowTitle = "Vivid-Volcano Analytics Dashboard",
  
  # Include external CSS
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0")
  ),
  
  # Main Dashboard Tab
  tabPanel("Dashboard",
           div(class = "dash-container",
               fluidRow(
                 # Time Range
                 column(4,
                        h4("Time Range:", class = "filter-header"),
                        div(class = "filter-group",
                            radioGroupButtons(
                              inputId = "timeRange",
                              label = NULL,
                              choices = c("All Time" = "all", "7 Days" = "7", "30 Days" = "30", "Custom" = "custom"),
                              justified = TRUE,
                              size = "sm",
                              status = "primary",
                              checkIcon = list(yes = icon("check"))
                            )
                        )
                 ),
                 # Custom Date Range
                 column(4,
                        h4("Custom Time Range:", class = "filter-header"),
                        div(class = "filter-group",
                            dateRangeInput(
                              "customDateRange",
                              label = NULL,
                              start = Sys.Date() - 30,
                              end = Sys.Date(),
                              width = "100%"
                            )
                        )
                 ),
                 # Last Updated
                 column(4,
                        div(class = "last-updated", 
                            h4(icon("database"), "Last database query:", class = "filter-header"),
                            p(icon("clock"), " ", textOutput("lastQueryTime", inline = TRUE))
                        )
                 )
               ),
     

               
               # Stats cards
               fluidRow(
                 column(width = 3,
                        div(class = "stat-card",
                            div(class = "stat-icon sessions",
                                icon("users")
                            ),
                            div(class = "stat-details",
                                h3(class = "stat-value", uiOutput("totalSessionsValue")),
                                p(class = "stat-label", "Total Sessions"),
                                div(class = "stat-trend", uiOutput("sessionsComparisonText"))
                            )
                        )
                 ),
                 column(width = 3,
                        div(class = "stat-card",
                            div(class = "stat-icon visits",
                                icon("user-plus")
                            ),
                            div(class = "stat-details",
                                h3(class = "stat-value", uiOutput("firstTimeVisitsValue")),
                                p(class = "stat-label", "New Users"),
                                div(class = "stat-trend", uiOutput("visitsComparisonText"))
                            )
                        )
                 ),
                 column(width = 3,
                        div(class = "stat-card",
                            div(class = "stat-icon uploads",
                                icon("cloud-upload-alt")
                            ),
                            div(class = "stat-details",
                                h3(class = "stat-value", uiOutput("totalUploadsValue")),
                                p(class = "stat-label", "Data Uploads"),
                                div(class = "stat-trend", uiOutput("uploadsComparisonText"))
                            )
                        )
                 ),
                 column(width = 3,
                        div(class = "stat-card",
                            div(class = "stat-icon analyses",
                                icon("chart-bar")
                            ),
                            div(class = "stat-details",
                                h3(class = "stat-value", uiOutput("totalAnalysesValue")),
                                p(class = "stat-label", "Total Analyses"),
                                div(class = "stat-trend", uiOutput("analysesComparisonText"))
                            )
                        )
                 )
               ),
               
               # First row of charts - 3 plots
               fluidRow(
                 column(width = 4,
                        box(
                          width = NULL, # Takes full width of the column
                          title = "Sessions Over Time",
                          status = "primary",
                          solidHeader = TRUE,
                          highchartOutput("sessionsTimeSeries", height = "250px")
                        )
                 ),
                 column(width = 4,
                        box(
                          width = NULL,
                          title = "Uploads Over Time",
                          status = "primary", 
                          solidHeader = TRUE,
                          highchartOutput("uploadsTimeSeries", height = "250px")
                        )
                 ),
                 column(width = 4,
                        box(
                          width = NULL,
                          title = "Activities Over Time",
                          status = "primary",
                          solidHeader = TRUE,
                          highchartOutput("analysesTimeSeries", height = "250px")
                        )
                 )
               ),
               
               # Second row of charts - 3 plots
               fluidRow(
                 column(width = 4,
                        box(
                          width = NULL,
                          title = "Session Duration",
                          status = "primary",
                          solidHeader = TRUE,
                          highchartOutput("sessionDurationHist", height = "250px")
                        )
                 ),
                 column(width = 4,
                        box(
                          width = NULL,
                          title = "Browser Distribution",
                          status = "primary",
                          solidHeader = TRUE,
                          highchartOutput("browserTree", height = "250px")
                        )
                 ),
                 column(width = 4,
                        box(
                          width = NULL,
                          title = "Trend Analysis",
                          status = "primary",
                          solidHeader = TRUE,
                          highchartOutput("Trend_plot", height = "250px")
                        )
                        )
                 )# Close the fluidRow for second row of charts
                 ) # Close the div with class="dash-container"
               ), # Close the Dashboard tabPanel
  
  # About Tab
  tabPanel("About",
           div(class = "about-container",
               div(class = "about-card",
                   div(class = "card-header",
                       h2("Vivid Volcano Analytics Dashboard")
                   ),
                   div(class = "card-body",
                       h4("About This Dashboard"),
                       p("This dashboard provides analytics for the VividVolcano application, showing usage metrics and user behavior."),
                       p("The data is collected through a telemetry module and stored in a PostgreSQL database hosted on Supabase."),
                       
                       h4("Features"),
                       tags$ul(
                         tags$li("Real-time analytics of application usage"),
                         tags$li("Session tracking and visualization"),
                         tags$li("Analysis of user workflows and tool usage"),
                         tags$li("Time-based filtering options")
                       ),
                       
                       h4("Technical Details"),
                       p("Built with R Shiny, leveraging packages such as shinydashboard, plotly, and DBI for PostgreSQL connections."),
                       
                       div(class = "action-buttons",
                           tags$a(href = APP_URL, target = "_blank", icon("desktop"), "Open App", class = "action-btn app-btn"),
                           tags$a(href = GITHUB_URL, target = "_blank", icon("github"), "GitHub Repository", class = "action-btn github-btn"),
                           tags$a(href = LINKEDIN_URL, target = "_blank", icon("linkedin"), "Developer LinkedIn", class = "action-btn linkedin-btn"),
                           tags$a(href = DASHBOARD_CODE_URL, target = "_blank", icon("code"), "Dashboard Source", class = "action-btn code-btn")
                       )
                   )
               )
           )
  )
)