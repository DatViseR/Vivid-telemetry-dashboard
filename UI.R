ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = tags$span(
      tags$img(src = "vividvolcano_logo.png", height = "25px", 
               style = "margin-right: 10px;"),
      "VividVolcano Analytics"
    ),
    titleWidth = 300
  ),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Time Series", tabName = "timeseries", icon = icon("chart-line")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    ),
    radioGroupButtons(
      inputId = "timeRange",
      label = "Time Range:",
      choices = c("All Time" = "all", "Last 7 Days" = "7", "Last 30 Days" = "30"),
      justified = TRUE,
      status = "primary",
      checkIcon = list(yes = icon("check"))
    ),
    br(),
    tags$div(
      class = "sidebar-menu",
      tags$ul(
        class = "sidebar-links",
        tags$li(
          tags$a(href = APP_URL, target = "_blank", icon("desktop"), "Open App")
        ),
        tags$li(
          tags$a(href = GITHUB_URL, target = "_blank", icon("github"), "GitHub Repository")
        ),
        tags$li(
          tags$a(href = LINKEDIN_URL, target = "_blank", icon("linkedin"), "Developer LinkedIn")
        ),
        tags$li(
          tags$a(href = DASHBOARD_CODE_URL, target = "_blank", icon("code"), "Dashboard Source")
        )
      )
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style(HTML("
        .small-box {border-radius: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.1);}
        .content-wrapper {background-color: #f8f9fa;}
        .time-filters {margin-bottom: 20px; text-align: center;}
        .box {border-radius: 5px; box-shadow: 0 2px 5px rgba(0,0,0,0.1);}
        .sidebar-links {list-style: none; padding-left: 20px;}
        .sidebar-links li {margin-bottom: 10px;}
        .sidebar-links a {color: #fff; text-decoration: none;}
        .sidebar-links a:hover {color: #ccc;}
      "))
    ),
    
    tabItems(
      tabItem(
        tabName = "dashboard",
        fluidRow(
          column(
            width = 12,
            h2("App Usage Analytics", style = "margin-bottom: 20px;"),
            dateRangeInput(
              "customDateRange",
              "Custom Date Range:",
              start = Sys.Date() - 30,
              end = Sys.Date(),
              width = "100%"
            )
          )
        ),
        
        # Key metrics
        fluidRow(
          valueBoxOutput("totalSessions", width = 3),
          valueBoxOutput("firstTimeVisits", width = 3),
          valueBoxOutput("totalUploads", width = 3),
          valueBoxOutput("totalAnalyses", width = 3)
        ),
        
        # Charts
        fluidRow(
          box(
            title = "Session Duration Distribution",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("sessionDurationHist") %>% withSpinner()
          ),
          box(
            title = "Browser Distribution",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("browserPie") %>% withSpinner()
          )
        ),
        
        # Analysis metrics
        fluidRow(
          box(
            title = "Analysis Metrics",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("analysisMetricsBar") %>% withSpinner()
          )
        )
      ),
      
      tabItem(
        tabName = "timeseries",
        h2("Time Series Analytics"),
        fluidRow(
          box(
            title = "Sessions Over Time",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("sessionsTimeSeries") %>% withSpinner()
          )
        ),
        fluidRow(
          box(
            title = "Uploads Over Time",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("uploadsTimeSeries") %>% withSpinner()
          ),
          box(
            title = "Analyses Over Time",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("analysesTimeSeries") %>% withSpinner()
          )
        )
      ),
      
      tabItem(
        tabName = "about",
        h2("About This Dashboard"),
        box(
          width = 12,
          title = "VividVolcano Analytics Dashboard",
          status = "primary",
          solidHeader = TRUE,
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
          p("Built with R Shiny, leveraging packages such as shinydashboard, plotly, and DBI for PostgreSQL connections.")
        )
      )
    )
  )
)