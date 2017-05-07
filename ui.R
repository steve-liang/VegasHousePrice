library(shiny)
library(shinydashboard)
library(plotly)

sidebar <- dashboardSidebar(
  sidebarMenu(id = "sbm",   # crucial to name sidebarMenu 
              menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
              uiOutput("homeTypeUi"),
              menuItem("Factor", tabName = "factor", icon = icon("line-chart")),
              conditionalPanel(
                condition = "input.sbm == 'factor'",
                uiOutput("driverUi")
              ),
              menuItem("Geographic", tabName = "geographic", icon = icon("globe")
              ),
              conditionalPanel(
                condition = "input.sbm == 'geographic'",
                uiOutput("comparerUi")
              ),
              menuItem("Top Zip Analysis", tabName = "topZip", icon = icon("list-ol")
              ),
              conditionalPanel(
                condition = "input.sbm == 'topZip'",
                uiOutput("attrUi")
              ),
              uiOutput("removeOutlierUi")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "overview",
            fluidRow(
              column(width = 12,
                     valueBoxOutput("total_obs", width = 4),
                     valueBoxOutput("median_price", width = 4),
                     valueBoxOutput("median_pps", width = 4)
              )),
            
            fluidRow(
              column(width = 4,
                     "sometext here",
                     
                     
                     box(
                       title = "Home Type",
                       status = "primary",
                       width = 12,
                       height = 300,
                       solidHeader = TRUE,
                       plotOutput("home_type", height = 250)
                     )
                     
              ),
              column(width = 8, 
                     box(
                       title = "Top 10 Zip Code By Median Home Price",
                       status = "primary",
                       width = 12,
                       height = 300,
                       solidHeader = TRUE,
                       plotOutput("top10", height = 250)
                     ),
                     box(
                       title = "Top 10 Zip Code By Median Price Per Sqft",
                       status = "primary",
                       width = 12,
                       height = 300,
                       solidHeader = TRUE,
                       plotOutput("top10pps", height = 250)
                     )
              )
            )
    ),
    tabItem(tabName = "factor",
            fluidRow(
              box(
                title = "Regressing PPS by Factor",
                status = "primary",
                width = 12,
                height = 800,
                solidHeader = TRUE,
                plotlyOutput("regression", height = 700)
              )
            )
            
    ),
    tabItem(tabName = "geographic",
            fluidRow(
              box(
                title = "Visualizer",
                status = "primary",
                width = 12,
                height = 800,
                solidHeader = TRUE,
                plotOutput("geoplot", height = 700)
              )
            )
    ),
    tabItem(tabName = "topZip",
            fluidRow(
              box(
                title = "for top zip analysis",
                status = "primary",
                width = 12,
                height = 800,
                solidHeader = TRUE,
                plotOutput("zipAnalyzer", height = 700)
              )
            )
    )
  )
  
  
)

# Put them together into a dashboardPage
shinyUI(dashboardPage(
  dashboardHeader(title = "LV Dashboard"),
  sidebar,
  body
))