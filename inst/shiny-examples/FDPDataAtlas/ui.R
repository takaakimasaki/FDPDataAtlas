# pacman::p_load(dplyr, stringr,ggplot2,tidyr,DT,leaflet,leaflet.providers,htmltools,htmlwidgets,mapview,leafem,sf, viridis, shiny, shinydashboard,shinyWidgets,shinyBS,RColorBrewer,FDPDataAtlas)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(DT)
library(leaflet)
library(leaflet.providers)
library(htmltools)
library(htmlwidgets)
library(mapview)
library(leafem)
library(sf)
library(plotly)
library(viridis)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyBS)
library(RColorBrewer)
library(FDPDataAtlas)

if (webshot::is_phantomjs_installed() == FALSE) {
  webshot::install_phantomjs()
}

## ui.R ##

easyprint_js_file <- "https://rawgit.com/rowanwins/leaflet-easyPrint/gh-pages/dist/bundle.js"


sidebar <- dashboardSidebar(

  # sidebarUserPanel("FDP Data Atlas Nav"),
  sidebarMenu(
    id = "main_sidebar",
    menuItem("About FDP Data Atlas",
      tabName = "about",
      icon = icon("question")
    ),
    menuItem("Data Atlas",
      tabName = "home",
      icon = icon("map")
    ),
    menuItem("Descriptive Plots",
      tabName = "insightplots",
      icon = icon("home")
    ),
    menuItem("Heatmap",
      tabName = "heatmap",
      icon = icon("fire")
    ),
    menuItem("Database",
      tabName = "data",
      icon = icon("database")
    ),
    menuItem("Resources",
      tabName = "resources",
      icon = icon("list")
    ),
    menuItem("View Code",
      href = "https://github.com/takaakimasaki/FDPDataAtlas",
      icon = icon("github")
    )
  )
)


home <- tags$html(
  tags$head(
    includeHTML("www/google-analytics.html"),
    tags$title("FDP Data Atlas"),
    tags$script(src = easyprint_js_file)
  ),
  tags$style(
    type = "text/css",
    "#map {height: calc(100vh - 240px) !important;}"
  ),
  tags$body(
    leafletOutput("map", height = 600)
  )
)

body <- dashboardBody(
  tag("style", HTML("
                    .right-side {
                    background-color: #dbf0ee;
                    }
                    .skin-blue .main-header .logo {
                    background-color: #009FDA;
                    color: #ffffff;
                    }
                    .skin-blue .main-header .logo:hover {
                    background-color: #0072BC;
                    }
                    .skin-blue .main-header .navbar {
                    background-color: #009FDA;
                    }
                    .skin-blue .main-header .sidebar-toggle {
                    background-color: #0072BC;
                    }
                    ")),
  tabItems(
    tabItem(
      tabName = "about",
      fluidRow(
        column(
          12, wellPanel(
            htmlOutput("start_text")
          ),
          wellPanel(
            h3("Data Dictionary"),
            tableOutput("data_summary")
          )
        )
      )
    ),
    tabItem(
      tabName = "home",
      fluidRow(
  column(9, box(width = 12, home), style = "padding-right: 0px;"), # show map
  column(3, wellPanel(
            fluidRow(
              selectInput(
                inputId = "selected_variable",
                label = "Basemaps available",
                choices = unique(FDPDataAtlas::ref_data$indicator),
                selected = unique(FDPDataAtlas::ref_data$indicator)[1]
              )
            ),
    tags$style(type='text/css', '#country_info { max-height: 600px; overflow-y: auto; }'),
    uiOutput("country_info")), style = "padding-left: 0px;")
)
    ),
    tabItem(
      tabName = "data",
      fluidRow(
        column(
          12,
          DT::dataTableOutput("filtered_table")
        )
      )
    ),
tabItem(tabName = "insightplots",
        fluidRow(
          column(12, uiOutput("location_plot_selector"))  # Full width
        ),
          plotlyOutput("plot2", width = "100%", height = "75vh")
),
    tabItem(
      tabName = "heatmap",
      fluidRow(
        uiOutput("heatmap_selector")
      ),
      fluidRow(
          plotlyOutput("heatmap", width = "100%", height = "75vh")
      )
    ),
    tabItem(
      tabName = "resources",
      fluidRow(
        column(
          12,
          wellPanel(
            tabsetPanel()
          )
        )
      )
    )
  )
)

shinyUI(
  dashboardPage(
    dashboardHeader(title = "FDP Data Atlas"),
    sidebar,
    body
  )
)
