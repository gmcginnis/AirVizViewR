# Libraries
# remove.packages("AirVizR")
# devtools::install_github("gmcginnis/AirVizR")
library(shiny)
library(shinydashboard)
library(tidyverse)
library(AirVizR)
library(AirSensor)

# UI ----------------------------------------------------------------------

ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "AirVizViewR"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Welcome", tabName = "tab_welcome", icon = icon("paper-plane")),
      menuItem("Inputs", tabName = "tab_inputs", icon = icon("i-cursor")),
      menuItem("Map", tabName = "tab_map", icon = icon("map-marker-alt")),
      menuItem("Heatmaps", tabName = "tab_heat", icon = icon("layer-group")),
      menuItem("Time Series", tabName = "tab_ts", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
# WELCOMETAB  ------------------------------------------------------------------
    tabItems(
      tabItem(
        tabName = "tab_welcome",
        fluidRow(
          column(
            width = 4,
            box(
              width = NULL, solidHeader = TRUE, background = "black", status = "primary",
              title = h3("Welcome!"),
              h4("This dashboard will grab information from",
                 tags$a(href="https://www.purpleair.com/", "PurpleAir"),
                 "based on provided inputs, and create some visual analysis using the",
                 tags$a(href="https://github.com/gmcginnis/AirVizR", tags$code("AirVizR")), "package."),
              "To conduct more advanced analysis or use your own spatio-temporal atmospheric data (STAD),
              including uploading your own PurpleAir data or comparing Federal Reference Monitor,
              you can install the package to your own RStudio for analysis."
            ),
            box(
              width = NULL, solidHeader = TRUE, background = "black", status = "primary",
              title = "How to use this dashboard",
              "Go to the \"Inputs\" tab first to provide the area and dates ranges of interest.
              Once the data has loaded, visit any of the other tabs
              to construct visualizations, following any additional instructions on the respective pages."
            )
          ),
          column(
            width = 4,
            box(
              width = NULL, solidHeader = TRUE, background = "black", status = "primary",
              title = "Author",
              "This dashboard was created by",
              tags$a(href="https://github.com/gmcginnis", "Gillian McGinnis"),
              "(Reed College '22) in August 2021.",
              "Behind-the-scenes:",
              tags$a(href="https://github.com/gmcginnis/AirVizViewR", "GitHub repository")
            ),
            box(
              width = NULL, solidHeader = TRUE, background = "black", status = "primary",
              title = "Acknowledgments",
              tags$ul(
                tags$li(tags$a(href="https://neighborsforcleanair.org/", "Neighbors for Clean Air (NCA)")),
                tags$li(tags$a(href="https://reed.edu/", "Reed College"))
              ),
              "Special thanks:",
              tags$ul(
                tags$b(
                  tags$li("Dr. Juliane Fry, Reed College"),
                  tags$li("Mary Peveto, NCA"),
                  tags$li("Micah Bishop, NCA")
                ),
                tags$li("Dr. Christine Kendrick, City of Portland"),
                tags$li("Lance Giles, Lane Regional Protection Agency"),
                tags$li("John Wasiutynski, Multnomah County"),
                tags$li("Knowledge Murphy, Multnomah County")
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "tab_inputs",
        fluidRow(
          column(
            width = 4,
            box(
              width = NULL, solidHeader = TRUE, background = "black", status = "primary",
              title = h3("Inputs"),
              h4("Location"),
              "You will need to input the location data as a boundary box (W, S, E, N).
              The easiest way to do so is to visit", tags$a(href="http://bboxfinder.com/", "bboxfinder"),
              "and using the drawing tools on the site to draw a small box of interest.
              At the bottom of the screen will be coordinates labeled", tags$b("'Box'"), "which you can copy and paste below:",
              textInput("input_location", label = "Boundary box:", value = "-122.854, 45.4, -122.58, 45.6"),
              tags$hr(),
              h4("Dates"),
              dateRangeInput(
                "input_dates",
                label = "Start and end dates of interest:",
                start = "2020-07-01", end = "2020-07-07",
                format = "MM d, yyyy"
              ),
              tags$hr(),
              HTML("Press the button below once the area and dates of interest have been entered!
                 The data gathering process will take a few moments.
                 When complete, you will see a preview of the data, and analysis can be conducted in the other tabs.<br><br>"),
              actionButton(
                inputId = "action_pasmap",
                label = "Get PAS!",
                icon = icon("hand-point-right"),
                class = "btn-info"
              )
            )
          ),
          column(
            width = 6,
            box(
              width = NULL, solidHeader = TRUE, background = "black", status = "primary",
              title = "PAS",
              plotOutput("output_pasmap", height = "800px")
            )
          )
        )
      )
    )
  )
)

# SERVER ------------------------------------------------------------------

server <- function(input, output){
  
  setArchiveBaseUrl("https://airsensor.aqmd.gov/PurpleAir/v1/")
  
  plot_pasmap <- eventReactive(input$action_pasmap, {
    pas <- pas_load(
      datestamp = inputs$input_dates[1],
      archival = TRUE
    ) %>% 
      pas_filterArea(
        w = "a",
        e = "e",
        s = "s",
        n = "n"
        
      )
  })
  
  output$output_pasmap <- renderPlot({plot_pasmap()})
}

# -------
shinyApp(ui = ui, server = server)