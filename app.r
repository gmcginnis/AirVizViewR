# Libraries
# remove.packages("AirVizR")
# devtools::install_github("gmcginnis/AirVizR")
library(shiny)
library(shinydashboard)
library(tidyverse)
library(AirVizR)
library(stringr)
# library(AirSensor)

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
              h3("Required"),
              "The following inputs are required to get the PurpleAir data.
              Because this dashboard is designed for visualizing more than one monitors' data,
              errors might occur if filters return only one matching monitor.",
              h4("Location"),
              "You will need to input the location data as a boundary box (W, S, E, N).
              The easiest way to do so is to visit", tags$a(href="http://bboxfinder.com/", "bboxfinder"),
              "and using the drawing tools on the site to draw a small box of interest.
              At the bottom of the screen will be coordinates labeled", tags$b("'Box'"), "which you can copy and paste below:",
              textInput(
                "input_coords",
                label = "Boundary box:",
                value = "-122.854, 45.4, -122.58, 45.6"
              ),
              h4("Dates"),
              dateRangeInput(
                "input_dates",
                label = "Start and end dates of interest:",
                start = "2020-07-01", end = "2020-07-07",
                format = "MM d, yyyy"
              ),
              h4("Locations"),
              checkboxInput(
                "input_outside",
                label = "Outdoor monitors",
                value = TRUE
              ),
              checkboxInput(
                "input_inside",
                label = "Indoor monitors",
                value = TRUE
              ),
              tags$hr(),
              h3("Optional"),
              h4("Labels"),
              "Filter the data set to only include monitors tagged with specific label(s).
              If using more than one, separate with commas and a space.",
              textInput(
                "input_labels",
                label = "Label filter:",
                value = NULL
              ),
              h4("State"),
              "Filter the data set to only include values from a specified state code:",
              textInput(
                "input_statecode",
                label = "State code:",
                value = NULL
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
              "Once you have entered inputs and pressed the 'Get PAS' button, a map will appear shortly after
              to display the monitors that match the inputs of interest. If there are more than 100 monitrs displayed,
              or if your date ranges are quite lengthy, consider applying more filters, as data loading can take the longest amount of time.",
              tags$br(),
              plotOutput("output_pasmap", height = "800px"),
              tags$br(),
              "Map look good? Press the button below to load the data!",
              tags$br(),
              actionButton(
                inputId = "action_pat",
                label = "Get PAT!",
                icon = icon("hand-point-right"),
                class = "btn-info"
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "tab_map",
        fluidRow(
          column(
            width = 4,
            box(
              width = NULL, solidHeader = TRUE, background = "black", status = "primary",
              title = h3("Inputs"),
              "blah"
            )
          )
        )
      )
    )
  )
)

# SERVER ------------------------------------------------------------------

server <- function(input, output){
  
  coord_list <- eventReactive(input$action_pasmap, {
    unlist(str_split(input$input_coords, ",")) %>%
      str_replace(" ", "") %>%
      as.double()
  })
  
  statecode <- eventReactive(input$action_pasmap, {
    if (input$input_statecode != "") {
      input$input_statecode
    } else { NULL }
  })
  
  labelcode <- eventReactive(input$action_pasmap, {
    if (input$input_labels != "") {
      c(unlist(str_split(input$input_labels, ", ")))
    } else { NULL }
  })
  
  pas_area <- eventReactive(input$action_pasmap, {
    get_area_pas(
      west = (coord_list()[1]),
      south = (coord_list()[2]),
      east = (coord_list()[3]),
      north = (coord_list()[4]),
      datestamp = (input$input_dates[2]),
      startdate = (input$input_dates[1]),
      state_code = statecode(),
      labels = labelcode()
    )
  })
  
  plot_pasmap <- eventReactive(input$action_pasmap, {
    
    pas_area_temp <- pas_area() %>% 
      rename(location = DEVICE_LOCATIONTYPE) %>% 
      select(latitude, longitude, location) %>% 
      drop_na(location) 
    
    if(isFALSE(input$input_outside)){
      pas_area_temp <- pas_area_temp %>% 
        filter(location != "outside")
    }
    
    if(isFALSE(input$input_inside)){
      pas_area_temp <- pas_area_temp %>% 
        filter(location != "inside")
    }
    
    ggmap::qmplot(
      longitude,
      latitude,
      color = location,
      data = pas_area_temp,
      main = paste("Number of monitors:",
                   nrow(pas_area_temp))) +
    theme(legend.position = "bottom")
    
  })
  
  output$output_pasmap <- renderPlot({plot_pasmap()})
  
  pat_ids <- eventReactive(input$action_pat, {
    withProgress(message = "Gathering data!",
                 detail = "This will be the longest step.",
                 get_ids(
                   pas = pas_area(),
                   outside = input$input_outside,
                   inside = input$input_inside
                 )
    )
  })
}

# -------
shinyApp(ui = ui, server = server)