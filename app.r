# Libraries
# remove.packages("AirVizR")
# devtools::install_github("gmcginnis/AirVizR")
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(AirVizR)
library(stringr)
# library(DT)
# library(AirSensor)

list_var <- list(
  "PM2.5 (EPA corrected)" = "pm25_epa_2021",
  "PM2.5 (LRAPA corrected)" = "pm25_lrapa",
  "PM2.5 (CF=ATM)" = "pm25_atm",
  "PM2.5 (CF=1)" = "pm25_cf1",
  "Temperature (ºC)" = "temperature_c",
  "Temperature (ºF)" = "temperature",
  "Humidity" = "humidity"
)

# UI ----------------------------------------------------------------------

ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "AirVizViewR"),
  dashboardSidebar(
    sidebarMenu(
      id = "id_tabs",
      menuItem("Welcome", tabName = "tab_welcome", icon = icon("paper-plane")),
      menuItem("Inputs", tabName = "tab_inputs", icon = icon("i-cursor")),
      menuItem("Visualize", tabName = "tab_viz", icon = icon("hand-pointer"))
      # menuItem("Select", tabName = "tab_select", icon = icon("hand-pointer")),
#       menuItem("Select", tabName = "tab_map", icon = icon("map-marker-alt")),
#       menuItem("Large heatmap", tabName = "tab_heatmap", icon = icon("barcode")),
#       menuItem("Single site heatmap", tabName = "tab_heatmap_single", icon = icon("layer-group")),
#       menuItem("Time Series", tabName = "tab_ts", icon = icon("chart-line"))
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
                 "based on provided inputs, and allow you to conduct visual analysis using the",
                 tags$a(href="https://github.com/gmcginnis/AirVizR", tags$code("AirVizR")), "package."),
              "To conduct more advanced analysis or use your own spatio-temporal atmospheric data (STAD),
              including uploading your own PurpleAir data or comparing Federal Reference Monitor,
              you can install the package to your own RStudio for analysis."
            ),
            box(
              width = NULL, solidHeader = TRUE, background = "black", status = "primary",
              title = "How to use this dashboard",
              "Go to the \"Inputs\" tab first to provide the area and dates ranges of interest.
              Once the data has loaded, visit the \"Visualize\" tab to construct visualizations.",
              tags$hr(),
              tags$i("Data visualization is a powerful tool. Use it with both curiosity and caution.")
            ),
            box(
              width = NULL, solidHeader = TRUE, background = "black", status = "primary", collapsible = TRUE, collapsed = TRUE,
              title = "Disclaimer",
              "The inforamtion displayed on this dashboard is intended for informational and educational purposes only.",
              HTML("Health effects and weather predictions <b>cannot</b> be extrapolated from these results alone."),
              "The data is subject to change. While correction factors from the EPA and LRAPA are offered as forms of preliminary data validation,
              there is the possibility of faulty data due to erroneous monitor reports.",
              tags$br(),
              HTML("<b>If users should choose to share results from this dashboard,
              they are responsible for ensuring that all disclaimers of data sources, settings applied</b>
              (such as color caps)<b>, and limitations of results are explicitly stated.</b>")
            )
          ),
          column(
            width = 4,
            box(
              width = NULL, solidHeader = TRUE, background = "black", status = "primary",
              title = "Author",
              "This dashboard was created by",
              tags$a(href="https://github.com/gmcginnis", "Gillian McGinnis"),
              "(Reed College '22) in August 2021, and is actively being updated.",
              tags$br(),
              "This dashboard and the code within are open to the public, but if you share results please consider including authorship credit!",
              tags$hr(),
              "Behind-the-scenes:",
              tags$a(href="https://github.com/gmcginnis/AirVizViewR", "GitHub repository")
            ),
            box(
              width = NULL, solidHeader = TRUE, background = "black", status = "primary",
              title = "Acknowledgments",
              tags$ul(
                tags$b(
                  tags$li(tags$a(href="https://neighborsforcleanair.org/", "Neighbors for Clean Air (NCA)")),
                  tags$li(tags$a(href="https://reed.edu/", "Reed College"))
                )
              ),
              "Special thanks:",
              tags$ul(
                tags$b(
                  tags$li("Dr. Juliane Fry, Reed College"),
                  tags$li("Mary Peveto, NCA"),
                  tags$li("Micah Bishop, NCA")
                ),
                tags$li("Dr. Christine Kendrick, City of Portland"),
                tags$li("Lance Giles, LRAPA"),
                tags$li("ACSI Air Toxins Subcommittee, Multnomah County"),
                tags$li("John Wasiutynski, Multnomah County"),
                tags$li("Knowledge Murphy, Multnomah County")
              )
            ),
            box(
              width = NULL, solidHeader = TRUE, background = "black", status = "primary", collapsible = TRUE, collapsed = TRUE,
              title = "See also",
              tags$ul(
                tags$li(tags$a(href="https://neighborsforcleanair.org/", "Neighbors for Clean Air (NCA)")),
                tags$li(tags$a(href="https://www.lrapa.org/", "Lane Regional Air Protection Agency (LRAPA)")),
                tags$li(tags$a(href="https://fire.airnow.gov", "AirNow Fire and Smoke map")),
                tags$li(tags$a(href="https://www.iqair.com/", "IQAir map and AirVisual application")),
                tags$li(tags$a(href="https://www.purpleair.com/map", "PurpleAir map"))
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
              title = "Required inputs",
              "The following inputs are required to get the PurpleAir data.
              Because this dashboard is designed for visualizing more than one monitors' data,
              errors might occur if filters return only one matching monitor.",
              h4("Location"),
              "You will need to input the location data as a boundary box (W, S, E, N).
              The easiest way to do so is to visit", tags$a(href="http://bboxfinder.com/", "bboxfinder"),
              "and using the drawing tools on the site to draw a small box of interest.
              At the bottom of the screen will be coordinates labeled",
              tags$b("'Box'"), "which you can copy and paste below:",
              textInput(
                "input_coords",
                label = "Boundary box:",
                # value = "-122.65, 45.46, -122.60, 45.50"
                value = "-122.65, 45.48, -122.57, 45.52"
                # value = "-122.854, 45.4, -122.58, 45.6"
              ),
              h4("Dates"),
              dateRangeInput(
                "input_dates",
                label = "Start and end dates of interest:",
                start = "2021-07-01", end = "2021-07-07",
                # start = "2020-07-01", end = "2020-07-07",
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
              )
            ),
            box(
              collapsible = TRUE, collapsed = TRUE,
              width = NULL, solidHeader = TRUE, background = "black", status = "primary",
              title = "Optional inputs",
              h4("Labels"),
              "Filter the data set to only include monitors tagged with specific label(s).
              If using more than one, separate with commas and a space.",
              textInput(
                "input_labels",
                label = "Label filter:",
                value = NULL
              ),
              h4("State"),
              "If your coordinates include more than one state, but you only want results from one, consider filtering to only include values from a specified state:",
              selectizeInput(
              # textInput(
                "input_statecode",
                label = "State code:",
                # value = NULL,
                selected = "",
                choices = c("", state.abb)
              )
            ),
            box(
              width = NULL, solidHeader = TRUE, background = "black", status = "primary",
              "Press the button below once the area and dates of interest have been entered!
              The data gathering process will take a few moments.
              When complete, you will see a preview of the accessible data.",
              tags$br(),
              tags$br(),
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
              to display the monitors that match the inputs of interest. If there are more than 25 monitors displayed,
              or if your date ranges are quite lengthy, consider applying more filters, as data loading can take a long time.",
              tags$br(),
              "It should be noted that not all monitors displayed below will report complete data sets.",
              tags$br(),
              plotOutput("output_pasmap", height = "500px"),
              tags$br()
            ),
            box(
              width = NULL, solidHeader = TRUE, background = "black", status = "primary",
              title = "Next steps",
              "Map look good? Press the button below to load the data!",
              tags$br(),
              shinyjs::useShinyjs(),
              shinyjs::disabled(
                actionButton(
                  inputId = "action_pat",
                  label = "Get PAT!",
                  icon = icon("hand-point-right"),
                  class = "btn-info"
                )
              ),
              tags$br(),
              "Once the data has loaded, a table will appear below, and you can plot on the \'Visualize\' tab.",
              tableOutput("output_table_results")
            )
          )
        )
      ),
      tabItem(
        tabName = "tab_viz",
        fluidRow(
          column(
            width = 4,
            box(
              width = NULL, solidHeader = TRUE, background = "black", status = "primary", collapsible = TRUE,
              title = "Variables",
              "Select the values and variables to be plotted",
              radioButtons(
                inputId = "input_set",
                label = "Choose the data set to plot",
                choices = list(
                  "Full (maximum granularity)" = 1,
                  "Hourly (by day)" = 2,
                  "Daily" = 3
                  # "Diurnal (24 hour cycle)" = 4
                ),
                selected = 2
              ),
              tags$hr(),
              # varSelectizeInput(
              selectizeInput(
                # selectInput(
                inputId = "input_var",
                label = "Choose the variable of interest to plot",
                choices = list_var,
                # data = july_api_daily %>% slice(1) %>% select_if(is.numeric),
                selected = "pm25_epa_2021"
              ),
              tags$hr(),
              radioButtons(
                inputId = "input_viz",
                label = "Select the visualization of interest!",
                choices = list(
                  "Map" = 1,
                  "Heatmap (multiple)" = 3,
                  "Heatmap (single; hourly)" = 2,
                  "Time series" = 4
                ),
                selected = 1
              )
            ),
            box(
              width = NULL, solidHeader = TRUE, background = "black", status = "primary", collapsible = TRUE, collapsed = TRUE,
              title = "Color cap",
              "If the color scale is oversaturated, consider adding a cap. Values at or above the following selection will be colored separately.",
              sliderInput(
                inputId = "input_cap",
                label = "Cap value:",
                min = 0, max = 100,
                value = 100
              ),
              selectizeInput(
                inputId = "input_cap_color",
                label = "Cap color:",
                choices = str_subset(colors(), "[:digit:]", negate = TRUE),
                # choices = colors(),
                selected = "green"
              )
            ),
            box(
              width = NULL, solidHeader = TRUE, background = "black", status = "primary", collapsible = TRUE, collapsed = FALSE,
              title = "Map settings",
              shinyjs::useShinyjs(),
              shinyjs::disabled(
                sliderInput(
                  inputId = "input_pt",
                  label = "Point size:",
                  min = 1, max = 25,
                  value = 3
                )
              ),
              tags$hr(),
              "Do the streets appear to be fuzzy? Increase the following:",
              shinyjs::disabled(
                sliderInput(
                  inputId = "input_zoom",
                  label = "Map zoom value:",
                  min = 10, max = 16,
                  value = 13
                )
              )
            ),
            box(
              width = NULL, solidHeader = TRUE, background = "black", status = "primary", collapsible = TRUE, collapsed = TRUE,
              shinyjs::useShinyjs(),
              title = "Heatmap settings",
              shinyjs::disabled(
                checkboxInput(
                  inputId = "input_drop",
                  label = "Drop incomplete sets",
                  value = FALSE
                )
              ),
              tags$hr(),
              "For a heatmap of type 'single', select 1 site to visualize.",
              shinyjs::disabled(
                selectInput(
                  inputId = "input_site",
                  label = "Site of interest:",
                  choices = NULL
                )
              )
            ),
            box(
              width = NULL, solidHeader = TRUE, background = "black", status = "primary", collapsible = TRUE, collapsed = TRUE,
              shinyjs::useShinyjs(),
              title = "Time series settings",
              shinyjs::disabled(
                pickerInput(
                  inputId = "input_sites",
                  label = "Sites of interest:",
                  choices = c(),
                  selected = c(),
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE)
                )
              ),
              tags$hr(),
              shinyjs::disabled(
                checkboxInput("input_points", label = "Add points", value = TRUE),
                checkboxInput("input_extrema", label = "Add extrema", value = TRUE),
                checkboxInput("input_average", label = "Add average", value = TRUE),
                checkboxInput("input_column", label = "Single column", value = FALSE)
              )
            )
          ),
          column(
            width = 6,
            box(
              width = NULL, solidHeader = TRUE, background = "black", status = "primary",
              title = h3("Visualization"),
              plotOutput("output_viz", height = "800px")
            )
          )
        )
      )
    )
  )
)

# SERVER ------------------------------------------------------------------

server <- function(session, input, output){
  
  observe({
    
    input_startdate <- input$input_dates[1]
    
    updateDateRangeInput(
      session,
      "input_dates",
      max = input_startdate + 365
    )
  })
  
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
    withProgress(
      message = "Generating PAS!",
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
    
    if(as.numeric(input$input_dates[2] - input$input_dates[1]) > 31 & nrow(pas_area_temp) > 25){
      showNotification(
        "WARNING: Large data set selected. Please reduce the monitor area and time range.",
        duration = NULL
      )
      shinyjs::disable("action_pat")
    } else if(nrow(pas_area_temp) > 100){
      showNotification(
        "WARNING: Large data set selected. Please reduce the monitor area.",
        duration = NULL
      )
      shinyjs::disable("action_pat")
    } else {shinyjs::enable("action_pat")}
    
    withProgress(
      message = "Generating map!",
      ggmap::qmplot(
        longitude,
        latitude,
        geom = "blank",
        darken = c(0.5, "black"),
        data = pas_area_temp,
        main = paste("Number of monitors:", nrow(pas_area_temp))) +
        geom_point(aes(fill = location), size = 3, color = "white", shape = 21, alpha = 0.95) +
        theme(legend.position = "bottom")
    )
    
  })
  
  output$output_pasmap <- renderPlot({plot_pasmap()})
  
  pat_ids <- eventReactive(input$action_pat, {
    withProgress(
      message = "Gathering IDs!",
      detail = "This shouldn't take too long.",
      get_ids(
        pas = pas_area(),
        outside = input$input_outside,
        inside = input$input_inside
      )
    )
  })
  
  results <- eventReactive(input$action_pat, {
    withProgress(
      message = "Collecting data!",
      detail = "This will be the longest step. Yes, the progress bar is slow.",
      get_area_pat(
        id_list = pat_ids(),
        pas_input = pas_area(),
        startdate = input$input_dates[1],
        enddate = input$input_dates[2]
      )
    )
  })
  
  data_meta <- eventReactive(input$action_pat, {
    wrangle_meta(results()$raw_meta)
  })
  
  output$output_table_results <- renderTable({
    left_join(dataset(), data_meta()) %>%
      count(label) %>%
      rename(observations = n)
  })
  
  data_full <- eventReactive(input$action_pat, {
    withProgress(
      message = "Wrangling raw data!",
      wrangle_data(
        raw_pm_data = results()$raw_data,
        raw_meta_data = results()$raw_meta,
        drop_high = FALSE
      )
    )
  })
  
  data_hourly <- eventReactive(input$action_pat, {
    withProgress(
      message = "Applying correction factors!",
      apply_functions(data_full(), TRUE, TRUE, FALSE, FALSE)
    )
  })
  data_diurnal <- eventReactive(input$action_pat, {
    withProgress(
      message = "Applying correction factors!",
      apply_functions(data_full(), FALSE, TRUE, FALSE, FALSE)
    )
  })
  data_daily <- eventReactive(input$action_pat, {
    withProgress(
      message = "Applying correction factors!",
      apply_functions(data_full(), TRUE, FALSE, FALSE, FALSE)
    )
  })
  
  observeEvent(input$action_pat, {
      updateSliderInput(
        session,
        "input_cap",
        min = data_full() %>% select_if(is.numeric) %>% min(na.rm = TRUE),
        max = data_full() %>% select_if(is.numeric) %>% max(na.rm = TRUE),
        value = data_full() %>% select_if(is.numeric) %>% max(na.rm = TRUE)
      )
  })
  
  observe({
    updatePickerInput(
      session,
      "input_sites",
      choices = pull(data_meta(), label),
      selected = pull(data_meta(), label)
    )
  })
  
  dataset <- reactive({
    if (input$input_set == 1) {data_full()}
    else if (input$input_set == 2) {data_hourly()}
    else if (input$input_set == 3) {data_daily()}
    else if (input$input_set == 4) {data_diurnal()}
  })
  
  observe({
    if(input$input_set == 1){
      if(input$input_var %in% list_var[3:7]){
        selected_var <- input$input_var
        updateSelectizeInput(
          session,
          "input_var",
          choices = list_var[3:7],
          selected = selected_var
        )
      } else {
        updateSelectizeInput(
          session,
          "input_var",
          choices = list_var[3:7]
        )
      }
    } else {
      selected_var <- input$input_var
      updateSelectizeInput(
        session,
        "input_var",
        choices = list_var,
        selected = selected_var
      )
    }
    
#     if(input$input_set == 2) {
#       updateRadioGroupButtons(
#         input_viz
#       )
#     }
  })
  
  observe({
    if(input$input_viz == 1){
      shinyjs::enable("input_zoom")
      shinyjs::enable("input_pt")
    } else {
      shinyjs::disable("input_zoom")
      shinyjs::disable("input_pt")
    }
    
    if(input$input_viz == 2){
      shinyjs::enable("input_site")
    } else {
      shinyjs::disable("input_site")
    }
    
    if(input$input_viz == 3){
      shinyjs::enable("input_drop")
    } else {
      shinyjs::disable("input_drop")
    }
    
    if(input$input_viz == 4){
      shinyjs::enable("input_sites")
      shinyjs::enable("input_points")
      shinyjs::enable("input_extrema")
      shinyjs::enable("input_average")
      # shinyjs::enable("input_column")
    } else {
      shinyjs::disable("input_sites")
      shinyjs::disable("input_points")
      shinyjs::disable("input_extrema")
      shinyjs::disable("input_average")
      # shinyjs::disable("input_column")
    }
  })
  
#   observe({
#     updateSliderInput(
#       session,
#       "input_cap",
#       min = dataset() %>% select_if(is.numeric) %>% min(na.rm = TRUE),
#       max = dataset() %>% select_if(is.numeric) %>% max(na.rm = TRUE),
#       value = dataset() %>% select_if(is.numeric) %>% max(na.rm = TRUE)
#     )
#   })
  
  ## MAP
  viz_map <- reactive({
    if(input$input_var == "pm25_epa_2021"){
      map_stad(
        variable_of_interest = pm25_epa_2021,
        dataset = dataset(),
        location_data = data_meta(),
        zoom = input$input_zoom,
        cap_value = input$input_cap,
        cap_color = input$input_cap_color,
        point_size = input$input_pt
      )
    } else if(input$input_var == "pm25_lrapa"){
      map_stad(
        variable_of_interest = pm25_lrapa,
        dataset = dataset(),
        location_data = data_meta(),
        zoom = input$input_zoom,
        cap_value = input$input_cap,
        cap_color = input$input_cap_color,
        point_size = input$input_pt
      )
    } else if(input$input_var == "pm25_atm"){
      map_stad(
        variable_of_interest = pm25_atm,
        dataset = dataset(),
        location_data = data_meta(),
        zoom = input$input_zoom,
        cap_value = input$input_cap,
        cap_color = input$input_cap_color,
        point_size = input$input_pt
      )
    } else if(input$input_var == "pm25_cf1"){
      map_stad(
        variable_of_interest = pm25_cf1,
        dataset = dataset(),
        location_data = data_meta(),
        zoom = input$input_zoom,
        cap_value = input$input_cap,
        cap_color = input$input_cap_color,
        point_size = input$input_pt
      )
    } else if(input$input_var == "temperature_c"){
      map_stad(
        variable_of_interest = temperature_c,
        dataset = dataset(),
        location_data = data_meta(),
        zoom = input$input_zoom,
        cap_value = input$input_cap,
        cap_color = input$input_cap_color,
        point_size = input$input_pt
      )
    } else if(input$input_var == "temperature"){
      map_stad(
        variable_of_interest = temperature,
        dataset = dataset(),
        location_data = data_meta(),
        zoom = input$input_zoom,
        cap_value = input$input_cap,
        cap_color = input$input_cap_color,
        point_size = input$input_pt
      )
    } else if(input$input_var == "humidity"){
      map_stad(
        variable_of_interest = humidity,
        dataset = dataset(),
        location_data = data_meta(),
        zoom = input$input_zoom,
        cap_value = input$input_cap,
        cap_color = input$input_cap_color,
        point_size = input$input_pt
      )
    }
  })
  
  ## HEATMAP
  viz_heatmap <- reactive({
    if(input$input_var == "pm25_epa_2021"){
      heatmap_cross(
        variable_of_interest = pm25_epa_2021,
        dataset = dataset(),
        location_data = data_meta(),
        drop_incomplete = input$input_drop,
        cap_value = input$input_cap,
        cap_color = input$input_cap_color
      )
    } else if(input$input_var == "pm25_lrapa"){
      heatmap_cross(
        variable_of_interest = pm25_lrapa,
        dataset = dataset(),
        location_data = data_meta(),
        drop_incomplete = input$input_drop,
        cap_value = input$input_cap,
        cap_color = input$input_cap_color
      )
    } else if(input$input_var == "pm25_atm"){
      heatmap_cross(
        variable_of_interest = pm25_atm,
        dataset = dataset(),
        location_data = data_meta(),
        drop_incomplete = input$input_drop,
        cap_value = input$input_cap,
        cap_color = input$input_cap_color
      )
    } else if(input$input_var == "pm25_cf1"){
      heatmap_cross(
        variable_of_interest = pm25_cf1,
        dataset = dataset(),
        location_data = data_meta(),
        drop_incomplete = input$input_drop,
        cap_value = input$input_cap,
        cap_color = input$input_cap_color
      )
    } else if(input$input_var == "temperature_c"){
      heatmap_cross(
        variable_of_interest = temperature_c,
        dataset = dataset(),
        location_data = data_meta(),
        drop_incomplete = input$input_drop,
        cap_value = input$input_cap,
        cap_color = input$input_cap_color
      )
    } else if(input$input_var == "temperature"){
      heatmap_cross(
        variable_of_interest = temperature,
        dataset = dataset(),
        location_data = data_meta(),
        drop_incomplete = input$input_drop,
        cap_value = input$input_cap,
        cap_color = input$input_cap_color
      )
    } else if(input$input_var == "humidity"){
      heatmap_cross(
        variable_of_interest = humidity,
        dataset = dataset(),
        location_data = data_meta(),
        drop_incomplete = input$input_drop,
        cap_value = input$input_cap,
        cap_color = input$input_cap_color
      )
    }
  })
  
  ## SINGLE HEATMAP
  observe({
    updateSelectInput(
      session,
      "input_site",
      choices = pull(data_meta(), label),
      selected = pull(data_meta(), label)[1]
    )
  })
  
  viz_heatmap_single <- reactive({
    if(input$input_var == "pm25_epa_2021"){
      heatmap_single(
        variable_of_interest = pm25_epa_2021,
        site_of_interest = input$input_site,
        dataset = data_hourly(),
        location_data = data_meta(),
        cap_value = input$input_cap,
        cap_color = input$input_cap_color
      )
    } else if(input$input_var == "pm25_lrapa"){
      heatmap_single(
        variable_of_interest = pm25_lrapa,
        site_of_interest = input$input_site,
        dataset = data_hourly(),
        location_data = data_meta(),
        cap_value = input$input_cap,
        cap_color = input$input_cap_color
      )
    } else if(input$input_var == "pm25_atm"){
      heatmap_single(
        variable_of_interest = pm25_atm,
        site_of_interest = input$input_site,
        dataset = data_hourly(),
        location_data = data_meta(),
        cap_value = input$input_cap,
        cap_color = input$input_cap_color
      )
    } else if(input$input_var == "pm25_cf1"){
      heatmap_single(
        variable_of_interest = pm25_cf1,
        site_of_interest = input$input_site,
        dataset = data_hourly(),
        location_data = data_meta(),
        cap_value = input$input_cap,
        cap_color = input$input_cap_color
      )
    } else if(input$input_var == "temperature_c"){
      heatmap_single(
        variable_of_interest = temperature_c,
        site_of_interest = input$input_site,
        dataset = data_hourly(),
        location_data = data_meta(),
        cap_value = input$input_cap,
        cap_color = input$input_cap_color
      )
    } else if(input$input_var == "temperature"){
      heatmap_single(
        variable_of_interest = temperature,
        site_of_interest = input$input_site,
        dataset = data_hourly(),
        location_data = data_meta(),
        cap_value = input$input_cap,
        cap_color = input$input_cap_color
      )
    } else if(input$input_var == "humidity"){
      heatmap_single(
        variable_of_interest = humidity,
        site_of_interest = input$input_site,
        dataset = data_hourly(),
        location_data = data_meta(),
        cap_value = input$input_cap,
        cap_color = input$input_cap_color
      )
    }
  })
  
  
  ## LINE
  viz_line <- reactive({
    if(input$input_var == "pm25_epa_2021"){
      ts_line(
        variable_of_interest = pm25_epa_2021,
        dataset = dataset(),
        location_data = data_meta(),
        label_filter = input$input_sites,
        cap_value = input$input_cap,
        cap_color = input$input_cap_color,
        add_points = input$input_points,
        add_average = input$input_average,
        add_extrema = input$input_extrema,
        single_column = input$input_column
      )
    } else if(input$input_var == "pm25_lrapa"){
      ts_line(
        variable_of_interest = pm25_lrapa,
        dataset = dataset(),
        location_data = data_meta(),
        label_filter = input$input_sites,
        cap_value = input$input_cap,
        cap_color = input$input_cap_color,
        add_points = input$input_points,
        add_average = input$input_average,
        add_extrema = input$input_extrema,
        single_column = input$input_column
      )
    } else if(input$input_var == "pm25_atm"){
      ts_line(
        variable_of_interest = pm25_atm,
        dataset = dataset(),
        location_data = data_meta(),
        label_filter = input$input_sites,
        cap_value = input$input_cap,
        cap_color = input$input_cap_color,
        add_points = input$input_points,
        add_average = input$input_average,
        add_extrema = input$input_extrema,
        single_column = input$input_column
      )
    } else if(input$input_var == "pm25_cf1"){
      ts_line(
        variable_of_interest = pm25_cf1,
        dataset = dataset(),
        location_data = data_meta(),
        label_filter = input$input_sites,
        cap_value = input$input_cap,
        cap_color = input$input_cap_color,
        add_points = input$input_points,
        add_average = input$input_average,
        add_extrema = input$input_extrema,
        single_column = input$input_column
      )
    } else if(input$input_var == "temperature_c"){
      ts_line(
        variable_of_interest = temperature_c,
        dataset = dataset(),
        location_data = data_meta(),
        label_filter = input$input_sites,
        cap_value = input$input_cap,
        cap_color = input$input_cap_color,
        add_points = input$input_points,
        add_average = input$input_average,
        add_extrema = input$input_extrema,
        single_column = input$input_column
      )
    } else if(input$input_var == "temperature"){
      ts_line(
        variable_of_interest = temperature,
        dataset = dataset(),
        location_data = data_meta(),
        label_filter = input$input_sites,
        cap_value = input$input_cap,
        cap_color = input$input_cap_color,
        add_points = input$input_points,
        add_average = input$input_average,
        add_extrema = input$input_extrema,
        single_column = input$input_column
      )
    } else if(input$input_var == "humidity"){
      ts_line(
        variable_of_interest = humidity,
        dataset = dataset(),
        location_data = data_meta(),
        label_filter = input$input_sites,
        cap_value = input$input_cap,
        cap_color = input$input_cap_color,
        add_points = input$input_points,
        add_average = input$input_average,
        add_extrema = input$input_extrema,
        single_column = input$input_column
      )
    }
  })
  
  output_plot <- reactive({
#     if (input$input_viz == 1) {
#       input_startdate <- input$input_dates[1]
#       input_enddate <- input$input_dates[2]
#       viz_map()
#     }
    if (input$input_viz == 1) {viz_map()}
    else if (input$input_viz == 2) {viz_heatmap_single()}
    else if (input$input_viz == 3) {viz_heatmap()}
    else if (input$input_viz == 4) {viz_line()}
  })

  output$output_viz <- renderPlot({output_plot()})
}

# -------
shinyApp(ui = ui, server = server)