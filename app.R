library(shiny)
library(shinydashboard)
library(here)
library(tidyverse)
library(ggsankey)

data <- read_csv(here('data/shiny_qol_data.csv'), show_col_types = F)

symptom_list <- data %>% 
  pull(symptom_labels) %>% 
  unique()

treatment_list <- data %>% 
  pull(trt) %>% 
  unique()

response_list <- data %>% 
  pull(response) %>% 
  unique()

time_interval <- data %>% 
  summarise(min = min(time_point), max = max(time_point)) %>% 
  unlist()

# -------------------------------------------------------------------------

dashboard_header <- dashboardHeader(title = 'Breast Cancer Symptom Explorer', titleWidth = 500)

dashboard_body <- dashboardBody(
  fluidRow(
    box(title = 'Total Patients', width = 4),
    box(title = 'Symptom', width = 4),
    box(title = 'Severity', width = 4)
  ),
  fluidRow(
    box(
      title = 'Sankey Diagram',
      width = 12,
      plotOutput('sankey_diagram'),
      height = '50%'
    )
  ),
  fluidRow(
    valueBoxOutput('box_total_responded', width = 12),
  ),
  fluidRow(
    valueBoxOutput('box_not_at_all', width = 12),
  ),
  fluidRow(
    valueBoxOutput('box_slightly', width = 3),
    valueBoxOutput('box_moderately', width = 3),
    valueBoxOutput('box_quiteabit', width = 3),
    valueBoxOutput('box_extremely', width = 3)
  )
)

dashboard_sidebar <- dashboardSidebar(
  width = '500',
  collapsed = F,
  selectInput(
    'symptom',
    label = 'Symptom',
    choices = symptom_list,
    width = '100%'
  ),
  selectInput(
    'treatment',
    label = 'Treatment',
    choices = treatment_list,
    width = '100%'
  ),
  sliderInput(
    'timeinterval',
    label = 'Time Interval',
    min = time_interval[[1]],
    max = time_interval[[2]],
    step = 6,
    value = c(0, 12),
    width = '100%'
  ),
  selectInput(
    'severity',
    label = 'Severity',
    choices = response_list,
    width = '100%'
  ),
  actionButton('visualize', 'Visualize Cohort', width = '94%',
               style = 'align: center;')
)

ui <- dashboardPage(
  header = dashboard_header,
  sidebar = dashboard_sidebar,
  body = dashboard_body
)

server <- function(input, output) {
  
  cohort <- eventReactive(input$visualize, {
    select_cohort(
      data = data,
      selected_treatment = input$treatment,
      selected_symptom = input$symptom,
      selected_timepoints = input$timeinterval,
      selected_response = input$severity
    )
  })
  
  output$sankey_diagram <- renderPlot({
    make_sankey_diagram(cohort())
    
  }, res = 100)
  
  box_results <- reactive({
    value_box_results(cohort())
  })
  
  output$box_total_responded <- renderValueBox({
    
    val <- extract_total_responded(box_results()$total_responded)
    
    valueBox(value = val, subtitle = 'Total Patients Responded')
    
  })
  
  output$box_not_at_all <- construct_valuebox(box_results()$total_responded, 'Not at all')
  
  output$box_slightly <- construct_valuebox(box_results()$total_responded, 'Slightly')
  
  output$box_moderately <- construct_valuebox(box_results()$total_responded, 'Moderately')

  output$box_quiteabit <- construct_valuebox(box_results()$total_responded, 'Quite a bit')

  output$box_extremely <- construct_valuebox(box_results()$total_responded, 'Extremely')
  
}

shinyApp(ui, server)























