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
    infoBoxOutput('box_total_patients'),
    infoBoxOutput('box_symptom'),
    infoBoxOutput('box_severity'),
  ),
  fluidRow(
    # box(
    #   title = 'Sankey Diagram',
    #   width = 12,
    #   plotOutput('sankey_diagram'),
    #   height = '50%'
    # )
    uiOutput('box_sankey')
  ),
  fluidRow(
    infoBoxOutput('box_total_responded', width = 12),
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
  
  selected_severity <- eventReactive(input$visualize, {
    input$severity
  })
  
  selected_symptom <- eventReactive(input$visualize, {
    input$symptom
  })
  
  output$sankey_diagram <- renderPlot({
    make_sankey_diagram(cohort())
    
  }, res = 100)
  
  output$box_sankey <- renderUI({
    box(
      title = 'Sankey Diagram',
      width = 12,
      plotOutput('sankey_diagram'),
      height = '50%'
    )
  })
  
  box_results <- eventReactive(input$visualize, {
    value_box_results(cohort())
  })
  
  output$box_total_responded <- renderInfoBox({
    
    val <- extract_total_responded(box_results()$total_responded)
    
    infoBox(value = val, title = 'Total Patients Responded')
    
  })
  
  output$box_not_at_all <- renderValueBox({
    
    val <- extract_box_results(box_results()$total_responded, box_response = 'Not at all')
    
    valueBox(value = val, subtitle = 'Not at all')
    
  })
  
  output$box_slightly <- renderValueBox({
    
    val <- extract_box_results(box_results()$total_responded, box_response ='Slightly')
    
    valueBox(value = val, subtitle = 'Slightly', color = 'green')
    
  })
  
  output$box_moderately <- renderValueBox({
    
    val <- extract_box_results(box_results()$total_responded, box_response ='Moderately')
    
    valueBox(value = val, subtitle = 'Moderately', color = 'yellow')
    
  })

  output$box_quiteabit <- renderValueBox({
    
    val <- extract_box_results(box_results()$total_responded, box_response ='Quite a bit')
    
    valueBox(value = val, subtitle = 'Quite a bit', color = 'orange')
    
  })

  output$box_extremely <- renderValueBox({
    
    val <- extract_box_results(box_results()$total_responded, box_response ='Extremely')
    
    valueBox(value = val, subtitle = 'Extremely', color = 'red')
    
  })
  
  output$box_symptom <- renderInfoBox({
    infoBox(title = 'Selected Symptom', value = selected_symptom(), icon = icon('stethoscope'),
            color = 'green')
  })
  
  output$box_total_patients <- renderInfoBox({
    
    val <- c(box_results()$total_responded$n, box_results()$total_not_responded$n) %>% 
      sum()
    
    infoBox(title = 'Initial Patient Cohort', value = val, icon = icon('female'),
            color = 'green')
  })
  
  output$box_severity <- renderInfoBox({
    
    val <- selected_severity()
    
    infoBox(title = 'Severity', value = val, icon = icon('bar-chart'),
            color = 'green')
  })
  
}

shinyApp(ui, server)























