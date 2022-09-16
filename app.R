library(shiny)
library(shinydashboard)
library(bs4Dash)
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

response_list <- local({
  response_list <- data %>%
    pull(response) %>%
    unique()
  
  response_list <-
    response_list[!response_list %in% c("No Response", "Off trial")]
  
  factor(response_list,
         c(
           "Not at all",
           "Slightly",
           "Moderately",
           "Quite a bit",
           "Extremely"
         ))
})

time_interval <- data %>% 
  summarise(min = min(time_point), max = max(time_point)) %>% 
  unlist()

# -------------------------------------------------------------------------

dashboard_header <- dashboardHeader(title = 'Breast Cancer Symptom Explorer', titleWidth = '100%')

dashboard_body <- dashboardBody(
  fluidRow(
    box(width = 12, 
        title = 'Initial Selected Cohort',
        status = 'primary',
        footer = 'The following are the selected cohort of interest based upon the tool inputs.',
        fluidRow(
          width = 12,
          infoBoxOutput('box_total_patients'),
          infoBoxOutput('box_symptom'),
          infoBoxOutput('box_severity'))
        )
  ),
  fluidRow(
    box(
      title = 'Sankey Diagram',
      width = 12,
      status = 'primary',
      plotOutput('sankey_diagram'),
      height = '50%',
      footer = 'The following is a Sankey Diagram. 
      The purpose of this diagram is to allow the user to visualize the flow of patient 
      from one timepoint to another by varying levels of severity as selected by the inputs. The following diagram is visualizing the 
      initial timepoint of interest to the final timepoint of interest.
      The size of each "block" or "node" represents the relative proportion of the 
      patients that provided that response at the given month (column). 
      The colored height or size of the bands represents the relative proportion of the patients flowing from one timepoint to the next.'
    )
  ),
  fluidRow(
    box(
      title = 'Results at Final Timepoint',
      width = 12,
      status = 'primary',
      footer = 'The following are the outcomes among the patients who responded at the final timepoint of interest.',
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
  )
)

dashboard_sidebar <- dashboardSidebar(
  width = '50%',
  collapsed = F,
  minified = F,
  expandOnHover = F,
  skin = 'light',
  fluidRow(
    box(
      title = 'Symptom Explore Inputs',
      width = 12,
      collapsible = F,
      status = 'primary',
      helpText('Please select one item from the list of symptoms using the drop-down menu or 
               typing text in the box.'),
      selectInput(
        'symptom',
        label = '1. Select symptom',
        choices = symptom_list,
        width = '100%',
        selected = sample(symptom_list, size = 1)
      ),
      hr(),
      helpText('Select either Tamoxifen or Anastrozole treatment from the drop-down menu'),
      selectInput(
        'treatment',
        label = '2. Select treatment',
        choices = treatment_list,
        width = '100%',
        selected = sample(treatment_list, size = 1)
      ),
      hr(),
      helpText('Use the slider to select the starting and ending timepoint. 
               This can start at 0 months (or baseline) and can go up to 72 months (or 6 years).'),
      sliderInput(
        'timeinterval',
        label = '3. Select time interval (months)',
        min = time_interval[[1]],
        max = time_interval[[2]],
        step = 6,
        value = c(0, sample(seq(6, 72, 6), size = 1)),
        width = '100%'
      ),
      hr(),
      helpText('Select a corresponding severity level from the drop-down menu. 
               Severity ranges from “Not at all” 
               (the lowest level of severity) to “Extremely” 
               (the highest level of severity experienced)'),
      selectInput(
        'severity',
        label = '4. Select initial severity',
        choices = response_list,
        width = '100%',
        selected = sample(response_list, size = 1)
      ),
      actionButton(
        'visualize',
        'Click to Visualize',
        width = '100%',
        style = 'margin: auto;',
        icon = icon('bar-chart')
      )
    )
  )
)

ui <- dashboardPage(
  header = dashboard_header,
  sidebar = dashboard_sidebar,
  body = dashboard_body
)

server <- function(input, output) {
  
  showModal(
    modalDialog(
      title = "Welcome to the Breast Cancer Symptom Explorer!",
      'This tool allows you to view different trajectories for common symptoms of
      varying severity levels experienced by breast cancer patients treated with either tamoxifen or
      anastrozole over a 5 year time period. By selecting the specific symptom type, treatment,
      and severity you can visualize different scenarios across different points of time and
      how they may relate to you or someone you know.',
      easyClose = TRUE,
      footer = NULL,
      size = 'l'
    )
  )
  
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

  box_results <- eventReactive(input$visualize, {
    value_box_results(cohort())
  })
  
  output$box_total_responded <- renderInfoBox({
    
    val <- extract_total_responded(box_results()$total_responded)
    
    infoBox(value = val, title = 'Total Patients Responded', icon = icon('chart-bar'))
    
  })
  
  output$box_not_at_all <- renderValueBox({
    
    val <- extract_box_results(box_results()$total_responded, box_response = 'Not at all')
    
    valueBox(value = val, subtitle = 'Not at all', color = 'lightblue')
    
  })
  
  output$box_slightly <- renderValueBox({
    
    val <- extract_box_results(box_results()$total_responded, box_response ='Slightly')
    
    valueBox(value = val, subtitle = 'Slightly', color = 'olive')
    
  })
  
  output$box_moderately <- renderValueBox({
    
    val <- extract_box_results(box_results()$total_responded, box_response ='Moderately')
    
    valueBox(value = val, subtitle = 'Moderately', color = 'warning')
    
  })

  output$box_quiteabit <- renderValueBox({
    
    val <- extract_box_results(box_results()$total_responded, box_response ='Quite a bit')
    
    valueBox(value = val, subtitle = 'Quite a bit', color = 'orange')
    
  })

  output$box_extremely <- renderValueBox({
    
    val <- extract_box_results(box_results()$total_responded, box_response ='Extremely')
    
    valueBox(value = val, subtitle = 'Extremely', color = 'danger')
    
  })
  
  output$box_symptom <- renderInfoBox({
    infoBox(title = 'Selected Symptom', value = selected_symptom(), icon = icon('stethoscope'),
            color = 'success')
  })
  
  output$box_total_patients <- renderInfoBox({
    
    val <- c(box_results()$total_responded$n, box_results()$total_not_responded$n) %>% 
      sum()
    
    infoBox(title = 'Initial Patient Cohort', value = val, icon = icon('female'),
            color = 'success')
  })
  
  output$box_severity <- renderInfoBox({
    
    val <- selected_severity()
    
    infoBox(title = 'Severity', value = val, icon = icon('bar-chart'),
            color = 'success')
  })
  
}

shinyApp(ui, server)























