

# data <- read_csv(here('data/shiny_qol_data.csv'), show_col_types = F)

select_cohort <- \(data, selected_treatment, selected_symptom, selected_timepoints, selected_response) {
  data %>%
    filter(trt == selected_treatment) %>%
    filter(symptom_labels == selected_symptom) %>%
    filter(time_point >= selected_timepoints[[1]] & time_point <= selected_timepoints[[2]]) %>% 
    group_by(patientid) %>%
    filter(first(time_point == selected_timepoints[[1]]) &
             first(response) == selected_response) %>%
    ungroup()
  
}


# select_cohort(data, 'Anastrozole', 'Hot Flashes', c(6, 24), 'Moderately')
