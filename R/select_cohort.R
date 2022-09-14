

select_cohort <- \(data, selected_treatment, selected_symptom, selected_timepoints, selected_response) {
  data %>%
    filter(trt == selected_treatment) %>%
    filter(symptom_labels == selected_symptom) %>%
    group_by(patientid) %>%
    filter(first(time_point == selected_timepoints[[1]]) &
             first(response) == selected_response) %>%
    ungroup() %>%
    filter(time_point <= selected_timepoints[[2]])
  
}
