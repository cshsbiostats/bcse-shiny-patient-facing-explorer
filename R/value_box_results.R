

# temp <- select_cohort(data, 'Anastrozole', 'Hot Flashes', c(0, 24), 'Moderately')

value_box_results <- \(data) {
  total_patients <- data %>%
    pull(patientid) %>%
    unique() %>%
    length()
  
  total_responded <- data %>%
    group_by(patientid) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    filter(!(response %in% c('No Response', 'Off trial'))) %>%
    count(response) %>%
    mutate(prop = n / total_patients) %>%
    mutate(label = glue::glue('{n} ({scales::label_percent(accuracy = .1)(prop)})'))
  
  total_not_responded <- data %>%
    group_by(patientid) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    filter((response %in% c('No Response', 'Off trial'))) %>%
    count(response) %>%
    mutate(prop = n / total_patients) %>%
    mutate(label = glue::glue('{n} ({scales::label_percent(accuracy = .1)(prop)})'))

  tibble::lst(total_responded, total_not_responded)
}

extract_box_results <- \(data, box_response) {

  data %>%
    filter(response == box_response) %>%
    pull(label)

}

extract_total_responded <- \(data) {
  data %>% pull(n) %>% sum()
}

construct_valuebox <- \(box_results, subtitle) {
  
  renderValueBox({
    
    val <- extract_box_results(box_results, box_response = subtitle)
    
    valueBox(value = val, subtitle = subtitle)
    
  })
  
}

construct_infobox <- \(box_results, title) {
  
  renderInfoBox({
    
    val <- extract_box_results(box_results, box_response = title)
    
    infoBox(title = title, value = val)
    
  })
  
}

# res <- value_box_results(data)

# extract_box_results(res$total_responded, 'Not at all')
