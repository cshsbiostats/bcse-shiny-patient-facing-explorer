



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
    mutate(label = glue::glue('{n} ({scales::label_percent(accuracy = .1)(prop)})')) %>% 
    complete(response = c('Not at all', 'Slightly', 'Quite a bit', 'Moderately', 'Extremely'), fill = list(label = '0 (0.0%)', n = 0))
  
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

