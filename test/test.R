

temp <- select_cohort(data, 'Anastrozole', 'Hot Flashes', c(0, 24), 'Moderately')

res <- value_box_results(data)

# debugonce(value_box_results)
extract_box_results(res$total_responded, 'Not at all')
