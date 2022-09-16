

make_sankey_diagram <- \(data) {
  
  plot_data <- data %>% 
    pivot_wider(
      names_from = time_point,
      values_from = response
    ) %>% 
    make_long(., 6:ncol(.)) %>% 
    mutate(
    node = factor(
      node,
      levels = c(
        'No Response',
        'Not at all',
        'Slightly',
        'Moderately',
        'Quite a bit',
        'Extremely',
        'Off trial'
      )
    )
  )
  
  manual_colors <- c('No Response' = "#808080",
                     'Not at all' = "#3d9970", 
                     'Slightly' = "#007bff", 
                     'Moderately' = "#ffc107", 
                     'Quite a bit' = "#ff851b", 
                     'Extremely' = "#dc3545", 
                     'Off trial' = "#000000")
  
  ggplot(plot_data,
         aes(
           x = x,
           next_x = next_x,
           node = node,
           next_node = next_node,
           fill = factor(node)
         )) +
    geom_sankey(flow.alpha = .3) +
    theme_sankey(base_size = 20) +
    theme(
      legend.position = 'right',
      legend.title = element_blank(),
      axis.text.x = element_text(face = 'bold'),
      axis.title = element_blank(),
      legend.text = element_text(size = 20)
    ) +
    scale_color_manual(values = manual_colors) +
    scale_fill_manual(values = manual_colors) +
    guides(color = guide_legend(reverse = T),
           fill = guide_legend(reverse = T))
  
}

