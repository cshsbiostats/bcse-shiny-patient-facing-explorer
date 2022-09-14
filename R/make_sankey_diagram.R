

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
  
  manual_colors <- c('No Response' = "#1F77B4FF",
                     'Not at all' = "#FF7F0EFF", 
                     'Slightly' = "#2CA02CFF", 
                     'Moderately' = "#D62728FF", 
                     'Quite a bit' = "#9467BDFF", 
                     'Extremely' = "#8C564BFF", 
                     'Off trial' = "#E377C2FF")
  
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

