output$plot_discount <- renderPlot({
  ggplot(simul_baseline$baseline_discount, aes(x = year, y = discount)) +
    geom_point() +
    scale_x_continuous(breaks = seq(2017, 2030, 2)) +
    scale_y_continuous(labels = scales::percent, limits=c(0, 1)) +
    labs(x = NULL, y = NULL) +
    theme_minimal() + 
    theme(text = element_text(size = 15))
})