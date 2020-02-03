output$sa_plot_treated <- renderPlot({
  input$go_sa
  req(simul_sa$simul)
  
  dta <- simul_sa$simul %>%
    filter(year >= 2016, year < 2030) %>%
    group_by(package, year, run_nb) %>%
    summarise_if(is.numeric, sum) %>%
    ungroup()
  
  ggplot(dta, aes(x = year, y = trtf, color = package, group = interaction(run_nb, package))) +
    geom_line() + 
    ylim(c(0, NA)) +
    theme_light() +
    theme(legend.position = "bottom", legend.title = element_blank(), text = element_text(size = 13)) + 
    labs(x = "Year", y = "Cases", title = "Treated Cases", subtitle = "One line/run. Lines may be superimposed.")
})