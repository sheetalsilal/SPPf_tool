output$plot_pf_sa <- renderPlot({
  req(simul_sa$simul)

  simul_sa$simul %>%
    mutate(run_nb = as.character(run_nb)) %>%
    filter(year >= 2016, year < 2030) %>%
    group_by(package, year, run_nb) %>%
    summarise_if(is.numeric, sum) %>%
    ungroup() %>%
    
    ggplot(aes_string(x = "year", y = input$var_sa, group = "run_nb")) +
    geom_line() + 
    geom_point() +
    ylim(c(0, NA)) +
    theme_light() +
    labs(x = "Year", y = NULL, title = "vmwf", subtitle = "One line per run") +
    facet_wrap(~ package)
})