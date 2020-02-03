output$plot_baseline_pf <- renderPlot({
  req(simul_baseline$baseline_available)
  
  simul_baseline$results_epi %>%
    filter(year >= 2010, year <= 2018) %>%
    select(times, API, clinmonthf, trtf) %>%
    ggplot(aes(x = times)) +
    geom_line(aes(y = API, color = "API (Inputs)")) + # geom_point(aes(y = API, color = "API (Inputs)")) +
    geom_line(aes(y = clinmonthf, color = "Clinical Incidence (Simul)")) + geom_point(aes(y = clinmonthf, color = "Clinical Incidence (Simul)")) +
    geom_line(aes(y = trtf, color = "Reported Incidence (Simul)")) + geom_point(aes(y = trtf, color = "Reported Incidence (Simul)")) +
    geom_segment(aes(xend = times, yend = trtf, y = API, color = "Residuals"), alpha = 0.8) +
    scale_x_continuous(limits = c(2012, NA), breaks = seq(2012, 2030, 2)) +
    labs(x= NULL, y = "Cases", title = NULL) +
    scale_colour_manual(name = NULL, values = c(`Reported Incidence (Simul)` = "firebrick", `Clinical Incidence (Simul)` = "dodgerblue3", 
                                                `Residuals` = "darkgrey", `API (Inputs)` = "orange")) + 
    theme_minimal() + theme(text = element_text(size = 16), legend.position = "top")
})