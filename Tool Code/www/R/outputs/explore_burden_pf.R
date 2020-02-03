output$explore_burden_pf <- renderHighchart({
  input$go_baseline
  req(input$rank_list_show)
  
  dta <- simul_results() %>% 
    filter(year >= 2016, year < 2030, package %in% input$rank_list_show) %>%
    group_by(package, year) %>%
    summarise(clinmonthf = sum(clinmonthf)) %>%
    ungroup()
  

    highchart() %>%
      hc_add_series(dta, type = 'line', hcaes(x = year, y = clinmonthf, group = package)) %>%
      hc_tooltip(pointFormat = "<b>{point.package}</b><br>
                 Clinical: {point.clinmonthf:.1f}") %>%
      hc_yAxis(min = 0) %>%
      hc_add_theme(hc_theme_elementary()) %>%
      hc_exporting(enabled = TRUE)
})