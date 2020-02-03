output$explore_incidence_pf <- renderHighchart({
  input$go_baseline
  req(input$rank_list_show)
  
  dta <- simul_results() %>% 
    filter(year >= 2016, year < 2030, package %in% input$rank_list_show) %>%
    group_by(package, year) %>%
    summarise(reported = sum(reported), trtf = sum(trtf)) %>%
    ungroup()
  

    highchart() %>%
      hc_add_series(dta, type = 'arearange', hcaes(x = year, low = reported, high = trtf, group = package)) %>%
      hc_tooltip(pointFormat = "<b>{point.package}</b><br>
                 Treated: {point.trtf:.1f} <br>
                 Reported: {point.reported:.1f}") %>%
      hc_yAxis(min = 0) %>%
      hc_add_theme(hc_theme_elementary()) %>%
      hc_exporting(enabled = TRUE)
})