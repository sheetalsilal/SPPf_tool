output$explore_costs <- renderHighchart({
  input$go_baseline
  req(input$rank_list_show)

dta <- simul_costs() %>%
  filter(year >= 2016, year < 2030, package %in% input$rank_list_show) %>%
  transmute(year, cost_total = cost_itn + cost_irs + cost_inpatient + cost_outpatient + 
              cost_vmw + cost_other + cost_acd + cost_ms, what, package) %>%
  group_by(year, package, what) %>%
  summarise(cost = sum(cost_total)) %>%
  ungroup() %>%
  pivot_wider(names_from = what, values_from = cost)

highchart() %>%
  hc_add_series(dta, type = 'arearange', hcaes(x = year, low = Minimum, high = Maximum, group = package)) %>%
  # hc_add_series(dta, type = 'line', hcaes(x = year, y = cost, group = package)) %>%
  hc_tooltip(pointFormat = "<b>{point.package}</b><br>
                 ({point.Minimum:.0f}  ; {point.Maximum:.0f}) USD") %>%
  hc_yAxis(min = 0) %>%
  hc_add_theme(hc_theme_elementary()) %>%
  hc_exporting(enabled = TRUE)
})