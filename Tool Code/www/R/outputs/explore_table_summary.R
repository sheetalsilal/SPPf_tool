output$explore_table_summary <- renderDT({
  dta_1 <- simul_results() %>% 
    filter(year >= 2016, year < 2030, package %in% input$rank_list_show) %>%
    group_by(package, year) %>%
    summarise(`Reported Cases` = round(sum(reported), 1), 
              `Treated Cases` = round(sum(trtf), 1), 
              `Clinical Cases` = round(sum(clinmonthf), 1)) %>%
    ungroup()
  
  dta_2 <- simul_costs() %>% 
    filter(year >= 2016, year < 2030, package %in% input$rank_list_show, what == "Median") %>%
    transmute(year, cost_total = cost_itn + cost_irs + cost_inpatient + cost_outpatient + 
                cost_vmw + cost_other + cost_acd + cost_ms, package) %>%
    group_by(package, year) %>%
    summarise(`Median Total Cost` = ceiling(sum(cost_total))) %>%
    ungroup()
  
  dta <- left_join(dta_1, dta_2, by = c("package", "year")) %>%
    rename(Package = package, Year = year)
  
  
  datatable(dta, rownames = FALSE, style = "bootstrap", filter = "top", 
            options = list(scrollX = TRUE,
                           scrollY = 260,
                           paging = FALSE)) %>%
    formatCurrency("Median Total Cost", currency = "US$ ", digits = 0)
  
})