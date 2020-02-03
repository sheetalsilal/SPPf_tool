output$sa_text_runtime <- renderText({
  
  run_time <- 0
  if ("Baseline" %in% input$select_simul_sa)  run_time <- run_time + simul_baseline$run_time
  for (pack in setdiff(input$select_simul_sa, "Baseline"))  run_time <- run_time + simul_packages$simul[[which(simul_packages$names == pack)]]$run_time
  
  paste0("Estimated running time of ", floor(run_time*input$nb_run_sa), " seconds.")
})