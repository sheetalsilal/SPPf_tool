output$delete_packages <- renderUI({
  if("Baseline" %in% input$rank_list_hide) return(HTML("To delete packages, remove the baseline from the hidden section."))
  
  if(! is_empty(input$rank_list_hide)) actionLink("confirm_delete_packages", label = span(icon("trash-alt"), " Delete packages that are hidden."))
})