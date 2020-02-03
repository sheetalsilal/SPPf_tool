output$select_packages <- renderUI({
  bucket_list(
    header = "Display/Hide",
    group_name = "bucket_list_group",
    orientation = "horizontal",
    add_rank_list(
      text = span(icon("eye"), "Drag here to display Baseline/Packages"),
      labels = c("Baseline", as.list(simul_packages$names)),
      input_id = "rank_list_show"
    ),
    add_rank_list(
      text = span(icon("eye-slash"), "Drag here to hide Baseline/Packages"),
      labels = NULL,
      input_id = "rank_list_hide"
    )
  )
})