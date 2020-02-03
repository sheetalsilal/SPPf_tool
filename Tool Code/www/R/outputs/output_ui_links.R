output$links <- renderUI({
  tagList(
    conditionalPanel(condition = "input.tabs == 'welcome'",
                     fileInput("file_RData", label = NULL, accept = ".RData", buttonLabel = span(icon("upload"), "Proj. Dataset")) %>% 
                       helper(content = "welcome_upload_data", colour = "red")
    ),
    conditionalPanel(condition = "input.tabs == 'baseline' | input.tabs == 'packages'",
                       fileInput("file_parameters", label = NULL, multiple = TRUE, accept = ".xlsx", buttonLabel = span(icon("upload"), "Parameters")) %>% 
                       helper(content = "across_upload_parameters", colour = "red")
    ),
    conditionalPanel(condition = "input.tabs == 'tab_explore'",
                     br(), downloadLink('go_download', 'Download Results'), br(), p("(does not include sensitivity analysis data)")
    ),
    conditionalPanel(condition = "input.tabs == 'tab_sensitivity_analysis' & output.exist_sa",
                     br(), downloadLink('go_download_sa', 'Download Sensitivity Analysis Results'), br(), p("(does not include other simulation data)")
    )
  )
})