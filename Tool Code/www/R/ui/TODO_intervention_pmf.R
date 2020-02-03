div(class = 'interventions',
    fluidRow(
      column(width = 8,
             materialSwitch(inputId = "pmf_switch", label = 'Info Mobile & FG', value = FALSE,
                            status = "info", right = TRUE, inline = FALSE, width = NULL)
      ),
      column(width = 4,
             conditionalPanel(condition = "input.pmf_switch",
                              dropdownButton(label = "Settings", circle = FALSE, status = "primary", size = 'sm', icon = icon("gear"), width = "300px", tooltip = FALSE, right = TRUE,
                                             fluidRow(
                                               column(12,
                                                      HTML("Placeholder")
                                                      # sliderInput(inputId="minc", label = "average annual incidence in endemic villages", value = 1, min=.25, max=10, step=.25, sep=NULL) #.25 timesteps
                                               )
                                             )
                              )
             )
      )
    ),
    tags$small("Information and prevention for mobiles and forest goers")
)