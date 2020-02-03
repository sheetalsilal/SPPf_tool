div(class = 'interventions',
    fluidRow(
      column(width = 8,
             materialSwitch(inputId = "mda_switch", label = 'Mass Drug Administration (Not Yet Implemented)', value = FALSE,
                            status = "info", right = TRUE, inline = FALSE, width = NULL)
      ),
      column(width = 4,
             conditionalPanel(condition = "input.mda_switch & ! input.mda_switch",
                              dropdownButton(label = "Settings", circle = FALSE, status = "primary", size = 'sm', icon = icon("gear"), width = "300px", tooltip = FALSE,
                                             fluidRow(
                                               column(12,
                                                      sliderInput(inputId="minc", label = "average annual incidence in endemic villages", value = 1, min=.25, max=10, step=.25, sep=NULL), #.25 timesteps
                                                      sliderInput(inputId="sdinc", label = "std deviation of annual incidence in endemic villages", value = 0.5, min=0, max=2, step=.25, sep=NULL), #.25 timesteps
                                                      sliderInput(inputId="thresh_inc", label = "incidence threshold to apply ACD", value = 2, min=0, max=10, step=.25, sep=NULL) #.25 timesteps
                                               )
                                             )
                              )
             )
      )
    ),
    tags$small('(Not Yet Implemented)')
)