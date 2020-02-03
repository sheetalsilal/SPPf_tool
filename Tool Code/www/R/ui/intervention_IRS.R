div(class = 'interventions',
    fluidRow(
      column(width = 8,
             materialSwitch(inputId = "irssc_switch", label = 'Indoor Residual Spraying', value = FALSE,
                            status = "info", right = TRUE, inline = FALSE, width = NULL)
      ),
      column(width = 4,
             conditionalPanel(condition = "input.irssc_switch",
                              dropdownButton(label = "Settings", circle = FALSE, status = "primary", size = 'sm', icon = icon("gear"), 
                                             width = "700px", margin = "15px", right = FALSE,
                                             includeMarkdown('www/markdown/intervention_IRS.md'),
                                             fluidRow(
                                               column(4,
                                                      sliderInputReg(inputId="cppirs2", label = "Cost per person protected by IRS", value = c(5, 6), min=0, max=20,step=0.1, post = " US$")
                                               ),
                                               column(4,
                                                      h4("Scale up/down (1)"),
                                                      sliderInputReg(inputId="irssc_start1", label = "Year to start (1)", value = 2019, min=2019, max=2030,step=1, sep=NULL),
                                                      sliderInputReg(inputId="yrs_irssc1", label = "Nb Years to scale (1)", value = 1, min=.25, max=3, step=.25, sep=NULL, post = " year(s)"),
                                                      sliderInputReg(inputId="irssc_cov1", label = "New Coverage (1)", value = 40, min=0, max=100,step=5, post = " %"),
                                                      sliderInputReg(inputId="irssc_eff1", label = "Reduction in biting rate due to IRS (1)", value = 40, min=0, max=100,step=1, post = " %")
                                               ),
                                               column(4,
                                                      h4("Scale up/down (2)"),
                                                      sliderInputReg(inputId="irssc_start2", label = "Year to start (2)", value = 2025, min=2019, max=2030,step=1, sep=NULL),
                                                      sliderInputReg(inputId="yrs_irssc2", label = "Nb Years to scale (2)", value = 1, min=.25, max=3, step=.25, sep=NULL, post = " year(s)"),
                                                      sliderInputReg(inputId="irssc_cov2", label = "New Coverage (2)", value = 40, min=0, max=100,step=5, post = " %"),
                                                      sliderInputReg(inputId="irssc_eff2", label = "Reduction in biting rate due to IRS (2)", value = 40, min=0, max=100,step=1, post = " %")
                                               )
                                             )
                              )
             )
      )
    ),
    tags$small("Two opportunities to increase/decrease the IRS programme.")
)