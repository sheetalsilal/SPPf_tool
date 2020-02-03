div(class = 'interventions',
    fluidRow(
      column(width = 8,
             materialSwitch(inputId = "itnsc_switch", label = 'ITN Programme', value = FALSE,
                            status = "info", right = TRUE, inline = FALSE, width = NULL)
      ),
      column(width = 4,
             conditionalPanel(condition = "input.itnsc_switch",
                              dropdownButton(label = "Settings", circle = FALSE, status = "primary", size = 'sm', icon = icon("gear"), 
                                             width = "700px", margin = "15px", right = FALSE,
                                             includeMarkdown('www/markdown/intervention_ITN.md'),
                                             fluidRow(
                                               column(4,
                                                      sliderInputReg(inputId="cpitn2", label = "Cost per person protected by ITN", value = c(5, 8), min=0, max=20,step=0.1, post = "US$"),
                                                      sliderInputReg(inputId="cppiec2", label = "IEC cost for Hang-up Campaign", value = c(0.8, 1.2), min=0, max=10, step=.05, sep=NULL, post = "US$") 
                                               ),
                                               column(4,
                                                      h4("Scale up/down (1)"),
                                                      sliderInputReg(inputId="itnsc_start1", label = "Year to Start (1)", value = 2019, min=2019, max=2030,step=0.5, sep=NULL),
                                                      sliderInputReg(inputId="yrs_itnsc1", label = "Nb Years to Scale (1)", value = 1, min=0, max=5, step=0.25, sep=NULL, post = " year(s)"),
                                                      sliderInputReg(inputId="itnsc_cov1", label = "New Coverage (1)", value = 99, min=0, max=100,step=1, post = " %"),
                                                      sliderInputReg(inputId="itnsc_eff1", label = "New Effectiveness (usage and efficacy) (1)", value = 24, min=0, max=100,step=1, post = " %")
                                               ),
                                               column(4,
                                                      h4("Scale up/down (2)"),
                                                      sliderInputReg(inputId="itnsc_start2", label = "Year to Start (2)", value = 2025, min=2019, max=2030,step=1, sep=NULL),
                                                      sliderInputReg(inputId="yrs_itnsc2", label = "Nb Years to Scale (2)", value = 1, min=0, max=5, step=0.25, sep=NULL, post = " year(s)"),
                                                      sliderInputReg(inputId="itnsc_cov2", label = "New Coverage (2)", value = 99, min=0, max=100,step=1,  post = " %"),
                                                      sliderInputReg(inputId="itnsc_eff2", label = "New Effectiveness (usage and efficacy) (2)", value = 24, min=0, max=100,step=1, post = " %")
                                               )
                                             )
                              )
             )
      )
    ),
    tags$small("Two opportunities to scale up/down ITN programme.")
)