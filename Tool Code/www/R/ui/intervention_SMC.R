div(class = 'interventions',
    fluidRow(
      column(width = 8,
             materialSwitch(inputId = "smcsc_switch", label = 'Seasonal Malaria Chemoprevention', value = FALSE,
                            status = "info", right = TRUE, inline = FALSE, width = NULL)
      ),
      column(width = 4,
             conditionalPanel(condition = "input.smcsc_switch",
                              dropdownButton(label = "Settings", circle = FALSE, status = "primary", size = 'sm', icon = icon("gear"), 
                                             width = "700px", tooltip = FALSE, right = TRUE,
                                             fluidRow(
                                               column(4,
                                                      sliderInputReg(inputId="cppsmcsc", label = "Cost per person enrolled in SMC", value = c(0, 1), min=0, max=20,step=0.01, post = ' US$'),
                                                      sliderInputReg(inputId="cpdrug_smcsc", label = "Cost per drug for SMC", value = c(0.75, 1.25), min=0, max=20,step=0.01, post = ' US$')
                                               ),
                                               column(4,
                                                      h4("Scale up/down (1)"),
                                                      sliderInputReg(inputId="yr_smc_sc1", label = "Year to start scale up/down (1)", value = 2019, min=2019, max=2030,step=0.5, sep=NULL),
                                                      sliderInputReg(inputId="smc_covsc1", label = "New coverage of SMC programme (1)", value = 20, min=0, max=100,step=1, post = " %")
                                               ),
                                               column(4,
                                                      h4("Scale up/down (2)"),
                                                      sliderInputReg(inputId="yr_smc_sc2", label = "Year to start scale up/down (2)", value = 2025, min=2019, max=2030,step=1, sep=NULL),
                                                      sliderInputReg(inputId="smc_covsc2", label = "New coverage of SMC programme (2)", value = 40, min=0, max=100,step=1)
                                               )
                                             )
                              )
             )
      )
    ),
    tags$small("Seasonal Malaria Chemoprevention")
)