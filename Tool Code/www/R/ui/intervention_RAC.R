div(class = 'interventions',
  fluidRow(
    column(width = 8,
           materialSwitch(inputId = "prim_switch", label = 'Radical Cure & G6PD Testing', value = FALSE,
                          status = "info", right = TRUE, inline = FALSE, width = NULL)
    ),
    column(width = 4,
           conditionalPanel(condition = "input.prim_switch",
                            dropdownButton(label = "Settings", circle = FALSE, status = "primary", size = 'sm', icon = icon("gear"), 
                                           width = "500px", margin = "15px", right = TRUE,
                                           fluidRow(
                                             column(6,
                                                    sliderInputReg(inputId="yr_primstart", label = "Year to Start", value = 2019, min=2010, max=2030, step=1, sep=NULL),
                                                    sliderInputReg(inputId="sensgd", label = "Sensitivity of G6PD test", value = 90, min=0, max=100,step=5, post = " %"),
                                                    sliderInputReg(inputId="cpdrugprim", label = "Average cost of drug for radical cure", value = c(0.3, 0.8), min=0, max=10,step=0.25, post = " US$"),
                                                    sliderInputReg(inputId="cpg6test", label = "Average cost per G6PD test", value = c(5, 9), min=0, max=15, step=0.25, post = " US$")
                                             ),
                                             column(6,
                                                    sliderInputReg(inputId="vphprim", label = "Probability of relapse under radical cure", value = 3.5, min=0, max=100, step=0.5, post = " %"),
                                                    sliderInputReg(inputId="vnup", label = "Course of treatment (days)", value = 14, min=0, max=20, step=0.5, post = " day(s)"),
                                                    sliderInputReg(inputId="vptfp", label = "Probability of treatment failure (due to clinical failure and lack of adherence)", value = 23.5, min=0, max=100, step=0.5, post = " %")
                                             )
                                           )
                            )
           )
    )
  ),
  tags$small("TODO: fill placeholder for short explanations on the Radical Cure & G6PD Testing strategy.")
)