div(class = 'interventions',
    fluidRow(
      column(width = 8,
             materialSwitch(inputId = "vmwsc_switch", label = 'CHW Programme', value = FALSE,
                            status = "info", right = TRUE, inline = FALSE, width = NULL)
      ),
      column(width = 4,
             conditionalPanel(condition = "input.vmwsc_switch",
                              dropdownButton(label = "Settings", circle = FALSE, status = "primary", size = "sm", icon = icon("gear"), tooltip = FALSE,
                                             width = "700px", margin = "15px", right = FALSE,
                                             includeMarkdown('www/markdown/intervention_CHW.md'),
                                             fluidRow(
                                               column(4,
                                                      h4("Cost"),
                                                      sliderInputReg(inputId="cpvmwsc", label = "Average Cost per CHW per Year", value = c(750, 1250), min=0, max=3500, step=50, post = " US$")
                                                      
                                               ),
                                               column(4,
                                                      h4("Scale up/down (1)"),
                                                      sliderInputReg(inputId="vmwsc_start1", label = "Year to Start (1)", value = 2019, min=2019, max=2030,step=1, sep=NULL),
                                                      sliderInputReg(inputId="yrs_vmwsc1", label = "Nb Years to Scale (1) ", value = 1, min=.25, max=3, step=.25, sep=NULL, post = " year(s)"),
                                                      sliderInputReg(inputId="vmwsc_cov1", label = "New Coverage (1)", value = 70, min=0, max=100,step=1, post = " %")
                                                ),
                                               column(4,
                                                      h4("Scale up/down (2)"),
                                                      sliderInputReg(inputId="vmwsc_start2", label = "Year to Start (2)", value = 2025, min=2019, max=2030,step=1, sep=NULL),
                                                      sliderInputReg(inputId="yrs_vmwsc2", label = "Nb Years to Scale(2) ", value = 1, min=.25, max=3, step=.25, sep=NULL, post = " year(s)"),
                                                      sliderInputReg(inputId="vmwsc_cov2", label = "New Coverage (2)", value = 70, min=0, max=100,step=1, post = " %")
                                               )
                                             )
                              )
             )
      )
    ),
    tags$small("Two opportunities to increase/decrease the coverage of the CHW programme.")
)