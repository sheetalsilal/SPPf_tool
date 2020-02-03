div(class = 'interventions',
    fluidRow(
      column(width = 8,
             materialSwitch(inputId = "fail_switch", label = 'Treatment Failure Exploration', value = FALSE,
                            status = "info", right = TRUE, inline = FALSE, width = NULL)
      ),
  column(width = 4,
         conditionalPanel(condition = "input.fail_switch",
                          dropdownButton(label = "Settings", circle = FALSE, status = "primary", size = 'sm', icon = icon("gear"), width = "700px", tooltip = FALSE, right = TRUE,
                                         fluidRow(
                                           column(3,
                                                  numericInput(inputId="ptf08", label = "2008 (%)", value=5, min=0, max=100, step=5),
                                                  numericInput(inputId="ptf09", label = "2009 (%)", value=5, min=0, max=100, step=5),
                                                  numericInput(inputId="ptf10", label = "2010 (%)", value=5, min=0, max=100, step=5),
                                                  numericInput(inputId="ptf11", label = "2011 (%)", value=5, min=0, max=100, step=5)
                                           ),
                                           column(3,
                                                  numericInput(inputId="ptf12", label = "2012 (%)", value=5, min=0, max=100, step=5),
                                                  numericInput(inputId="ptf13", label = "2013 (%)", value=5, min=0, max=100, step=5),
                                                  numericInput(inputId="ptf14", label = "2014 (%)", value=5, min=0, max=100, step=5),
                                                  numericInput(inputId="ptf15", label = "2015 (%)", value=5, min=0, max=100, step=5)
                                           ),
                                           column(3,
                                                  numericInput(inputId="ptf16", label = "2016 (%)", value=5, min=0, max=100, step=5),
                                                  numericInput(inputId="ptf17", label = "2017 (%)", value=5, min=0, max=100, step=5),
                                                  numericInput(inputId="ptf18", label = "2018-2019 (%)", value=5, min=0, max=100, step=5),
                                                  numericInput(inputId="ptf20", label = "2020-2021 (%)", value=5, min=0, max=100, step=5)
                                                  
                                           ),
                                           column(3,
                                                  numericInput(inputId="ptf22", label = "2022-2023 (%)", value=5, min=0, max=100, step=5),
                                                  numericInput(inputId="ptf24", label = "2024-2025 (%)", value=5, min=0, max=100, step=5),
                                                  numericInput(inputId="ptf26", label = "2026-2027 (%)", value=5, min=0, max=100, step=5),
                                                  numericInput(inputId="ptf28", label = "2028-2029 (%)", value=5, min=0, max=100, step=5)
                                                  
                                           )
                          )
         )
  )
)
),
tags$small("TODO: Clarify what the % refer to")
)