div(class = 'interventions',
    fluidRow(
      column(width = 8,
             materialSwitch(inputId = "trt_switch", label = 'Health System Strengthening', value = FALSE,
                            status = "info", right = TRUE, inline = FALSE, width = NULL)
      ),
      column(width = 4,
             conditionalPanel(condition = "input.trt_switch",
                              dropdownButton(label = "Settings", circle = FALSE, status = "primary", size = 'sm', icon = icon("gear"), tooltip = FALSE, 
                                             width = "700px", margin = "15px", right = FALSE,
                                             includeMarkdown('www/markdown/intervention_HSS.md'),
                                             fluidRow(
                                               column(4,
                                                      h4("Cost"),
                                                      sliderInputReg(inputId="cppmgt", label = "Training cost per capita to strengthen treatment", 
                                                                     value = c(0.12, 0.22), min=0, max=10, step = 0.01, sep = NULL, post = " US$"), 
                                                      sliderInputReg(inputId="cppiec3", label = "IEC cost per capita to improve treatment-seeking", 
                                                                     value = c(0.05, 0.08), min=0, max=10, step = 0.01, sep = NULL, post = " US$") 
                                               ),
                                               column(4,
                                                      h4("Scale up/down (1)"),
                                                      sliderInputReg(inputId="hsssc_start1", label = "Year to Start (1)", value = 2020, min=2019, max=2030,step=1, sep=NULL),
                                                      sliderInputReg(inputId="yrs_hsssc1", label = "Nb Years to Scale (1) ", value = 3, min=.25, max=3, step=.25, sep=NULL, post = " year(s)"),
                                                      
                                                      h4("Facility-based treatment"),
                                                      sliderInputReg(inputId="pseek1", label = "Probabilty of seeking treatment at health facility (1)", value = 73, min=0, max=100, step=1, post = "%"),
                                                      sliderInputReg(inputId="ptest1", label = "Probability of being tested at health facility (1)", value = 99, min=0, max=100, step=1, post = "%"),
                                                      sliderInputReg(inputId="ptreat1", label = "Probabilty of being treated after positive test (1)", value = 100, min=0, max=100,step=1, post = "%"),
                                                      
                                                      h4("Community-based treatment"),
                                                      sliderInputReg(inputId="pseekv1", label = "Probabilty of seeking treatment with CHW (1)", value = 0, min=0, max=100, step=1, post = "%"),
                                                      sliderInputReg(inputId="ptestv1", label = "Probability of being tested with CHW (1)", value = 75, min=0, max=100, step=1, post = "%"),
                                                      sliderInputReg(inputId="ptreatv1", label = "Probabilty of being treated after positive test (1)", value = 90, min=0, max=100,step=1, post = "%")
                                               ),
                                               column(4,
                                                      h4("Scale up/down (2)"),
                                                      sliderInputReg(inputId="hsssc_start2", label = "Year to Start (2)", value = 2023, min=2019, max=2030,step=1, sep=NULL),
                                                      sliderInputReg(inputId="yrs_hsssc2", label = "Nb Years to Scale (2) ", value = 1, min=.25, max=3, step=.25, sep=NULL, post = " year(s)"),
                                                      
                                                      h4("Facility-based treatment"),
                                                      sliderInputReg(inputId="pseek2", label = "Probabilty of seeking treatment at health facility (2)", value = 73, min=0, max=100, step=1, post = "%"),
                                                      sliderInputReg(inputId="ptest2", label = "Probability of being tested at health facility (2)", value = 99, min=0, max=100, step=1, post = "%"),
                                                      sliderInputReg(inputId="ptreat2", label = "Probabilty of being treated after positive test (2)", value = 100, min=0, max=100,step=1, post = "%"),
                                                      
                                                      h4("Community-based treatment"),
                                                      sliderInputReg(inputId="pseekv2", label = "Probabilty of seeking treatment with CHW (2)", value = 0, min=0, max=100, step=1, post = "%"),
                                                      sliderInputReg(inputId="ptestv2", label = "Probability of being tested with CHW (2)", value = 75, min=0, max=100, step=1, post = "%"),
                                                      sliderInputReg(inputId="ptreatv2", label = "Probabilty of being treated after positive test (2)", value = 90, min=0, max=100,step=1, post = "%")
                                                      
                                               )
                                             )
                              )
             )
      )
    ),
    tags$small("Two opportunities to increase/decrease access/effectiveness of the Health System.")
)
