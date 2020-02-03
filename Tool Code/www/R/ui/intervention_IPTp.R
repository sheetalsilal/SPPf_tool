div(class = 'interventions',
    fluidRow(
      column(width = 8,
             materialSwitch(inputId = "iptpsc_switch", label = 'Intermittent Preventive Treatment for pregnant women', value = FALSE,
                            status = "info", right = TRUE, inline = FALSE, width = NULL)
      ),
      column(width = 4,
             conditionalPanel(condition = "input.iptpsc_switch",
                              dropdownButton(label = "Settings", circle = FALSE, status = "primary", size = 'sm', icon = icon("gear"), 
                                             width = "700px", tooltip = FALSE, right = TRUE,
                                             fluidRow(
                                               column(4,
                                                      sliderInputReg(inputId="cppiptsc", label = "Cost per person enrolled in IPTp", value = c(0, 1), min=0, max=20,step=0.01, post = ' US$'),
                                                      sliderInputReg(inputId="cpdrug_iptpsc", label = "Cost per drug for IPTp", value = c(0.75, 1.25), min=0, max=20,step=0.01, post = ' US$')
                                               ),
                                               column(4,
                                                      h4("Scale up/down (1)"),
                                                      sliderInputReg(inputId="yr_iptp_sc1", label = "Year to start scale up/down (1)", value = 2019, min=2019, max=2030,step=0.5, sep=NULL),
                                                      sliderInputReg(inputId="iptp1_covsc1", label = "Target IPTp 1 coverage (1)", value = 90, min=0, max=100,step=1, post = " %"),
                                                      sliderInputReg(inputId="iptp2_covsc1", label = "Target IPTp 2 coverage (1)", value = 85, min=0, max=100,step=1, post = " %"),
                                                      sliderInputReg(inputId="iptp3_covsc1", label = "Target IPTp 3 coverage (1)", value = 80, min=0, max=100,step=1, post = " %"),
                                                      sliderInputReg(inputId="iptp4_covsc1", label = "Target IPTp 4 coverage (1)", value = 26, min=0, max=100,step=1, post = " %"),
                                                      sliderInputReg(inputId="iptp5_covsc1", label = "Target IPTp 5 coverage (1)", value = 11, min=0, max=100,step=1, post = " %")
                                                      
                                               ),
                                               column(4,
                                                      h4("Scale up/down (2)"),
                                                      sliderInputReg(inputId="yr_iptp_sc2", label = "Year to start scale up/down (2)", value = 2025, min=2019, max=2030,step=0.5, sep=NULL),
                                                      sliderInputReg(inputId="iptp1_covsc2", label = "Target IPTp 1 coverage (2)", value = 90, min=0, max=100,step=1, post = " %"),
                                                      sliderInputReg(inputId="iptp2_covsc2", label = "Target IPTp 2 coverage (2)", value = 85, min=0, max=100,step=1, post = " %"),
                                                      sliderInputReg(inputId="iptp3_covsc2", label = "Target IPTp 3 coverage (2)", value = 80, min=0, max=100,step=1, post = " %"),
                                                      sliderInputReg(inputId="iptp4_covsc2", label = "Target IPTp 4 coverage (2)", value = 26, min=0, max=100,step=1, post = " %"),
                                                      sliderInputReg(inputId="iptp5_covsc2", label = "Target IPTp 5 coverage (2)", value = 11, min=0, max=100,step=1, post = " %")
                                               )
                                             )
                              )
             )
      )
    ),
    tags$small("Aimed at treating and preventing malaria episodes in pregnent women.")
)