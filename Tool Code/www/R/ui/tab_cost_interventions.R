tagList(
  br(),
  bs_accordion_sidebar(id = "inputs_costs",
                       spec_side = c(width = 4, offset = 0),
                       spec_main = c(width = 8, offset = 0)) %>%
    bs_set_opts(panel_type_active = "primary", panel_type_inactive = "default",
                use_main_enclosure = TRUE) %>%
    bs_append(
      title_side = "CHWs Programme Costs",
      content_side = NULL,
      content_main = tagList(
        sliderInputReg(inputId="cpvmw", label = "Average cost per CHW per year", value = c(1000, 1200), min=0, max=3500, step=50, post = ' US$'),
        sliderInputReg(inputId="cprdt", label = "Unit cost RDT ", value = c(0.75, 1.25), min=0, max=10,step=0.01, post = ' US$'),
        sliderInputReg(inputId="cpmic", label = "Unit cost slide", value = c(0.54, 0.9), min=0, max=10, step=0.01, post = ' US$')
      )) %>%
    bs_append(
      title_side = "ITN & IRS",
      content_side = NULL,
      content_main = tagList(
        sliderInputReg(inputId="cpitn", label = "Cost per ITN distributed", value = c(5.35, 8.92), min=0, max=20,step=0.01, post = ' US$'),
        sliderInputReg(inputId="cppirs", label = "Cost per person protected by IRS", value = c(4.07, 6.79), min=0, max=20,step=0.01, post = ' US$')
      )) %>%
    bs_append(
      title_side = "Treatment Costs",
      content_side = NULL,
      content_main = tagList(
        sliderInputReg(inputId="cpdrugfov", label = "Average cost of drug per Pf case", value=c(1.40, 2.33), min=0, max=20, step=0.01, post = ' US$'),
        sliderInputReg(inputId="cpdrugi", label = "Inpatients: average cost of drug per case", value=c(22.40, 37.33), min=0, max=100, step=0.01, post = ' US$'),
        sliderInputReg(inputId="cptrti", label = "Inpatients: average cost of treatment per case", value = c(82.50, 137.50), min=0, max=100,step=0.01, post = ' US$'),
        sliderInputReg(inputId="cptrto", label = "Outpatients: average cost of treatment per case", value = c(10.50, 17.50), min=0, max=20,step=0.01, post = ' US$')
      )) %>%
    bs_append(
      title_side = "Prophylaxis",
      content_side = NULL,
      content_main = tagList(
        sliderInputReg(inputId="cppsmc", label = "Cost per person enrolled in SMC", value = c(0, 0), min=0, max=20,step=0.01, post = ' US$'),
        sliderInputReg(inputId="cpdrug_smc", label = "Cost per drug for SMC", value = c(0.72, 1.21), min=0, max=20,step=0.01, post = ' US$'),
        sliderInputReg(inputId="cppipt", label = "Cost per person enrolled in IPTp", value = c(0, 0), min=0, max=20,step=0.01, post = ' US$'),
        sliderInputReg(inputId="cpdrug_iptp", label = "Cost per drug for IPTp", value = c(0.78, 1.29), min=0, max=20,step=0.01, post = ' US$')
      )) %>%
    bs_append(
      title_side = "Miscellaneous",
      content_side = NULL,
      content_main = tagList(
        sliderInputReg(inputId="cppsurv", label = "Average cost of surveillance per capita", value = c(0, 0), min=0, max=10,step=0.01, post = ' US$'),
        sliderInputReg(inputId="cpptrain", label = "Average cost of training per capita", value = c(0, 0), min=0, max=10,step=0.01, post = ' US$'),
        sliderInputReg(inputId="cppiec", label = "Average cost of Information/Education/Communication per capita", value = c(0.08, 0.13), min=0, max=10,step=0.01, post = ' US$'),
        sliderInputReg(inputId = "d", label = "Annual discount rate (from XXXX onwards)", value = 3, min = 0, max = 10, step = 0.5, post = '%'),
        
        plotOutput('plot_discount', height = '150px')
      ))
)