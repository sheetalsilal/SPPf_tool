tagList(
  br(),
  bs_accordion_sidebar(id = "inputs_programme_info",
                       spec_side = c(width = 4, offset = 0),
                       spec_main = c(width = 8, offset = 0)) %>%
    bs_set_opts(panel_type_active = "primary", panel_type_inactive = "default",
                use_main_enclosure = TRUE) %>%
    bs_append(
      title_side = "Population",
      content_side = NULL,
      content_main = tagList(
        numericInputReg(inputId="initP", label = "Population at risk (current)", value = 1000, min = 1000, max = 50*10^6),
        numericInputReg(inputId="pgrow", label = "Population annual growth rate (%)", value = 2.5, min = 0, max = 15),
        sliderInputReg(inputId="vilsize", label = "Average number of people per village", value = 1000, min=0, max=10000, step=10)
      )) %>%
    bs_append(
      title_side = "Annual Prevalence Index (API)",
      content_side = NULL,
      content_main = tagList(
        h4('Annual Prevalence Index for', em('Plasmodium falciparum'), 'cases'),
        sliderInputLeft(inputId="APIf12", label = "2012", value = 5, min=0, max=400,step=1, post = ' per 1,000'),
        sliderInputLeft(inputId="APIf13", label = "2013", value = 5, min=0, max=400,step=1, post = ' per 1,000'),
        sliderInputLeft(inputId="APIf14", label = "2014", value = 5, min=0, max=400,step=1, post = ' per 1,000'),
        sliderInputLeft(inputId="APIf15", label = "2015", value = 5, min=0, max=400,step=1, post = ' per 1,000'),
        sliderInputLeft(inputId="APIf16", label = "2016", value = 5, min=0, max=400,step=1, post = ' per 1,000'),
        sliderInputLeft(inputId="APIf17", label = "2017", value = 5, min=0, max=400,step=1, post = ' per 1,000'),
        sliderInputLeft(inputId="APIf18", label = "2018", value = 5, min=0, max=400,step=1, post = ' per 1,000')
      )) %>%
    bs_append(
      title_side = "Imported Cases",
      content_side = NULL,
      content_main = tagList(
        h4('Imported Cases'),
        sliderInputReg(inputId="muC", label = "Imported clinical cases per 1,000 population per year ", value = 0.1, min=0, max=5,step=0.1),
        sliderInputReg(inputId="muA", label = "Imported asymptomatic microscopically detectable carriers per 1,000 population per year ", value = 0.5, min=0, max=5,step=0.1),
        sliderInputReg(inputId="muU", label = "Imported asymptomatic microscopically UNdetectable carriers per 1,000 population per year ", value = 0.5, min=0, max=5,step=0.1)
      )) %>%
    bs_append(
      title_side = "Diagnosis & non-malaria",
      content_side = NULL,
      content_main = tagList(
        h4("Diagnosis & non-malaria"),
        sliderInputReg(inputId="prop_rdth", label = "HIS: percentage of cases diagnosed with RDT", value = 66, min=0, max=100, step=1, post = '%'),
        sliderInputReg(inputId="rdt_pos", label = "RDT positivity rate", value = 58, min=0, max=100, step=1, post = '%'),
        sliderInputReg(inputId="slide_pos", label = "Slide positivity rate", value = 45, min=0, max=100, step=1, post = '%')
      )) %>%
    bs_append(
      title_side = "Seasonality",
      content_side = NULL,
      content_main = tagList(
        h4('Seasonality'),
        sliderInputReg(inputId="amp", label = "Amplitude", value = 1, min=0, max=1, step=0.01),
        sliderInputReg(inputId="phi", label = "Peak month", value = 1, min=0, max=12, step=1),
        sliderInputReg(inputId="pe", label = "Pointedness", value = 1, min=1, max=50, step=1)
      )) %>%
    bs_append(
      title_side = "Health System",
      content_side = NULL,
      content_main = tagList(
        sliderInputLeft(inputId="pseek", label = "Probabilty of seeking treatment at health facility", value = 30, min=0, max=100, step=1, post = "%"),
        sliderInputLeft(inputId="ptest", label = "Probability of being tested at health facility", value = 75, min=0, max=100, step=1, post = "%"),
        sliderInputLeft(inputId="ptreat", label = "Probability of being treated after positive test", value = 90, min=0, max=100,step=1, post = "%"),
        sliderInputLeft(inputId="report", label = "Probability of cases reported", value = 90, min=0, max=100,step=1,  post = "%"),
        sliderInputLeft(inputId="reportdeath", label = "Probability of deaths reported", value = 50, min=0, max=100,step=1, post = "%")
      )) %>%
    bs_append(
      title_side = "Treatment Failure",
      content_side = NULL,
      content_main = tagList(
        sliderInputLeft(inputId="res_base", label = "Baseline of Treatment Failure", value = 5, min=0, max=100, step=1, post = "%"),
        sliderInputLeft(inputId="ressc2018", label = "2018", value = 5, min=0, max=100, step=1, post = "%"),
        sliderInputLeft(inputId="ressc2019", label = "2019", value = 5, min=0, max=100,step=1, post = "%"),
        sliderInputLeft(inputId="ressc2020", label = "2020 and Beyond", value = 5, min=0, max=100,step=1, post = "%")
      )) %>%
    bs_append(
      title_side = "Community Health Worker (CHW)",
      content_side = NULL,
      content_main = tagList(
        sliderInputLeft(inputId="pseekv", label = "Probabilty of seeking treatment with CHW", value = 30, min=0, max=100, step=1, post = "%"),
        sliderInputLeft(inputId="ptestv", label = "Probability of being tested with CHW", value = 75, min=0, max=100, step=1, post = "%"),
        sliderInputLeft(inputId="ptreatv", label = "Probability of being treated after positive test", value = 90, min=0, max=100,step=1, post = "%"),
        sliderInputLeft(inputId="reportv", label = "Probability of cases reported from CHW", value = 90, min=0, max=100,step=1, post = "%"),
        h4('Coverage of CHW Programme'),
        sliderInputLeft(inputId="vmw_cov12", label = "2012", value = 30, min=0, max=100, step=.5, post = '%'),
        sliderInputLeft(inputId="vmw_cov13", label = "2013", value = 30, min=0, max=100, step=.5, post = '%'),
        sliderInputLeft(inputId="vmw_cov14", label = "2014", value = 30, min=0, max=100, step=.5, post = '%'),
        sliderInputLeft(inputId="vmw_cov15", label = "2015", value = 30, min=0, max=100, step=.5, post = '%'),
        sliderInputLeft(inputId="vmw_cov16", label = "2016", value = 30, min=0, max=100, step=.5, post = '%'),
        sliderInputLeft(inputId="vmw_cov17", label = "2017", value = 30, min=0, max=100, step=.5, post = '%'),
        sliderInputLeft(inputId="vmw_cov18", label = "2018", value = 30, min=0, max=100, step=.5, post = '%')
      )) %>%
    bs_append(
      title_side = "Insecticide Treated Nets (ITN)",
      content_side = NULL,
      content_main = tagList(
        sliderInputReg(inputId="itn_aversion", label = "Effectiveness of LLIN (usage and efficacy) ", value = 20, min=0, max=50, post = '%'),
        p('Insecticide Treated Nets Distribution'),
        numericInputReg(inputId="itn_dis12", label = "2012", value = 350, min=0, max=50*10^6),
        numericInputReg(inputId="itn_dis13", label = "2013", value = 350, min=0, max=50*10^6, step=1),
        numericInputReg(inputId="itn_dis14", label = "2014", value = 350, min=0, max=50*10^6, step=1),
        numericInputReg(inputId="itn_dis15", label = "2015", value = 350, min=0, max=50*10^6, step=1),
        numericInputReg(inputId="itn_dis16", label = "2016", value = 350, min=0, max=50*10^6, step=1),
        numericInputReg(inputId="itn_dis17", label = "2017", value = 350, min=0, max=50*10^6, step=1),
        numericInputReg(inputId="itn_dis18", label = "2018", value = 350, min=0, max=50*10^6, step=1),
        sliderInputReg(inputId="net_multiplier", label = "No. of people sleeping under a net", value = 1.8, min=0, max=3, step=0.01)
      )) %>%
    bs_append(
      title_side = "Insecticide Residual Spraying (IRS)",
      content_side = NULL,
      content_main = tagList(
        sliderInputReg(inputId="irs_aversion", label = "Infections averted due to IRS", value = 15, min=0, max=100, step=5, post = '%'),
        p('Coverage of IRS Programme'),
        sliderInputLeft(inputId="irs_cov12", label = "2012", value = 0, min=0, max=100, step=0.5, post = '%'),
        sliderInputLeft(inputId="irs_cov13", label = "2013", value = 0, min=0, max=100, step=0.5, post = '%'),
        sliderInputLeft(inputId="irs_cov14", label = "2014", value = 0, min=0, max=100, step=0.5, post = '%'),
        sliderInputLeft(inputId="irs_cov15", label = "2015", value = 0, min=0, max=100, step=0.5, post = '%'),
        sliderInputLeft(inputId="irs_cov16", label = "2016", value = 0, min=0, max=100, step=0.5, post = '%'),
        sliderInputLeft(inputId="irs_cov17", label = "2017", value = 0, min=0, max=100, step=0.5, post = '%'),
        sliderInputLeft(inputId="irs_cov18", label = "2018", value = 0, min=0, max=100, step=0.5, post = '%')
      )) %>%
    bs_append(
      title_side = "Seasonal Malaria Chemoprevention",
      content_side = NULL,
      content_main = tagList(
        sliderInputReg(inputId="mon_smc", label = "Month to start programme", value = 7, min=1, max=12, step=1),
        sliderInputReg(inputId="dur_smc", label = "No. of monthly doses", value = 4, min=1, max=12, step=1),
        p('Coverage of SMC'),
        sliderInputLeft(inputId="smc_cov12", label = "2012", value = 0, min=0, max=100, step=.5, post = '%'),
        sliderInputLeft(inputId="smc_cov13", label = "2013", value = 0, min=0, max=100, step=.5, post = '%'),
        sliderInputLeft(inputId="smc_cov14", label = "2014", value = 0, min=0, max=100, step=.5, post = '%'),
        sliderInputLeft(inputId="smc_cov15", label = "2015", value = 0, min=0, max=100, step=.5, post = '%'),
        sliderInputLeft(inputId="smc_cov16", label = "2016", value = 0, min=0, max=100, step=.5, post = '%'),
        sliderInputLeft(inputId="smc_cov17", label = "2017", value = 0, min=0, max=100, step=.5, post = '%'),
        sliderInputLeft(inputId="smc_cov18", label = "2018", value = 0, min=0, max=100, step=.5, post = '%')
      )) %>%
    bs_append(
      title_side = "Intermittent Preventive Treatment for pregnant women (IPTp)",
      content_side = NULL,
      content_main = tagList(
        numericInputReg(inputId="yr_iptp_st", label = "Start Year", value = 2015, min=2000, max=2030, step=1),
        sliderInputReg(inputId="fert_rate", label = "Fertility Rate", value = 3.5, min=0, max=10, step=0.1, post = 'per 1000'),
        sliderInputReg(inputId="anc_rate", label = "ANC Registration", value = 80, min=0, max=100, step=1, post = '%'),
        p('Coverage of IPTp doses'),
        sliderInputLeft(inputId="iptp1_cov", label = "IPTp 1", value = 10, min=0, max=100, step=.5, post = '%'),
        sliderInputLeft(inputId="iptp2_cov", label = "IPTp 2", value = 10, min=0, max=100, step=.5, post = '%'),
        sliderInputLeft(inputId="iptp3_cov", label = "IPTp 3", value = 10, min=0, max=100, step=.5, post = '%'),
        sliderInputLeft(inputId="iptp4_cov", label = "IPTp 4", value = 10, min=0, max=100, step=.5, post = '%'),
        sliderInputLeft(inputId="iptp5_cov", label = "IPTp 5", value = 10, min=0, max=100, step=.5, post = '%')
      ))
)