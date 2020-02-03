# Single Patch Plasmodium falciparum model App

# Load packages
source("./www/R/load_packages.R")

# Small modifications of UI inputs (label placement...)
source('./www/R/fun/numericInputReg.R')
source('./www/R/fun/sliderInputLeft.R')
source('./www/R/fun/sliderInputReg.R')


# Functions to run the model
sourceCpp("./www/model/eq0.cpp")
source('./www/model/model_setup.R')
source('./www/model/model_run.R')
source('./www/model/compute_cost.R')


# Define UI ---------------------------------------------------------

ui <- function(request) {
  fluidPage(
    theme = shinytheme("flatly"),
    includeCSS("./www/styles.css"),
    use_vov(),
    chooseSliderSkin('HTML5'),
    title = "MASHA SPPf — Single Patch Plasmodium falciparum Model",
    
    div(id = "links_topright", fade_in_top_right(duration = "slow", uiOutput("links"))),
    
    navbarPage("", id = 'tabs',
               tabPanel(value = 'welcome', title = h5("Welcome"), 
                        fluidRow(
                          column(4, 
                                 tags$img(src = "./images/logo_uct.png", id = "uct"),
                                 tags$img(src = "./images/logo_masha.png", id = "masha"),
                                 includeMarkdown('www/markdown/about_0.md')
                          ),
                          column(6, 
                                 bs_accordion(id = "about") %>%
                                   bs_set_opts(panel_type = "default", use_heading_link = TRUE) %>%
                                   bs_append(title = span("The Single Patch", em("Plasmodium falciparum"),  "model (SPPf)"), content = includeMarkdown('www/markdown/about_1.md')) %>%
                                   bs_append(title = "Using the SPPf model", content = includeMarkdown('www/markdown/about_2.md')) %>%
                                   bs_append(title = "Understanding the SPPf model", content = includeMarkdown('www/markdown/about_3.md'))
                          )
                        )
               ),
               tabPanel(value = 'baseline', title = h5("Build Baseline"),
                        verticalTabsetPanel(contentWidth = 10, color = "#2C3E50",
                                            verticalTabPanel(value = 'epi', title = span(icon("cog"), "Programme Info"),
                                                             source("./www/R/ui/tab_baseline.R", local = TRUE)$value,
                                                             use_bs_accordion_sidebar()
                                            ),
                                            verticalTabPanel(value = 'costs', title = span(icon("cog"), "Programme & Interventions Costs"),
                                                             source("./www/R/ui/tab_cost_interventions.R", local = TRUE)$value,
                                                             use_bs_accordion_sidebar()
                                            ),
                                            verticalTabPanel(value = "simulate", title = span(icon("play"), "Baseline Calibration"),
                                                             fluidRow(
                                                               column(2,
                                                                      h5("Pf Transmission Level:"),
                                                                      numericInput(inputId = "tranp", label = NULL, value = 18, min = 0, max = 1000, step = 1, width = "95%"),
                                                                      h5("Case Fatality Rate:"),
                                                                      numericInput(inputId = "pmortt", label = NULL, value = 0.0018, min = 0, max = 1, step = 0.0001, width = "95%"),
                                                                      h5("Severe Illness Treatment Seeking:"),
                                                                      numericInput(inputId = "tausev", label = NULL, value = 0.8, min = 0, max = 1, step = 0.01, width = "95%"),
                                                                      actionButton("go_baseline", icon = icon("play"), "Simulate", width = "95%", class = "btn btn-success")  %>% 
                                                                        helper(content = "baseline_simulation", colour = "red")
                                                               ),
                                                               column(8, offset = 1,
                                                                      conditionalPanel(condition = "output.exist_baseline",
                                                                                       h4("Calibration Results"),
                                                                                       DTOutput("build_table_calibration"),
                                                                                       br(),
                                                                                       h4("Monthly", em("P. falciparum"), " Cases"),
                                                                                       plotOutput('plot_baseline_pf') %>% withSpinner(),
                                                                                       br(),
                                                                                       actionButton("validate_calibration", label = span(icon("thumbs-up"), "Validate Baseline Calibration"), class="btn btn-success"),
                                                                                       br(), br())
                                                               )
                                                             )
                                            )
                        )
               ),
               tabPanel(value = 'packages', title = h5("Simulate Interventions"),
                        fluidRow(column(12, h3(icon('cubes'), 'Build a Package of Interventions'))),
                        fluidRow(
                          column(width = 4,
                                 h4(icon('box'), "Traditional Interventions"),
                                 source("./www/R/ui/intervention_HSS.R", local = TRUE)$value,
                                 source("./www/R/ui/intervention_CHW.R", local = TRUE)$value,
                                 source("./www/R/ui/intervention_ITN.R", local = TRUE)$value,
                                 source("./www/R/ui/intervention_IRS.R", local = TRUE)$value
                          ),
                          column(width = 8,
                                 h4(icon('box'), "Innovative Interventions"),
                                 fluidRow(
                                   column(width = 6,
                                          source("./www/R/ui/intervention_RAC.R", local = TRUE)$value,
                                          source("./www/R/ui/intervention_ACD.R", local = TRUE)$value,
                                          # Not yet implemented:
                                          source("./www/R/ui/TODO_intervention_AMS.R", local = TRUE)$value,
                                          source("./www/R/ui/TODO_intervention_MDA.R", local = TRUE)$value
                                   ),
                                   column(width = 6,
                                          source("./www/R/ui/intervention_SMC.R", local = TRUE)$value,
                                          source("./www/R/ui/intervention_IPTp.R", local = TRUE)$value,
                                          source("./www/R/ui/intervention_TFE.R", local = TRUE)$value
                                          
                                   )
                                 )
                          )
                        ),
                        # Floating box to name and run a package of interventions
                        div(class= "float-go-interv",
                            div(class = "panel panel-success",
                                div(class = "panel-heading", "Simulation of the Package"),
                                div(class = "panel-body",
                                    fluidRow(
                                      column(9,
                                             span("Package Name:", 
                                                  textInput("package_name", label = NULL, placeholder = "e.g. Scale up ITN + IRS", width = "250px")
                                             )
                                      ),
                                      column(3,
                                             br(), br(),
                                             actionButton("go_interventions", "Run", class="btn btn-success")
                                      )
                                    )
                                )
                            )
                        )
               ),
               tabPanel(value = 'tab_explore', title = h5("Explore"),
                        fluidRow(
                          column(2, 
                                 uiOutput("select_packages"),
                                 uiOutput("delete_packages")
                          ),
                          column(4, 
                                 h3("Yearly Costs"), p("Range of costs in US$"),
                                 highchartOutput("explore_costs") %>% withSpinner()
                          ),
                          column(6, 
                                 DTOutput("explore_table_summary")
                          )
                        ),
                        fluidRow(
                          column(6, 
                                 h3("Pf Incidence"), p("Treated & Reported Cases per Year"),
                                 highchartOutput("explore_incidence_pf") %>% withSpinner()
                          ),
                          column(6, 
                                 h3("Pf Burden"), p("Clinical Cases per Year"),
                                 highchartOutput("explore_burden_pf") %>% withSpinner()
                          )
                        )
               ),
               tabPanel(value = 'tab_sensitivity_analysis', title = h5('Sensitivity Analysis'),
                        fluidRow(
                          column(3, 
                                 p("Select Baseline/Package(s) to include in the SA:"),
                                 awesomeCheckboxGroup("select_simul_sa", choices = " ", label = NULL, inline = TRUE),
                                 p("Provide range of values for the following parameters:"),
                                 sliderInputLeft(inputId="report_sa", label = "Probability of cases reported", value = c(10, 90), min=0, max=100,step=1,  post = "%"),
                                 sliderInputLeft(inputId="reportv_sa", label = "Probability of cases reported from VMW", value = c(10, 90), min=0, max=100,step=1,  post = "%"),
                                 br(),
                                 p("Choose a number of run:"),
                                 fluidRow(
                                   column(6,
                                          sliderInput("nb_run_sa", label = NULL, min = 2, max = 100, value = 10, ticks = FALSE, post = " runs")
                                   ),
                                   column(6,
                                          blur_in(duration = "slower", textOutput("sa_text_runtime"))
                                   )
                                 ),
                                 br(),
                                 actionButton("go_sa", span(icon("play"), "Run Sensitivity Analysis"), class="btn btn-success")
                          ),
                          column(9,
                                 conditionalPanel(condition = "output.exist_sa",
                                                  # selectInput("var_sa", choices = c("vmwf", "hisf", "fatal", "severef", "clinmonthf", "prevf", "trtf"), label = NULL),
                                                  # plotOutput("plot_pf_sa") %>% withSpinner()
                                                  fluidRow(
                                                    column(8,
                                                           h3("Pf Incidence"),
                                                           fluidRow(
                                                             column(6,
                                                                    plotOutput("sa_plot_reported") %>% withSpinner()
                                                             ),
                                                             column(6, 
                                                                    plotOutput("sa_plot_treated") %>% withSpinner()
                                                             )
                                                           )  
                                                    ),
                                                    column(4, 
                                                           h3("Pf Burden"),
                                                           plotOutput("sa_plot_clinical") %>% withSpinner()
                                                    )
                                                  )
                                 )
                          )
                        )
                        
               )
    )
  )
}

# Define server -----------------------------------------------------

server <- function(input, output, session) {
  
  # Initiating server ----
  hideTab(inputId = "tabs", target = "packages")
  hideTab(inputId = "tabs", target = "tab_explore")
  hideTab(inputId = "tabs", target = "tab_sensitivity_analysis")
  
  observe_helpers(help_dir = "./www/markdown")
  
  file_list <- list.files(path = "./www/R/outputs/", pattern = "*.R")
  for (file in file_list) source(paste0("./www/R/outputs/", file), local = TRUE)$value
  
  # Update inputs when a Excel file is uploaded ----
  observeEvent(input$file_parameters, {
    for(i in 1:length(input$file_parameters[, 1])) {
      
      parameters <- read_excel(input$file_parameters[[i, 'datapath']])
      
      # Update all sliders with one value
      if(!is_empty(parameters$Parameter[parameters$Type == 'slider'])) {
        for (input_excel in parameters$Parameter[parameters$Type == 'slider']){
          updateSliderInput(session = session, inputId = input_excel, value = parameters$Value1[parameters$Parameter == input_excel])
        }}
      
      # Update all sliders with two values (costs included in sensitivity analysis)
      if(!is_empty(parameters$Parameter[parameters$Type == 'slider_dual'])) {
        for (input_excel in parameters$Parameter[parameters$Type == 'slider_dual']){
          updateSliderInput(session = session, inputId = input_excel, 
                            value = c(parameters$Value1[parameters$Parameter == input_excel], parameters$Value2[parameters$Parameter == input_excel]))
        }}
      
      # Update all numeric values
      if(!is_empty(parameters$Parameter[parameters$Type == 'numeric'])) {
        for (input_excel in parameters$Parameter[parameters$Type == 'numeric']){
          updateNumericInput(session = session, inputId = input_excel, value = parameters$Value1[parameters$Parameter == input_excel])
        }}
      
    }
  }
  )
  
  # Provide a default name for created package ----
  observe({
    label_default <- paste0(
      # Traditional
      ifelse(input$trt_switch, "+ HSS", ""),
      ifelse(input$vmwsc_switch, "+ CHW", ""), 
      ifelse(input$itnsc_switch, "+ ITN", ""),
      ifelse(input$irssc_switch, "+ IRS", ""),
      
      # Innovative
      ifelse(input$prim_switch, "+ RAC", ""),
      ifelse(input$rcdsc_switch, "+ ACD", ""),
      ifelse(input$rcdMS_switch, "+ AMS (Not Yet Implemented)", ""),
      ifelse(input$mda_switch, "+ MDA (Not Yet Implemented)", ""),
      
      ifelse(input$smcsc_switch, "+ SMC", ""),
      ifelse(input$iptpsc_switch, "+ IPTp", ""),
      ifelse(input$fail_switch, "+ TFE", "")) %>%
      substring(first = 3)
    
    updateTextInput(session, "package_name", label = label_default, value = label_default)
  })
  
  
  # Define reactive elements ----
  simul_baseline <- reactiveValues(
    input = NULL,
    name = "Baseline",
    results_epi = tibble(),
    results_costs = tibble(),
    run_time = NULL,
    baseline_available = FALSE,
    baseline_discount = tibble(),
    baseline_calibration = tibble())
  
  simul_packages <- reactiveValues(simul = list(), nb = 0, names = NULL)
  
  simul_sa <- reactiveValues(simul = NULL)
  
  simul_results <- reactive({
    req(simul_baseline$baseline_available)
    
    dta <- simul_baseline$results_epi
    
    if(simul_packages$nb > 0){
      for (i in 1:length(simul_packages$simul)) dta <- bind_rows(dta, simul_packages$simul[[i]]$results_epi)
    }
    return(dta)
  })
  
  simul_costs <- reactive({
    req(simul_baseline$baseline_available)
    
    dta <- simul_baseline$results_costs
    
    if(simul_packages$nb > 0){
      for (i in 1:length(simul_packages$simul)) dta <- bind_rows(dta, simul_packages$simul[[i]]$results_costs)
    }
    return(dta)
  })
  
  
  # Notify if any baseline input changes ----
  observeEvent(c(input$initP, input$pgrow, input$vilsize, input$APIf12, input$APIf13, input$APIf14, input$APIf15, input$APIf16, input$APIf17, 
                 input$APIf18, input$muC, input$muA, input$muU, input$prop_rdth, input$rdt_pos, input$slide_pos,input$amp, input$phi, input$pe, input$pseek, input$ptest, input$ptreat, input$report,input$res_base, 
                 input$ressc2018, input$ressc2019, input$ressc2020, input$pseekv, input$ptestv, input$ptreatv, input$reportv, input$reportdeath, input$vmw_cov12, input$vmw_cov13, input$vmw_cov14, 
                 input$vmw_cov15, input$vmw_cov16, input$vmw_cov17, input$vmw_cov18, input$itn_aversion, input$itn_dis12, input$itn_dis13, 
                 input$itn_dis14, input$itn_dis15, input$itn_dis16, input$itn_dis17, input$itn_dis18, input$net_multiplier, input$irs_aversion, input$irs_cov12, input$irs_cov13, 
                 input$irs_cov14, input$irs_cov15, input$irs_cov16, input$irs_cov17, input$irs_cov18, input$mon_smc, input$dur_smc, input$smc_cov12, input$smc_cov13, input$smc_cov14, input$smc_cov15, 
                 input$smc_cov16,input$smc_cov17, input$smc_cov18,input$yr_iptp_st, input$fert_rate, input$anc_rate, input$ipt1_cov, input$inpt2_cov, input$ipt3_cov, input$ipt4_cov, input$ipt5_cov,
                 input$cpvmw, input$cprdt, input$cpmic, input$cpitn, input$cppirs, input$cpdrugfov, input$cpdrugi, input$cptrti, 
                 input$cptrto, input$cppsurv, input$cpptrain, input$cppiec, input$d, input$tranp, input$tausev, input$pmortt), 
               
               if(simul_baseline$baseline_available) showNotification(HTML("Some inputs have changed, please run the baseline again"), duration = NULL, 
                                                                      type = "warning", id = "parameters_baseline", session = session), ignoreInit = TRUE)
  
  
  # Compute discount ----
  observe({
    simul_baseline$baseline_discount <- data.frame(year = 2017:2030, 
                                                   discount = c(1, 1, (1+ (input$d / 100)) ^ (-(1:(startyear + tyears - 2018)))))
  })
  
  # Helpers to show/hide elements of UI ----
  output$exist_baseline <- reactive({
    simul_baseline$baseline_available
  })
  outputOptions(output, "exist_baseline", suspendWhenHidden = FALSE)
  
  output$exist_sa <- reactive({
    ! is.null(simul_sa$simul)
  })
  outputOptions(output, "exist_sa", suspendWhenHidden = FALSE)
  
  
  # Process on "Simulate Baseline" ----
  observeEvent(input$go_baseline, {
    removeNotification(id = "parameters_baseline", session = session)
    showNotification(span(h4(icon("hourglass-half"), "Simulation Running..."), "typically runs in 10 to 30 secs."),
                     duration = NULL, type = "message", id = "model_baseline")
    
    updateAwesomeCheckboxGroup(session, inputId = "select_simul_sa", choices = "Baseline", selected = "Baseline")
    
    
    time_start_simul <- Sys.time()
    
    simul_run <- run(initprevf = input$APIf18 / 1000, 
                     input = reactiveValuesToList(input),
                     package = "Baseline")
    
    simul_baseline$input <- reactiveValuesToList(input)
    
    simul_baseline$results_epi <- simul_run  %>%
      left_join(tibble(year = 2012:2018, 
                       API = (input$initP / 1000)*c(input$APIf12, input$APIf13, 
                                                    input$APIf14, input$APIf15, input$APIf16, 
                                                    input$APIf17, input$APIf18) / 12), by = 'year') %>%
      mutate(error = abs(API - trtf))
    
    simul_baseline$results_costs <- compute_cost(simul_epi = simul_run, 
                                                 discount = simul_baseline$baseline_discount, 
                                                 input = input, 
                                                 package = "Baseline")
    
    simul_baseline$run_time <- round(as.numeric(Sys.time() - time_start_simul), 1)
    simul_baseline$baseline_available <- TRUE
    simul_baseline$baseline_calibration <- bind_rows(simul_baseline$baseline_calibration, 
                                                     tibble(`Transmission level for Pf` = input$tranp, 
                                                            `Sum of the squares of the residuals` = round(sqrt(sum((simul_baseline$results_epi$API - simul_baseline$results_epi$trtf)^2, na.rm = TRUE)), 2),
                                                            `Severe Illness Treatment Seeking` = input$tausev,
                                                            `Annual Severe Cases (2018)` = round(sum(simul_baseline$results_epi %>% filter(year == 2018) %>% pull(severef), na.rm = TRUE), 2),
                                                            `Case Fatality Rate` = input$tausev,
                                                            `Annual Deaths (2018)` = round(sum(simul_baseline$results_epi %>% filter(year == 2018) %>% pull(fatal), na.rm = TRUE), 2))
    )
    
    removeNotification(id = "model_baseline", session = session)
    showNotification(span(h4(icon("check"), "Done"), paste0("Simulation ran in ", simul_baseline$run_time, " seconds.")), duration = 4, type = "default")
  })
  
  # Process on "Validation of Baseline Calibration" ----
  observeEvent(input$validate_calibration, {
    showTab(inputId = "tabs", target = "packages")
    showTab(inputId = "tabs", target = "tab_explore")
    showTab(inputId = "tabs", target = "tab_sensitivity_analysis")
    updateTabsetPanel(session, "tabs", selected = "tab_explore")
  })
  
  # Process on "Run Package" ----
  observeEvent(input$go_interventions, {
    if((input$vmwsc_switch + input$itnsc_switch + input$irssc_switch + input$prim_switch +
        input$rcdsc_switch + input$rcdpf_switch + input$rcdMS_switch + input$rcdMSpf_switch +
        input$mda_switch + input$trt_switch + input$fail_switch + input$smcsc_switch + input$iptpsc_switch) == 0){
      sendSweetAlert(
        session = session,
        title = "Select Interventions",
        text = "Please select at least one intervention to be part of the package.",
        type = "warning"
      )
      return(NULL)
    }
    
    if(input$package_name == ""){
      sendSweetAlert(
        session = session,
        title = "Provide Name",
        text = "Please provide a name to your package and run again",
        type = "warning"
      )
      return(NULL)
    }
    
    if(input$package_name %in% c("Baseline", simul_packages$names)){
      sendSweetAlert(session = session, title = "Provide Another Name", 
                     text = "This name is already taken, please provide a different name to your package", type = "warning")
      return(NULL)
    }
    
    showNotification(span(h4(icon("hourglass-half"), "Simulation Running..."), "typically runs in 10 to 30 secs."), duration = NULL, type = "message", id = "model_interventions", session = session)
    time_start <- Sys.time()
    
    simul_run <- run(initprevf = input$APIf18 / 1000, 
                     input = reactiveValuesToList(input),
                     package = input$package_name)
    
    simul_cost <- compute_cost(simul_epi = simul_run, 
                               discount = simul_baseline$baseline_discount,
                               input = input,
                               package = input$package_name)
    
    simul_packages_this <- list(
      input = reactiveValuesToList(input),
      name = input$package_name,
      results_epi = simul_run,
      results_costs = simul_cost,
      run_time = ceiling(as.numeric(Sys.time() - time_start, units = "secs")))
    
    simul_packages$simul[[simul_packages$nb + 1]] <- simul_packages_this
    simul_packages$nb <- simul_packages$nb + 1
    simul_packages$names <- c(simul_packages$names, input$package_name)
    
    removeNotification(id = "model_interventions", session = session)
    showNotification(span(h4(icon("check"), "Done"), paste0("Simulation ran in ", ceiling(as.numeric(Sys.time() - time_start, units = "secs")), " seconds.")), 
                     duration = 4, type = "default")
    
    updateAwesomeCheckboxGroup(session, inputId = "select_simul_sa", choices = c("Baseline", simul_packages$names), selected = "Baseline")
    updateTextInput(session = session, inputId = "package_name", value = "")
    updateTabsetPanel(session, "tabs", selected = "tab_explore")
  })
  
  # Process on "Download results" ----
  output$go_download <- downloadHandler(
    filename = paste0("SPPF_App_", Sys.time(), ".zip"),
    content = function(file) {
      sep <- ","  # also possible sep = "\t" or sep = ";"
      write.table(simul_results(), "epi.csv", sep = sep, row.names = F)
      write.table(simul_costs(), "costs.csv", sep = sep, row.names = F)
      simul_baseline_dl <- reactiveValuesToList(simul_baseline)
      simul_packages_dl <- reactiveValuesToList(simul_packages)
      
      save(simul_baseline_dl, simul_packages_dl, file = "SPPF_App_Data.RData")
      zip(file, c("epi.csv", "costs.csv", "SPPF_App_Data.RData"))
    })
  
  output$go_download_sa <- downloadHandler(
    filename = paste0("SPPF_App_SensitivityAnalysis_", Sys.time(), ".RData"),
    content = function(file) {
      data_sa <- simul_sa$simul
      save(data_sa, file = file)
    })
  
  # Process on "Delete package" ----
  observeEvent(input$confirm_delete_packages, {
    packages <- setdiff(input$rank_list_hide, "Baseline")
    
    simul_packages$simul[-which(simul_packages$names %in% packages)]
    simul_packages$nb <- simul_packages$nb - length(packages)
    simul_packages$names <- setdiff(simul_packages$names, packages)
    
    updateAwesomeCheckboxGroup(session, inputId = "select_simul_sa", choices = input$rank_list_show, selected = "Baseline")
  })
  
  
  # Process on "Run Sensitivity Analysis" ----
  observeEvent(input$go_sa, {
    simul_sa$simul <- NULL
    time_start <- Sys.time()
    showNotification(h4(icon("hourglass-half"), "Simulation Running..."), duration = NULL, type = "message", id = "sa", session = session)
    
    for (i in 1:input$nb_run_sa) {
      
      reportv_sa <- runif(n = input$nb_run_sa, min = input$reportv_sa[[1]], max = input$reportv_sa[[2]])
      report_sa <- runif(n = input$nb_run_sa, min = input$report_sa[[1]], max = input$report_sa[[2]])
      
      for (package in input$select_simul_sa) {
        
        if(package == "Baseline")  input_sa <- simul_baseline$input
        if(package != "Baseline")  input_sa <- simul_packages$simul[[which(simul_packages$names == package)]]$input
        
        input_sa$reportv <- reportv_sa[i]
        input_sa$report <- report_sa[i]
        
        simul_run <- run(initprevf = input_sa$APIf18 / 1000, 
                         input = input_sa,
                         package = package) %>% 
          mutate(run_nb = i)
        
        simul_sa$simul <- bind_rows(simul_sa$simul, simul_run)
      }
    }
    
    removeNotification(id = "sa", session = session)
    showNotification(span(h4(icon("check"), "Done"), paste0("Simulation ran in ", ceiling(as.numeric(Sys.time() - time_start, units = "secs")), " seconds.")), 
                     duration = 4, type = "default")
  })
  
  # Process on "Upload of data" ----
  observeEvent(input$file_RData, {
    
    inFile <- input$file_RData
    file <- inFile$datapath
    load(file, envir = .GlobalEnv)
    
    # Update reactive values based on data uploaded
    simul_baseline$baseline_available <- simul_baseline_dl$baseline_available
    simul_baseline$results_costs <- simul_baseline_dl$results_costs
    simul_baseline$input <- simul_baseline_dl$input
    simul_baseline$baseline_discount <- simul_baseline_dl$baseline_discount
    simul_baseline$baseline_calibration <- simul_baseline_dl$baseline_calibration
    simul_baseline$results_epi <- simul_baseline_dl$results_epi
    simul_baseline$run_time <- simul_baseline_dl$run_time
    simul_baseline$name <- simul_baseline_dl$name
    
    simul_packages$nb <- simul_packages_dl$nb
    simul_packages$simul <- simul_packages_dl$simul
    simul_packages$names <- simul_packages_dl$names
    
    # show all tabs and activate explore tab
    showTab(inputId = "tabs", target = "packages")
    showTab(inputId = "tabs", target = "tab_explore")
    showTab(inputId = "tabs", target = "tab_sensitivity_analysis")
    updateTabsetPanel(session, "tabs", selected = "tab_explore")
    
    # Update elements for the UI
    updateAwesomeCheckboxGroup(session, inputId = "select_simul_sa", choices = c("Baseline", simul_packages$names), selected = "Baseline")
    
    
  },
  ignoreInit = TRUE)
}


# ---------------------------------------------------------
# Run the App
shinyApp(ui = ui, server = server)

