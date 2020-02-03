compute_cost  <- function(simul_epi, discount, input, package) {
  
  # Special Ghana code (TODO) to be removed when ITN is updated
  itn_mass<-rep(0,(tsteps+1)); itn_mass[338:(tsteps+1)]<-c(rep(c(1, rep(0,35)),round((startyear+tyears-2018)/3)))
  itn_dist2018 = input$itn_dist2018 / input$initP #national
  
  # variables that should be included in range of costs
  vars_interval <- map_lgl(reactiveValuesToList(input),  function(x) { ifelse(length(x) == 1, FALSE, TRUE) }) & 
    str_detect(names(reactiveValuesToList(input)), "cp")
  # vars_distinct_interval <- map_lgl(reactiveValuesToList(input),  function(x) { ifelse(length(x) == 1, FALSE, x[1] != x[2]) }) & 
  #   str_detect(names(reactiveValuesToList(input)), "cp")
  
  # print(str_detect(names(reactiveValuesToList(input)), "cp"))
  
  # ifelse(any(vars_distinct_interval), total_sample <- 300, total_sample <- 1)
  costs_all <- list()
  set.seed(2019)
  
  for (i in 1:100) {
    
    # Sample values
    input_sample <- reactiveValuesToList(input)
    for (var in which(vars_interval)) input_sample[[var]] <- runif(1, min = input_sample[[var]][1], max = input_sample[[var]][2])
    
    # Baseline costs
    costs <- simul_epi %>%
      mutate(
        cost_itn = ((input_sample$cpitn / 3) * input_sample$itn_dis18 + input_sample$cpitn * totIPTp_predf) / 12, # spreading costs over 3 years
        cost_irs = input_sample$cppirs * (population_pred / 12) * (input_sample$irs_cov18 / 100),
        cost_inpatient =  (input_sample$cpdrugi + input_sample$cptrti) * severef,
        cost_outpatient = (input_sample$cpdrugfov + input_sample$cptrto) * hisf +
          input_sample$cprdt * (input_sample$prop_rdth / 100) * (hisf / ((input_sample$prop_rdth / 100) * (input_sample$rdt_pos / 100) + (1-(input_sample$prop_rdth / 100)) * (input_sample$slide_pos / 100) )) +
          input_sample$cpmic * (1 - (input_sample$prop_rdth / 100)) * (hisf/((input_sample$prop_rdth / 100) * (input_sample$rdt_pos / 100) + (1-(input_sample$prop_rdth / 100)) * (input_sample$slide_pos / 100) )),
        cost_vmw = input_sample$cpdrugfov * vmwf + 
          input_sample$cpvmw/12 * (population_pred / input_sample$vilsize * (input_sample$vmw_cov18 / 100)) + 
          input_sample$cprdt * (vmwf /(input_sample$rdt_pos / 100) ),
        cost_other = population_pred * (input_sample$cppsurv + input_sample$cpptrain + input_sample$cppiec) / 12,
        cost_smc  = (input_sample$cppsmc + input_sample$cpdrug_smc*4)*totSMCtr_ode,
        cost_iptp = input_sample$cppipt*totIPTp_predf +input_sample$cpdrug_iptp*totIPTpdose_predf,
        # additional to baseline
        cost_acd = 0,
        cost_ms = 0
      )
    
    # Add costs of packages
    if(package != "Baseline"){
      
      # ITN
      # Assuming nets replaced every 3 years e.g 80% coverage annually implies 26.6% new nets distributed per year /12 to get monthly costs
      if(input_sample$itnsc_switch){
        period_start_1 <- (input_sample$itnsc_start1 - startyear) * 12 + 1
        period_end_1 <- (input_sample$itnsc_start1 - startyear + input_sample$yrs_itnsc1) * 12
        period_start_2 <- (input_sample$itnsc_start2 - startyear) * 12 + 1
        period_end_2 <- (input_sample$itnsc_start2 - startyear + input_sample$yrs_itnsc2) * 12
        
        itn_cov18 <-  input_sample$itn_dis18 / input_sample$initP * input_sample$net_multiplier
        itnsc_cov1  <- input_sample$itnsc_cov1 / 100
        itnsc_cov2  <- input_sample$itnsc_cov2 / 100
        
        costs$cost_itn[period_start_1:period_end_1] <- (input_sample$cpitn2 / 3 / input_sample$net_multiplier ) * (costs$population_pred[period_start_1:period_end_1] / 12) *
          seq(itn_cov18 + (itnsc_cov1 - itn_cov18) / (input_sample$yrs_itnsc1 * 12),
              itnsc_cov1,
              (itnsc_cov1 - itn_cov18) / (input_sample$yrs_itnsc1 * 12)) 
        
        costs$cost_itn[(period_end_1 + 1):(period_start_2 - 1)] <- (input_sample$cpitn2 / 3) * (costs$population_pred[(period_end_1 + 1):(period_start_2 - 1)] / 12) / input_sample$net_multiplier * itnsc_cov1
        
        costs$cost_itn[period_start_2:period_end_2] <- (input_sample$cpitn2 / 3) * (costs$population_pred[period_start_2:period_end_2] / 12) / input_sample$net_multiplier *
          seq(itnsc_cov1 + (itnsc_cov2 - itnsc_cov1)/(input_sample$yrs_itnsc2 * 12),
              itnsc_cov2,
              (itnsc_cov2 - itnsc_cov1)/(input_sample$yrs_itnsc2 * 12))
        
        costs$cost_itn[(period_end_2 + 1):tmonths] <- (input_sample$cpitn2 / 3) * (costs$population_pred[(period_end_2 + 1):tmonths] / 12)  / input_sample$net_multiplier * itnsc_cov2
      }
      
      
      
      # IRS
      if(input_sample$irssc_switch){
        period_start_1 <- (input_sample$irssc_start1 - startyear) * 12 + 1
        period_end_1 <- (input_sample$irssc_start1 - startyear + input_sample$yrs_irssc1) * 12
        period_start_2 <- (input_sample$irssc_start2 - startyear) * 12 + 1
        period_end_2 <- (input_sample$irssc_start2 - startyear + input_sample$yrs_irssc2) * 12
        
        irs_cov18 <- input_sample$irs_cov18 / 100
        irssc_cov1  <- input_sample$irssc_cov1 / 100
        irssc_cov2  <- input_sample$irssc_cov2 / 100
        
        
        costs$cost_irs[period_start_1:period_end_1] <- input_sample$cppirs2 * ( costs$population_pred[period_start_1:period_end_1] / 12) *
          seq(irs_cov18 + (irssc_cov1 - irs_cov18) / (input_sample$yrs_irssc1 * 12),
              irssc_cov1,
              (irssc_cov1 - irs_cov18) / (input_sample$yrs_irssc1 * 12))
        
        costs$cost_irs[(period_end_1 + 1):(period_start_2 - 1)] <- input_sample$cppirs2 * irssc_cov1 * ( costs$population_pred[(period_end_1 + 1):(period_start_2 - 1)] / 12)
        
        costs$cost_irs[period_start_2:period_end_2] <- input_sample$cppirs2 * ( costs$population_pred[period_start_2:period_end_2] / 12) *
          seq(irssc_cov1 + (irssc_cov2 - irssc_cov1) / (input_sample$yrs_irssc2 * 12),
              irssc_cov2,
              (irssc_cov2 - irssc_cov1) / (input_sample$yrs_irssc2 * 12))
        
        costs$cost_irs[(period_end_2 + 1):tmonths] <- input_sample$cppirs2 * irssc_cov2 * ( costs$population_pred[(period_end_2 + 1):tmonths] / 12)
      }
      
      # CHW
      if(input_sample$vmwsc_switch){
        
        vmw_cov18 <-  input_sample$vmw_cov18/100
        vmwsc_cov1  <- input_sample$vmwsc_cov1/100
        vmwsc_cov2  <- input_sample$vmwsc_cov2/100
        
        period_start_1 <- (input_sample$vmwsc_start1 - startyear) * 12 + 1
        period_end_1 <- (input_sample$vmwsc_start1 - startyear + input_sample$yrs_vmwsc1) * 12
        period_start_2 <- (input_sample$vmwsc_start2 - startyear) * 12 + 1
        period_end_2 <- (input_sample$vmwsc_start2 - startyear + input_sample$yrs_vmwsc2) * 12
        
        novmw <-  simul_epi$population_pred[1] / input_sample$vilsize * vmw_cov18
        novmwsc1 <-  simul_epi$population_pred[period_start_1] / input_sample$vilsize * input_sample$vmwsc_cov1
        novmwsc2 <- simul_epi$population_pred[period_start_2] / input_sample$vilsize * input_sample$vmwsc_cov2
        
        
        
        costs$cost_vmw[period_start_1:period_end_1] <- 
          simul_epi$vmwf[period_start_1:period_end_1] * input_sample$cpdrugfov + 
          (input_sample$cpvmwsc / 12) * seq(novmw + (novmwsc1 - novmw)/(input_sample$yrs_vmwsc1 * 12),
                                     novmwsc1,
                                     (novmwsc1 - novmw) / (input_sample$yrs_vmwsc1 * 12)) +
          input_sample$cprdt * (simul_epi$vmwf[period_start_1:period_end_1] / (input_sample$rdt_pos / 100) )
        
        costs$cost_vmw[(period_end_1 + 1):(period_start_2 - 1)] <- 
          simul_epi$vmwf[(period_end_1 + 1):(period_start_2 - 1)] * input_sample$cpdrugfov +
          (input_sample$cpvmwsc / 12) * novmwsc1 + 
          input_sample$cprdt * (simul_epi$vmwf[(period_end_1 + 1):(period_start_2 - 1)]/(input_sample$rdt_pos / 100))
        
        costs$cost_vmw[period_start_2:period_end_2] <- 
          simul_epi$vmwf[period_start_2:period_end_2] * input_sample$cpdrugfov + 
          (input_sample$cpvmwsc / 12) * seq(novmwsc1 + (novmwsc2 - novmwsc1)/(input_sample$yrs_vmwsc2 * 12),
                                     novmwsc2,
                                     (novmwsc2 - novmwsc1)/(input_sample$yrs_vmwsc2 * 12)) +
          input_sample$cprdt * (simul_epi$vmwf[period_start_2:period_end_2] / (input_sample$rdt_pos / 100))
        
        costs$cost_vmw[(period_end_2 + 1):tmonths] <- 
          input_sample$cpdrugfov * simul_epi$vmwf[(period_end_2 + 1):tmonths] + 
          (input_sample$cpvmwsc / 12) * novmwsc2 + 
          input_sample$cprdt * (simul_epi$vmwf[(period_end_2 + 1):tmonths] / (input_sample$rdt_pos / 100))
      }
      
      # HSS
      if(input_sample$trt_switch){
        period <- ((input_sample$hsssc_start1 - startyear) * 12 + 1):tmonths
        
        costs$cost_other[period] <- 
          costs$cost_other[period] +
          (input_sample$cppmgt * simul_epi$population_pred[period]) / 12 +  # Increased cost for mgt, training added to Other etc
          (input_sample$cppiec3 * simul_epi$population_pred[period]) / 12  # Increased IEC cost to improve treatment seeking etc
      }
      
      
      # ACD
      if(input_sample$fail_switch){
        period <- ((input_sample$yr_rcdscstart - startyear) * 12 + 1):tmonths
        
        costs$cost_acd[period] <- 
          input_sample$cpdrugfov * simul_epi$prevf[period] + # drugs
          input_sample$cpacdvisit * simul_epi$rcdvisits[period] + # cost per vist 
          input_sample$cprdt * simul_epi$rcdsample[period]  # diagnostics for sample per case investigated
      }
      
      # AMS (Not Yet Implemented)
      if(input_sample$rcdMSpf_switch){
        period <- ((input_sample$yr_rcdMSstart - startyear) * 12 + 1):tmonths
        novmw <-  input_sample$initP / input_sample$vilsize * vmw_cov18
        novmwsc1 <-  input_sample$initP / input_sample$vilsize * input_sample$vmwsc_cov1
        
        costs$cost_ms[period] <- 
          input_sample$cpdrugfov * simul_epi$totMS_predf_ode[period] + # drugs
          input_sample$costMS * (1 - input_sample$vmwsc_switch) * novmw + 
          input_sample$vmwsc_switch * novmwsc1 + # cost per vist 
          input_sample$cprdt * simul_epi$totMS_sample_ode[period]  # diagnostics for sample per case investigated
      }
      
      # SMC
      if(input_sample$smcsc_switch){
        period <- ((input_sample$yr_smc_sc1 - startyear) * 12 + 1):tmonths
        
        costs$cost_smc[period] <- 
          input_sample$cppsmcsc * simul_epi$totSMCtr_ode[period] + # cost per person to treat
          input_sample$cpdrug_smcsc * 4 * simul_epi$totSMCtr_ode[period]  # cost per drug
      }
      
      # IPTp
      if(input_sample$iptpsc_switch){
        period <- ((input_sample$yr_iptp_sc1 - startyear) * 12 + 1):tmonths
        
        costs$cost_iptp[period] <- 
          input_sample$cppiptsc * simul_epi$totIPTp_predf[period] + # cost per person to treat
          input_sample$cpdrug_iptpsc * simul_epi$totIPTpdose_predf[period]  # cost per drug
      }
    }
    
    costs_all[[i]] <- costs
  }
  
  costs <- rbindlist(costs_all) %>% 
    select_if(is.numeric)
  
  costs <- rbind(
    costs[, lapply(.SD, min), list(times)] %>% mutate(what = "Minimum"),
    costs[, lapply(.SD, quantile, probs = 0.25), list(times)] %>% mutate(what = "Q-25th"),
    costs[, lapply(.SD, median), list(times)] %>% mutate(what = "Median"),
    costs[, lapply(.SD, quantile, probs = 0.75), list(times)] %>% mutate(what = "Q-75th"),
    costs[, lapply(.SD, max), list(times)] %>% mutate(what = "Maximum")) %>%
    mutate(what = factor(what, levels = c("Minimum", "Q-25th", "Median", "Q-75th", "Maximum")))
  
  # Add a "yearly discount" column
  costs <- left_join(costs, discount, by = 'year') %>%
    mutate(discount = replace_na(discount, 1))
  
  # Apply discount and add package name
  costs <-  costs %>% 
    mutate(cost_itn = discount * cost_itn,
           cost_irs = discount * cost_irs,
           cost_inpatient = discount * cost_inpatient,
           cost_outpatient = discount * cost_outpatient,
           cost_vmw = discount * cost_vmw,
           cost_other = discount * cost_other,
           cost_smc = discount * cost_smc, 
           cost_iptp = discount * cost_iptp, 
           cost_acd = discount * cost_acd, 
           cost_ms = discount * cost_ms,
           package = package)
  
  return(costs)
}