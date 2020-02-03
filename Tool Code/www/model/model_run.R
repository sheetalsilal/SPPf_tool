# MODEL RUN FUNCTION
run <- function(initprevf, input, package){
  
  # Define initial conditions ----
  pop_start <- input$initP * ((1+(input$pgrow / 100))^-(2018 - startyear))
  initcondrun <- c(0.5*(1-initprevf/12)*pop_start,
                   0,
                   0.3*initprevf/12*pop_start,
                   0.7*initprevf/12*pop_start,
                   0,
                   0,
                   0,
                   0,
                   0.4*(1-initprevf/12)*pop_start,
                   0.1*(1-initprevf/12)*pop_start,
                   0,
                   0,
                   0,
                   0)
  
  scenario <- c(vmwsc_switch = input$vmwsc_switch,  # VMW
                itnsc_switch = input$itnsc_switch,  # ITN
                irssc_switch = input$irssc_switch,  # IRS
                prim_switch = input$prim_switch,  # Radical Cure
                rcdsc_switch = input$rcdsc_switch,  # Active case detection with house to house fever screening every week
                rcdpf_switch = input$rcdpf_switch,  # Pf only
                rcdMS_switch = input$rcdMS_switch,  # Active case detection with mass screening with highly sensitive RDTs
                rcdMSpf_switch = input$rcdMSpf_switch,  # Pf only
                mda_switch = input$mda_switch, # Mass Drug Administration
                trt_switch = input$trt_switch, # Health System Strengthening
                fail_switch = input$fail_switch,  # Treatment Failure Exploration
                smcsc_switch = input$smcsc_switch,  # Seasonal Malaria Chemoprophylaxis
                iptpsc_switch = input$iptpsc_switch)  # Intermittent Preventative Treatment
  
  if(package == "Baseline") {
    for(i in 1:length(scenario)) scenario[i] <- 0
  }
  
  # Solve the system of ODE
  outoderun <- ode(y = c(initcondrun, 0), 
                   times = seq(0, tyears, by = dtout), 
                   func = epiModel, 
                   parms = input, 
                   method  = "vode", 
                   nun_sens = nun_sens, 
                   scenario = scenario)
  
  # Compute transitions at each time step ----
  tranoderun <- matrix(data = 0, 
                       nrow = length(outoderun[,1]), 
                       ncol = length(transitions))
  
  for (ti in 1:(tsteps + 1)) { 
    tranoderun[ti, ] <- t(malrates(x = outoderun[ti, 2:(1+V)], 
                                   nun_sens = nun_sens, 
                                   input_UI = input, 
                                   t = 0, 
                                   ti = ti, 
                                   scenario = scenario))
  }
  
  
  # Process and return outputs ----
  ppout <- postproc(parpro = input, 
                    out = outoderun, 
                    tran = tranoderun)
  
  return(
    tibble(times = startyear + outoderun[, 1], 
           vmwf = input$reportv/100*ppout[, 1:N],
           hisf = input$report/100*ppout[, (1*N+1):(2*N)],
           fatal = ppout[, (2*N+1):(3*N)],
           severef = input$report/100*ppout[, (3*N+1):(4*N)],
           clinmonthf = ppout[, (4*N+1):(5*N)],
           prevf = ppout[, (5*N+1):(6*N)],
           rcdf = ppout[, (12*N+1):(13*N)],
           rcdvisits = ppout[, (13*N+1):(14*N)],
           rcdsample = ppout[, (14*N+1):(15*N)],
           totMS_predf_ode = ppout[, (15*N+1):(16*N)],
           totMS_sample_ode = ppout[, (16*N+1):(17*N)],
           totSMCtr_ode = ppout[, (17*N+1):(18*N)],
           SMCtrt_predf = ppout[, (18*N+1):(19*N)],
           totIPTp_predf = ppout[, (19*N+1):(20*N)],
           totIPTpdose_predf = ppout[, (20*N+1):(21*N)], 
           repfatal_pred = ppout[, (21*N+1):(22*N)],
           population_pred = ppout[, (22*N+1):(23*N)] ) %>%
      mutate(year = floor(times), 
             trtf = hisf + vmwf + severef, 
             reported = (input$report/100 * hisf) + (input$reportv/100 * vmwf) + severef, 
             package = package)
  )
}