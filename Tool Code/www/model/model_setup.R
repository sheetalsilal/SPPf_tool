# "Expert" parameters ----

N <- 1   # number of patches
B <- 14   # number of variables per patch
A <- 103  # number of transitions per patch
V <- N * B # total number of variables
L <- N * A # total number of transitions

startyear <- 1990 # starting year of simulation
tyears <- 40 # total years of simulation
tmonths <- tyears * 12  # total number of months
dtout <- 1/12 # output timestep
tsteps <- round(tyears / dtout) # number of time steps
time <- startyear + seq(0, tyears, dtout) # time vector

Pmax=0.085 # the maximum populations of mosquitoes in each patch
bites=365.25/3 #no. of bites per mosquito/day
delta_m=365.25/14 # death rate of mosquitoes
#amp=1 # amplitude of seasonal forcing
#phi=10/12 # phase angle of seasonal forcing
#pe = 1 #Peakedness
prob_h=0.5 # probability that a bite will result in infection (mosquito to human)
demog=1/62 # birth/death rate (mu)
#pgrow=0.025 #population growth rate
hl_net=1.5 # half-life of bednets
#   eff_vmw=0.6, # efficacy of VMW
stableveff=0.4 # steady state of vmw_effective
#eff_his=0.2 # efficacy of HIS
eff_oth=0.1 # efficacy of other systems
elneff=21 # smoothing effect of el nino patterns.
#    propgd=0.153, #Proportion of patch that is G6PDdef #cambodia value is 0.153
#    pgd=0.3, #probability of clinical if G6PDdef
# sensgd=0.97, #Sensitivity of test to detect G6PDdef
#    mask=0.05, #probability of vivax mix cases masked as falciparum
rep_fat=0.5


# Humans
#falciparum parameters
gamma_m=365.25/10 # latent period
prob_m=0.5 # probability that a bite will result in infection (human to mosquito)
ps=0.9 # probability of clinical if non-immune
psn=0.1 # probability of sub-patent given not clin, non-immune
pr=0.1 # probability of clinical if immune
prn=0.5 # probability of sub-patent given not clin, immune
nuc=365.25/10 # recovery from clinical symptoms untreated (r_c)
nua=365.25/130 # recovery from asym untreated (r_a)
nus=365.25/5 # recovery from severe (r_s)
psev=0.3 # probability of severe disease given clinical (original 0.03)
pmort=0.7 # proportional of all untreated severe cases that die (theta1)

# moved as input, 2019-12-23
# pmortt=0.0018 # proportional of all treated severe cases that die (theta2)
# tausev=0.8 # probability that a severe infection is treated

gamma_h=365.25/21 # latent period in humans
zeta_a=12.6/27 # relative infectiousness of asym to clinical
zeta_n=3.9/27 # relative infectiousness of submicro to clinical
chi=365.25/28 # rate of loss of detectable HRP2 by RDT
# this will change if the RDT detection limit changes chi=chi_old*(mn_c-dl_RDTold)/(mn_c-dl_RDTnew)
omega=1 # loss of immunity

#control
nut=365.25/3 # recovery rate under treatment (r_t)
nuq=365.25/6 # recovery rate under quinine treatment (r_q)
#ptf=0.05, #Probability of treatment failure
ptfc=0.75 # Probability of being clinical after treatment failure
ptftr=0.27 #Probability of seeking trt if clinical, after treatment failure
# pseek=0.75 #Probability of treatment seeking
# ptest=0.9 #Probability of being tested 
# ptreat=1 # Probability of being treated given positive test
# pseekv=0.75 #Probability of treatment seeking to VMW
# ptestv=0.9 #Probability of being tested by VMW
# ptreatv=1 # Probability of being treated given positive test by VMW
itn_start = 1990 #Baseline starting year of ITN
yrs_itnsc0 = 1 # yrs to scale to current ITN value
irs_start = 1990 #Baseline starting year of IRS
yrs_irssc0 = 1 # yrs to scale to current IRS value
vmw_start = 1990 #Baseline starting year of IRS
yrs_vmwsc0 = 1 # yrs to scale to current IRS value
iptp_seek = 52/4 # start iptp at 4 weeks of pregnancy
nu_ipt1 = 12/1 # period of protection for 1 IPT dose
nu_ipt2 = 12/2 # period of protection for 2 IPT dose
nu_ipt3 = 12/3 # period of protection for 3 IPT dose
nu_ipt4 = 12/4 # period of protection for 4 IPT dose
nu_ipt5 = 12/5 # period of protection for 5 IPT dose


# diagnostics
dl_RDT=log10(200) # standard RDT detection limit
dl_micro=log10(100) # micro detection limit
dl_qPCR=log10(2) # standard PCR detection limit
# dl_inf=log10(26), # detection limit for infectiousness
mn_n=log10(5) # mean parasiteamia for sub micro
mn_a=log10(1000) # mean parasiteamia for asym
mn_c=log10(25000) # mean parasiteamia for clinical
mn_s=log10(350000) # mean parasiteamia for severe
sd_n=0.75 # sd parasiteamia for sub micro
sd_a=1.5 # sd parasiteamia for asym
sd_c=1.3 # sd parasiteamia for clinical
sd_s=0.26 # sd parasiteamia for severe

#RCD parameters
kRCD = 0.017
cRCD = 105
bRCD = 0.024
gRCD = 230
muRCDw=4
sdRCDw=1.5

t1=1                  # Entanglement 1 - dual treatment switch
t2=1                 # Entanglement 2 - triggering relapse from Pf infection switch


nuc <- 365.25/10 # recovery from clinical symptoms untreated (r_c)
nua <- 365.25/130 # recovery from asym untreated (r_a)

# diagnostics
dl_RDT <- log10(200) # standard RDT detection limit
dl_micro <- log10(100) # micro detection limit
dl_qPCR <- log10(2) # standard PCR detection limit
# dl_inf=log10(26), # detection limit for infectiousness
mn_n <- log10(5) # mean parasiteamia for sub micro
mn_a <- log10(1000) # mean parasiteamia for asym
mn_c <- log10(25000) # mean parasiteamia for clinical
mn_s <- log10(350000) # mean parasiteamia for severe
sd_n <- 0.75 # sd parasiteamia for sub micro
sd_a <- 1.5 # sd parasiteamia for asym
sd_c <- 1.3 # sd parasiteamia for clinical
sd_s <- 0.26 # sd parasiteamia for severe


# define the error function
erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1
sens_c_micro<-1-0.5*(1+erf((dl_micro-mn_c)/(sd_c*(2^0.5))))
sens_c_RDT<-1-0.5*(1+erf((dl_RDT-mn_c)/(sd_c*(2^0.5))))

sens_vmw<-sens_c_RDT  # test used by VMW
sens_his<-sens_c_micro # test used by HIS
sens_oth<-1 # test used by other

# detection limits
dl_0 <- dl_qPCR

# durations of infection
nun <- 1/((1/nua)*(mn_n-dl_0)/(mn_a-mn_n)) #(r_n)

# input of several functions
nun_sens <- list(nun = nun,
                 sens_vmw = sens_vmw, 
                 sens_his = sens_his, 
                 sens_oth = sens_oth, 
                 sens_c_RDT = sens_c_RDT, 
                 sens_c_micro = sens_c_micro )



# ************************************************************************************* #
# define variables
# ************************************************************************************* #
# FALCIPARUM
# 1=S: uninfected non-immune
# 2=In: infected submicro
# 3=Ia: infected asymp
# 4=Ic: infected clinical
# 5=Is: infected severe
# 6=To: treated unrecorded
# 7=Tv: treated VMW
# 8=Th: treated HIS
# 9=R: uninfected immune
# 10=H: uninfected and HRP2 positive
# 11=Sp: uninfected non-immune under prophylaxis due to intervention SMC/MDA
# 12=Tp: Infected and Immune on prophylaxis due to intervention SMC/MDA
# 13=Sipt: uninfected non-immune under prophylaxis due to intervention IPTp
# 14=Tipt: Infected and Immune on prophylaxis due to intervention IPTp

falpop <- 1:14



# Define transitions ----
varind <- matrix(seq(1, B*N), nrow = B, ncol = N)
traind <- matrix(seq(1, A*N), nrow = A, ncol = N)

# first transition is given without index
transitions <- adaptivetau::ssa.maketrans(V, rbind(varind[4,1], + 1)) # birth mos patch 1

for (n in 1:N){
  # Falciparum indep flows (1:50)
  transitions[traind[1,n]]  <-  ssa.maketrans(V,rbind(varind[1,n],0,varind[1,n], +1)) # birth humans
  
  transitions[traind[2,n]]  <-  ssa.maketrans(V,rbind(varind[1,n], -1,varind[1,n],0)) # death S=1 
  transitions[traind[3,n]] <- ssa.maketrans(V,rbind(varind[2,n], -1,varind[1,n],0)) # death In=2 
  transitions[traind[4,n]] <- ssa.maketrans(V,rbind(varind[3,n], -1,varind[1,n],0)) # death Ia=3
  transitions[traind[5,n]] <- ssa.maketrans(V,rbind(varind[4,n], -1,varind[1,n],0)) # death Ic=4
  transitions[traind[6,n]] <- ssa.maketrans(V,rbind(varind[5,n], -1,varind[1,n],0)) # death Is=5  
  transitions[traind[7,n]] <- ssa.maketrans(V,rbind(varind[6,n], -1,varind[1,n],0)) # death To=6 
  transitions[traind[8,n]] <- ssa.maketrans(V,rbind(varind[7,n], -1,varind[1,n],0)) # death Tv=7
  transitions[traind[9,n]] <- ssa.maketrans(V,rbind(varind[8,n], -1,varind[1,n],0)) # death Th=8
  transitions[traind[10,n]] <- ssa.maketrans(V,rbind(varind[9,n], -1,varind[1,n],0)) # death R=9
  transitions[traind[11,n]] <- ssa.maketrans(V,rbind(varind[10,n], -1,varind[1,n],0)) # death H=10
  
  transitions[traind[12,n]] <- ssa.maketrans(V,rbind(varind[1,n], -1,varind[2,n],+1)) # incidence S to In
  transitions[traind[13,n]] <- ssa.maketrans(V,rbind(varind[1,n], -1,varind[3,n],+1)) # incidence S to Ia
  transitions[traind[14,n]] <- ssa.maketrans(V,rbind(varind[1,n], -1,varind[4,n],+1)) # incidence S to Ic 
  transitions[traind[15,n]] <- ssa.maketrans(V,rbind(varind[1,n], -1,varind[6,n],+1)) # incidence S to To
  transitions[traind[16,n]] <- ssa.maketrans(V,rbind(varind[1,n], -1,varind[7,n],+1)) # incidence S to Tv
  transitions[traind[17,n]] <- ssa.maketrans(V,rbind(varind[1,n], -1,varind[8,n],+1)) # incidence S to Th
  
  transitions[traind[18,n]] <- ssa.maketrans(V,rbind(varind[5,n], -1, varind[4,n], +1)) # recovery Is to Ic
  transitions[traind[19,n]] <- ssa.maketrans(V,rbind(varind[4,n], -1, varind[3,n], +1)) # recovery Ic to Ia
  transitions[traind[20,n]] <- ssa.maketrans(V,rbind(varind[3,n], -1, varind[2,n], +1)) # recovery Ia to In
  transitions[traind[21,n]] <- ssa.maketrans(V,rbind(varind[2,n], -1, varind[9,n], +1)) # recovery In to R
  transitions[traind[22,n]] <- ssa.maketrans(V,rbind(varind[6,n], -1, varind[10,n], +1)) # recovery To to H
  transitions[traind[23,n]] <- ssa.maketrans(V,rbind(varind[7,n], -1, varind[10,n], +1)) # recovery Tv to H
  transitions[traind[24,n]] <- ssa.maketrans(V,rbind(varind[8,n], -1, varind[10,n], +1)) # recovery Th to H
  transitions[traind[25,n]] <- ssa.maketrans(V,rbind(varind[5,n], -1, varind[10,n], +1)) # recovery Is to H
  
  transitions[traind[26,n]] <- ssa.maketrans(V,rbind(varind[2,n], -1, varind[3,n], +1)) # incidence In to Ia
  transitions[traind[27,n]] <- ssa.maketrans(V,rbind(varind[3,n], -1, varind[4,n], +1)) # incidence Ia to Ic
  transitions[traind[28,n]] <- ssa.maketrans(V,rbind(varind[3,n], -1, varind[6,n], +1)) # incidence Ia to To
  transitions[traind[29,n]] <- ssa.maketrans(V,rbind(varind[3,n], -1, varind[7,n], +1)) # incidence Ia to Tv
  transitions[traind[30,n]] <- ssa.maketrans(V,rbind(varind[3,n], -1, varind[8,n], +1)) # incidence Ia to Th
  transitions[traind[31,n]] <- ssa.maketrans(V,rbind(varind[4,n], -1, varind[5,n], +1)) # incidence Ic to Is
  transitions[traind[32,n]] <- ssa.maketrans(V,rbind(varind[2,n], -1, varind[4,n], +1)) # incidence In to Ic
  transitions[traind[33,n]] <- ssa.maketrans(V,rbind(varind[2,n], -1, varind[6,n], +1)) # incidence In to To
  transitions[traind[34,n]] <- ssa.maketrans(V,rbind(varind[2,n], -1, varind[7,n], +1)) # incidence In to Tv
  transitions[traind[35,n]] <- ssa.maketrans(V,rbind(varind[2,n], -1, varind[8,n], +1)) # incidence In to Th
  transitions[traind[36,n]] <- ssa.maketrans(V,rbind(varind[9,n], -1, varind[2,n], +1)) # incidence R to In 
  transitions[traind[37,n]] <- ssa.maketrans(V,rbind(varind[9,n], -1, varind[3,n], +1)) # incidence R to Ia 
  transitions[traind[38,n]] <- ssa.maketrans(V,rbind(varind[9,n], -1, varind[4,n], +1)) # incidence R to Ic 
  transitions[traind[39,n]] <- ssa.maketrans(V,rbind(varind[9,n], -1, varind[6,n], +1)) # incidence R to To 
  transitions[traind[40,n]] <- ssa.maketrans(V,rbind(varind[9,n], -1, varind[7,n], +1)) # incidence R to Tv
  transitions[traind[41,n]] <- ssa.maketrans(V,rbind(varind[9,n], -1, varind[8,n], +1)) # incidence R to Th
  
  transitions[traind[42,n]] <- ssa.maketrans(V,rbind(varind[9,n], -1, varind[1,n], +1)) # loss imm R to S 
  
  transitions[traind[43,n]] <- ssa.maketrans(V,rbind(varind[10,n], -1, varind[2,n], +1)) # incidence H to In 
  transitions[traind[44,n]] <- ssa.maketrans(V,rbind(varind[10,n], -1, varind[3,n], +1)) # incidence H to Ia
  transitions[traind[45,n]] <- ssa.maketrans(V,rbind(varind[10,n], -1, varind[4,n], +1)) # incidence H to Ic
  transitions[traind[46,n]] <- ssa.maketrans(V,rbind(varind[10,n], -1, varind[6,n], +1)) # incidence H to To
  transitions[traind[47,n]] <- ssa.maketrans(V,rbind(varind[10,n], -1, varind[7,n], +1)) # incidence H to Tv
  transitions[traind[48,n]] <- ssa.maketrans(V,rbind(varind[10,n], -1, varind[8,n], +1)) # incidence H to Th
  transitions[traind[49,n]] <- ssa.maketrans(V,rbind(varind[10,n], -1, varind[9,n], +1)) # loss HRP2 H to  R
  transitions[traind[50,n]] <- ssa.maketrans(V,rbind(varind[5,n], -1, varind[1,n],0)) # death Is fatal malaria
  transitions[traind[51,n]] <- ssa.maketrans(V,rbind(varind[6,n], -1, varind[3,n],+1)) # failed trt To to Ia
  transitions[traind[52,n]] <- ssa.maketrans(V,rbind(varind[6,n], -1, varind[4,n],+1)) # failed trt To to Ic
  transitions[traind[53,n]] <- ssa.maketrans(V,rbind(varind[6,n], -1, varind[8,n],+1)) # failed trt To to Th
  transitions[traind[54,n]] <- ssa.maketrans(V,rbind(varind[7,n], -1, varind[3,n],+1)) # failed trt Tv to Ia
  transitions[traind[55,n]] <- ssa.maketrans(V,rbind(varind[7,n], -1, varind[4,n],+1)) # failed trt Tv to Ic
  transitions[traind[56,n]] <- ssa.maketrans(V,rbind(varind[7,n], -1, varind[7,n],+1)) # failed trt Tv to Tv
  transitions[traind[57,n]] <- ssa.maketrans(V,rbind(varind[8,n], -1, varind[3,n],+1)) # failed trt Th to Ia
  transitions[traind[58,n]] <- ssa.maketrans(V,rbind(varind[8,n], -1, varind[4,n],+1)) # failed trt Th to Ic
  transitions[traind[59,n]] <- ssa.maketrans(V,rbind(varind[8,n], -1, varind[8,n],+1)) # failed trt Th to Th
  
  
  transitions[traind[60,n]] <- ssa.maketrans(V,rbind(varind[2,n], -1, varind[8,n], +1)) # RCD  - Pf additional cases In to Th
  transitions[traind[61,n]] <- ssa.maketrans(V,rbind(varind[3,n], -1, varind[8,n], +1)) # RCD  - Pf additional cases Ia to Th
  transitions[traind[62,n]] <- ssa.maketrans(V,rbind(varind[4,n], -1, varind[8,n], +1)) # RCD  - Pf additional cases Ic to Th
  
  transitions[traind[63,n]] <- ssa.maketrans(V,rbind(varind[4,n], 0, varind[4,n], +1)) # Imported Pf Clinical Cases
  transitions[traind[64,n]] <- ssa.maketrans(V,rbind(varind[3,n], 0, varind[3,n], +1)) # Imported Pf Asymptomatic Cases
  transitions[traind[65,n]] <- ssa.maketrans(V,rbind(varind[2,n], 0, varind[2,n], +1)) # Imported Pf Submicroscopic Cases
  
  transitions[traind[66,n]] <- ssa.maketrans(V,rbind(varind[1,n], -1, varind[1,n], 0)) # Importation Balance
  transitions[traind[67,n]] <- ssa.maketrans(V,rbind(varind[2,n], -1, varind[2,n], 0)) # Importation Balance
  transitions[traind[68,n]] <- ssa.maketrans(V,rbind(varind[3,n], -1, varind[3,n], 0)) # Importation Balance
  transitions[traind[69,n]] <- ssa.maketrans(V,rbind(varind[4,n], -1, varind[4,n], 0)) # Importation Balance
  transitions[traind[70,n]] <- ssa.maketrans(V,rbind(varind[5,n], -1, varind[5,n], 0)) # Importation Balance
  transitions[traind[71,n]] <- ssa.maketrans(V,rbind(varind[6,n], -1, varind[6,n], 0)) # Importation Balance
  transitions[traind[72,n]] <- ssa.maketrans(V,rbind(varind[7,n], -1, varind[7,n], 0)) # Importation Balance
  transitions[traind[73,n]] <- ssa.maketrans(V,rbind(varind[8,n], -1, varind[8,n], 0)) # Importation Balance
  transitions[traind[74,n]] <- ssa.maketrans(V,rbind(varind[9,n], -1, varind[9,n], 0)) # Importation Balance
  transitions[traind[75,n]] <- ssa.maketrans(V,rbind(varind[10,n], -1, varind[10,n], 0)) # Importation Balance
  transitions[traind[76,n]] <- ssa.maketrans(V,rbind(varind[10,n], 0, varind[10,n], 0)) # counter - number of ACD visits 
  transitions[traind[77,n]] <- ssa.maketrans(V,rbind(varind[10,n], 0, varind[10,n], 0)) # counter - ACD sample assessed
  transitions[traind[78,n]] <- ssa.maketrans(V,rbind(varind[2,n], -1, varind[8,n], +1)) # MS  - Pf additional cases In to Th
  transitions[traind[79,n]] <- ssa.maketrans(V,rbind(varind[3,n], -1, varind[8,n], +1)) # MS  - Pf additional cases Ia to Th
  transitions[traind[80,n]] <- ssa.maketrans(V,rbind(varind[4,n], -1, varind[8,n], +1)) # MS  - Pf additional cases Ic to Th
  transitions[traind[81,n]] <- ssa.maketrans(V,rbind(varind[10,n], 0, varind[10,n], 0)) # counter - MS sample assessed
  transitions[traind[82,n]] <- ssa.maketrans(V,rbind(varind[11,n], -1, varind[1,n], 0)) # death Sp = 11
  transitions[traind[83,n]] <- ssa.maketrans(V,rbind(varind[12,n], -1, varind[1,n], 0)) # death Tp = 12
  transitions[traind[84,n]] <- ssa.maketrans(V,rbind(varind[1,n], -1, varind[11,n], +1)) # SMC S to Sp
  transitions[traind[85,n]] <- ssa.maketrans(V,rbind(varind[2,n], -1, varind[12,n], +1)) # SMC In to Th
  transitions[traind[86,n]] <- ssa.maketrans(V,rbind(varind[3,n], -1, varind[12,n], +1)) # SMC Ia to Th
  transitions[traind[87,n]] <- ssa.maketrans(V,rbind(varind[4,n], -1, varind[12,n], +1)) # SMC Ic to Th
  transitions[traind[88,n]] <- ssa.maketrans(V,rbind(varind[9,n], -1, varind[12,n], +1)) # SMC R to Th
  transitions[traind[89,n]] <- ssa.maketrans(V,rbind(varind[10,n], -1, varind[12,n], +1)) # SMC H to Th
  transitions[traind[90,n]] <- ssa.maketrans(V,rbind(varind[11,n], -1, varind[1,n], +1)) # SMC recovery Sp to S
  transitions[traind[91,n]] <- ssa.maketrans(V,rbind(varind[12,n], -1, varind[10,n], +1)) # SMC Tp to H
  transitions[traind[92,n]] <- ssa.maketrans(V,rbind(varind[1,n], -1, varind[13,n], +1)) # IPTp S to Sipt
  transitions[traind[93,n]] <- ssa.maketrans(V,rbind(varind[2,n], -1, varind[14,n], +1)) # IPTp In to Tipt
  transitions[traind[94,n]] <- ssa.maketrans(V,rbind(varind[3,n], -1, varind[14,n], +1)) # IPTp Ia to Tipt
  transitions[traind[95,n]] <- ssa.maketrans(V,rbind(varind[4,n], -1, varind[14,n], +1)) # IPTp Ic to Tipt
  transitions[traind[96,n]] <- ssa.maketrans(V,rbind(varind[9,n], -1, varind[14,n], +1)) # IPTp R to Tipt
  transitions[traind[97,n]] <- ssa.maketrans(V,rbind(varind[10,n], -1, varind[14,n], +1)) # IPTp H to Tipt
  transitions[traind[98,n]] <- ssa.maketrans(V,rbind(varind[13,n], -1, varind[1,n], +1)) # IPTp recovery Sipt to S
  transitions[traind[99,n]] <- ssa.maketrans(V,rbind(varind[14,n], -1, varind[10,n], +1)) # IPTp Tipt to H
  transitions[traind[100,n]] <- ssa.maketrans(V,rbind(varind[13,n], -1, varind[1,n], 0)) # death Sipt = 11
  transitions[traind[101,n]] <- ssa.maketrans(V,rbind(varind[14,n], -1, varind[10,n], 0)) # death Tipt = 11
  transitions[traind[102,n]] <- ssa.maketrans(V,rbind(varind[13,n], 0, varind[10,n], 0)) # Counter: no.of doses IPTp
  transitions[traind[103,n]] <- ssa.maketrans(V,rbind(varind[5,n], 0, varind[1,n],0)) # reported fatalities from malaria
}


# Alternative formulation of transitions matrix (used in epimodel function)
transitions_alt <- NULL
for (i in 1: length(transitions)){
  transitions_alt <- rbind(transitions_alt,
                           cbind(as.integer(names(transitions[[i]]))[1],
                                 as.integer(names(transitions[[i]]))[2], 
                                 transitions[[i]][1], 
                                 transitions[[i]][2]))
}
transitionsiu1 <- transitions_alt[, 1]
transitionsiu2 <- transitions_alt[, 2]
transitionsiv1 <- transitions_alt[, 3]
transitionsiv2 <- transitions_alt[, 4]


# malrates function, compute transitions at each time step ----
malrates <- function(x, nun_sens, input_UI, t, ti, scenario) {
  # with(reactiveValuesToList(input_UI),
       with(input_UI,
       {
         t_internal <- (ti - 1) * dtout + t + startyear
         #Set up matrices
         seas<-c(rep(1,N)) # seasonality
         popf<-c(rep(0,N))    # population sizes  
         foif<-lamf<-lamfc<-c(rep(0,N))     # forces of infection falciparum
         tranrate<-matrix(0,nrow=N,ncol=A)   # transition rate matrix
         c_vmw<-c(rep(0,N)) #VMW coverage
         veff<-heff<-c(rep(0,N)) #VMW effect decline
         tau<-c(rep(0,N)) #Treatment probabilty
         tauo<-c(rep(0,N)) #Treatment with other
         tauh<-c(rep(0,N)) #Treatment with HIS
         tauv<-c(rep(0,N)) #Treatment with VMW
         
         # Pmaxf = 0.001*APIf*initP
         #  Pmaxv = 0.001*APIv*initP
         # bites=365.25/bh_max
         # vmw_cov = vmw_cov/100
         pgrow = pgrow/100
         phi = phi/12 # divide by total months
         pseek = pseek/100
         ptest = ptest/100
         ptreat = ptreat/100
         vmw_cov12 = vmw_cov12/100
         vmw_cov13 = vmw_cov13/100
         vmw_cov14 = vmw_cov14/100
         vmw_cov15 = vmw_cov15/100
         vmw_cov16 = vmw_cov16/100
         vmw_cov17 = vmw_cov17/100
         vmw_cov18 = vmw_cov18/100
         pseekv = pseekv/100
         ptestv = ptestv/100
         ptreatv = ptreatv/100
         reportdeath = reportdeath/100
         # itn_cov = itn_cov/100
         itn_cov12 = itn_dis12/initP*net_multiplier
         itn_cov13 = itn_dis13/initP*net_multiplier
         itn_cov14 = itn_dis14/initP*net_multiplier
         itn_cov15 = itn_dis15/initP*net_multiplier
         itn_cov16 = itn_dis16/initP*net_multiplier
         itn_cov17 = itn_dis17/initP*net_multiplier
         itn_cov18 = itn_dis18/initP*net_multiplier
         itn_aversion = itn_aversion/100
         irs_cov12 = irs_cov12/100
         irs_cov13 = irs_cov13/100
         irs_cov14 = irs_cov14/100
         irs_cov15 = irs_cov15/100
         irs_cov16 = irs_cov16/100
         irs_cov17 = irs_cov17/100
         irs_cov18 = irs_cov18/100 
         irs_aversion = irs_aversion/100
         mon_smc = mon_smc/12
         nu_smc = 12/dur_smc
         smc_cov12 = smc_cov12/100
         smc_cov13 = smc_cov13/100
         smc_cov14 = smc_cov14/100
         smc_cov15 = smc_cov15/100
         smc_cov16 = smc_cov16/100
         smc_cov17 = smc_cov17/100
         smc_cov18 = smc_cov18/100
         fert_rate = fert_rate/1000
         anc_rate = anc_rate/100
         iptp1_cov = iptp1_cov/100
         iptp2_cov = iptp2_cov/100
         iptp3_cov = iptp3_cov/100
         iptp4_cov = iptp4_cov/100
         iptp5_cov = iptp5_cov/100
         
         
         itnsc_cov1 = itnsc_cov1/100
         itnsc_eff1 = itnsc_eff1/100
         itnsc_cov2 = itnsc_cov2/100
         itnsc_eff2 = itnsc_eff2/100
         irssc_cov1 = irssc_cov1/100
         irssc_eff1 = irssc_eff1/100
         irssc_cov2 = irssc_cov2/100
         irssc_eff2 = irssc_eff2/100
         vmwsc_cov1 = vmwsc_cov1/100
         vmwsc_cov2 = vmwsc_cov2/100
         smc_covsc1 = smc_covsc1/100
         smc_covsc2 = smc_covsc2/100
         iptp1_covsc1 = iptp1_covsc1/100
         iptp2_covsc1 = iptp2_covsc1/100
         iptp3_covsc1 = iptp3_covsc1/100
         iptp4_covsc1 = iptp4_covsc1/100
         iptp5_covsc1 = iptp5_covsc1/100
         iptp1_covsc2 = iptp1_covsc2/100
         iptp2_covsc2 = iptp2_covsc2/100
         iptp3_covsc2 = iptp3_covsc2/100
         iptp4_covsc2 = iptp4_covsc2/100
         iptp5_covsc2 = iptp5_covsc2/100
         
         
         
         
         pseekv1 = pseekv1/100
         ptestv1 = ptestv1/100
         ptreatv1 = ptreatv1/100
         pseekv2 = pseekv2/100
         ptestv2 = ptestv2/100
         ptreatv2 = ptreatv2/100
         res_base = res_base/100
         ressc2018 = ressc2018/100
         ressc2019 = ressc2019/100
         ressc2020 = ressc2020/100
         pseek1 = pseek1/100
         ptest1 = ptest1/100
         ptreat1 = ptreat1/100
         pseek2 = pseek2/100
         ptest2 = ptest2/100
         ptreat2 = ptreat2/100
         
         muC = muC/1000   #imported rate per 1000 
         muA = muA/1000
         muU = muU/1000
         mu_out = muC+muA+muU #cumulative imported rate for balance
         
         
         # covRCDi = covRCDi/100
         clustRCDcoex = clustRCDcoex/100
         #  covRCDi = pnorm(thresh_inc,minc,sdinc)
         covRCDi=1
         #  propinv = propinv/100
         mcov = mcov/100
         mcov2 = mcov2/100
         rcdfsensC = rcdfsensC/100
         rcdfsensA = rcdfsensA/100
         rcdfsensN = rcdfsensN/100
         
         ptf08 = ptf08/100
         ptf09 = ptf09/100
         ptf10 = ptf10/100
         ptf11 = ptf11/100
         ptf12 = ptf12/100
         ptf13 = ptf13/100
         ptf14 = ptf14/100
         ptf15 = ptf15/100
         ptf16 = ptf16/100
         ptf17 = ptf17/100
         ptf18 = ptf18/100
         ptf20 = ptf20/100
         ptf22 = ptf22/100
         ptf24 = ptf24/100
         ptf26 = ptf26/100
         ptf28 = ptf28/100
         
         
         #  res_switch = as.integer(res_switch)
         # ressc_switch = as.integer(ressc_switch)
         
         itn_scale0<-1;itnscapp0<-itnscapp1<-0;itnscapp2<-0
         for (i in 1:36) { itn_scale0 <-c(itn_scale0, 0.985*itn_scale0[length(itn_scale0)])}
         itn_scale<-rep(itn_scale0,round((tyears+1)/3))
         if ((t_internal>=2018)){
           itnscapp0<-approx(seq(2018,startyear+tyears+2/12,1/12), itn_scale[1:length(seq(2018,startyear+tyears+2/12,1/12))],t_internal)$y
         }
         if ((itnsc_switch==1)&(t_internal>=(itnsc_start1+yrs_itnsc1))&(t_internal<(itnsc_start2))){
           itnscapp1<-approx(seq((itnsc_start1+yrs_itnsc1),itnsc_start2,1/12), itn_scale[1:length(seq((itnsc_start1+yrs_itnsc1),itnsc_start2,1/12))],t_internal)$y
         }
         if ((itnsc_switch==1)&(t_internal>=(itnsc_start2+yrs_itnsc2))){
           itnscapp2<-approx(seq((itnsc_start2+yrs_itnsc2),startyear+tyears+2/12,1/12), itn_scale[1:length(seq((itnsc_start2+yrs_itnsc2),startyear+tyears+2/12,1/12))],t_internal)$y
         }
         
         
         itn<-(t_internal>=itn_start)*(t_internal< (itn_start+yrs_itnsc0))*(t_internal*(itn_cov12)/yrs_itnsc0 -(itn_cov12)/yrs_itnsc0*itn_start)*itn_aversion+(t_internal>=(itn_start+yrs_itnsc0))*itn_cov12*itn_aversion +
           (t_internal>=2012)*(t_internal<2013)*itn_cov12*itn_aversion  +
           (t_internal>=2013)*(t_internal<2014)*itn_cov13*itn_aversion  +
           (t_internal>=2014)*(t_internal<2015)*itn_cov14*itn_aversion  +
           (t_internal>=2015)*(t_internal<2016)*itn_cov15*itn_aversion  +
           (t_internal>=2016)*(t_internal<2017)*itn_cov16*itn_aversion  +
           (t_internal>=2017)*(t_internal<2018)*itn_cov17*itn_aversion  +
           (t_internal>=2018)*(1-itnsc_switch)*itn_cov18*itn_aversion*itnscapp0  + 
           itnsc_switch*((t_internal>=2018)*(t_internal<itnsc_start1)*itn_cov18*itnscapp0*itn_aversion +
                           (t_internal>=itnsc_start1)*(t_internal<(itnsc_start1+yrs_itnsc1))*(t_internal*((itnsc_cov1 - itn_cov18)/yrs_itnsc1) + itn_cov18 - ((itnsc_cov1 - itn_cov18)/yrs_itnsc1)*itnsc_start1)*itnsc_eff1 +
                           (t_internal>=(itnsc_start1+yrs_itnsc1))*(t_internal<(itnsc_start2))*itnsc_cov1*itnscapp1*itnsc_eff1 +
                           (t_internal>=itnsc_start2)*(t_internal<(itnsc_start2+yrs_itnsc2))*(t_internal*((itnsc_cov2 - itnsc_cov1)/yrs_itnsc2) + itnsc_cov1 - ((itnsc_cov2 - itnsc_cov1)/yrs_itnsc2)*itnsc_start2)*itnsc_eff2 +
                           (t_internal>=(itnsc_start2+yrs_itnsc2))*itnsc_cov2*itnscapp2*itnsc_eff2
           )
         
         
         
         irs<-(t_internal>=irs_start)*(t_internal< (irs_start+yrs_irssc0))*(t_internal*(irs_cov12)/yrs_irssc0 -(irs_cov12)/yrs_irssc0*irs_start)*irs_aversion+(t_internal>=(irs_start+yrs_irssc0))*irs_cov12*irs_aversion + 
           (t_internal>=2012)*(t_internal<2013)*irs_cov12*irs_aversion  +
           (t_internal>=2013)*(t_internal<2014)*irs_cov13*irs_aversion  +
           (t_internal>=2014)*(t_internal<2015)*irs_cov14*irs_aversion  +
           (t_internal>=2015)*(t_internal<2016)*irs_cov15*irs_aversion  +
           (t_internal>=2016)*(t_internal<2017)*irs_cov16*irs_aversion  +
           (t_internal>=2017)*(t_internal<2018)*irs_cov17*irs_aversion  +
           (t_internal>=2018)*(1-irssc_switch)*irs_cov18*irs_aversion  + 
           irssc_switch*((t_internal>=2018)*(t_internal<irssc_start1)*irs_cov18*irs_aversion +
                           (t_internal>=irssc_start1)*(t_internal<(irssc_start1+yrs_irssc1))*(t_internal*((irssc_cov1 - irs_cov18)/yrs_irssc1) +irs_cov18 - ((irssc_cov1 - irs_cov18)/yrs_irssc1)*irssc_start1)*irssc_eff1 +
                           (t_internal>=(irssc_start1+yrs_irssc1))*(t_internal<irssc_start2)*irssc_cov1*irssc_eff1 +
                           (t_internal>=irssc_start2)*(t_internal<(irssc_start2+yrs_irssc2))*(t_internal*((irssc_cov2 - irssc_cov1)/yrs_irssc2) +irssc_cov1 - ((irssc_cov2 - irssc_cov1)/yrs_irssc2)*irssc_start2)*irssc_eff2 +
                           (t_internal>=(irssc_start2+yrs_irssc2))*irssc_cov2*irssc_eff2
                         
           )
         cov_vmw<-(t_internal>=vmw_start)*(t_internal< (vmw_start+yrs_vmwsc0))*(t_internal*(vmw_cov12)/yrs_vmwsc0 -(vmw_cov12)/yrs_vmwsc0*vmw_start)+(t_internal>=(vmw_start+yrs_vmwsc0))*vmw_cov12 + 
           (t_internal>=2012)*(t_internal<2013)*vmw_cov12  +
           (t_internal>=2013)*(t_internal<2014)*vmw_cov13  +
           (t_internal>=2014)*(t_internal<2015)*vmw_cov14  +
           (t_internal>=2015)*(t_internal<2016)*vmw_cov15  +
           (t_internal>=2016)*(t_internal<2017)*vmw_cov16  +
           (t_internal>=2017)*(t_internal<2018)*vmw_cov17  +
           (t_internal>=2018)*(1- scenario[1])*vmw_cov18  +
           scenario[1]*((t_internal>=2018)*(t_internal<vmwsc_start1)*vmw_cov18 + 
                          (t_internal>=vmwsc_start1)*(t_internal<(vmwsc_start1+yrs_vmwsc1))*(t_internal*((vmwsc_cov1 - vmw_cov18)/yrs_vmwsc1) +vmw_cov18 - ((vmwsc_cov1 - vmw_cov18)/yrs_vmwsc1)*vmwsc_start1) +
                          (t_internal>=(vmwsc_start1+yrs_vmwsc1))*(t_internal<vmwsc_start2)*vmwsc_cov1 +
                          (t_internal>=vmwsc_start2)*(t_internal<(vmwsc_start2+yrs_vmwsc2))*(t_internal*((vmwsc_cov2 - vmwsc_cov1)/yrs_vmwsc2) +vmwsc_cov1 - ((vmwsc_cov2 - vmwsc_cov1)/yrs_vmwsc2)*vmwsc_start2) +
                          (t_internal>=(vmwsc_start2+yrs_vmwsc2))*vmwsc_cov2 
           )
         
         
         for (n in 1:N){
           seas<-1+amp*cos(2*pi*(t_internal-phi))^pe # seasonal forcing signal
           popf[n]<-sum(x[varind[falpop,n]]) # list of the variable indices for the human population
           
           #bites ^2 check
           lamf[n]<-seas*(bites*prob_m*prob_h*tranp*(zeta_n*x[varind[2,n]]+zeta_a*x[varind[3,n]]+x[varind[4,n]]+x[varind[5,n]])/popf)/(prob_h*tranp+delta_m)*(gamma_m/(gamma_m+delta_m))
           
           lamfc[n]<-(1-itn[n])*(1-irs[n])*lamf[n]
           
           # foif[n]<-seas*(((1-itn[n])*bites)^2*prob_m*prob_h*((1-irs[n])*Pmaxf)/popf[n]*(zeta_n*x[varind[2,n]]+zeta_a*x[varind[3,n]]+x[varind[4,n]]+x[varind[5,n]])/popf)/((1-itn[n])*bites*prob_h*(1-irs[n])*Pmaxf/popf[n]+delta_m)*(gamma_m/(gamma_m+delta_m))
           # foiv[n]<-seas*(((1-itn[n])*bites)^2*prob_m*vprob_h*((1-irs[n])*Pmaxv)/popv[n]*(vzeta_n*x[varind[12,n]]+vzeta_a*x[varind[13,n]]+x[varind[14,n]]+x[varind[15,n]])/popv)/((1-itn[n])*bites*vprob_h*(1-irs[n])*Pmaxv/popv[n]+delta_m)*(vgamma_m/(vgamma_m+delta_m))
           
           foif[n]<-lamfc[n]
           
           foifa<-(1/(1/(foif[n])+1/gamma_h+1/gamma_m))
           
           
           c_vmw[n]<-cov_vmw
           vmw_eff =pseekv*ptestv*ptreatv
           vmwsc_eff1 = pseekv1*ptestv1*ptreatv1
           vmwsc_eff2 = pseekv2*ptestv2*ptreatv2
           veff[n]<-(scenario[10]==0)*vmw_eff+(scenario[10]==1)*(
             (t_internal<hsssc_start1)*vmw_eff+
               (t_internal>=hsssc_start1)*(t_internal<(hsssc_start1+yrs_hsssc1))*(t_internal*((vmwsc_eff1 - vmw_eff)/yrs_hsssc1) +vmw_eff - ((vmwsc_eff1 - vmw_eff)/yrs_hsssc1)*hsssc_start1) +
               (t_internal>=(hsssc_start1+yrs_hsssc1))*(t_internal<hsssc_start2)*vmwsc_eff1 +
               (t_internal>=hsssc_start2)*(t_internal<(hsssc_start2+yrs_hsssc2))*(t_internal*((vmwsc_eff2 - vmwsc_eff1)/yrs_hsssc2) +vmwsc_eff1 - ((vmwsc_eff2 - vmwsc_eff1)/yrs_hsssc2)*hsssc_start2) +
               (t_internal>=hsssc_start2+yrs_hsssc2)*vmwsc_eff2)
           
           his_eff<-pseek*ptest*ptreat 
           hissc_eff1<-pseek1*ptest1*ptreat1 
           hissc_eff2<-pseek2*ptest2*ptreat2 
           heff[n]<-(scenario[10]==0)*his_eff+(scenario[10]==1)*(
             (t_internal<hsssc_start1)*his_eff+
               (t_internal>=hsssc_start1)*(t_internal<(hsssc_start1+yrs_hsssc1))*(t_internal*((hissc_eff1 - his_eff)/yrs_hsssc1) +his_eff - ((hissc_eff1 - his_eff)/yrs_hsssc1)*hsssc_start1) +
               (t_internal>=hsssc_start1+yrs_hsssc1)*(t_internal<hsssc_start2)*hissc_eff1 +
               (t_internal>=hsssc_start2)*(t_internal<(hsssc_start2+yrs_hsssc2))*(t_internal*((hissc_eff2 - hissc_eff1)/yrs_hsssc2) +hissc_eff1 - ((hissc_eff2 - hissc_eff1)/yrs_hsssc2)*hsssc_start2) +
               (t_internal>=hsssc_start2+yrs_hsssc2)*hissc_eff2)
           
           tau[n]<-c_vmw[n]*nun_sens$sens_vmw*veff[n]+(1-c_vmw[n]*veff[n])*nun_sens$sens_his*heff[n]+(1-c_vmw[n]*veff[n]-(1-c_vmw[n]*veff[n])*heff[n])*nun_sens$sens_oth*eff_oth #prob of treatment
           tauo[n]<-(1-c_vmw[n]*veff[n]-(1-c_vmw[n]*veff[n])*heff[n])*nun_sens$sens_oth*eff_oth #Prob treated by Other
           tauh[n]<-(1-c_vmw[n]*veff[n])*nun_sens$sens_his*heff[n]#Prob treated by HIS
           tauv[n]<-c_vmw[n]*veff[n]*nun_sens$sens_vmw  #Prob treated by VMW
           
           #primaquine
           t3=0
           #    if (t_internal>yr_primstart & prim_switch==T){t3=1} #start radical cure from policy start date
           
           #increased resistance
           ptf=(1-fail_switch)*(
             (t_internal<2017)*res_base
             +(t_internal>=2017)*(t_internal<2018)*((ressc2018 - res_base)*t_internal+res_base-(ressc2018-res_base)*2017)
             +(t_internal>=2018)*(t_internal<2019)*((ressc2019-ressc2018)*t_internal+ressc2018-(ressc2019-ressc2018)*2018)
             +(t_internal>=2019)*(t_internal<2020)*((ressc2020 -ressc2019)*t_internal+ressc2019-(ressc2020-ressc2019)*2019)
             +(t_internal>=2020)*ressc2020
           )+ fail_switch*(
             (t_internal<2008)*res_base
             +(t_internal>=2008)*(t_internal<2009)*ptf08
             +(t_internal>=2009)*(t_internal<2010)*ptf09
             +(t_internal>=2010)*(t_internal<2011)*ptf10
             +(t_internal>=2011)*(t_internal<2012)*ptf11
             +(t_internal>=2012)*(t_internal<2013)*ptf12
             +(t_internal>=2013)*(t_internal<2014)*ptf13
             +(t_internal>=2014)*(t_internal<2015)*ptf14
             +(t_internal>=2015)*(t_internal<2016)*ptf15
             +(t_internal>=2016)*(t_internal<2017)*ptf16
             +(t_internal>=2017)*(t_internal<2018)*ptf17
             +(t_internal>=2018)*(t_internal<2020)*ptf18
             +(t_internal>=2020)*(t_internal<2022)*ptf20
             +(t_internal>=2022)*(t_internal<2024)*ptf22
             +(t_internal>=2024)*(t_internal<2026)*ptf24
             +(t_internal>=2026)*(t_internal<2028)*ptf26
             +(t_internal>=2028)*ptf28)
           
           
           #Reactive Case Detection  
           #Incf and Incv are passively treated pop 
           incf<-covRCDi*(ps*(tauh[n]+tauv[n])*foifa*x[varind[1,n]]+pr*(tauh[n]+tauv[n])*foifa*(x[varind[2,n]]+x[varind[3,n]]+x[varind[9,n]]+x[varind[10,n]]))  # new clinical treated infections from S, In, Ia, R, H)
           
           
           # with clustering  linear decrease that stays at 0 for all inc/mvis>P/ss - i.e. 0 cluster for MSAT
           clus_func <- (((incf)/mvis)<initP/ss)*clustRCDcoex*ss/initP*((incf)/mvis)+clustRCDcoex
           
           
           rateRCD<- (rcdsc_switch*(t_internal>=yr_rcdscstart)*
                        (((incf)<mvis)*min((incf)*ss/initP*(1+clus_func),mcov)+
                           ((incf)>=mvis)*(mvis*min((incf)/mvis*ss/initP*(1+clus_func),mcov))))   # act on all cases if inc is less than max visits. cov=inc*sample coverage
           tauRCD<- rcdsc_switch*rateRCD
           
           no_visits<-(rcdsc_switch*(t_internal>=yr_rcdscstart)*
                         (((incf)<mvis)*(incf)+
                            ((incf)>=mvis)*mvis))
           sample_rcd<-(rcdsc_switch*(t_internal>=yr_rcdscstart)*
                          (((incf)<mvis)*min((incf)*ss,mcov*initP)+
                             ((incf)>=mvis)*(mvis*min((incf)/mvis*ss,mcov*initP))))
           
           #AMS
           rateRCDMS<- (rcdMS_switch*(t_internal>=yr_rcdMSstart)*
                          (((incf)<mvis2)*mcov2+
                             ((incf)>=mvis2)*(mvis2*mcov2)))   # act on all cases if inc is less than max visits. cov=inc*sample coverage
           tauMS<-rateRCDMS
           
           sample_MS<-(rcdMS_switch*(t_internal>=yr_rcdMSstart)*
                         (((incf)<mvis2)*mcov2*initP+
                            ((incf)>=mvis2)*(mvis2*mcov2*initP)))   # act on all cases if inc is less than max visits. cov=inc*sample coverage
           
           #SMC
           covSMC<-(t_internal>=(2012+mon_smc))*(t_internal<(2012+mon_smc+1/12))*smc_cov12  +
             (t_internal>=(2013+mon_smc))*(t_internal<(2013+mon_smc+1/12))*smc_cov13  +
             (t_internal>=(2014+mon_smc))*(t_internal<(2014+mon_smc+1/12))*smc_cov14  +
             (t_internal>=(2015+mon_smc))* (t_internal<(2015+mon_smc+1/12))*smc_cov15  +
             (t_internal>=(2016+mon_smc))*(t_internal<(2016+mon_smc+1/12))*smc_cov16  +
             (t_internal>=(2017+mon_smc))*(t_internal<(2017+mon_smc+1/12))*smc_cov17  +
             (t_internal>=(2018+mon_smc))*(t_internal<(2018+mon_smc+1/12))*smc_cov18 + (1-scenario[12])*(t_internal>=2019)*(t_internal>=(floor(t_internal)+mon_smc))*(t_internal<(floor(t_internal)+mon_smc+1/12))*smc_cov18 +
             scenario[12]*((t_internal>=2019)*(t_internal<yr_smc_sc1)*(t_internal==(floor(t_internal)+mon_smc))*smc_cov18 +
                             (t_internal>=yr_smc_sc1)*(t_internal<yr_smc_sc2)*(t_internal>=(floor(t_internal)+mon_smc))*(t_internal<(floor(t_internal)+mon_smc+1/12))*smc_covsc1 +
                             (t_internal>=yr_smc_sc2)*(t_internal>=(floor(t_internal)+mon_smc))*(t_internal<(floor(t_internal)+mon_smc+1/12))*smc_covsc2 
             )
           tauSMC<-(-log(1-covSMC)/(1/12))
           
           #IPTp
           covIPTp<-fert_rate*anc_rate*((1-scenario[13])*((t_internal>yr_iptp_st)*iptp1_cov) +
                                          scenario[13]*((t_internal>yr_iptp_st)*(t_internal<yr_iptp_sc1)*iptp1_cov + (t_internal>=yr_iptp_sc1)*(t_internal<yr_iptp_sc2)*iptp1_covsc1 + (t_internal>=yr_iptp_sc2)*iptp1_covsc2  
                                          ) 
           ) #fertile, who go to anc, and participate in IPTp
           tauIPTp<-(-log(1-covIPTp)/(12/12))
           
           prop_ipt1=(1-scenario[13])*(t_internal>yr_iptp_st)*(iptp1_cov - iptp2_cov)/iptp1_cov + 
             scenario[13]*((t_internal>yr_iptp_st)*(t_internal<yr_iptp_sc1)*(iptp1_cov - iptp2_cov)/iptp1_cov +
                             (t_internal>=yr_iptp_sc1)*(t_internal<yr_iptp_sc2)*(iptp1_covsc1 - iptp2_covsc1)/iptp1_covsc1 + 
                             (t_internal>=yr_iptp_sc2)*(iptp1_covsc2 - iptp2_covsc2)/iptp1_covsc2
             )
           prop_ipt2=(1-scenario[13])*(t_internal>yr_iptp_st)*(iptp2_cov - iptp3_cov)/iptp1_cov + 
             scenario[13]*((t_internal>yr_iptp_st)*(t_internal<yr_iptp_sc1)*(iptp2_cov - iptp3_cov)/iptp1_cov +
                             (t_internal>=yr_iptp_sc1)*(t_internal<yr_iptp_sc2)*(iptp2_covsc1 - iptp3_covsc1)/iptp1_covsc1 + 
                             (t_internal>=yr_iptp_sc2)*(iptp2_covsc2 - iptp3_covsc2)/iptp1_covsc2
             )
           
           prop_ipt3=(1-scenario[13])*(t_internal>yr_iptp_st)*(iptp3_cov - iptp4_cov)/iptp1_cov + 
             scenario[13]*((t_internal>yr_iptp_st)*(t_internal<yr_iptp_sc1)*(iptp3_cov - iptp4_cov)/iptp1_cov +
                             (t_internal>=yr_iptp_sc1)*(t_internal<yr_iptp_sc2)*(iptp3_covsc1 - iptp4_covsc1)/iptp1_covsc1 + 
                             (t_internal>=yr_iptp_sc2)*(iptp3_covsc2 - iptp4_covsc2)/iptp1_covsc2
             )
           
           prop_ipt4=(1-scenario[13])*(t_internal>yr_iptp_st)*(iptp4_cov - iptp5_cov)/iptp1_cov + 
             scenario[13]*((t_internal>yr_iptp_st)*(t_internal<yr_iptp_sc1)*(iptp4_cov - iptp5_cov)/iptp1_cov +
                             (t_internal>=yr_iptp_sc1)*(t_internal<yr_iptp_sc2)*(iptp4_covsc1 - iptp5_covsc1)/iptp1_covsc1 + 
                             (t_internal>=yr_iptp_sc2)*(iptp4_covsc2 - iptp5_covsc2)/iptp1_covsc2
             )
           
           prop_ipt5=(1-scenario[13])*(t_internal>yr_iptp_st)*iptp5_cov/iptp1_cov + 
             scenario[13]*((t_internal>yr_iptp_st)*(t_internal<yr_iptp_sc1)*iptp5_cov/iptp1_cov +
                             (t_internal>=yr_iptp_sc1)*(t_internal<yr_iptp_sc2)*iptp5_covsc1/iptp1_covsc1 + 
                             (t_internal>=yr_iptp_sc2)*iptp5_covsc2/iptp1_covsc2
             )
           
           pgrowc<-pgrow/demog
           
           tranrate[n,]<-c(      #falciparum
             demog*(1+pgrowc)*popf[n] + (1-tausev)*pmort*nuq*x[varind[5,n]]+tausev*nuq*pmortt*x[varind[5,n]] , # rate of birth 1
             demog*x[varind[1,n]],       # rate of death of S                                      2
             demog*x[varind[2,n]],       # rate of death of In                                     3
             demog*x[varind[3,n]],       # rate of death of Ia                                     4
             demog*x[varind[4,n]],       # rate of death of Ic                                     5
             demog*x[varind[5,n]],       # rate of death of Is                                     6
             demog*x[varind[6,n]],       # rate of death of To                                     7
             demog*x[varind[7,n]],       # rate of death of Tv                                     8
             demog*x[varind[8,n]],       # rate of death of Th                                     9
             demog*x[varind[9,n]],       # rate of death of R                                      10
             demog*x[varind[10,n]],       # rate of death of H                                     11
             psn*(1-ps)*foifa*x[varind[1,n]], #incidence S to In               12
             (1-psn)*(1-ps)*foifa*x[varind[1,n]],  #      incidence S to Ia    13
             (1-tau[n])*ps*foifa*x[varind[1,n]],          #      incidence S to Ic    14
             tauo[n]*ps*foifa*x[varind[1,n]],      #             incidence S to To    15
             tauv[n]*ps*foifa*x[varind[1,n]],      #             incidence S to Tv    16
             tauh[n]*ps*foifa*x[varind[1,n]],      #             incidence S to Th    17
             (1-tausev)*(1-pmort)*nuq*x[varind[5,n]],             #recovery Is to Ic    18
             (1-psev)*nuc*x[varind[4,n]],      # recovery Ic to Ia                         19
             nua*x[varind[3,n]],      # recovery Ia to In                                         20
             nun_sens$nun*x[varind[2,n]],      # recovery In to R                                           21
             (1-ptf)*nut*x[varind[6,n]],      # recovery To to H                                          22
             (1-ptf)*nut*x[varind[7,n]],      # recovery Tv to H                                          23
             (1-ptf)*nut*x[varind[8,n]],      # recovery Th to H                                          24
             tausev*nuq*(1-pmortt)*x[varind[5,n]],      # recovery Is to H                            25
             (1-prn)*(1-pr)*foifa*x[varind[2,n]], # incidence In to Ia          26
             pr*(1-tau[n])*foifa*x[varind[3,n]],         # incidence Ia to Ic          27      
             pr*tauo[n]*foifa*x[varind[3,n]],         # incidence Ia to To             28      
             pr*tauv[n]*foifa*x[varind[3,n]],         # incidence Ia to Tv             29      
             pr*tauh[n]*foifa*x[varind[3,n]],         # incidence Ia to Th             30      
             psev*nuc*x[varind[4,n]],         # incidence Ic to Is              31
             pr*(1-tau[n])*foifa*x[varind[2,n]],   # incidence In to Ic                32
             pr*tauo[n]*foifa*x[varind[2,n]],      # incidence In to To                33
             pr*tauv[n]*foifa*x[varind[2,n]],      # incidence In to Tv                34
             pr*tauh[n]*foifa*x[varind[2,n]],      # incidence In to Th                35
             prn*(1-pr)*foifa*x[varind[9,n]],     #       incidence R to In     36
             (1-prn)*(1-pr)*foifa*x[varind[9,n]],  #      incidence R to Ia     37
             (1-tau[n])*pr*foifa*x[varind[9,n]],          #      incidence R to Ic     38
             tauo[n]*pr*foifa*x[varind[9,n]],      #             incidence R to To     39
             tauv[n]*pr*foifa*x[varind[9,n]],      #             incidence R to Tv     40
             tauh[n]*pr*foifa*x[varind[9,n]],      #             incidence R to Th     41
             omega*x[varind[9,n]],                 #   loss of immunity from R to S    42
             prn*(1-pr)*foifa*x[varind[10,n]],     #       incidence H to In    43
             (1-prn)*(1-pr)*foifa*x[varind[10,n]],  #      incidence H to Ia    44
             (1-tau[n])*pr*foifa*x[varind[10,n]],          #      incidence H to Ic    45
             tauo[n]*pr*foifa*x[varind[10,n]],      #             incidence H to To    46
             tauv[n]*pr*foifa*x[varind[10,n]],      #             incidence H to Tv    47
             tauh[n]*pr*foifa*x[varind[10,n]],      #             incidence H to Th    48
             chi*x[varind[10,n]],                    #      loss HRP2 H to  R          49
             (1-tausev)*pmort*nuq*x[varind[5,n]]+tausev*nuq*pmortt*x[varind[5,n]],     # death untreated+treated Is  50
             (1-ptfc)*ptf*nut*x[varind[6,n]],      # failure To to Ia                 51
             ptfc*(1-ptftr)*ptf*nut*x[varind[6,n]],      # failure To to Ic    52
             ptfc*ptftr*ptf*nut*x[varind[6,n]],      # failure To to Th        53
             (1-ptfc)*ptf*nut*x[varind[7,n]],      # failure Tv to Ia                54
             ptfc*(1-ptftr)*ptf*nut*x[varind[7,n]],      # failure Tv to Ic   55
             ptfc*ptftr*ptf*nut*x[varind[7,n]],      # failure Tv to Tv       56
             (1-ptfc)*ptf*nut*x[varind[8,n]],          # failure Th to Ia             57
             ptfc*(1-ptftr)*ptf*nut*x[varind[8,n]],  # failure Th to Ic        58
             ptfc*ptftr*ptf*nut*x[varind[8,n]],    # failure Th to Th          59
             
             #Reactive Case detection 
             # min(tauRCD*rcdfsensN*x[varind[2,n]],(incf+(1-rcdpf_switch)*incv)*ss),  #Rcd In to Th (f) 262
             # min(tauRCD*rcdfsensA*x[varind[3,n]],(incf+(1-rcdpf_switch)*incv)*ss),  #Rcd Ia to Th (f) 263
             # min(tauRCD*rcdfsensC*x[varind[4,n]],(incf+(1-rcdpf_switch)*incv)*ss),  #Rcd In to Th (f) 264
             # min((1-propgd[n])*tauRCD*rcdvsensN*x[varind[12,n]],(incf+(1-rcdpf_switch)*incv)*ss),  #Rcd In to Th (v) 265 
             # min((1-propgd[n])*tauRCD*rcdvsensA*x[varind[13,n]],(incf+(1-rcdpf_switch)*incv)*ss),  #Rcd Ia to Th (v) 266 
             # min((1-propgd[n])*tauRCD*rcdvsensC*x[varind[14,n]],(incf+(1-rcdpf_switch)*incv)*ss),  #Rcd Ic to Th (v) 267 
             # min(propgd[n]*tauRCD*rcdvsensN*x[varind[12,n]],(incf+(1-rcdpf_switch)*incv)*ss),  #Rcd In to Thgd (v) 268 
             # min(propgd[n]*tauRCD*rcdvsensA*x[varind[13,n]],(incf+(1-rcdpf_switch)*incv)*ss),  #Rcd Ia to Thgd (v) 269 
             # min(propgd[n]*tauRCD*rcdvsensC*x[varind[14,n]],(incf+(1-rcdpf_switch)*incv)*ss),  #Rcd Ic to Thgd (v) 270 
             
             tauRCD*rcdfsensN*x[varind[2,n]],  #Rcd In to Th (f) 60
             tauRCD*rcdfsensA*x[varind[3,n]],  #Rcd Ia to Th (f) 61
             tauRCD*rcdfsensC*x[varind[4,n]], #Rcd Ic to Th (f)  62
             
             # Importation of cases
             muC*popf[n],             #imported Pf clinical cases                        63
             muA*popf[n],             #imported Pf assymptomatic cases                   64
             muU*popf[n],             #imported Pf sub-microscopic cases                 65
             mu_out*x[varind[1,n]],             #balance                                 66
             mu_out*x[varind[2,n]],             #balance                                 67
             mu_out*x[varind[3,n]],             #balance                                 68
             mu_out*x[varind[4,n]],             #balance                                 69
             mu_out*x[varind[5,n]],             #balance                                 70
             mu_out*x[varind[6,n]],             #balance                                 71
             mu_out*x[varind[7,n]],             #balance                                 72
             mu_out*x[varind[8,n]],             #balance                                 73
             mu_out*x[varind[9,n]],             #balance                                 74
             mu_out*x[varind[10,n]],             #balance                                75
             
             no_visits,                          #counter - number of ACD visits         76
             sample_rcd,                          #coutner - sample assessed ACD         77
             
             tauMS*rcdfsensN*x[varind[2,n]],  #MS In to Th (f)                            78
             tauMS*rcdfsensA*x[varind[3,n]],  #MS Ia to Th (f)                            79
             tauMS*rcdfsensC*x[varind[4,n]], #MS Ic to Th (f)                             80
             sample_MS,                          #counter - sample assessed MS            81
             
             demog*x[varind[11,n]], # rate of death of Sp                                 82
             demog*x[varind[12,n]], # rate of death of Tp                                 83
             tauSMC*x[varind[1,n]], #SMC S to Sp (f)                                      84
             tauSMC*x[varind[2,n]], #SMC In to Tp (f)                                     85
             tauSMC*x[varind[3,n]], #SMC Ia to Tp (f)                                     86
             tauSMC*x[varind[4,n]], #SMC Ic to Tp (f)                                     87
             tauSMC*x[varind[9,n]], #SMC R to Tp (f)                                      88
             tauSMC*x[varind[10,n]], #SMC H to Tp (f)                                     89
             nu_smc*x[varind[11,n]], #SMC recovery Sp to S                                 90
             nu_smc*x[varind[12,n]], #SMC recovery Tp to H                                  91
             
             tauIPTp*iptp_seek*x[varind[1,n]], #IPTp S to Sipt (f)  92
             tauIPTp*iptp_seek*x[varind[2,n]], #IPTp In to Tipt (f)  93
             tauIPTp*iptp_seek*x[varind[3,n]], #IPTp Ia to Tipt (f)  94
             tauIPTp*iptp_seek*x[varind[4,n]], #IPTp Ic to Tipt (f)  95
             tauIPTp*iptp_seek*x[varind[9,n]], #IPTp R to Tipt (f)  96
             tauIPTp*iptp_seek*x[varind[10,n]], #IPTp H to Tipt (f)  97
             (prop_ipt1*nu_ipt1+prop_ipt2*nu_ipt2+prop_ipt3*nu_ipt3+prop_ipt4*nu_ipt4+prop_ipt5*nu_ipt5)*x[varind[13,n]], #IPTp recovery Sipt to S 98
             (prop_ipt1*nu_ipt1+prop_ipt2*nu_ipt2+prop_ipt3*nu_ipt3+prop_ipt4*nu_ipt4+prop_ipt5*nu_ipt5)*x[varind[14,n]], #IPTp recovery Tipt to H 99
             demog*x[varind[13,n]], # rate of death of Sipt            100
             demog*x[varind[14,n]], # rate of death of Tipt            101
             (prop_ipt1*1+prop_ipt2*2+prop_ipt3*3+prop_ipt4*4+prop_ipt5*5), # counter IPTp no of doses #102
             reportdeath*tausev*nuq*pmortt*x[varind[5,n]]  # counter: reported deaths from malaria   103
           )
         }
         return(c(t(tranrate)))
       })
}

# epiModel function, is an argument to deSolve::ode ----
# as indicated in help('ode'):
# If func is an R-function, it must be defined as: func <- function(t, y, parms,...). 
# t is the current time point in the integration, 
# y is the current estimate of the variables in the ODE system. 
# If the initial values y has a names attribute, the names will be available inside func. 
# parms is a vector or list of parameters; ... (optional) are any other arguments passed to the function.
# The return value of func should be a list, whose first element is a vector containing the derivatives of y with respect to time, and whose next elements are global values that are required at each point in times. The derivatives must be specified in the same order as the state variables y.

epiModel <- function(t, state, parode, nun_sens, scenario) {
  
  # rates of change
  transit <- malrates(x = state[1:V],
                      nun_sens = nun_sens,
                      input_UI = parode,
                      t = state[V + 1],
                      ti = 1,
                      scenario = scenario)  
  
  if (any(is.na(transit))) stop("transit NA   ", state[V+1], "      ",  as.data.frame(transit))
  
  eq <- EQ(L, N, oldeq = rep(0, V), transit, transitionsiu1, transitionsiu2, 
           transitionsiv1, transitionsiv2, eq = rep(0, V))
  eq[V + 1] <- 1
  
  # return the rate of change
  list(eq)
}


# postproc function ----
postproc <- function(parpro, out, tran) {
  with(as.list(c( parpro)),
       {
         # sensitivities
         sens_n_micro<-1-0.5*(1+erf((dl_micro-mn_n)/(sd_n*(2^0.5))))
         sens_n_RDT<-1-0.5*(1+erf((dl_RDT-mn_n)/(sd_n*(2^0.5))))
         sens_n_qPCR<-1-0.5*(1+erf((dl_qPCR-mn_n)/(sd_n*(2^0.5))))
         sens_a_micro<-1-0.5*(1+erf((dl_micro-mn_a)/(sd_a*(2^0.5))))
         sens_a_RDT<-1-0.5*(1+erf((dl_RDT-mn_a)/(sd_a*(2^0.5))))
         sens_a_qPCR<-1-0.5*(1+erf((dl_qPCR-mn_a)/(sd_a*(2^0.5))))
         sens_c_micro<-1-0.5*(1+erf((dl_micro-mn_c)/(sd_c*(2^0.5))))
         sens_c_RDT<-1-0.5*(1+erf((dl_RDT-mn_c)/(sd_c*(2^0.5))))
         sens_c_qPCR<-1-0.5*(1+erf((dl_qPCR-mn_c)/(sd_c*(2^0.5))))
         sens_s_micro<-1-0.5*(1+erf((dl_micro-mn_s)/(sd_s*(2^0.5))))
         sens_s_RDT<-1-0.5*(1+erf((dl_RDT-mn_s)/(sd_s*(2^0.5))))
         sens_s_qPCR<-1-0.5*(1+erf((dl_qPCR-mn_s)/(sd_s*(2^0.5))))
         sens_H_micro<-0
         sens_H_RDT<-1
         sens_H_qPCR<-0
         
         sens_vmw<-sens_c_RDT  # test used by VMW
         sens_his<-sens_c_micro # test used by HIS
         sens_oth<-1 # test used by other
         
         
         # ************************************************************************************* #
         # for outputting the  time series for each patch
         # ************************************************************************************* #
         
         # VMW outputs
         vmw_predf<-matrix(0,nrow=length(out[,1]),ncol=N)
         for (n in 1:N){
           vmw_predf[,n]<-rowSums(tran[,c(traind[c(16,29,34,40,47,56),n])])/12
         }
         
         #HIS outputs
         his_predf<-matrix(0,nrow=length(out[,1]),ncol=N)
         for (n in 1:N){
           his_predf[,n]<-rowSums(tran[,c(traind[c(17,30,35,41,48,53,59),n])])/12 #
         }
         
         # Treated cases (His+other)(Uncomplicated cases accessing Health system (incidence +relapses+failures))
         #must be updated to include VMW!!!!! What about Severe
         trt_predf<-matrix(0,nrow=length(out[,1]),ncol=N)
         for (n in 1:N){
           trt_predf[,n]<-rowSums(tran[,c(traind[c(15:17,28:30,33:35,39:41,46:48,53,56,59),n])])/12
         }
         
         # Reported severe cases (successfully treated +deaths (trt+untrt))
         severe_predf<-matrix(0,nrow=length(out[,1]),ncol=N)
         for (n in 1:N){
           severe_predf[,n]<-(tran[,traind[25,n]]+tran[,traind[103,n]] )/12
         }
         
         # fatalities
         repfatal_pred<-fatal_pred<-matrix(0,nrow=length(out[,1]),ncol=N)
         for (n in 1:N){
           fatal_pred[,n]<-(tran[,traind[50,n]])/12
           repfatal_pred[,n]<-(tran[,traind[103,n]])/12
           
         }
         
         
         
         # true clinical burden (all clinical cases+failures - uncomp+severe; trt+untreated) but no rcd
         totclininc_predf<-matrix(0,nrow=length(out[,1]),ncol=N)
         for (n in 1:N){
           totclininc_predf[,n]<-rowSums(tran[,c(traind[c(14:17,27:35,38:41,45:48,52,53,55,56,58,59),n])])/12
         }
         
         # true incidence (all uncomplicated +severe+failures)
         totinc_predf<-matrix(0,nrow=length(out[,1]),ncol=N)
         for (n in 1:N){
           totinc_predf[,n]<-rowSums(tran[,c(traind[c(12:17,26:41,43:48,51:59),n])])/12
         }
         
         # Costing outcomes
         
         totIPTpdose<-totIPTp<-totSMCtr<-totMS_sample<-totMS_predf<-totrcd_sample<-totrcd_visits<-totrc_pred<-totrcd_predf<-matrix(0,nrow=length(out[,1]),ncol=N)
         #RCD indicators and counters are not divided by 12 as they reflect monthly parameters already. 
         for (n in 1:N){
           #    totrc_pred[,n]<-rowSums(tran[,c(traind[c(179,180,182,183,191, 192,194,195),n])])/12   #those getting RC on vivax: g6neg + g6testneg (V+H)
           totrcd_predf[,n]<-rowSums(tran[,c(traind[c(60:62),n])])   #those identified through RCD Pf (In/a/c -> Th)
           totrcd_visits[,n]<-tran[,c(traind[c(76),n])]   #visits made during RCD
           totrcd_sample[,n]<-tran[,c(traind[c(77),n])]   #sample assessed during RCD
           totMS_predf[,n]<-rowSums(tran[,c(traind[c(78:80),n])])   #those identified through MS Pf (In/a/c -> Th)
           totMS_sample[,n]<-tran[,c(traind[c(81),n])]   #sample assessed during RCD
           totSMCtr[,n]<-rowSums(tran[,c(traind[c(84:89),n])])/12 # total number of SMC treated
           totIPTp[,n]<-rowSums(tran[,c(traind[c(92:97),n])])/12 # no.of people to participate in IPTp (at least 1 dose)
           totIPTpdose[,n]<-tran[,c(traind[c(102),n])]*totIPTp[,n] # no. of IPTp doses
         }
         
         SMCtrt_predf<-matrix(0,nrow=length(out[,1]),ncol=N)
         for (n in 1:N){
           SMCtrt_predf[,n]<-rowSums(out[,c(varind[11:12,n])+1]) # SMC returned.
         }
         
         # ************************************************************************************* #
         # for predicting prevalence with different tests
         # ************************************************************************************* #
         # true prevalence
         totalinf_pred<-rowSums(out[,c(varind[2:5,])+1])
         
         population_pred<-prevalence_predf<-prevalence_predv<-matrix(0,nrow=length(out[,1]),ncol=N)
         for (n in 1:N){
           # prevalence_predf[,n]<-100*rowSums(out[,c(varind[2:5,n])+1])/rowSums(out[,(varind[falpop,n]+1)])
           population_pred[,n] <-rowSums(out[,c(varind[falpop,n])+1])
           prevalence_predf[,n]<-rowSums(out[,c(varind[2:5,n])+1])
         }
         prev_micro_predf<-prev_micro_predv<-matrix(0,nrow=length(out[,1]),ncol=N)
         prev_RDT_predf<- prev_RDT_predv<-matrix(0,nrow=length(out[,1]),ncol=N)
         prev_qPCR_predf<-prev_qPCR_predv<-matrix(0,nrow=length(out[,1]),ncol=N)
         for (n in 1:N){
           # prevalence by Micro
           prev_micro_predf[,n]<-100*(sens_n_micro*out[,c(varind[2,n])+1]+sens_a_micro*out[,c(varind[3,n])+1]+sens_c_micro*out[,c(varind[4,n])+1]+sens_s_micro*out[,c(varind[5,n])+1]+sens_H_micro*out[,c(varind[10,n])+1])/rowSums(out[,(varind[falpop,n]+1)])
           
           # prevalence by RDT
           prev_RDT_predf[,n]<-100*(sens_n_RDT*out[,c(varind[2,n])+1]+sens_a_RDT*out[,c(varind[3,n])+1]+sens_c_RDT*out[,c(varind[4,n])+1]+sens_s_RDT*out[,c(varind[5,n])+1]+sens_H_RDT*out[,c(varind[10,n])+1])/rowSums(out[,(varind[falpop,n]+1)])
           
           # prevalence by qPCR
           prev_qPCR_predf[,n]<-100*(sens_n_qPCR*out[,c(varind[2,n])+1]+sens_a_qPCR*out[,c(varind[3,n])+1]+sens_c_qPCR*out[,c(varind[4,n])+1]+sens_s_qPCR*out[,c(varind[5,n])+1]+sens_H_qPCR*out[,c(varind[10,n])+1])/rowSums(out[,(varind[falpop,n]+1)])
         }
         
         return(cbind(vmw_predf,    #1
                      his_predf,    #2
                      fatal_pred,    #3
                      severe_predf, #4
                      totclininc_predf,    #5
                      prevalence_predf,    #6
                      prev_micro_predf,    #7
                      prev_RDT_predf,    #8
                      prev_qPCR_predf,    #9
                      trt_predf, #10
                      totinc_predf, #11
                      totrc_pred, #12
                      totrcd_predf, #13
                      totrcd_visits, #14
                      totrcd_sample, #15
                      totMS_predf, #16
                      totMS_sample, #17
                      totSMCtr, #18
                      SMCtrt_predf, #19
                      totIPTp, #20
                      totIPTpdose, #21
                      repfatal_pred, #22
                      population_pred, #23
                      totalinf_pred
         ))
         
       })
}