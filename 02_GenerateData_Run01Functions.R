################################################################################
# 02_GenerateData_Run01Functions.R
################################################################################
# Created: 11Jun2024
# Aim: Run functions from 01
################################################################################

################################################################################
# Load relevant libraries 
################################################################################
library(dplyr)
library(tidyverse)
library(mice)
library(broom)
library(pROC)

####################################


################################################################################
# Dependent Code
################################################################################
source("C://Users//maecj//OneDrive - Nexus365//A DPhil//Simulation studies//Programs//Study 1//SimulationStudy1_11Jun2024//SimulationStudy//01_Functions_SimulatingData.R") 

################################################################################
# Simulation Parameters
################################################################################


sims_parameters <- crossing(
  n_iter = 3, 
  N_dev = 100,
  N_val = 100, 
  Y_prev = c(0.1), ## 0.5 for now
  R_prev = c(0.5),
  ## Beta = affect on Missingness R   
  beta_x1 = c(0), ## 0 for MAR
  beta_x2 = c(0.5), ## Affect on missingness  
  beta_x3 = c(0), 
  beta_x4 = c(0), 
  beta_x5 = c(0), 
  # Gamma = affect on Y
  gamma_x1 = c(0.5), 
  gamma_x2 = c(0.5), 
  gamma_x3 = c(0.5),
  gamma_x4 = c(0.5), 
  gamma_x5 = c(0.5), 
)




################################################################################
# Store Simulation Results
################################################################################

s <-1

simulation_results <- simulation_nrun_fnc(
  n_iter = sims_parameters$n_iter[s],
  N_dev = sims_parameters$N_dev[s],
  N_val = sims_parameters$N_val[s],
  Y_prev = sims_parameters$Y_prev[s],
  R_prev = sims_parameters$R_prev[s],
  beta_x1 = sims_parameters$beta_x1[s],
  beta_x2 = sims_parameters$beta_x2[s],
  beta_x3 = sims_parameters$beta_x3[s],
  beta_x4 = sims_parameters$beta_x4[s],
  beta_x5 = sims_parameters$beta_x5[s],
  gamma_x1 = sims_parameters$gamma_x1[s],
  gamma_x2 = sims_parameters$gamma_x2[s],
  gamma_x3 = sims_parameters$gamma_x3[s],
  gamma_x4 = sims_parameters$gamma_x4[s],
  gamma_x5 = sims_parameters$gamma_x5[s])

warnings()

# Get today's date Y Prev and R Previn a format suitable for filenames
today <- format(Sys.Date(), "%d%b%Y")  
Yprev <- paste("Yprev",sims_parameters$Y_prev)
Rprev <- paste("Rprev",sims_parameters$R_prev)


# Construct the filename with today's date
filename <- paste0("Results_","Yprev", sims_parameters$Y_prev, "_Rprev", sims_parameters$R_prev, "_",  today, ".Rdata")

# Save results
write_rds(simulation_results, file = filename)

