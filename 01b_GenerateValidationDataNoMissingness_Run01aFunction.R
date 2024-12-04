################################################################################
# 00_GenerateValidationDataNoMissingness.R
################################################################################
# Created: 07Oct2024
# Aim: Run functions from 00 to simulation validation data with no missingness
################################################################################

################################################################################
# Load relevant libraries 
################################################################################
library(dplyr)
library(tidyverse)
library(mice)
library(broom)
library(pROC)

################################################################################
################################################################################
#Load development_dataset
setwd("C:\\Users\\maecj\\OneDrive - Nexus365\\A DPhil\\Simulation studies\\Programs\\Study 1\\SimulationStudy1_11Jun2024\\SimulationStudy\\Data")
load("Development_Dataset_Yprev_0.01.Rdata")
# load("Development_Dataset_Yprev_0.05.Rdata")
# load("Development_Dataset_Yprev_0.1.Rdata")

model <- development_dataset[["model"]]

################################################################################
# Dependent Code
################################################################################
source("C://Users//maecj//OneDrive - Nexus365//A DPhil//Simulation studies//Programs//Study 1//SimulationStudy1_11Jun2024//SimulationStudy//01a_Function_SimulateValidationDataNoMissingness.R") 

################################################################################
# Simulation Parameters
################################################################################


sims_parameters <- crossing(
  n_iter = 200, 
  N_val = c(500, 10000, 100000),
  Y_prev = c(0.01,0.05,0.1), 
  # Gamma = affect on Y
  gamma_x1 = c(0.5), 
  gamma_x2 = c(0.5), 
  gamma_x3 = c(0.5),
  gamma_x4 = c(0.5), 
  gamma_x5 = c(0.5), 
  gamma_U = c(0.5) 
  
)


###############################################################################
# Run through combinations 
###############################################################################
for (i in 1:nrow(sims_parameters)) {
  # Extract parameters for the current iteration
  n_iter <- sims_parameters$n_iter[i]
  N_val <- sims_parameters$N_val[i]
  Y_prev <- sims_parameters$Y_prev[i]
  gamma_x1 <- sims_parameters$gamma_x1[i]
  gamma_x2 <- sims_parameters$gamma_x2[i]
  gamma_x3 <- sims_parameters$gamma_x3[i]
  gamma_x4 <- sims_parameters$gamma_x4[i]
  gamma_x5 <- sims_parameters$gamma_x5[i]
  gamma_U <- sims_parameters$gamma_U[i]
  
  
################################################################################
# Store Simulation Results
################################################################################

s <-1

## Simulation Results
simulation_results <- simulation_nrun_fnc(
  n_iter = n_iter,
  N_val = N_val,
  Y_prev = Y_prev,
  gamma_x1 = gamma_x1,
  gamma_x2 = gamma_x2,
  gamma_x3 = gamma_x3,
  gamma_x4 = gamma_x4,
  gamma_x5 = gamma_x5, 
  gamma_U = gamma_U)


# Get today's date Y Prev and R Previn a format suitable for filenames
today <- format(Sys.Date(), "%d%b%Y")  

# Construct the filename with today's date
filename <- paste0("Validation_NoMissingness_Nval_", sims_parameters$N_val[i], "_Yprev_", sims_parameters$Y_prev[i], "_", today, ".Rdata")
# Save results
setwd("C:\\Users\\maecj\\OneDrive - Nexus365\\A DPhil\\Simulation studies\\Programs\\Study 1\\SimulationStudy1_11Jun2024\\SimulationStudy\\Data")
save(simulation_results, file = filename)

}

warnings()


