################################################################################
# 02_GenerateData_Run01Functions_DoParallel.R
################################################################################
# Created: 11Jun2024
# Modified: Added doParallel processing (fastest option)
# Aim: Run functions from 01 with doParallel processing
################################################################################

################################################################################
# Load relevant libraries 
################################################################################
library(dplyr)
library(tidyverse)
library(mice)
library(broom)
library(pROC)
library(foreach)      # For parallel foreach loops
library(doParallel)   # Backend for parallel processing
library(doSNOW)       # Optional: for progress bars

################################################################################
################################################################################
#Load development_dataset
setwd("C:\\Users\\maecj\\OneDrive - Nexus365\\A DPhil\\Simulation studies\\Programs\\Study 1\\SimulationStudy1_11Jun2024\\SimulationStudy\\Data")
load("Development_Dataset_Yprev_0.01.Rdata")
model_1 <- development_dataset[["model"]]
load("Development_Dataset_Yprev_0.05.Rdata")
model_5 <- development_dataset[["model"]]
load("Development_Dataset_Yprev_0.1.Rdata")
model_10 <- development_dataset[["model"]]
load("Development_Dataset_Yprev_0.5.Rdata")
model_50 <- development_dataset[["model"]]
################################################################################
# Dependent Code
################################################################################
source("C://Users//maecj//OneDrive - Nexus365//A DPhil//Simulation studies//Programs//Study 1//SimulationStudy1_11Jun2024//SimulationStudy//01_Functions_SimulatingData.R") 

################################################################################
# Simulation Parameters
################################################################################

combinations_of_parameters <- crossing(
  n_iter = 10, 
  N_val = c(100000),
  Y_prev = c(0.01,0.05,0.10), 
  R_prev = c(0.25,0.50,0.75),  ## to give 75%, 50% and 25% missingness
  ## Beta = affect on Missingness R   
  beta_x1 = c(0, 0.5), ## 0 for MAR and MCAR and 0.5 for MNAR
  beta_x2 = c(0, 0.5), ## Affect on missingness  0 for MCAR, 0.5 for MAR and MNAR
  beta_x3 = c(0), 
  beta_x4 = c(0), 
  beta_x5 = c(0), 
  beta_U = c(0, 0.5), # 0 for Mar and  0.5 for MNAR
  # Gamma = affect on Y
  gamma_x1 = c(0.5), 
  gamma_x2 = c(0.5), 
  gamma_x3 = c(0.5),
  gamma_x4 = c(0.5), 
  gamma_x5 = c(0.5), 
  gamma_U = c(0.5) 
)

## Filter out those combinations which we aren't analysing
## We want MCAR: (beta_x1, beta_x2, beta_U == 0) 
## MAR: (beta_x2== 0.5)
## MNAR: (beta_x1, beta_x2, beta_U == 0.5) 

# Define the specific combinations of beta_x1, beta_x2, and beta_U that you want
valid_combinations <- tibble(
  beta_x1 = c(0, 0, 0.5),
  beta_x2 = c(0, 0.5, 0.5),
  beta_U = c(0, 0, 0.5)
)

# Filter sims_parameters to include only valid combinations of beta_x1, beta_x2, and beta_U
sims_parameters <- combinations_of_parameters %>%
  semi_join(valid_combinations, by = c("beta_x1", "beta_x2", "beta_U"))

# Add on label of missing mechanism
sims_parameters <- sims_parameters %>% 
  mutate(label=case_when(beta_x1==0 & beta_x2 ==0 & beta_U == 0 ~ "MCAR", 
                         beta_x1==0 & beta_x2 ==0.5 & beta_U == 0 ~ "MAR",
                         beta_x1==0.5 & beta_x2 ==0.5 & beta_U == 0.5 ~ "MNAR"))

# # Filter out certain conditions 
#  sims_parameters <- sims_parameters %>%
#                mutate(remove=case_when(label=="MAR" ~ 1,
#                                       TRUE ~0 ))
#  sims_parameters <- sims_parameters %>%
#                      filter(remove==0)

 
 
 
 
 
