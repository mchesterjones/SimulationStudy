################################################################################
# 02_GenerateData_Run01Functions_DoParallel.R
################################################################################
# Created: 11Jun2024
# Modified: 14Aug2025 update parallel method 
# Aim: Run functions from 01 with doParallel processing
################################################################################
### Directories 
results_dir <- "C:\\Users\\maecj\\Documents\\Simulation_Data_Study1"
development_dir <- "C:\\Users\\maecj\\OneDrive - Nexus365\\A DPhil\\Simulation studies\\Programs\\Study 1\\SimulationStudy1_11Jun2024\\SimulationStudy\\Data\\"
programs_dir <- "C:\\Users\\maecj\\OneDrive - Nexus365\\A DPhil\\Simulation studies\\Programs\\Study 1\\SimulationStudy1_11Jun2024\\SimulationStudy\\"

################################################################################
# Load relevant libraries 
################################################################################
library(dplyr)        # Organisation
library(tidyverse)    # Organisation 
library(mice)         # MI
library(broom)
library(pROC)         #AUC
library(foreach)      # For parallel foreach loops
library(doParallel)   # Backend for parallel processing
library(doRNG)       # Optional: for progress bars

################################################################################
################################################################################
#Load development_dataset
load(paste0(development_dir,"Development_Dataset_Yprev_0.1.Rdata"))
model <- development_dataset[["model"]]
rm(development_dataset)

################################################################################
# Dependent Code
################################################################################

################################################################################
# Simulation Parameters
################################################################################

combinations_of_parameters <- crossing(
  n_iter = 10, 
  N_val = c(100000),
  Y_prev = c(0.10), 
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

################################################################################
# Run in parallel 
################################################################################

num_cores <- 9
# cat("Using", num_cores, "cores for", RUN_MECHANISM, "processing\n")
 cat("Using", num_cores, "cores for processing\n")

# Set mechanism-specific seed
base_seed <- 14825 ## Note: different seed
# mechanism_offset <- switch(RUN_MECHANISM, MCAR=0, MAR=1000, MNAR=2000)
# set.seed(base_seed + mechanism_offset)
set.seed(base_seed)

# Load function definitions before parallel starts
source(paste0(programs_dir,"01_Functions_SimulatingData.R"))

# Register parallel backend

cl <- makeCluster(num_cores)
registerDoParallel(cl)

results <- foreach(i = 1:nrow(sims_parameters), 
                   .combine = rbind,
                   .packages = c("dplyr","tidyverse","mice","broom","pROC","MASS"),
                   .export = c("model")) %dorng% {
                     
                     params <- sims_parameters[i, ]
                     
                     tryCatch({
                       sim_res <- simulation_nrun_fnc(
                         n_iter = params$n_iter,
                         N_val = params$N_val,
                         Y_prev = params$Y_prev,
                         R_prev = params$R_prev,
                         beta_x1 = params$beta_x1,
                         beta_x2 = params$beta_x2,
                         beta_x3 = params$beta_x3,
                         beta_x4 = params$beta_x4,
                         beta_x5 = params$beta_x5,
                         beta_U  = params$beta_U,
                         gamma_x1 = params$gamma_x1,
                         gamma_x2 = params$gamma_x2,
                         gamma_x3 = params$gamma_x3,
                         gamma_x4 = params$gamma_x4,
                         gamma_x5 = params$gamma_x5,
                         gamma_U  = params$gamma_U
                       )
                       
                       filename <- sprintf("%s_Nval_%d_Yprev_%s_Rprev_%s.Rdata",
                                           params$label,
                                           params$N_val,
                                           as.character(params$Y_prev),
                                           as.character(params$R_prev))
                       
                       save(sim_res, file = file.path(results_dir, filename))
                       
                       data.frame(iteration = i, filename = filename, status = "SUCCESS", 
                                  N_val = params$N_val, Y_prev = params$Y_prev, 
                                  R_prev = params$R_prev, label = params$label,
                                  timestamp = Sys.time(), stringsAsFactors = FALSE)
                       
                     }, error = function(e) {
                       data.frame(iteration = i, filename = NA, 
                                  status = paste("ERROR:", e$message),
                                  N_val = params$N_val, Y_prev = params$Y_prev, 
                                  R_prev = params$R_prev, label = params$label,
                                  timestamp = Sys.time(), stringsAsFactors = FALSE)
                     })
                   }

stopCluster(cl)
