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

################################################################################
# Dependent Code
################################################################################
source("C://Users//maecj//OneDrive - Nexus365//A DPhil//Simulation studies//Programs//Study 1//SimulationStudy1_11Jun2024//SimulationStudy//01_Functions_SimulatingData.R") 

################################################################################
# Simulation Parameters
################################################################################

combinations_of_parameters <- crossing(
  n_iter = 1000, 
  N_val = c(100000),
  Y_prev = c(0.05), 
  R_prev = c(0.25,0.50,0.75), 
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

# Filter out certain conditions 
 sims_parameters <- sims_parameters %>%
               mutate(remove=case_when(label=="MCAR" ~ 1,
                                      label == "MNAR" ~1, 
                                      TRUE ~0 ))
 sims_parameters <- sims_parameters %>%
                     filter(remove==0)

################################################################################
# doParallel Setup (FASTEST OPTION)
################################################################################

# Detect number of cores (leave one free for system)
num_cores <- detectCores() - 1
cat("Using", num_cores, "cores for doParallel processing\n") ## 3 cores here

# STRATEGIC BATCHING: Run by N_val groups for better resource management
# Uncomment the N_val you want to run:
#target_N_val <- 500      # 
#target_N_val <- 10000  # Around 3.5 hours
 target_N_val <- 100000 # Have to run in batches 

# Filter to only run specific N_val
sims_parameters <- sims_parameters %>% 
  filter(N_val == target_N_val)

cat("Running", nrow(sims_parameters), "simulations for N_val =", target_N_val, "\n")

# Register the parallel backend
registerDoParallel(cores = num_cores)

# Optional: Set up progress bar (requires doSNOW)
 cl <- makeCluster(num_cores, type = "SOCK")
 registerDoSNOW(cl)
 pb <- txtProgressBar(max = nrow(sims_parameters), style = 3)
 progress <- function(n) setTxtProgressBar(pb, n)
 opts <- list(progress = progress)

###############################################################################
# Run simulations with doParallel (MUCH CLEANER CODE!)
###############################################################################

cat("Starting doParallel simulations...\n")
start_time <- Sys.time()

# This replaces your entire for loop with much cleaner syntax!
results <- foreach(i = 1:nrow(sims_parameters), 
                   .combine = rbind,
                   .packages = c("dplyr", "tidyverse", "mice", "broom", "pROC"),
                   .export = c("simulation_nrun_fnc", "model_1", "model_5", "model_10"),
                   .errorhandling = "pass") %dopar% {
                     
                     # Source functions (need to do this in each worker)
                     source("C://Users//maecj//OneDrive - Nexus365//A DPhil//Simulation studies//Programs//Study 1//SimulationStudy1_11Jun2024//SimulationStudy//01_Functions_SimulatingData.R")

                     # Extract parameters for the current iteration
                     n_iter <- sims_parameters$n_iter[i]
                     N_val <- sims_parameters$N_val[i]
                     Y_prev <- sims_parameters$Y_prev[i]
                     R_prev <- sims_parameters$R_prev[i]
                     beta_x1 <- sims_parameters$beta_x1[i]
                     beta_x2 <- sims_parameters$beta_x2[i]
                     beta_x3 <- sims_parameters$beta_x3[i]
                     beta_x4 <- sims_parameters$beta_x4[i]
                     beta_x5 <- sims_parameters$beta_x5[i]
                     beta_U <- sims_parameters$beta_U[i]
                     gamma_x1 <- sims_parameters$gamma_x1[i]
                     gamma_x2 <- sims_parameters$gamma_x2[i]
                     gamma_x3 <- sims_parameters$gamma_x3[i]
                     gamma_x4 <- sims_parameters$gamma_x4[i]
                     gamma_x5 <- sims_parameters$gamma_x5[i]
                     gamma_U <- sims_parameters$gamma_U[i]
                     
                     # Select the appropriate model based on Y_prev
                     model <- switch(as.character(Y_prev),
                                     "0.01" = model_1,
                                     "0.05" = model_5,
                                     "0.1" = model_10,
                                     stop("Invalid Y_prev value"))
                     
                     # Run simulation
                     tryCatch({
                       simulation_results <- simulation_nrun_fnc(
                         n_iter = n_iter,
                         N_val = N_val,
                         Y_prev = Y_prev,
                         R_prev = R_prev,
                         beta_x1 = beta_x1,
                         beta_x2 = beta_x2,
                         beta_x3 = beta_x3,
                         beta_x4 = beta_x4,
                         beta_x5 = beta_x5,
                         beta_U = beta_U,
                         gamma_x1 = gamma_x1,
                         gamma_x2 = gamma_x2,
                         gamma_x3 = gamma_x3,
                         gamma_x4 = gamma_x4,
                         gamma_x5 = gamma_x5, 
                         gamma_U = gamma_U)
                       
                       # Get today's date for filename
                       today <- format(Sys.Date(), "%d%b%Y")  
                       
                       # Construct the filename with today's date
                       filename <- paste0(sims_parameters$label[i], "_Nval_", sims_parameters$N_val[i], 
                                          "_Yprev_", sims_parameters$Y_prev[i], "_Rprev_", 
                                          sims_parameters$R_prev[i], ".Rdata")
                       
                       # Save results
                       setwd("C:\\Users\\maecj\\Documents\\Simulation_Data_Study1")
                       save(simulation_results, file = filename)
                       
                       # Return summary info
                       data.frame(
                         iteration = i,
                         filename = filename,
                         status = "SUCCESS",
                         N_val = N_val,
                         Y_prev = Y_prev,
                         R_prev = R_prev,
                         label = sims_parameters$label[i],
                         timestamp = as.character(Sys.time()),
                         stringsAsFactors = FALSE
                       )
                       
                     }, error = function(e) {
                       # Return error info if simulation fails
                       data.frame(
                         iteration = i,
                         filename = NA,
                         status = paste("ERROR:", e$message),
                         N_val = N_val,
                         Y_prev = Y_prev,
                         R_prev = R_prev,
                         label = sims_parameters$label[i],
                         timestamp = as.character(Sys.time()),
                         stringsAsFactors = FALSE
                       )
                     })
                   }

end_time <- Sys.time()
cat("doParallel processing completed in:", 
    round(difftime(end_time, start_time, units = "mins"), 2), "minutes\n")

# Stop the parallel backend
stopImplicitCluster()

# Optional: close progress bar
# if(exists("pb")) close(pb)
# if(exists("cl")) stopCluster(cl)

################################################################################
# Results Summary
################################################################################

cat("\n=== SIMULATION SUMMARY ===\n")
cat("Total parameter combinations:", nrow(sims_parameters), "\n")
cat("Successful simulations:", sum(results$status == "SUCCESS"), "\n")
cat("Failed simulations:", sum(results$status != "SUCCESS"), "\n")

# Show any errors
if(any(results$status != "SUCCESS")) {
  cat("\nERRORS ENCOUNTERED:\n")
  error_results <- results[results$status != "SUCCESS", ]
  print(error_results[, c("iteration", "N_val", "Y_prev", "R_prev", "status")])
}

# Show successful completions by category
cat("\nSUCCESSFUL SIMULATIONS BY CATEGORY:\n")
success_summary <- results[results$status == "SUCCESS", ] %>%
  group_by(label, N_val, Y_prev, R_prev) %>%
  summarise(count = n(), .groups = "drop")
print(success_summary)

# Save the results summary
save(results, file = paste0("simulation_summary_", format(Sys.Date(), "%d%b%Y"), ".Rdata"))

warnings()
