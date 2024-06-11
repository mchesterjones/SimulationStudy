################################################################################
# 02_GenerateData_Run01Functions.R
################################################################################
# Created: 11Jun2024
# Aim: Run functions from 01
################################################################################

################################################################################
# Load relevant libraries 
################################################################################
library(tidyverse)
library(furrr)


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
  Y_prev = c(0.5), ## 0.5 for now
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
################################################################################
# Check Generate Development Dataset Generation 
################################################################################
iters=3

# Check for NAs
#--------------------------------------------------------
for (i in 1:iters) {
  current_data <- simulation_results[["dev_data"]][[i]]
  for (j in 1:5) {
    # Extract the logical value (TRUE/FALSE) from the result of is.na()
    is_missing <- is.na(current_data[[paste0("x_", j)]])
    if (is_missing) {  # Check if the condition is TRUE (missing value)
      cat(i, " ", j, sep = "\n")
    }
  }
}




# Produce summaries
#--------------------------------------------------------

  for (i in 1:iters) {
    for (j in 1:5) {
      print(paste("Iteration:", i, "Variable: x_", j))
      print(summary(simulation_results[["dev_data"]][[i]][[paste0("x_", j)]]))
    }
  }

# DENSITY Plots of coefficients 
#--------------------------------------------------------
# Define the colors for each variable
colors <- c("black", "red", "blue", "green", "purple")

# Loop over each simulation
for (i in 1:iters) {

    # Create an empty vector to store the maximum y values for each variable
    max_y <- c()
    
    # Loop over each variable
    for (j in 1:5) {
      # Calculate the density and add the maximum y value to max_y
      var_density <- density(simulation_results[["dev_data"]][[i]][[paste0("x_", j)]])
      max_y <- c(max_y, max(var_density$y))
    }
    
    # Plot the density of the first variable
    plot(density(simulation_results[["dev_data"]][[i]][["x_1"]]), main = paste0("Density Plots for Simulation ", i),
         xlab = "Values", ylim = c(0, max(max_y)), 
         xlim = c(-5, 5))  # Set the x-axis limits to -5 and 5
    
    # Add the densities of the other variables
    for (j in 2:5) {
      lines(density(simulation_results[["dev_data"]][[i]][[paste0("x_", j)]]), col = colors[j])
    }
    
    # Add a legend
    legend("topright", legend = c("x1", "x2", "x3", "x4", "x5"), fill = colors)
  }
  







