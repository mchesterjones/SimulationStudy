################################################################################
## Simulation Performance Measures
################################################################################
setwd("C:\Users\maecj\OneDrive - Nexus365\A DPhil\Simulation studies\Programs\Study 1\SimulationStudy1_11Jun2024\SimulationStudy")
load("Results_Yprev0.05_Rprev0.25_01Jul2024.Rdata")



################################################################################
## Store n, bias 
################################################################################
# Define the number of iterations
num_iterations <- 3

# Define an empty data frame to store bias
bias_summary <- data.frame(
  iteration = integer(),  # Pre-allocate for efficiency
  dataset = character(),
  n = integer(),
  bias = numeric()
)


# Iterate over iterations
for (i in 1:num_iterations) {
  # Loop through each dataset
  for (dataset_id in c("CCA_val_data", "mean_val", "MI_val_data_noY", "MI_val_data_withY")) {
  
    # Extract true Y and estimated Y values
    true_Y <- simulation_results[["iterations"]][[i]][["preds"]][["preds_per_data_set"]][[dataset_id]][["Y"]]
    estimated_Y <- simulation_results[["iterations"]][[i]][["preds"]][["preds_per_data_set"]][[dataset_id]][["Prediction_Model"]]
    
    # Calculate number of observations (n)
    n <- length(true_Y)
    
    # Calculate bias
    bias <- sum(true_Y - estimated_Y) / n
    
    # Create a temporary row for the current iteration and dataset
    new_row <- data.frame(iteration = i, dataset = dataset_id, n = n, bias = bias)
    
    # Append the temporary row to the bias_summary data frame
    bias_summary <- rbind(bias_summary, new_row)
  }
}


################################################################################
## Create Empty Data Frame 
################################################################################


