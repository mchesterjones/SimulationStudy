################################################################################
# Checking Results Script 
################################################################################
# Date Created: 28Jun2024
# Author: Mae Chester-Jones
# Date last updated: - 



################################################################################
# Check Generation of Validation Dataset Generation 
################################################################################


# Check prevalence of Y (outcome)
#--------------------------------------------------------
for (i in 1:iters) {
  print(paste("Prevalence of Y Iteration:", i))
  print(table(simulation_results[["iterations"]][[i]][["val_data"]][["val_data"]][["Y"]]))
} 

# Check for NAs in x01
#--------------------------------------------------------
for (i in 1:iters) {
  print(paste("Missing Proportion of X_1 in Iteration:", i))
  print(table(is.na(simulation_results[["iterations"]][[i]][["val_data"]][["val_data"]][["x_1"]])))
}


# Check for NAs in all coefficients 
#--------------------------------------------------------
for (i in 1:iters) {
  for (j in 1:5) {
    print(table(is.na(simulation_results[["iterations"]][[i]][["val_data"]][["val_data"]][[paste0("x_", j)]])))
  }
}


# Produce summaries
#--------------------------------------------------------
for (i in 1:iters) {
  print(paste("Iteration:", i, "Variable: x_1True"))
  print(summary(simulation_results[["iterations"]][[i]][["val_data"]][["val_data"]][["x_1true"]]))
  
  for (j in 1:5) {
    print(paste("Iteration:", i, "Variable: x_", j))
    print(summary(simulation_results[["iterations"]][[i]][["val_data"]][["val_data"]][[paste0("x_", j)]]))
    
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
  for (j in 2:5) {
    # Calculate the density and add the maximum y value to max_y
    var_density <- density(simulation_results[["iterations"]][[i]][["val_data"]][["val_data"]][[paste0("x_", j)]])
    max_y <- c(max_y, max(var_density$y))
  }
  
  # Plot the density of the first variable
  plot(density(simulation_results[["iterations"]][[i]][["val_data"]][["val_data"]][["x_1true"]]), main = paste0("Density Plots for Simulation ", i),
       xlab = "Validation SValues", ylim = c(0, max(max_y)),
       xlim = c(-5, 5))  # Set the x-axis limits to -5 and 5
  
  # Add the densities of the other variables
  for (j in 2:5) {
    lines(density(simulation_results[["iterations"]][[i]][["val_data"]][["val_data"]][[paste0("x_", j)]]), col = colors[j])
  }
  
  # Add a legend
  legend("topright", legend = c("x1TRUE", "x2", "x3", "x4", "x5"), fill = colors)
}


################################################################################
# Validation: check MI logged events
################################################################################
for (i in 1:iters) {
  
  print(simulation_results[["iterations"]][[i]][["imputed_datasets"]][["MI_val_data_noY"]][["loggedEvents"]])
  print(simulation_results[["iterations"]][[i]][["imputed_datasets"]][["MI_val_data_withY"]][["loggedEvents"]])
}


