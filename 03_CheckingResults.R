################################################################################
# Checking Results Script 
################################################################################
# Date Created: 28Jun2024
# Author: Mae Chester-Jones
# Date last updated: - 


################################################################################
# Check Generate Development Dataset Generation 
################################################################################

load("C:\\Users\\maecj\\OneDrive - Nexus365\\A DPhil\\Simulation studies\\Programs\\Study 1\\SimulationStudy1_11Jun2024\\SimulationStudy\\Results_Yprev0.1_Rprev0.5_28Jun2024.Rdata")


iters=3

# Check for NAs
#--------------------------------------------------------
for (i in 1:iters) {
  current_data <- simulation_results[["iterations"]][[i]][["dev_data"]]
  
  for (j in 1:5) {
    # Extract the first element (TRUE/FALSE) from the result of is.na()
    is_missing <- is.na(current_data[[paste0("x_", j)]])[1]
    if (is_missing) {  # Check if the first element (is_missing) is TRUE
      cat(i, " ", j, sep = "\n")
    }
  }
}


# Check prevalence 
#--------------------------------------------------------
for (i in 1:iters) {
  print(paste("Prevalence of Y Iteration:", i))
  print(table(simulation_results[["iterations"]][[i]][["dev_data"]][["Y"]]))
} 

# Produce summaries
#--------------------------------------------------------

for (i in 1:iters) {
  for (j in 1:5) {
    print(paste("Iteration:", i, "Variable: x_", j))
    print(summary(simulation_results[["iterations"]][[i]][["dev_data"]][[paste0("x_", j)]]))
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
    var_density <- density(simulation_results[["iterations"]][[i]][["dev_data"]][[paste0("x_", j)]])
    max_y <- c(max_y, max(var_density$y))
  }
  
  # Plot the density of the first variable
  plot(density(simulation_results[["iterations"]][[i]][["dev_data"]][["x_1"]]), main = paste0("Density Plots for Simulation ", i),
       xlab = "Values", ylim = c(0, max(max_y)), 
       xlim = c(-5, 5))  # Set the x-axis limits to -5 and 5
  
  # Add the densities of the other variables
  for (j in 2:5) {
    lines(density(simulation_results[["iterations"]][[i]][["dev_data"]][[paste0("x_", j)]]), col = colors[j])
  }
  
  # Add a legend
  legend("topright", legend = c("x1", "x2", "x3", "x4", "x5"), fill = colors)
}


################################################################################
# Check Development Model # 
################################################################################

# Check CIs contain true value (this will need to be coded separately)
for (i in 1:iters) {
  CI_current <-  confint(simulation_results[["iterations"]][[i]][["model"]]) 
  for (j in 1:5) {
    lower_bound <- CI_current[j+1, 1]  # Assuming first column is lower bound
    upper_bound <- CI_current[j+1, 2]  # Assuming second column is upper bound
    
    # Check if x_i falls within the CI (exclusive bounds)
    if (0.5 < lower_bound | x_i_value > upper_bound) {
      warning(paste("CI for iteration", i, "does not contain coefficient x", j, sep = " "))
    }
  }
  
}
CI_1 <- confint(simulation_results[["iterations"]][[1]][["model"]]) 
CI_2 <- confint(simulation_results[["iterations"]][[2]][["model"]]) 
CI_3 <- confint(simulation_results[["iterations"]][[3]][["model"]]) 



# Check the model 
for (i in 1:iters) {
  current_Y <- simulation_results[["iterations"]][[i]][["model"]][["y"]]
  current_X1 <- simulation_results[["iterations"]][[i]][["dev_data"]][["x_1"]]
  current_X2 <- simulation_results[["iterations"]][[i]][["dev_data"]][["x_2"]]
  current_X3 <- simulation_results[["iterations"]][[i]][["dev_data"]][["x_3"]]
  current_X4 <- simulation_results[["iterations"]][[i]][["dev_data"]][["x_4"]]
  current_X5 <- simulation_results[["iterations"]][[i]][["dev_data"]][["x_5"]]
  
  model.check <- glm(current_Y ~ current_X1 + current_X2 + current_X3 + current_X4 + current_X5,
                     data = simulation_results[["iterations"]][[i]][["dev_data"]],
                     family = binomial)
  summary(model.check)
  summary(simulation_results[["iterations"]][[i]][["model"]])
  confint(model.check)
  confint(simulation_results[["iterations"]][[i]][["model"]])
}

# Check deviance residuals against fitted values
library(arm)


for (i in 1:iters) {
  
  # Extract residuals, fitted values and Y 
  current_residuals <- residuals(simulation_results[["iterations"]][[i]][["model"]], type = "deviance")
  current_fitted <- predict(simulation_results[["iterations"]][[i]][["model"]], type = "response")
  current_Y <- simulation_results[["iterations"]][[i]][["model"]][["y"]]
  # Plot residuals vs. fitted values 
  plot(current_fitted, current_residuals)
  title(main=paste0("Fitted Values vs. Deviance Residuals for Iteration ", i))
  
  # Plot residuals against predictors
  for (j in 1:5) {
    current_X <- simulation_results[["iterations"]][[i]][["dev_data"]][[paste0("x_", j)]]
    plot(current_X, current_residuals, col = c("blue", "red")[1+current_Y])
    lines(lowess(current_X, current_residuals), col = "black", lwd=2)
    lines(lowess(current_X[current_Y==0], current_residuals[current_Y==0]), col = "blue")
    lines(lowess(current_X[current_Y==1], current_residuals[current_Y==1]), col = "red")
    abline(h=0,lty=2,col="grey")
    title(main=paste0("Iteration ", i, " Residuals vs Coefficient x_", j))
  }
  
  # Create binned residual plot 
  binnedplot(current_fitted, current_residuals) 
  
  # Plot against predictors 
  for (j in 1:5) {
    binnedplot(current_residuals, simulation_results[["iterations"]][[i]][["dev_data"]][[paste0("x_", j)]]) 
  }
  
}







################################################################################
# Check Generation of Validation Dataset Generation 
################################################################################
iters=3


# Check prevalence of Y (outcome)
#--------------------------------------------------------
for (i in 1:iters) {
  print(paste("Prevalence of Y Iteration:", i))
  print(table(simulation_results[["iterations"]][[i]][["val_data"]][["Y"]]))
} 

# Check proportion of missingness in X
#--------------------------------------------------------
for (i in 1:iters) {
  print("Missingness in X1")
  print(table(simulation_results[["iterations"]][[i]][["val_data"]][["x_1"]]))
} 

# Check for NAs in x01
#--------------------------------------------------------
for (i in 1:iters) {
  print(paste("Missing Proportion of X_1 in Iteration:", i))
  print(table(is.na(simulation_results[["iterations"]][[i]][["val_data"]][["x_1"]])))
}


# Check for NAs in all coefficients 
#--------------------------------------------------------
for (i in 1:iters) {
  for (j in 1:5) {
    print(table(is.na(simulation_results[["iterations"]][[i]][["val_data"]][[paste0("x_", j)]])))
  }
}


# Produce summaries
#--------------------------------------------------------
for (i in 1:iters) {
  print(paste("Iteration:", i, "Variable: x_1True"))
  print(summary(simulation_results[["iterations"]][[i]][["val_data"]][["x_1true"]]))
  
  for (j in 1:5) {
    print(paste("Iteration:", i, "Variable: x_", j))
    print(summary(simulation_results[["iterations"]][[i]][["val_data"]][[paste0("x_", j)]]))
    
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
    var_density <- density(simulation_results[["iterations"]][[i]][["val_data"]][[paste0("x_", j)]])
    max_y <- c(max_y, max(var_density$y))
  }
  
  # Plot the density of the first variable
  plot(density(simulation_results[["iterations"]][[i]][["val_data"]][["x_1true"]]), main = paste0("Density Plots for Simulation ", i),
       xlab = "Validation SValues", ylim = c(0, max(max_y)),
       xlim = c(-5, 5))  # Set the x-axis limits to -5 and 5
  
  # Add the densities of the other variables
  for (j in 2:5) {
    lines(density(simulation_results[["iterations"]][[i]][["val_data"]][[paste0("x_", j)]]), col = colors[j])
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



