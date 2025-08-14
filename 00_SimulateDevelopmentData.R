################################################################################
# 00_SimulateDevelopmentData.R
################################################################################
# Created: 18Sep2024
# Aim: Simulate developmentData
################################################################################

################################################################################
# Load relevant libraries 
################################################################################
library(dplyr)
library(tidyverse)
library(mice)
library(broom)
library(pROC)
library(MASS)

###############################################################################
## Functions for running iterations
################################################################################

#set.seed(189) ## 0.01
#set.seed(1189) ## 0.05
set.seed(11189) ## 10

sims_parameters <- crossing(
  N_dev = 100000,
  Y_prev = c(0.1), # Change manually 
  # Gamma = affect on Y
  gamma_x1 = c(0.5), 
  gamma_x2 = c(0.5), 
  gamma_x3 = c(0.5),
  gamma_x4 = c(0.5), 
  gamma_x5 = c(0.5), 
  gamma_U = c(0.5) 
)

#-------------------------------------------------------------------------------




###############################################################################
## 0. Function that takes the inverse logistic link
###############################################################################

## Function to extract the OR from the coeffiction
expit_function <- function(x) {1/(1+exp(-x))} ## Recal ex/1+ex this is the same to get the P(Y=1)

###############################################################################
## 1. Function that creates development dataset
###############################################################################


dev_data_simulation_function <- function(
    N_dev,
    Y_prev, ## Prevalence of Y
    gamma_0,  ## Intercept in Y model
    gamma_x1,  ## Coefficient of X1 on outcome Y
    gamma_x2,  ## Coefficient of X2 on outcome Y
    gamma_x3,  ## Coefficient of X3 on outcome Y
    gamma_x4,  ## Coefficient of X4 on outcome Y
    gamma_x5,  ## Coefficient of X5 on outcome Y
    gamma_U) ## Coefficient of U on outcome Y 
  
  
{
  
  
  ## Create relationship between x_1 and x_4 and x_5 
  mu = c(0, 0 ,0, 0,0) # all have mean 0 
  
  covar_mat = matrix(c(1.0, 0.6, 0.6, 0.6, 0.6, 
                       0.6, 1.0, 0.2, 0.2, 0.2, 
                       0.6, 0.2, 1.0, 0.2, 0.2, 
                       0.6, 0.2, 0.2, 1.0, 0.2, 
                       0.6, 0.2, 0.2, 0.2, 1.0), nrow=5)
  
  
  correlated_distributions <- mvrnorm(n=N_dev, 
                                      mu = mu,
                                      Sigma = covar_mat)
  
  # Create Development Data
  #---------------------------------------------
  dev_data_IPD <- tibble("x_1" = correlated_distributions[,1],
                         "x_2" = correlated_distributions[,2],
                         "x_3" = correlated_distributions[,3],
                         "x_4" = correlated_distributions[,4],
                         "x_5" = correlated_distributions[,5], 
                         "U" = rnorm(N_dev, mean = 0, sd = 1),
                         "ID" = 1:N_dev)
  

  #determine the prevalence of the outcome based on the gamma_0
  gamma_0 <- as.numeric(coef(glm(rbinom(N_dev, 1, prob = Y_prev) ~
                                   offset(gamma_x1*x_1 +
                                            gamma_x2*x_2 +
                                            gamma_x2*x_3 +
                                            gamma_x4*x_4 +
                                            gamma_x5*x_5 +
                                            gamma_U*U),
                                 family = binomial(link = "logit"),
                                 data = dev_data_IPD))[1])
  
  dev_data_IPD$Y = rbinom(N_dev, size = 1,
                          prob = expit_function(gamma_0 +
                                                  gamma_x1*dev_data_IPD$x_1 +
                                                  gamma_x2*dev_data_IPD$x_2 +
                                                  gamma_x3*dev_data_IPD$x_3 +
                                                  gamma_x4*dev_data_IPD$x_4 +
                                                  gamma_x5*dev_data_IPD$x_5 + 
                                                  gamma_U*dev_data_IPD$U))
  
  return(dev_data_IPD)
}

# 
####---------------------------------------
## 2. Model fitting function
####---------------------------------------

dev_mod_function <- function(dev_data) {
  
  # Fit Model
  model_1 <- glm(Y ~ x_1 + x_2 + x_3 + x_4 + x_5, data = dev_data, family = binomial)
  # Note: Y not dependent on U in development model
  
  # Check Convergence
  if (!model_1$converged) {
    warning("Model fitting might not have converged. Check coefficients and diagnostics.")
  }
  
  return(model_1)
}

 


#1.dev_data function------------
dev_data <- dev_data_simulation_function(N_dev = sims_parameters$N_dev,
                                         gamma_x1 = sims_parameters$gamma_x1,
                                         gamma_x2 = sims_parameters$gamma_x2,
                                         gamma_x3 = sims_parameters$gamma_x3,
                                         gamma_x4 = sims_parameters$gamma_x4,
                                         gamma_x5 = sims_parameters$gamma_x5,
                                         gamma_U = sims_parameters$gamma_U,
                                         Y_prev = sims_parameters$Y_prev)


#2.dev_mod_function------------------
model <- dev_mod_function(dev_data = dev_data)



#3. Store development dataset and model
  development_dataset <- list("dev_data" = dev_data,
                      "model" = model)
  
#4. Save
  # Construct the filename with today's date
  filename <- paste0("Development_Dataset_Yprev_", sims_parameters$Y_prev, ".Rdata")
  # Save results
  setwd("C:\\Users\\maecj\\OneDrive - Nexus365\\A DPhil\\Simulation studies\\Programs\\Study 1\\SimulationStudy1_11Jun2024\\SimulationStudy\\Data")
  save(development_dataset, file = filename)
  
  
  
  
  
  
#5. Check Model 
  ################################################################################
  # Check for NAs
  #--------------------------------------------------------
    current_data <-(development_dataset[["dev_data"]])
  
    for (j in 1:5) {
      # Extract the first element (TRUE/FALSE) from the result of is.na()
      is_missing <- is.na(current_data[[paste0("x_", j)]])[1]
      if (is_missing) {  # Check if the first element (is_missing) is TRUE
        cat(i, " ", j, sep = "\n")
      }
    }

  
  
  # Check prevalence 
  #--------------------------------------------------------
    print(paste("Prevalence of Y Iteration:", i))
    print(table((development_dataset[["dev_data"]][["Y"]])))
    print(prop.table(table((development_dataset[["dev_data"]][["Y"]])) * 100))
    
  
  # Produce summaries
  #--------------------------------------------------------
  

    for (j in 1:5) {
      print(paste("Iteration:", i, "Variable: x_", j))
      print(summary((development_dataset[["dev_data"]][[paste0("x_", j)]])))
    }

  
  # DENSITY Plots of coefficients 
  #--------------------------------------------------------
  # Define the colors for each variable
  colors <- c("black", "red", "blue", "green", "purple")
  
  # Loop over each simulation
    # Create an empty vector to store the maximum y values for each variable
    max_y <- c()
    
    # Loop over each variable
    for (j in 1:5) {
      # Calculate the density and add the maximum y value to max_y
      var_density <- density(development_dataset[["dev_data"]][[paste0("x_", j)]])
      max_y <- c(max_y, max(var_density$y))
    }
    
    # Plot the density of the first variable
    plot(density(development_dataset[["dev_data"]][["x_1"]]), main = paste0("Density Plots for Simulation ", i),
         xlab = "Values", ylim = c(0, max(max_y)), 
         xlim = c(-5, 5))  # Set the x-axis limits to -5 and 5
    
    # Add the densities of the other variables
    for (j in 2:5) {
      lines(density(development_dataset[["dev_data"]][[paste0("x_", j)]]), col = colors[j])
    }
    
    # Add a legend
    legend("topright", legend = c("x1", "x2", "x3", "x4", "x5"), fill = colors)
  
  
  
  ################################################################################
  # Check Development Model # 
  ################################################################################
  
  # Check CIs contain true value (this will need to be coded separately)
    CI_current <-  confint(development_dataset[["model"]]) 
    for (j in 1:5) {
      lower_bound <- CI_current[j+1, 1]  # Assuming first column is lower bound
      upper_bound <- CI_current[j+1, 2]  # Assuming second column is upper bound
      
      # Check if x_i falls within the CI (exclusive bounds)
      if (0.5 < lower_bound | 0.5 > upper_bound) {
        warning(paste("CI for iteration", i, "does not contain coefficient x", j, sep = " "))
      }
    }
    
  
  # Check the model 

    current_Y <- development_dataset[["model"]][["y"]]
    current_X1 <- development_dataset[["dev_data"]][["x_1"]]
    current_X2 <- development_dataset[["dev_data"]][["x_2"]]
    current_X3 <- development_dataset[["dev_data"]][["x_3"]]
    current_X4 <- development_dataset[["dev_data"]][["x_4"]]
    current_X5 <- development_dataset[["dev_data"]][["x_5"]]
    
    model.check <- glm(current_Y ~ current_X1 + current_X2 + current_X3 + current_X4 + current_X5,
                       data = development_dataset[["dev_data"]],
                       family = binomial)
    summary(model.check)
    summary(development_dataset[["model"]])
    confint(model.check)
    confint(development_dataset[["model"]])
  
  
  # Check deviance residuals against fitted values
  library(arm)
    
    # Extract residuals, fitted values and Y 
    current_residuals <- residuals(development_dataset[["model"]], type = "deviance")
    current_fitted <- predict(development_dataset[["model"]], type = "response")
    current_Y <- development_dataset[["model"]][["y"]]
    # Plot residuals vs. fitted values 
    plot(current_fitted, current_residuals)
    title(main=paste0("Fitted Values vs. Deviance Residuals for Iteration ", i))
    
    # Plot residuals against predictors
    for (j in 1:5) {
      current_X <- development_dataset[["dev_data"]][[paste0("x_", j)]]
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
      binnedplot(current_residuals, development_dataset[["dev_data"]][[paste0("x_", j)]]) 
    }
    
  
  
  
  
  
  
