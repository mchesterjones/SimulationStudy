################################################################################
# 01_Functions_SimulatingaData.R
################################################################################
# Created: 11Jun2024
# Aim: Simulate and analyse data with checks for failure 
################################################################################

################################################################################
# Load relevant libraries 
################################################################################
library(dplyr)
library(tidyverse)
library(broom)
library(pROC)

# library(boot)
# library(mice)



###############################################################################
## Functions for running iterations
################################################################################
####-----------------------------------------------------------------------------------------
## Define a function to repeat the simulation across all iterations (for a given scenario)
####-----------------------------------------------------------------------------------------

simulation_nrun_fnc <- function(n_iter,
                                N_dev = N_dev,
                                N_val = N_val,
                                Y_prev = Y_prev,
                                R_prev = R_prev,
                                beta_x1 = beta_x1,
                                beta_x2 = beta_x2,
                                beta_x3 = beta_x3,
                                beta_x4 = beta_x4,
                                beta_x5 = beta_x5,
                                gamma_x1 = gamma_x1,
                                gamma_x2 = gamma_x2,
                                gamma_x3 = gamma_x3,
                                gamma_x4 = gamma_x4,
                                gamma_x5 = gamma_x5)

{
  
  
  
  ## Define an empty variable, which will be used to store the results across all iterations
  results <- NULL
  set.seed(123456)
  
  all_dev_data <- list()
  all_models <- list()
  all_val_data <- list()
  
  ## Repeat through number of iterations
  for (iter in 1:n_iter) {
    
    dev_data_current <- dev_data_simulation_function(N_dev = N_dev,
                                                     Y_prev = Y_prev,
                                                     gamma_x1 = gamma_x1,
                                                     gamma_x2 = gamma_x2,
                                                     gamma_x3 = gamma_x3,
                                                     gamma_x4 = gamma_x4,
                                                     gamma_x5 = gamma_x5)
    
    
    
    
    val_data_current <- simulation_function(N_val = N_val,
                                            Y_prev = Y_prev,
                                            R_prev = R_prev,
                                            beta_x1 = beta_x1,
                                            beta_x2 = beta_x2,
                                            beta_x3 = beta_x3,
                                            beta_x4 = beta_x4,
                                            beta_x5 = beta_x5,
                                            gamma_x1 = gamma_x1,
                                            gamma_x2 = gamma_x2,
                                            gamma_x3 = gamma_x3,
                                            gamma_x4 = gamma_x4,
                                            gamma_x5 = gamma_x5)
    
    models_current <- simulation_singlerun_fnc(N_dev = N_dev,
                                               N_val = N_val,
                                               Y_prev = Y_prev,
                                               R_prev = R_prev,
                                               beta_x1 = beta_x1,
                                               beta_x2 = beta_x2,
                                               beta_x3 = beta_x3,
                                               beta_x4 = beta_x4,
                                               beta_x5 = beta_x5,
                                               gamma_x1 = gamma_x1,
                                               gamma_x2 = gamma_x2,
                                               gamma_x3 = gamma_x3,
                                               gamma_x4 = gamma_x4,
                                               gamma_x5 = gamma_x5)
    
    # Append models and data to separate lists
    all_models[[iter]] <- models_current
    all_dev_data[[iter]] <- dev_data_current
    all_val_data[[iter]] <- val_data_current
    
    print(paste("iteration ", iter, " complete", sep = ""))
  }
  
  
  return(list(models = all_models, dev_data = all_dev_data, val_data=all_val_data))
}

#-------------------------------------------------------------------------------


####---------------------------------------------
## Function that gives a single run per scenario
####---------------------------------------------

simulation_singlerun_fnc <- function(N_dev,
                                     Y_prev,
                                     N_val,
                                     R_prev,
                                     beta_x1,
                                     beta_x2,
                                     beta_x3,
                                     beta_x4,
                                     beta_x5,
                                     gamma_x1,
                                     gamma_x2,
                                     gamma_x3,
                                     gamma_x4,
                                     gamma_x5) {
  
  
  #1.dev_data function------------
  dev_data <- dev_data_simulation_function(N_dev = N_dev,
                                           gamma_x1 = gamma_x1,
                                           gamma_x2 = gamma_x2,
                                           gamma_x3 = gamma_x3,
                                           gamma_x4 = gamma_x4,
                                           gamma_x5 = gamma_x5,
                                           Y_prev = Y_prev)
  
  
  #2.dev_mod_function------------------
  models <- dev_mod_function(dev_data = dev_data)
  
  #3.val_imp_data function-------------
  df <- simulation_function(N_val = N_val,
                            Y_prev = Y_prev,
                            R_prev = R_prev,
                            beta_x1 = beta_x1,
                            beta_x2 = beta_x2,
                            beta_x3 = beta_x3,
                            beta_x4 = beta_x4,
                            beta_x5 = beta_x5,
                            gamma_x1 = gamma_x1,
                            gamma_x2 = gamma_x2,
                            gamma_x3 = gamma_x3,
                            gamma_x4 = gamma_x4,
                            gamma_x5 = gamma_x5)
  
  
  
  
  return(list("dev_data" = dev_data, "models"=models, "validation_data" = df))
  
  
}




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
    gamma_x5)  ## Coefficient of X5 on outcome Y
  
  
{
  
  
  dev_data_IPD <- tibble("x_1" = rnorm(N_dev, mean = 0, sd = 1),
                         "x_2" = rnorm(N_dev, mean = 0, sd = 1),
                         "x_3" = rnorm(N_dev, mean = 0, sd = 1),
                         "x_4" = rnorm(N_dev, mean = 0, sd = 1),
                         "x_5" = rnorm(N_dev, mean = 0, sd = 1),
                         "ID" = 1:N_dev
  )
  
  
  
  #determine the prevalence of the outcome based on the gamma_0
  gamma_0 <- as.numeric(coef(glm(rbinom(N_dev, 1, prob = Y_prev) ~
                                   offset(gamma_x1*x_1 +
                                            gamma_x2*x_2 +
                                            gamma_x2*x_3 +
                                            gamma_x4*x_4 +
                                            gamma_x5*x_5),
                                 family = binomial(link = "logit"),
                                 data = dev_data_IPD))[1])
  
  dev_data_IPD$Y = rbinom(N_dev, size = 1,
                          prob = expit_function(gamma_0 +
                                                  gamma_x1*dev_data_IPD$x_1 +
                                                  gamma_x2*dev_data_IPD$x_2 +
                                                  gamma_x3*dev_data_IPD$x_3 +
                                                  gamma_x4*dev_data_IPD$x_4 +
                                                  gamma_x5*dev_data_IPD$x_5))
  
  return(dev_data_IPD)
}

# 
####---------------------------------------
## 2. Model fitting function
####---------------------------------------

dev_mod_function <- function(dev_data) {
  
  # Fit Model
  model_1 <- glm(Y ~ x_1 + x_2 + x_3 + x_4 + x_5, data = dev_data, family = binomial)
  
  # Obtaining coeff for fi_0, fi_1, fi_2
  fi_0 <- coef(model_1)[1]
  fi_x1 <- coef(model_1)[2]
  fi_x2 <- coef(model_1)[3]
  fi_x3 <- coef(model_1)[4]
  fi_x4 <- coef(model_1)[5]
  fi_x5 <- coef(model_1)[6]
  

  return(model_1)
}


####---------------------------------------
## 3. Simulate Validation data
####---------------------------------------

simulation_function <- function(N_val,
                                Y_prev, ## Prevalence of Y
                                R_prev,
                                beta_x1,  ## Coefficient of X1 on Missing
                                beta_x2, ## Coefficient of X2 on Missing
                                beta_x3,  ## Coefficient of X3 on Missing
                                beta_x4, ## Coefficient of X4 on Missing
                                beta_x5,  ## Coefficient of X5 on Missing
                                gamma_0,  ## Intercept in Y model
                                gamma_x1,  ## Coefficient of X1 on outcome Y
                                gamma_x2,  ## Coefficient of X2 on outcome Y
                                gamma_x3,  ## Coefficient of X3 on outcome Y
                                gamma_x4,  ## Coefficient of X4 on outcome Y
                                gamma_x5)  ## Coefficient of X5 on outcome Y
{
  
  IPD <- tibble("x_1" = rnorm(N_val, mean = 0, sd = 1),
                "x_2" = rnorm(N_val, mean = 0, sd = 1),
                "x_3" = rnorm(N_val, mean = 0, sd = 1),
                "x_4" = rnorm(N_val, mean = 0, sd = 1),
                "x_5" = rnorm(N_val, mean = 0, sd = 1),
                "ID" = 1:N_val
  )
  
  
  #determine the prevalence of R through beta_0
  beta_0 <- as.numeric(coef(glm(rbinom(N_val, 1, prob = R_prev) ~
                                  offset(beta_x1*x_1 +
                                           beta_x2*x_2 +
                                           beta_x3*x_3 +
                                           beta_x4*x_4 +
                                           beta_x5*x_5), #R_prev is the missingness level so we control that? We control this by the Y_prev(the number of times R1 is 1 or 0)
                                family = binomial(link = "logit"),
                                data = IPD))[1])
  
  IPD$R_1 = rbinom(N_val, size = 1,
                   prob = expit_function(beta_0 +
                                           beta_x1*IPD$x_1 +
                                           beta_x2*IPD$x_2 +
                                           beta_x3*IPD$x_3 +
                                           beta_x4*IPD$x_4 +
                                           beta_x5*IPD$x_5))
  
  #determine the prevalence of Y through _0
  gamma_0 <- as.numeric(coef(glm(rbinom(N_val, 1, prob = Y_prev) ~
                                   offset(gamma_x1*x_1 +
                                            gamma_x2*x_2 +
                                            gamma_x2*x_3 +
                                            gamma_x4*x_4 +
                                            gamma_x5*x_5),
                                 family = binomial(link = "logit"),
                                 data = IPD))[1])
  
  ########
  IPD$Y = rbinom(N_val, size = 1,
                 prob = expit_function(gamma_0 +
                                         gamma_x1*IPD$x_1 +
                                         gamma_x2*IPD$x_2 +
                                         gamma_x3*IPD$x_3 +
                                         gamma_x4*IPD$x_4 +
                                         gamma_x5*IPD$x_5))
  ########
  
  
  validation_data <- IPD %>%
    mutate(x_1 = ifelse(R_1 == 1, x_1, NA))
  
  
  return(list("validation_data" = validation_data))
  
}
