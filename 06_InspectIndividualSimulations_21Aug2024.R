
# Load relevant libraries 
################################################################################
library(dplyr)
library(tidyverse)
library(mice)
library(broom)
library(pROC)

################################################################################
# Dependent Code
################################################################################
source("C://Users//maecj//OneDrive - Nexus365//A DPhil//Simulation studies//Programs//Study 1//SimulationStudy1_11Jun2024//SimulationStudy//01_Functions_SimulatingData.R") 

################################################################################
# Simulation Parameters
################################################################################


sims_parameters <- crossing(
  n_iter = 100, 
  N_dev = 500,
  N_val = 500, 
  Y_prev = c(0.01), 
  R_prev = c(0.25, 0.50, 0.75), 
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


###############################################################################
# Run through combinations 
###############################################################################
for (i in 1:nrow(sims_parameters)) {
  # Extract parameters for the current iteration
  n_iter <- sims_parameters$n_iter[i]
  N_val <- sims_parameters$N_val[i]
  N_dev <- sims_parameters$N_dev[i]
  Y_prev <- sims_parameters$Y_prev[i]
  R_prev <- sims_parameters$R_prev[i]
  beta_x1 <- sims_parameters$beta_x1[i]
  beta_x2 <- sims_parameters$beta_x2[i]
  beta_x3 <- sims_parameters$beta_x3[i]
  beta_x4 <- sims_parameters$beta_x4[i]
  beta_x5 <- sims_parameters$beta_x5[i]
  gamma_x1 <- sims_parameters$gamma_x1[i]
  gamma_x2 <- sims_parameters$gamma_x2[i]
  gamma_x3 <- sims_parameters$gamma_x3[i]
  gamma_x4 <- sims_parameters$gamma_x4[i]
  gamma_x5 <- sims_parameters$gamma_x5[i]
  
  
  ################################################################################
  # Store Simulation Results
  ################################################################################
  
  s <-1
  
  ## Simulation Results
  simulation_results <- simulation_nrun_fnc(
    n_iter = n_iter,
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

}

warnings()


################################################################################
# Performance Measures ReCreate
################################################################################
Predicted_Risks_CCA <-simulation_results[["iterations"]][[77]][["preds"]][["preds_per_data_set"]][["CCA_val_data"]][["Prediction_Model"]]
Y_CCA <- simulation_results[["iterations"]][[77]][["preds"]][["preds_per_data_set"]][["CCA_val_data"]][["Y"]]
hist(Predicted_Risks_CCA)
LP_CCA <- log(Predicted_Risks_CCA/ (1 - Predicted_Risks_CCA)) ## Converts predicted probabilities to continuouis (log scale) log odds (i.e., log(odds/1-odds))
hist(LP_CCA)
Cal_Int_CCA <- glm(Y_CCA ~ offset(Predicted_Risks_CCA), family = binomial(link = "logit"))  ## Fits a GLM with binomial family and logit link function, offset uses LP as offset fixing coefficient to 1
Cal_Int_var_CCA <- vcov(Cal_Int_CCA)[1,1] # Variance-covariance matrix of fitted model assesses uncertainty of calibration intercept estimate
summary(Cal_Int_CCA)
auc_CCA <- roc(response = Y_CCA, 
    predictor = as.vector(Predicted_Risks_CCA), 
    direction = "<", levels = c(0, 1))$auc

var(auc_CCA)


Predicted_Risks_mean <- simulation_results[["iterations"]][[77]][["preds"]][["preds_per_data_set"]][["mean_val"]][["Prediction_Model"]]
Y_mean <- simulation_results[["iterations"]][[77]][["preds"]][["preds_per_data_set"]][["mean_val"]][["Y"]]
LP_mean <- log(Predicted_Risks_mean/ (1 - Predicted_Risks_mean)) ## Converts predicted probabilities to continuouis (log scale) log odds (i.e., log(odds/1-odds))
hist(LP_mean)
var(LP_mean)
Cal_Int_mean <- glm(Y_mean ~ offset(LP_mean), family = binomial(link = "logit"))  ## Fits a GLM with binomial family and logit link function, offset uses LP as offset fixing coefficient to 1
Cal_Int_var_mean <- vcov(Cal_Int_mean)[1,1] # Variance-covariance matrix of fitted model assesses uncertainty of calibration intercept estimate
summary(Cal_Int_mean)
Cal_Slope_model_mean <- glm(Y_mean ~ LP_mean, family = binomial(link = "logit"))
as.numeric(coef(Cal_Int_mean))
roc(response = Y_mean, 
    predictor = as.vector(Predicted_Risks_mean), 
    direction = "<", levels = c(0, 1))$auc


Predicted_Risks_MInoY <- simulation_results[["iterations"]][[98]][["preds"]][["preds_per_data_set"]][["MI_val_data_noY"]][["Prediction_Model"]]
hist(Predicted_Risks_MInoY, breaks=20)
Y_MInoY <- simulation_results[["iterations"]][[98]][["preds"]][["preds_per_data_set"]][["MI_val_data_noY"]][["Y"]]
LP_MInoY <- log(Predicted_Risks_MInoY/ (1 - Predicted_Risks_MInoY)) ## Converts predicted probabilities to continuouis (log scale) log odds (i.e., log(odds/1-odds))
hist(LP_MInoY)
Cal_Int_MInoY <- glm(Y_MInoY ~ offset(LP_MInoY), family = binomial(link = "logit"))  ## Fits a GLM with binomial family and logit link function, offset uses LP as offset fixing coefficient to 1
Cal_Int_var_MInoY <- vcov(Cal_Int_MInoY)[1,1] # Variance-covariance matrix of fitted model assesses uncertainty of calibration intercept estimate
summary(Cal_Int_MInoY)
Cal_Slope_model_MInoY <- glm(Y_MInoY ~ LP_MInoY, family = binomial(link = "logit"))

Predicted_Risks_MIwithY <- simulation_results[["iterations"]][[42]][["preds"]][["preds_per_data_set"]][["MI_val_data_withY"]][["Prediction_Model"]]
Y_MIwithY <- simulation_results[["iterations"]][[42]][["preds"]][["preds_per_data_set"]][["MI_val_data_withY"]][["Y"]]
hist(Predicted_Risks_MIwithY)
LP_MIwithY <- log(Predicted_Risks_MIwithY/ (1 - Predicted_Risks_MIwithY)) ## Converts predicted probabilities to continuouis (log scale) log odds (i.e., log(odds/1-odds))
hist(LP_MIwithY)
Cal_Int_MIwithY <- glm(Y_MIwithY ~ offset(LP_MIwithY), family = binomial(link = "logit"))  ## Fits a GLM with binomial family and logit link function, offset uses LP as offset fixing coefficient to 1
Cal_Int_var_MIwithY <- vcov(Cal_Int_MIwithY)[1,1] # Variance-covariance matrix of fitted model assesses uncertainty of calibration intercept estimate
summary(Cal_Int_MIwithY)
Cal_Slope_model_MIwithY <- glm(Y_MIwithY ~ LP_MIwithY, family = binomial(link = "logit"))

as.numeric(coef(Cal_Int_MIwithY))

auc_MIwithY <- roc(response = Y_MIwithY, 
    predictor = as.vector(Predicted_Risks_MIwithY), 
    direction = "<", levels = c(0, 1))$auc
var(auc_MIwithY, method = "delong")



# Create a sequence of numbers from 1 to 100
iteration_numbers <- 1:100

# Use the sequence to extract the corresponding DataFrames
iteration_results <- lapply(iteration_numbers, function(i) simulation_results[["iterations"]][[i]][["preds"]][["target_measures"]])

# Concatenate the results
combined_results <- do.call(rbind, iteration_results)