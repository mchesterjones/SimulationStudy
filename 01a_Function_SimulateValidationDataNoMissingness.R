################################################################################
# 00_SimulateValidationDataNoMissingness.R
################################################################################
# Created: 07Oct024
# Aim: Simulate Validation Data without any missigness
################################################################################

################################################################################
# Load relevant libraries 
################################################################################
library(dplyr)
library(tidyverse)
library(broom)
library(pROC)





###############################################################################
## Functions for running iterations
################################################################################
####-----------------------------------------------------------------------------------------
## Define a function to repeat the simulation across all iterations (for a given scenario)
####-----------------------------------------------------------------------------------------

simulation_nrun_fnc <- function(n_iter,
                                N_val = N_val,
                                Y_prev = Y_prev,
                                gamma_x1 = gamma_x1,
                                gamma_x2 = gamma_x2,
                                gamma_x3 = gamma_x3,
                                gamma_x4 = gamma_x4,
                                gamma_x5 = gamma_x5,
                                gamma_U = gamma_U)

{
  
  
  
  ## Define an empty variable, which will be used to store the results across all iterations
  results <- NULL
  set.seed(n_iter*4)

  all_iterations <- list()
  
  ## Repeat through number of iteratons
  for (iter in 1:n_iter) {
    
    
    
    iter_current <- simulation_singlerun_fnc(N_val = N_val,
                                               Y_prev = Y_prev,
                                               gamma_x1 = gamma_x1,
                                               gamma_x2 = gamma_x2,
                                               gamma_x3 = gamma_x3,
                                               gamma_x4 = gamma_x4,
                                               gamma_x5 = gamma_x5,
                                               gamma_U = gamma_U)
    
    
    
    # Append iterations and data to separate lists
    all_iterations[[iter]] <- iter_current
    
    print(paste("iteration ", iter, " complete", sep = ""))
  }
  
  
  return(list(iterations = all_iterations))
}

  
  


#-------------------------------------------------------------------------------


####---------------------------------------------
## Function that gives a single run per scenario
####---------------------------------------------

simulation_singlerun_fnc <- function(Y_prev,
                                     N_val,
                                     gamma_x1,
                                     gamma_x2,
                                     gamma_x3,
                                     gamma_x4,
                                     gamma_x5,
                                     gamma_U) {
  
  
  parameters <- list(N_val = N_val,
                     Y_prev = Y_prev,
                     gamma_x1 = gamma_x1,
                     gamma_x2 = gamma_x2,
                     gamma_x3 = gamma_x3,
                     gamma_x4 = gamma_x4,
                     gamma_x5 = gamma_x5,
                     gamma_U = gamma_U)
  
  
  #1.val_imp_data function-------------
  df <- simulation_function(N_val = N_val,
                            Y_prev = Y_prev,
                            gamma_x1 = gamma_x1,
                            gamma_x2 = gamma_x2,
                            gamma_x3 = gamma_x3,
                            gamma_x4 = gamma_x4,
                            gamma_x5 = gamma_x5,
                            gamma_U = gamma_U)
  

  # predictions
  preds_per_data_set <- val_imp_mod_function(val_data = df, 
                                          model = model)
  
 
  return(list("val_data" = df, 
              "preds" = preds_per_data_set))
  

}




###############################################################################
## 0. Function that takes the inverse logistic link
###############################################################################

## Function to extract the OR from the coeffiction
expit_function <- function(x) {1/(1+exp(-x))} ## Recal ex/1+ex this is the same to get the P(Y=1)



####---------------------------------------
## 3. Function that simulates Validation data
####---------------------------------------

simulation_function <- function(N_val,
                                Y_prev, ## Prevalence of Y
                                gamma_0,  ## Intercept in Y model
                                gamma_x1,  ## Coefficient of X1 on outcome Y
                                gamma_x2,  ## Coefficient of X2 on outcome Y
                                gamma_x3,  ## Coefficient of X3 on outcome Y
                                gamma_x4,  ## Coefficient of X4 on outcome Y
                                gamma_x5,  ## Coefficient of X5 on outcome Y
                                gamma_U)   ## Coefficient on U on outcome Y 
{
  
  IPD <- tibble("x_1" = rnorm(N_val, mean = 0, sd = 1),
                "x_2" = rnorm(N_val, mean = 0, sd = 1),
                "x_3" = rnorm(N_val, mean = 0, sd = 1),
                "x_4" = rnorm(N_val, mean = 0, sd = 1),
                "x_5" = rnorm(N_val, mean = 0, sd = 1),
                "U" = rnorm(N_val, mean = 0, sd = 1), 
                "ID" = 1:N_val
  )
  
  

  #determine the prevalence of Y through _0
  gamma_0 <- as.numeric(coef(glm(rbinom(N_val, 1, prob = Y_prev) ~
                                   offset(gamma_x1*x_1 +
                                            gamma_x2*x_2 +
                                            gamma_x2*x_3 +
                                            gamma_x4*x_4 +
                                            gamma_x5*x_5 + 
                                            gamma_U*U),
                                 family = binomial(link = "logit"),
                                 data = IPD))[1])
  
  ########
  IPD$Y = rbinom(N_val, size = 1,
                 prob = expit_function(gamma_0 +
                                         gamma_x1*IPD$x_1 +
                                         gamma_x2*IPD$x_2 +
                                         gamma_x3*IPD$x_3 +
                                         gamma_x4*IPD$x_4 +
                                         gamma_x5*IPD$x_5 +
                                         gamma_U*IPD$U))
  ########
  
  
  ## Store true x values 
  val_data <- IPD 
  
  return(list("val_data" = val_data))
  
}


# 
####------------------------------------------------------
##  5. Function that returns prediction for a single dataset
####-----------------------------------------------------

#this function takes the results of each imputed dataset and the model(s)
# and then generates predictions from each model on each imputed dataset.
#The final output combines the original target variable with predictions from
#all models into a single data frame.

# The MICE datasets are structured differently and therefore need slightly different code


predict_single_imputed <- function(val_data, model) {

    # Extract Y from imputed dataset
    output_predictions <- data.frame("Y" = val_data$Y) #defining the Y as the Y column from the imputed datasets

    # Nested function to get predictions
    get_predictions <- function() {
      # Predictions hold predicted values from CCA and MI
      predictions <- tryCatch({
        predict(model, newdata=val_data, type = 'response')
      }, error = function(e) {
        warning("Error during prediction with mids object. Returning NA predictions:", e)
        return(NA)
      })
      # Output of function
      return(predictions)
    }

    predictions <- get_predictions()  ## Call nested function above


  # Combine data frames: "Y" values (output_predictions) and predicted values (predictions)
  final_predictions <- cbind(output_predictions, predictions)  # Assuming 'Y' and predictions have the same number of rows

  # Rename the column holding predictions (assuming it's the second column)
  names(final_predictions)[2] <- "Prediction_Model"

  # Output exploration (optional)
#  str(final_predictions)
#  head(final_predictions, n=5)

  return(final_predictions)
}




###-----------------------------------------------------------
# 6. Function that the model to all imputed datasets
###-----------------------------------------------------------
val_imp_mod_function <- function(val_data, model) {

  # Debugging
 # str(val_data)
  #print(val_data)

  ## Make predictions for each imputed dataset using the development model 
  preds_per_data_set <-  map(val_data, predict_single_imputed, model = model)
  head(preds_per_data_set, n=5)
  
  target_measures <- c()
  
  for (i in seq_along(preds_per_data_set)) {
    
    # Extract Y and Prediction_Model from the current sub-list
    current_Y <- preds_per_data_set[[i]]$Y
    current_pred <- preds_per_data_set[[i]]$Prediction_Model
    
  
    # Check for convergence issues (example: NA values in predictions)
    if (any(is.na(current_pred))) {
      warning(paste("Convergence issue detected in dataset:", names(preds_per_data_set)[i]))
    }
    
    # Create a data frame for this element and add it to target_measures
    data_frame_to_add <- data.frame(dataset = names(preds_per_data_set)[i],
                                    predictive.performance.function(Y = current_Y,
                                    Predicted_Risks = current_pred))
    
    # Bind dataset to create target measures
    target_measures <- rbind(target_measures, data_frame_to_add)
  }
  
  
  return(list("preds_per_data_set"=preds_per_data_set, "target_measures"=target_measures))
}




####---------------------------------------------------------------
## 7. Function to calculate the predictive performance of the models
####---------------------------------------------------------------
predictive.performance.function <- function(Y, Predicted_Risks) {
  
  library(pROC)
  
  #Input:
  # Y = a binary variable of observed outcomes
  # Predicted_Risks = a vector of predicted risks for each dataset
  # Within a given dataset, we want a table with the target measures (use dataframe from above)
  
  ## Calculate Brier Score (mean square error of predictions; the lower, the better)
  ####------------------------------------------------------------------
  # What is the accuracy of the probalistic predictions? 
  Brier_individuals <- (Predicted_Risks - Y)^2 ## Predicted risks - true outcome
  Brier <- mean(Brier_individuals) # Average of these sqaured errors
  Brier_var <- var(Brier_individuals)/length(Predicted_Risks) # Variance of the individual Brier Scores 
  # Calculate the baseline Brier score
  p_baseline <- mean(Y) # Prevalence of the outcome
  Brier_baseline <- mean((p_baseline - Y)^2)
  
  # Scale the Brier score
  if (mean(Y) == 0 || mean(Y) == 1) {
    Brier_scaled <- NA # Or another placeholder to indicate undefined
  } else {
    Brier_scaled <- 1 - (Brier / Brier_baseline)
  }
  
  
  ## Calibration intercept and Slope 
  ####-------------------------------------------------------------------------------
  ## (i.e. Calibration-in-the-large): Overall Bias in Predictions
  ## Why this measure? 
  ## We want to understand whether what we are predicting is systematically too high or too low compared to true OUTCOMES
  ## Does the average predicted probability match the average observed outcome 
  ## Named intercept because that's where regression line crosses y-axis
  ## Therefore if intercept is significantly different from zero, model's predictions are systematically biased
  
  ## Calibration slope: Spread or dispersion of predictions
  ####--------------------------------------------------------------------------
  ## Why this measure? 
  ## This measures the spread of predictions over entire range 
  ## Measures the agreements between observed and actual
  # 1 = perfect calibration
  # Slope < 1: Indicates that the model’s predictions are too extreme.  High probabilities are overestimated, and low probabilities are underestimated.
  # Slope > 1: Indicates that the model’s predictions are too conservative. High probabilities are underestimated, and low probabilities are overestimated.
  
  # Calculate log of predicted risks 
  LP <- log(Predicted_Risks/ (1 - Predicted_Risks))
  
  # Calculate variance of log of PRs
  variance_LP <- var(LP)
  # print(variance_LP)
  
  # Calculate variance of predicted risks
  variance_PR <- var(Predicted_Risks)
  # print(variance_PR)
  
  # Check if variance is zero
  if (variance_PR == 0) {
    message("All predicted risks are the same and variance is zero")
  }
  # Check if variance is very small 
  if (variance_PR < 1e-10) {
    message("All predicted risks are very similar and variance is very small")
  }
  
  
  # Check if variance of Log Odds is very large 
  if (variance_LP >= 8 ) {
    message("Log  risks variance is too high indicating extremes")
  }
  
  # Check if variance of Log Odds is very large 
  # if (variance_LP > 6 & variance_LP <9 ) {
  #    message("Does this exist and is not an error?")
  #  }
  
  # Check if all outcomes are zero 
  if (sum(Y) == 0) { 
    message("All outcomes are zero")
  }
  
  ## Put Calibration to NAs if outcomes are all the same and variance of predicted risks is 0 
  if ((sum(Y) == 0) || (variance_PR == 0) || (variance_PR <1e-10)  ||   (variance_LP > 8) ){
    Cal_Int <- NA
    Cal_Int_var <- NA
    Cal_Slope <- NA
    Cal_Slope_var <- NA
    Cal_Slope_SE <- NA
    
    message("Calibration intercept and slope cannot be calculated.")
    
  } else {
    ## Calibration in the Large
    Cal_Int_model <- glm(Y ~ offset(LP), family = binomial(link = "logit"))
    Cal_Int_var <- vcov(Cal_Int_model)[1, 1]
    Cal_Int <- as.numeric(coef(Cal_Int_model))
    # print(Cal_Int)
    
    ## Calibration Model
    Cal_Slope_model <- glm(Y ~ LP, family = binomial(link = "logit"))
    Cal_Slope_var <- vcov(Cal_Slope_model)[2, 2]
    Cal_Slope_SE <- summary(Cal_Slope_model)$coefficients[, 2][2]
    
    if (Cal_Slope_var >10 ) {
      message("Calibration Variance too high and slope cannot be calculated.")
      
      Cal_Slope <- NA
      Cal_Int <- NA
      Cal_Int_var <- NA
      Cal_Slope_var <- NA
    } else {
      Cal_Slope <- as.numeric(coef(Cal_Slope_model)[2])
      
      
    }
    #print(Cal_Slope)
  }
  
  
  
  ## Discrimination (c-statistic?)
  ####------------------------------------------------------------------------
  
  ## Updated code 25Nov
  if ((sum(Y) == 0) || (variance_PR == 0) || (variance_PR < 1e-10) || (variance_LP > 9)) {
    AUC <- NA
    AUC_var <- NA
  } else {
    AUC <- tryCatch({
      roc_obj <- roc(response = Y, predictor = as.vector(Predicted_Risks), direction = "<", levels = c(0, 1))
      roc_obj$auc
    }, error = function(e) {
      message("Error in ROC calculation: ", e$message)
      NA  # Return NA or any other placeholder value
    })
    
    if (!is.na(AUC) && AUC == 1) {
      AUC <- NA
      message("AUC is 1, setting AUC to NA.")
    }
    
    AUC_var <- tryCatch({
      if (!is.na(AUC)) {
        var(roc_obj, method = "delong")
      } else {
        NA
      }
    }, error = function(e) {
      message("Error in variance calculation: ", e$message)
      NA  # Return NA if there's an error in variance calculation
    })
    
    if (is.na(AUC_var) || AUC_var <= 0) {
      AUC <- NA
      AUC_var <- NA
      message("AUC variance cannot be calculated or is invalid. Setting AUC to NA.")
    }
  }
  
  
  
  
  ## Store performance results in a data.frame and return
  ####------------------------------------------------------------------------
  Target_measures <- data.frame(
    "Cal_Int" =  as.numeric(Cal_Int),
    "Cal_Int_var" = Cal_Int_var,
    "Cal_Slope" = Cal_Slope,
    "Cal_Slope_var" = Cal_Slope_var,
    "AUC" = as.numeric(AUC),
    "AUC_var" = as.numeric(AUC_var),
    "Brier" = as.numeric(Brier),
    "Brier_scaled" = as.numeric(Brier_scaled)
  )
  
  
  return(Target_measures)
  
}

