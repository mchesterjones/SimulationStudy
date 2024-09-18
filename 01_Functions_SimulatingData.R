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





###############################################################################
## Functions for running iterations
################################################################################
####-----------------------------------------------------------------------------------------
## Define a function to repeat the simulation across all iterations (for a given scenario)
####-----------------------------------------------------------------------------------------

simulation_nrun_fnc <- function(n_iter,
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

{
  
  
  
  ## Define an empty variable, which will be used to store the results across all iterations
  results <- NULL
  set.seed(n_iter*4)

  all_iterations <- list()
  
  ## Repeat through number of iteraitons
  for (iter in 1:n_iter) {
    
    
    
    iter_current <- simulation_singlerun_fnc(N_val = N_val,
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
                                     R_prev,
                                     beta_x1,
                                     beta_x2,
                                     beta_x3,
                                     beta_x4,
                                     beta_x5,
                                     beta_U,
                                     gamma_x1,
                                     gamma_x2,
                                     gamma_x3,
                                     gamma_x4,
                                     gamma_x5,
                                     gamma_U) {
  
  
  parameters <- list(N_val = N_val,
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
  
  
  #1.val_imp_data function-------------
  df <- simulation_function(N_val = N_val,
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
  
  #4.master imputation_function-----------------------
  # Note: not changed this
  imputed_datasets <- imputation_function(df = df, m=5)
  
  
  # predictions
  preds_per_data_set <- val_imp_mod_function(imputed_datasets = imputed_datasets, 
                                          model = model)
  
 
  return(list(#"Parameters" = parameters,
              #"dev_data" = dev_data, 
              #"model"=model,
              #"val_data" = df, 
             # "imputed_datasets" = imputed_datasets, 
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
                                R_prev,
                                beta_x1,  ## Coefficient of X1 on Missing
                                beta_x2, ## Coefficient of X2 on Missing
                                beta_x3,  ## Coefficient of X3 on Missing
                                beta_x4, ## Coefficient of X4 on Missing
                                beta_x5,  ## Coefficient of X5 on Missing
                                beta_U,   ## Coefficient of U on Missing 
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
  
  
  #determine the prevalence of R through beta_0
  beta_0 <- as.numeric(coef(glm(rbinom(N_val, 1, prob = R_prev) ~
                                  offset(beta_x1*x_1 +
                                           beta_x2*x_2 +
                                           beta_x3*x_3 +
                                           beta_x4*x_4 +
                                           beta_x5*x_5 + #R_prev is the missingness level so we control that? We control this by the Y_prev(the number of times R1 is 1 or 0)
                                           beta_U*U),
                                family = binomial(link = "logit"),
                                data = IPD))[1])
  
  IPD$R_1 = rbinom(N_val, size = 1,
                   prob = expit_function(beta_0 +
                                           beta_x1*IPD$x_1 +
                                           beta_x2*IPD$x_2 +
                                           beta_x3*IPD$x_3 +
                                           beta_x4*IPD$x_4 +
                                           beta_x5*IPD$x_5 + 
                                           beta_U*IPD$U))
  
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
  val_data <- IPD %>%
    mutate(x_1true = x_1)
  
  ## Remove x_1 if R_1 ==0
  val_data <- val_data %>%
    mutate(x_1 = ifelse(R_1 == 0, x_1, NA)) ## Error spotted 20Aug2024
  
  
  return(list("val_data" = val_data))
  
}



################################################################################
## Multiple Imputation Functions
################################################################################
#
####---------------------------------------------
## Multiple imputation function
####---------------------------------------------

mice_function <- function(df, m = 5, Y) {
  
  dummyrun <- mice(df, m = 1, maxit = 0, printFlag = FALSE)
  predmat <- dummyrun$predictorMatrix
  
  if (Y ==FALSE){        #if we don't want to include the Y, then we set it to be 0.
    predmat["Y",] <- 0
    predmat[,"Y"] <- 0
  }
  predmat[,"ID"] <- 0  #we dont want MI to impute the missing values based on ID, U, x_1true or R1 hence they're set to be 0
  predmat["ID",] <- 0
  predmat[,"R_1"] <- 0
  predmat["R_1",] <- 0
  predmat[,"x_1true"] <- 0
  predmat["x_1true",] <- 0
  predmat[,"U"] <- 0
  predmat["U",] <- 0
  
  # Attempt imputation using try-catch (unchanged)
  imp <- tryCatch({
    mice(df, m, predictorMatrix = predmat, printFlag = FALSE, maxit = 1)
  }, error = function(e) {
    message("MICE imputation failed:", e)
    return(NULL)  # Return NULL on error
  })
  
  # Check if imputation succeeded (unchanged)
  if (!is.null(imp)) {
    console_output <- capture.output(imp)
    return(list(console_output = console_output, MI_data = imp))
  } else {
    # Imputation failed, return NA for MI_data (unchanged)
    return(list(console_output = character(0), MI_data = data.frame(matrix(NA, nrow = nrow(df), ncol = ncol(df)))))  
  }

  
}


####---------------------------------------------
## Mean imputation function
####---------------------------------------------

mean_function <- function(df) {
  mean_imputed_df <- df
  
  mean_imputed_df <- mean_imputed_df %>%
    mutate(across(starts_with("x_"),
                  ~tidyr::replace_na(.x,
                                     mean(.x, na.rm = TRUE))))
  
  mean_imputed_df
}


####-------------
## CCA function
####-------------
CCA_function <- function(df) {
  df[complete.cases(df), ]
}



####--------------------------
## 3. Master imputation function
####--------------------------

imputation_function <- function(df, m = 5) {
  
  ## Extract MI with console output 
  MI_noY <- mice_function(df$val_data, m = m, Y = FALSE)
  MI_withY <- mice_function(df$val_data, m = m, Y = TRUE)
  
  ## Extract MIDS 
  MI_val_data_noY <- MI_noY$MI_data
 # str(MI_val_data_noY)
  MI_val_data_withY <- MI_withY$MI_data
 # str(MI_val_data_withY)
  
  # Check for logged events
  if (!is.null(MI_val_data_noY$loggedEvents)) {
    warning("Logged events occurred during imputation. Check console output for details.")
    cat(MI_noY$console_output)  # Print captured output
  }
  
  # Check for logged events
  if (!is.null(MI_val_data_withY$loggedEvents)) {
    warning("Logged events occurred during imputation. Check console output for details.")
    cat(MI_withY$console_output)  # Print captured output
  }
  
  
  # Handle potentially missing MI_data
  if (is.null(MI_val_data_noY)) {
    warning("MICE imputation failed for data without Y. Using NA values.")
    MI_val_data_noY <- data.frame(matrix(NA, nrow = nrow(df$val_data), ncol = ncol(df$val_data)))
  }
  if (is.null(MI_val_data_withY)) {
    warning("MICE imputation failed for data with Y. Using NA values.")
    MI_val_data_withY <- data.frame(matrix(NA, nrow = nrow(df$val_data), ncol = ncol(df$val_data)))
  }
  
  
  CCA_val_data <- CCA_function(df$val_data)
  
  mean_val <- mean_function(df$val_data)
  
 # str(CCA_val_data)
  
  return(list(
    "CCA_val_data" = CCA_val_data,
    "mean_val" = mean_val,
    "MI_val_data_noY" = MI_val_data_noY,
    "MI_val_data_withY" = MI_val_data_withY

  ))
  
}


####------------------------------------------------------
##  5. Function that returns prediction for a single dataset
####-----------------------------------------------------

#this function takes the results of each imputed dataset and the model(s)
# and then generates predictions from each model on each imputed dataset.
#The final output combines the original target variable with predictions from 
#all models into a single data frame.

# The MICE datasets are structured differently and therefore need slightly different code 


predict_single_imputed <- function(imputed_datasets, model) {
  
## First part is for CCA and MI analysis 
  if(is.mids(imputed_datasets) == FALSE){ #is.mids checking whether the object is mice or not
    
    # Check for existence of "Y" variable
    if (!"Y" %in% colnames(imputed_datasets)) {
      warning("Target variable 'Y' not found in imputed_datasets. Returning NA predictions.")
      return(data.frame(Y = NA, Prediction_Model = NA))
    }
    
    # Extract Y from imputed dataset
    output_predictions <- data.frame("Y" = imputed_datasets$Y) #defining the Y as the Y column from the imputed datasets
    
    # Nested function to get predictions 
    get_predictions <- function() {
      # Predictions hold predicted values from CCA and MI
      predictions <- tryCatch({
        predict(model, newdata=imputed_datasets, type = 'response')
      }, error = function(e) {
        warning("Error during prediction with mids object. Returning NA predictions:", e)
        return(NA)
      })
      # Output of function 
      return(predictions)
    }
    
    predictions <- get_predictions()  ## Call nested function above
    
  }else {
    
  ## For MICE datasets 
    MI_long <- mice::complete(imputed_datasets, action = 'long') #extracts imputed datasets from a 'mids' object
    
    # Structure of MI_long
   # str(MI_long)
    # Print the first few rows of MI_long 
   # head(MI_long, n=5)
    
    
    # Check for existence of "Y" variable
    if (!"Y" %in% colnames(MI_long)) {
      warning("Target variable 'Y' not found in MI_long after completing mids object. Returning NA predictions.")
      return(data.frame(Y = NA, Prediction_Model = NA))
    }
    ## This data frame would have additional columns indicating the imputation number and potentially the original row identifier.
    ## It then extracts the target variable "Y" from MI_long for prediction
    output_predictions <- data.frame("Y" = MI_long$Y) ## Extracts Y 
    
    ## Predicts new predictions
    predictions <- tryCatch({
      predict(model, newdata=MI_long, type = 'response')
    }, error = function(e) {
      warning("Error during prediction with mids object. Returning NA predictions:", e)
      return(NA)
    })
  }
  
  


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
val_imp_mod_function <- function(imputed_datasets, model) {

  # Debugging
 # str(imputed_datasets)
  #print(imputed_datasets)

  ## Make predictions for each imputed dataset using the development model 
  preds_per_data_set <-  map(imputed_datasets, predict_single_imputed, model = model)
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
  if ((sum(Y) == 0) | (variance_PR == 0) | (variance_PR <1e-10)  |   (variance_LP > 8) ){
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
      
      if (Cal_Slope_SE >10 ) {
        message("Calibration SE too high and slope cannot be calculated.")
        
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
 
  ## Updated the code 19Aug2024 to handle errors of no positve cases that may occur at small sample sizes
  if ((sum(Y) == 0) | (variance_PR == 0) | (variance_PR <1e-10)  |   (variance_LP > 9) ){
    AUC <-NA
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
    AUC_var <- if (!is.na(AUC)) {
      var(roc_obj, method = "delong")
    } else {
      NA
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
    "Brier_var" = as.numeric(Brier_var)
  )


  return(Target_measures)

}



