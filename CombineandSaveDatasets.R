################################################################################
## Combine and Save datasets
################################################################################
## Purpose: Post simulation, combine results from different scenarios to allow graphs to be generated with validation no missing
## Created: 07Oct2024
## Author: MCJ
################################################################################

################################################################################
## Packages
################################################################################
library(dplyr)
library(tidyr)
library(purrr)
################################################################################
## 100000 ## 
################################################################################
## Open datasets

## Set working directory 
setwd("C:\\Users\\maecj\\OneDrive - Nexus365\\A DPhil\\Simulation studies\\Programs\\Study 1\\SimulationStudy1_11Jun2024\\SimulationStudy\\Data") 
## Load required datasets 
#-----------------------------------------
load("Validation_NoMissingness_Nval_1e+05_Yprev_0.01_07Oct2024.Rdata")
simresults_Yprev1Rprev25 <- simulation_results
simresults_Yprev1Rprev50 <- simulation_results
simresults_Yprev1Rprev75 <- simulation_results
load("Validation_NoMissingness_Nval_1e+05_Yprev_0.05_07Oct2024.Rdata")
simresults_Yprev5Rprev25 <- simulation_results
simresults_Yprev5Rprev50 <- simulation_results
simresults_Yprev5Rprev75 <- simulation_results
load("Validation_NoMissingness_Nval_1e+05_Yprev_0.1_07Oct2024.Rdata")
simresults_Yprev10Rprev25 <- simulation_results
simresults_Yprev10Rprev50 <- simulation_results
simresults_Yprev10Rprev75 <- simulation_results


# Extract target measures 
extract_measures_fnc <- function(simresults) {
  combined_data <- list()
  
  for (i in 1:length(simresults[["iterations"]])) {
    df <- simresults[["iterations"]][[i]][["preds"]][["target_measures"]]
    df <- cbind(iteration = i, df)  # Add the iteration column
    combined_data[[i]] <- df
  }
  
  return(do.call(rbind, combined_data))
}

# Combine data for both datasets
target_measures <- list(
  simresults_Yprev10Rprev75 = extract_measures_fnc(simresults_Yprev10Rprev75),
  simresults_Yprev10Rprev50 = extract_measures_fnc(simresults_Yprev10Rprev50),
  simresults_Yprev10Rprev25 = extract_measures_fnc(simresults_Yprev10Rprev25),
  simresults_Yprev5Rprev75 = extract_measures_fnc(simresults_Yprev5Rprev75),
  simresults_Yprev5Rprev50 = extract_measures_fnc(simresults_Yprev5Rprev50),
  simresults_Yprev5Rprev25 = extract_measures_fnc(simresults_Yprev5Rprev25),
  simresults_Yprev1Rprev75 = extract_measures_fnc(simresults_Yprev1Rprev75),
  simresults_Yprev1Rprev50 = extract_measures_fnc(simresults_Yprev1Rprev50),
  simresults_Yprev1Rprev25 = extract_measures_fnc(simresults_Yprev1Rprev25)
)


## Create summary of bias 
# Define the number of iterations
num_iterations <- 200


# Initialize empty lists to store bias summaries for each dataset
bias_summaries <- list(
  simresults_Yprev10Rprev25 = data.frame(),
  simresults_Yprev10Rprev50 = data.frame(),
  simresults_Yprev10Rprev75 = data.frame(),
  simresults_Yprev5Rprev75 = data.frame(),
  simresults_Yprev5Rprev50 = data.frame(),
  simresults_Yprev5Rprev25 = data.frame(),
  simresults_Yprev1Rprev75 = data.frame(),
  simresults_Yprev1Rprev50 = data.frame(),
  simresults_Yprev1Rprev25 = data.frame()
)

# Go through each dataset and iteration to calculate
for (i in 1:num_iterations) {
  # Loop through each dataset
  for (dataset_name in names(bias_summaries)) {
    dataset <- get(dataset_name)
    
    
    # Extract true Y and estimated Y values
    # Extract true Y and estimated Y values
    true_Y <- dataset[["iterations"]][[i]][["preds"]][["preds_per_data_set"]][["val_data"]][["Y"]]
    estimated_Y <- dataset[["iterations"]][[i]][["preds"]][["preds_per_data_set"]][["val_data"]][["Prediction_Model"]]
    
    # Calculate number of observations (n)
    n <- length(true_Y)
    
    # Calculate bias
    bias <- sum(true_Y - estimated_Y) / n
    
    # Calculate MSE 
    mse <- sum((true_Y - estimated_Y)^2) / n
    
    # Calculate RMSE 
    rmse <- sqrt(mse)
    
    # Create a temporary row for the current iteration and dataset
    new_row <- data.frame(iteration = i, dataset = "val_data",n = n, bias = bias, mse=mse, rmse=rmse)
    
    # Append the temporary row to the corresponding bias_summary data frame
    bias_summaries[[dataset_name]] <- rbind(bias_summaries[[dataset_name]], new_row)
  }
  
}


# Combine target measures and bias_summary
combined_summaries <- list()

for (dataset_name in names(target_measures)) {
  target_measures_df <- as.data.frame(target_measures[[dataset_name]])
  bias_summary_df <- as.data.frame(bias_summaries[[dataset_name]])
  
  combined_df <- full_join(target_measures_df, bias_summary_df, by = c("iteration", "dataset"))
  
  combined_summaries[[dataset_name]] <- combined_df
}

# Combine into one dataset 
combined_df <- combined_summaries %>%
  imap_dfr(~ .x %>% mutate(df = .y))

## Add in Method Categories
combined_df <- combined_df %>%
  mutate(Method = case_when(dataset=="val_data" ~ "Validation data, no missingness"),
         Parameter = case_when(
           df == "simresults_Yprev10Rprev25" ~ "Outcome prevalence 10% and Missingness 25%",
           df == "simresults_Yprev10Rprev50" ~ "Outcome prevalence 10% and Missingness 50%",
           df == "simresults_Yprev10Rprev75" ~ "Outcome prevalence 10% and Missingness 75%",
           df == "simresults_Yprev5Rprev25" ~ "Outcome prevalence 5% and Missingness 25%",
           df == "simresults_Yprev5Rprev50" ~ "Outcome prevalence 5% and Missingness 50%",
           df == "simresults_Yprev5Rprev75" ~ "Outcome prevalence 5% and Missingness 75%",
           df == "simresults_Yprev1Rprev25" ~ "Outcome prevalence 1% and Missingness 25%",
           df == "simresults_Yprev1Rprev50" ~ "Outcome prevalence 1% and Missingness 50%",
           df == "simresults_Yprev1Rprev75" ~ "Outcome prevalence 1% and Missingness 75%")) %>%
  mutate(Parameter = factor(Parameter, levels = c(
    "Outcome prevalence 1% and Missingness 25%",
    "Outcome prevalence 1% and Missingness 50%",
    "Outcome prevalence 1% and Missingness 75%",
    "Outcome prevalence 5% and Missingness 25%",
    "Outcome prevalence 5% and Missingness 50%",
    "Outcome prevalence 5% and Missingness 75%",
    "Outcome prevalence 10% and Missingness 25%",
    "Outcome prevalence 10% and Missingness 50%",
    "Outcome prevalence 10% and Missingness 75%")))

# Function to summarize the data
summarize_data <- function(df) {
  df %>%
    group_by(dataset) %>%
    summarise(across(c(Cal_Int, Cal_Slope, AUC, Brier, bias, mse, rmse), 
                     list(AVG = ~ mean(.x, na.rm = TRUE),  
                          LCI = ~ quantile(.x, 0.025, na.rm = TRUE), 
                          UCI = ~ quantile(.x, 0.975, na.rm = TRUE), 
                          NACount = ~ sum(is.na(.x))), 
                     .names = "{fn}-{col}"))
}

# Apply the summarization function to each data frame in the combined_summaries list
summarized_summaries <- lapply(combined_summaries, summarize_data)



# Combine the summarized data frames into a single data frame
combined_summarized_df <- bind_rows(summarized_summaries, .id = "df")

# Reshape the combined data frame to a long format
simulation_parameters_long <- combined_summarized_df %>%
  pivot_longer(cols = -c(dataset, df),
               names_to = c(".value", "Metric"), 
               names_sep = "-")

# Rename metric to measure
simulation_parameters_long <- simulation_parameters_long %>%
  mutate(Measure = case_when(
    Metric == "Cal_Int" ~ "Calibration in the Large", 
    Metric == "Cal_Slope" ~ "Calibration Slope", 
    Metric == "AUC" ~ "AUC",
    Metric == "Brier" ~ "Brier Score", 
    Metric == "bias" ~ "Bias",
    Metric == "mse" ~ "Mean Square Error",
    Metric == "rmse" ~ "Root Mean Square Error"),
    Method = case_when(dataset=="val_data" ~ "Validation data, no missingness"), 
    Parameter = case_when(
      df == "simresults_Yprev10Rprev25" ~ "Outcome prevalence 10% and Missingness 25%",
      df == "simresults_Yprev10Rprev50" ~ "Outcome prevalence 10% and Missingness 50%",
      df == "simresults_Yprev10Rprev75" ~ "Outcome prevalence 10% and Missingness 75%",
      df == "simresults_Yprev5Rprev25" ~ "Outcome prevalence 5% and Missingness 25%",
      df == "simresults_Yprev5Rprev50" ~ "Outcome prevalence 5% and Missingness 50%",
      df == "simresults_Yprev5Rprev75" ~ "Outcome prevalence 5% and Missingness 75%",
      df == "simresults_Yprev1Rprev25" ~ "Outcome prevalence 1% and Missingness 25%",
      df == "simresults_Yprev1Rprev50" ~ "Outcome prevalence 1% and Missingness 50%",
      df == "simresults_Yprev1Rprev75" ~ "Outcome prevalence 1% and Missingness 75%")) %>%
  mutate(Parameter = factor(Parameter, levels = c(
    "Outcome prevalence 1% and Missingness 25%",
    "Outcome prevalence 1% and Missingness 50%",
    "Outcome prevalence 1% and Missingness 75%",
    "Outcome prevalence 5% and Missingness 25%",
    "Outcome prevalence 5% and Missingness 50%",
    "Outcome prevalence 5% and Missingness 75%",
    "Outcome prevalence 10% and Missingness 25%",
    "Outcome prevalence 10% and Missingness 50%",
    "Outcome prevalence 10% and Missingness 75%"
  )))

# Add Scale Group
simulation_parameters_long$scale_group <- ifelse(
  simulation_parameters_long$Metric %in% c("CalSlope", "AUC"), 
  "Group1", 
  "Group2"
)

## Save 10000
#----------------------------------------------
no_missing_long <- simulation_parameters_long
no_missing_combined <- combined_df

save(no_missing_long ,file = "Nomissing_100000_Combined_Long_08Oct2024.Rdata")
save(no_missing_combined ,file = "Nomissing_100000_Combined_08Oct2024.Rdata")



################################################################################
## 10,000 ## 
################################################################################
## Open datasets

## Set working directory 
 setwd("C:\\Users\\maecj\\OneDrive - Nexus365\\A DPhil\\Simulation studies\\Programs\\Study 1\\SimulationStudy1_11Jun2024\\SimulationStudy\\Data") 
## Load required datasets 
#-----------------------------------------
load("Validation_NoMissingness_Nval_10000_Yprev_0.01_07Oct2024.Rdata")
simresults_Yprev1Rprev25 <- simulation_results
simresults_Yprev1Rprev50 <- simulation_results
simresults_Yprev1Rprev75 <- simulation_results
load("Validation_NoMissingness_Nval_10000_Yprev_0.05_07Oct2024.Rdata")
simresults_Yprev5Rprev25 <- simulation_results
simresults_Yprev5Rprev50 <- simulation_results
simresults_Yprev5Rprev75 <- simulation_results
load("Validation_NoMissingness_Nval_10000_Yprev_0.1_07Oct2024.Rdata")
simresults_Yprev10Rprev25 <- simulation_results
simresults_Yprev10Rprev50 <- simulation_results
simresults_Yprev10Rprev75 <- simulation_results


# Extract target measures 
extract_measures_fnc <- function(simresults) {
  combined_data <- list()
  
  for (i in 1:length(simresults[["iterations"]])) {
    df <- simresults[["iterations"]][[i]][["preds"]][["target_measures"]]
    df <- cbind(iteration = i, df)  # Add the iteration column
    combined_data[[i]] <- df
  }
  
  return(do.call(rbind, combined_data))
}

# Combine data for both datasets
target_measures <- list(
  simresults_Yprev10Rprev75 = extract_measures_fnc(simresults_Yprev10Rprev75),
  simresults_Yprev10Rprev50 = extract_measures_fnc(simresults_Yprev10Rprev50),
  simresults_Yprev10Rprev25 = extract_measures_fnc(simresults_Yprev10Rprev25),
  simresults_Yprev5Rprev75 = extract_measures_fnc(simresults_Yprev5Rprev75),
  simresults_Yprev5Rprev50 = extract_measures_fnc(simresults_Yprev5Rprev50),
  simresults_Yprev5Rprev25 = extract_measures_fnc(simresults_Yprev5Rprev25),
  simresults_Yprev1Rprev75 = extract_measures_fnc(simresults_Yprev1Rprev75),
  simresults_Yprev1Rprev50 = extract_measures_fnc(simresults_Yprev1Rprev50),
  simresults_Yprev1Rprev25 = extract_measures_fnc(simresults_Yprev1Rprev25)
)


## Create summary of bias 
# Define the number of iterations
num_iterations <- 200


# Initialize empty lists to store bias summaries for each dataset
bias_summaries <- list(
  simresults_Yprev10Rprev25 = data.frame(),
  simresults_Yprev10Rprev50 = data.frame(),
  simresults_Yprev10Rprev75 = data.frame(),
  simresults_Yprev5Rprev75 = data.frame(),
  simresults_Yprev5Rprev50 = data.frame(),
  simresults_Yprev5Rprev25 = data.frame(),
  simresults_Yprev1Rprev75 = data.frame(),
  simresults_Yprev1Rprev50 = data.frame(),
  simresults_Yprev1Rprev25 = data.frame()
)

# Go through each dataset and iteration to calculate
for (i in 1:num_iterations) {
  # Loop through each dataset
  for (dataset_name in names(bias_summaries)) {
    dataset <- get(dataset_name)
    
    
    # Extract true Y and estimated Y values
    # Extract true Y and estimated Y values
    true_Y <- dataset[["iterations"]][[i]][["preds"]][["preds_per_data_set"]][["val_data"]][["Y"]]
    estimated_Y <- dataset[["iterations"]][[i]][["preds"]][["preds_per_data_set"]][["val_data"]][["Prediction_Model"]]
    
    # Calculate number of observations (n)
    n <- length(true_Y)
    
    # Calculate bias
    bias <- sum(true_Y - estimated_Y) / n
    
    # Calculate MSE 
    mse <- sum((true_Y - estimated_Y)^2) / n
    
    # Calculate RMSE 
    rmse <- sqrt(mse)
    
    # Create a temporary row for the current iteration and dataset
    new_row <- data.frame(iteration = i, dataset = "val_data",n = n, bias = bias, mse=mse, rmse=rmse)
    
    # Append the temporary row to the corresponding bias_summary data frame
    bias_summaries[[dataset_name]] <- rbind(bias_summaries[[dataset_name]], new_row)
  }
  
}


# Combine target measures and bias_summary
combined_summaries <- list()

for (dataset_name in names(target_measures)) {
  target_measures_df <- as.data.frame(target_measures[[dataset_name]])
  bias_summary_df <- as.data.frame(bias_summaries[[dataset_name]])
  
  combined_df <- full_join(target_measures_df, bias_summary_df, by = c("iteration", "dataset"))
  
  combined_summaries[[dataset_name]] <- combined_df
}

# Combine into one dataset 
combined_df <- combined_summaries %>%
  imap_dfr(~ .x %>% mutate(df = .y))

## Add in Method Categories
combined_df <- combined_df %>%
  mutate(Method = case_when(dataset=="val_data" ~ "Validation data, no missingness"),
         Parameter = case_when(
           df == "simresults_Yprev10Rprev25" ~ "Outcome prevalence 10% and Missingness 25%",
           df == "simresults_Yprev10Rprev50" ~ "Outcome prevalence 10% and Missingness 50%",
           df == "simresults_Yprev10Rprev75" ~ "Outcome prevalence 10% and Missingness 75%",
           df == "simresults_Yprev5Rprev25" ~ "Outcome prevalence 5% and Missingness 25%",
           df == "simresults_Yprev5Rprev50" ~ "Outcome prevalence 5% and Missingness 50%",
           df == "simresults_Yprev5Rprev75" ~ "Outcome prevalence 5% and Missingness 75%",
           df == "simresults_Yprev1Rprev25" ~ "Outcome prevalence 1% and Missingness 25%",
           df == "simresults_Yprev1Rprev50" ~ "Outcome prevalence 1% and Missingness 50%",
           df == "simresults_Yprev1Rprev75" ~ "Outcome prevalence 1% and Missingness 75%")) %>%
  mutate(Parameter = factor(Parameter, levels = c(
    "Outcome prevalence 1% and Missingness 25%",
    "Outcome prevalence 1% and Missingness 50%",
    "Outcome prevalence 1% and Missingness 75%",
    "Outcome prevalence 5% and Missingness 25%",
    "Outcome prevalence 5% and Missingness 50%",
    "Outcome prevalence 5% and Missingness 75%",
    "Outcome prevalence 10% and Missingness 25%",
    "Outcome prevalence 10% and Missingness 50%",
    "Outcome prevalence 10% and Missingness 75%")))

# Function to summarize the data
summarize_data <- function(df) {
  df %>%
    group_by(dataset) %>%
    summarise(across(c(Cal_Int, Cal_Slope, AUC, Brier, bias, mse, rmse), 
                     list(AVG = ~ mean(.x, na.rm = TRUE),  
                          LCI = ~ quantile(.x, 0.025, na.rm = TRUE), 
                          UCI = ~ quantile(.x, 0.975, na.rm = TRUE), 
                          NACount = ~ sum(is.na(.x))), 
                     .names = "{fn}-{col}"))
}

# Apply the summarization function to each data frame in the combined_summaries list
summarized_summaries <- lapply(combined_summaries, summarize_data)



# Combine the summarized data frames into a single data frame
combined_summarized_df <- bind_rows(summarized_summaries, .id = "df")

# Reshape the combined data frame to a long format
simulation_parameters_long <- combined_summarized_df %>%
  pivot_longer(cols = -c(dataset, df),
               names_to = c(".value", "Metric"), 
               names_sep = "-")

# Rename metric to measure
simulation_parameters_long <- simulation_parameters_long %>%
  mutate(Measure = case_when(
    Metric == "Cal_Int" ~ "Calibration in the Large", 
    Metric == "Cal_Slope" ~ "Calibration Slope", 
    Metric == "AUC" ~ "AUC",
    Metric == "Brier" ~ "Brier Score", 
    Metric == "bias" ~ "Bias",
    Metric == "mse" ~ "Mean Square Error",
    Metric == "rmse" ~ "Root Mean Square Error"),
    Method = case_when(dataset=="val_data" ~ "Validation data, no missingness"), 
    Parameter = case_when(
      df == "simresults_Yprev10Rprev25" ~ "Outcome prevalence 10% and Missingness 25%",
      df == "simresults_Yprev10Rprev50" ~ "Outcome prevalence 10% and Missingness 50%",
      df == "simresults_Yprev10Rprev75" ~ "Outcome prevalence 10% and Missingness 75%",
      df == "simresults_Yprev5Rprev25" ~ "Outcome prevalence 5% and Missingness 25%",
      df == "simresults_Yprev5Rprev50" ~ "Outcome prevalence 5% and Missingness 50%",
      df == "simresults_Yprev5Rprev75" ~ "Outcome prevalence 5% and Missingness 75%",
      df == "simresults_Yprev1Rprev25" ~ "Outcome prevalence 1% and Missingness 25%",
      df == "simresults_Yprev1Rprev50" ~ "Outcome prevalence 1% and Missingness 50%",
      df == "simresults_Yprev1Rprev75" ~ "Outcome prevalence 1% and Missingness 75%")) %>%
  mutate(Parameter = factor(Parameter, levels = c(
    "Outcome prevalence 1% and Missingness 25%",
    "Outcome prevalence 1% and Missingness 50%",
    "Outcome prevalence 1% and Missingness 75%",
    "Outcome prevalence 5% and Missingness 25%",
    "Outcome prevalence 5% and Missingness 50%",
    "Outcome prevalence 5% and Missingness 75%",
    "Outcome prevalence 10% and Missingness 25%",
    "Outcome prevalence 10% and Missingness 50%",
    "Outcome prevalence 10% and Missingness 75%"
  )))

# Add Scale Group
simulation_parameters_long$scale_group <- ifelse(
  simulation_parameters_long$Metric %in% c("CalSlope", "AUC"), 
  "Group1", 
  "Group2"
)

## Save 10000
#----------------------------------------------
no_missing_long <- simulation_parameters_long
no_missing_combined <- combined_df

save(no_missing_long ,file = "Nomissing_10000_Combined_Long_08Oct2024.Rdata")
save(no_missing_combined ,file = "Nomissing_10000_Combined_08Oct2024.Rdata")



################################################################################
## 500 ## 
################################################################################
## Open datasets

## Set working directory 
setwd("C:\\Users\\maecj\\OneDrive - Nexus365\\A DPhil\\Simulation studies\\Programs\\Study 1\\SimulationStudy1_11Jun2024\\SimulationStudy\\Data") 

## Load required datasets 
#-----------------------------------------
load("Validation_NoMissingness_Nval_500_Yprev_0.01_07Oct2024.Rdata")
simresults_Yprev1Rprev25 <- simulation_results
simresults_Yprev1Rprev50 <- simulation_results
simresults_Yprev1Rprev75 <- simulation_results
load("Validation_NoMissingness_Nval_500_Yprev_0.05_07Oct2024.Rdata")
simresults_Yprev5Rprev25 <- simulation_results
simresults_Yprev5Rprev50 <- simulation_results
simresults_Yprev5Rprev75 <- simulation_results
load("Validation_NoMissingness_Nval_500_Yprev_0.1_07Oct2024.Rdata")
simresults_Yprev10Rprev25 <- simulation_results
simresults_Yprev10Rprev50 <- simulation_results
simresults_Yprev10Rprev75 <- simulation_results


# Extract target measures 
extract_measures_fnc <- function(simresults) {
  combined_data <- list()
  
  for (i in 1:length(simresults[["iterations"]])) {
    df <- simresults[["iterations"]][[i]][["preds"]][["target_measures"]]
    df <- cbind(iteration = i, df)  # Add the iteration column
    combined_data[[i]] <- df
  }
  
  return(do.call(rbind, combined_data))
}

# Combine data for both datasets
target_measures <- list(
  simresults_Yprev10Rprev75 = extract_measures_fnc(simresults_Yprev10Rprev75),
  simresults_Yprev10Rprev50 = extract_measures_fnc(simresults_Yprev10Rprev50),
  simresults_Yprev10Rprev25 = extract_measures_fnc(simresults_Yprev10Rprev25),
  simresults_Yprev5Rprev75 = extract_measures_fnc(simresults_Yprev5Rprev75),
  simresults_Yprev5Rprev50 = extract_measures_fnc(simresults_Yprev5Rprev50),
  simresults_Yprev5Rprev25 = extract_measures_fnc(simresults_Yprev5Rprev25),
  simresults_Yprev1Rprev75 = extract_measures_fnc(simresults_Yprev1Rprev75),
  simresults_Yprev1Rprev50 = extract_measures_fnc(simresults_Yprev1Rprev50),
  simresults_Yprev1Rprev25 = extract_measures_fnc(simresults_Yprev1Rprev25)
)


## Create summary of bias 
# Define the number of iterations
num_iterations <- 200


# Initialize empty lists to store bias summaries for each dataset
bias_summaries <- list(
  simresults_Yprev10Rprev25 = data.frame(),
  simresults_Yprev10Rprev50 = data.frame(),
  simresults_Yprev10Rprev75 = data.frame(),
  simresults_Yprev5Rprev75 = data.frame(),
  simresults_Yprev5Rprev50 = data.frame(),
  simresults_Yprev5Rprev25 = data.frame(),
  simresults_Yprev1Rprev75 = data.frame(),
  simresults_Yprev1Rprev50 = data.frame(),
  simresults_Yprev1Rprev25 = data.frame()
)

# Go through each dataset and iteration to calculate
for (i in 1:num_iterations) {
  # Loop through each dataset
  for (dataset_name in names(bias_summaries)) {
    dataset <- get(dataset_name)
    

      # Extract true Y and estimated Y values
    # Extract true Y and estimated Y values
    true_Y <- dataset[["iterations"]][[i]][["preds"]][["preds_per_data_set"]][["val_data"]][["Y"]]
    estimated_Y <- dataset[["iterations"]][[i]][["preds"]][["preds_per_data_set"]][["val_data"]][["Prediction_Model"]]
    
      # Calculate number of observations (n)
      n <- length(true_Y)
      
      # Calculate bias
      bias <- sum(true_Y - estimated_Y) / n
      
      # Calculate MSE 
      mse <- sum((true_Y - estimated_Y)^2) / n
      
      # Calculate RMSE 
      rmse <- sqrt(mse)
      
      # Create a temporary row for the current iteration and dataset
      new_row <- data.frame(iteration = i, dataset = "val_data",n = n, bias = bias, mse=mse, rmse=rmse)
      
      # Append the temporary row to the corresponding bias_summary data frame
      bias_summaries[[dataset_name]] <- rbind(bias_summaries[[dataset_name]], new_row)
    }
  
}


# Combine target measures and bias_summary
combined_summaries <- list()

for (dataset_name in names(target_measures)) {
  target_measures_df <- as.data.frame(target_measures[[dataset_name]])
  bias_summary_df <- as.data.frame(bias_summaries[[dataset_name]])
  
  combined_df <- full_join(target_measures_df, bias_summary_df, by = c("iteration", "dataset"))
  
  combined_summaries[[dataset_name]] <- combined_df
}

# Combine into one dataset 
combined_df <- combined_summaries %>%
  imap_dfr(~ .x %>% mutate(df = .y))

## Add in Method Categories
combined_df <- combined_df %>%
  mutate(Method = case_when(dataset=="val_data" ~ "Validation data, no missingness"),
         Parameter = case_when(
           df == "simresults_Yprev10Rprev25" ~ "Outcome prevalence 10% and Missingness 25%",
           df == "simresults_Yprev10Rprev50" ~ "Outcome prevalence 10% and Missingness 50%",
           df == "simresults_Yprev10Rprev75" ~ "Outcome prevalence 10% and Missingness 75%",
           df == "simresults_Yprev5Rprev25" ~ "Outcome prevalence 5% and Missingness 25%",
           df == "simresults_Yprev5Rprev50" ~ "Outcome prevalence 5% and Missingness 50%",
           df == "simresults_Yprev5Rprev75" ~ "Outcome prevalence 5% and Missingness 75%",
           df == "simresults_Yprev1Rprev25" ~ "Outcome prevalence 1% and Missingness 25%",
           df == "simresults_Yprev1Rprev50" ~ "Outcome prevalence 1% and Missingness 50%",
           df == "simresults_Yprev1Rprev75" ~ "Outcome prevalence 1% and Missingness 75%")) %>%
  mutate(Parameter = factor(Parameter, levels = c(
    "Outcome prevalence 1% and Missingness 25%",
    "Outcome prevalence 1% and Missingness 50%",
    "Outcome prevalence 1% and Missingness 75%",
    "Outcome prevalence 5% and Missingness 25%",
    "Outcome prevalence 5% and Missingness 50%",
    "Outcome prevalence 5% and Missingness 75%",
    "Outcome prevalence 10% and Missingness 25%",
    "Outcome prevalence 10% and Missingness 50%",
    "Outcome prevalence 10% and Missingness 75%")))

# Function to summarize the data
summarize_data <- function(df) {
  df %>%
    group_by(dataset) %>%
    summarise(across(c(Cal_Int, Cal_Slope, AUC, Brier, bias, mse, rmse), 
                     list(AVG = ~ mean(.x, na.rm = TRUE),  
                          LCI = ~ quantile(.x, 0.025, na.rm = TRUE), 
                          UCI = ~ quantile(.x, 0.975, na.rm = TRUE), 
                          NACount = ~ sum(is.na(.x))), 
                     .names = "{fn}-{col}"))
}

# Apply the summarization function to each data frame in the combined_summaries list
summarized_summaries <- lapply(combined_summaries, summarize_data)



# Combine the summarized data frames into a single data frame
combined_summarized_df <- bind_rows(summarized_summaries, .id = "df")

# Reshape the combined data frame to a long format
simulation_parameters_long <- combined_summarized_df %>%
  pivot_longer(cols = -c(dataset, df),
               names_to = c(".value", "Metric"), 
               names_sep = "-")

# Rename metric to measure
simulation_parameters_long <- simulation_parameters_long %>%
  mutate(Measure = case_when(
    Metric == "Cal_Int" ~ "Calibration in the Large", 
    Metric == "Cal_Slope" ~ "Calibration Slope", 
    Metric == "AUC" ~ "AUC",
    Metric == "Brier" ~ "Brier Score", 
    Metric == "bias" ~ "Bias",
    Metric == "mse" ~ "Mean Square Error",
    Metric == "rmse" ~ "Root Mean Square Error"),
    Method = case_when(dataset=="val_data" ~ "Validation data, no missingness"), 
    Parameter = case_when(
      df == "simresults_Yprev10Rprev25" ~ "Outcome prevalence 10% and Missingness 25%",
      df == "simresults_Yprev10Rprev50" ~ "Outcome prevalence 10% and Missingness 50%",
      df == "simresults_Yprev10Rprev75" ~ "Outcome prevalence 10% and Missingness 75%",
      df == "simresults_Yprev5Rprev25" ~ "Outcome prevalence 5% and Missingness 25%",
      df == "simresults_Yprev5Rprev50" ~ "Outcome prevalence 5% and Missingness 50%",
      df == "simresults_Yprev5Rprev75" ~ "Outcome prevalence 5% and Missingness 75%",
      df == "simresults_Yprev1Rprev25" ~ "Outcome prevalence 1% and Missingness 25%",
      df == "simresults_Yprev1Rprev50" ~ "Outcome prevalence 1% and Missingness 50%",
      df == "simresults_Yprev1Rprev75" ~ "Outcome prevalence 1% and Missingness 75%")) %>%
  mutate(Parameter = factor(Parameter, levels = c(
    "Outcome prevalence 1% and Missingness 25%",
    "Outcome prevalence 1% and Missingness 50%",
    "Outcome prevalence 1% and Missingness 75%",
    "Outcome prevalence 5% and Missingness 25%",
    "Outcome prevalence 5% and Missingness 50%",
    "Outcome prevalence 5% and Missingness 75%",
    "Outcome prevalence 10% and Missingness 25%",
    "Outcome prevalence 10% and Missingness 50%",
    "Outcome prevalence 10% and Missingness 75%"
  )))

# Add Scale Group
simulation_parameters_long$scale_group <- ifelse(
  simulation_parameters_long$Metric %in% c("CalSlope", "AUC"), 
  "Group1", 
  "Group2"
)

## Save 500
#----------------------------------------------
no_missing_long <- simulation_parameters_long
no_missing_combined <- combined_df

save(no_missing_long ,file = "Nomissing_500_Combined_Long_07Oct2024.Rdata")
save(no_missing_combined ,file = "Nomissing_500_Combined_07Oct2024.Rdata")

# 
# ##############################################################################
# # Combine with no missingness 
# ##############################################################################
# ## Load datasets
# load("MCAR_500_Combined_Long_03Oct2024.Rdata")
# load("MCAR_500_Combined_03Oct2024.Rdata")
# load("Nomissing_500_Combined_Long_07Oct2024.Rdata")
# load("Nomissing_500_Combined_07Oct2024.Rdata")
# 
# # Remove unneeded columns
# simulation_parameters_long <- simulation_parameters_long %>% select(-"scale_group")
# ## Combine datasets 
# combined_df <- rbind(combined_df, no_missing_combined)
# combined_df <- combined_df %>%
#   mutate(Parameter = factor(Parameter, levels = c(
#     "Outcome prevalence 1%, no missingness",
#     "Outcome prevalence 1% and Missingness 25%",
#     "Outcome prevalence 1% and Missingness 50%",
#     "Outcome prevalence 1% and Missingness 75%",
#     "Outcome prevalence 5%, no missingness",
#     "Outcome prevalence 5% and Missingness 25%",
#     "Outcome prevalence 5% and Missingness 50%",
#     "Outcome prevalence 5% and Missingness 75%",
#     "Outcome prevalence 10%, no missingness",
#     "Outcome prevalence 10% and Missingness 25%",
#     "Outcome prevalence 10% and Missingness 50%",
#     "Outcome prevalence 10% and Missingness 75%"
#   )))
# 
# simulation_parameters_long <- rbind(simulation_parameters_long, no_missing_long)
# simulation_parameters_long <- simulation_parameters_long %>%
#   mutate(Parameter = factor(Parameter, levels = c(
#     "Outcome prevalence 1%, no missingness",
#     "Outcome prevalence 1% and Missingness 25%",
#     "Outcome prevalence 1% and Missingness 50%",
#     "Outcome prevalence 1% and Missingness 75%",
#     "Outcome prevalence 5%, no missingness",
#     "Outcome prevalence 5% and Missingness 25%",
#     "Outcome prevalence 5% and Missingness 50%",
#     "Outcome prevalence 5% and Missingness 75%",
#     "Outcome prevalence 10%, no missingness",
#     "Outcome prevalence 10% and Missingness 25%",
#     "Outcome prevalence 10% and Missingness 50%",
#     "Outcome prevalence 10% and Missingness 75%"
#   )))
# 
# 
# ## Save 100,00 
# no_missing_long <- simulation_parameters_long
# no_missing_combined <- combined_df
# 
# save(no_missing_long ,file = "Nomissing_500_Combined_Long_07Oct2024.Rdata")
# save(no_missing_combined ,file = "Nomissing_500_Combined_07Oct2024.Rdata")