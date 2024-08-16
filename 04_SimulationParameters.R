################################################################################
## Simulation Performance Measures
################################################################################
## Libraries 
library(dplyr)
library(ggplot2)
library(tidyr)
################################################################################
## Set working directory 
  setwd("C:\\Users\\maecj\\OneDrive - Nexus365\\A DPhil\\Simulation studies\\Programs\\Study 1\\SimulationStudy1_11Jun2024\\SimulationStudy")

## Load required datasets 
################################
  load("Results_4_Nsim_20_Yprev_0.1_Rprev_0.5_15Aug2024.Rdata")
  simresults_Yprev10Rprev50 <- simulation_results
  load("Results_3_Nsim_20_Yprev_0.1_Rprev_0.25_15Aug2024.Rdata")
  simresults_Yprev10Rprev25 <- simulation_results
  load("Results_2_Nsim_20_Yprev_0.05_Rprev_0.5_15Aug2024.Rdata")
  simresults_Yprev5Rprev50 <- simulation_results
  load("Results_1_Nsim_20_Yprev_0.05_Rprev_0.25_15Aug2024.Rdata")
  simresults_Yprev5Rprev25 <- simulation_results
  

## Store parameters 
################################
  # Note: doesn't matter which iteration, all the same. 
  parameters_Yprev10Rprev50 <- data.frame(simresults_Yprev10Rprev50[["iterations"]][[1]][["Parameters"]])
  parameters_Yprev10Rprev25 <- data.frame(simresults_Yprev10Rprev25[["iterations"]][[1]][["Parameters"]])
  parameters_Yprev5Rprev50 <- data.frame(simresults_Yprev5Rprev50[["iterations"]][[1]][["Parameters"]])
  parameters_Yprev5Rprev25 <- data.frame(simresults_Yprev5Rprev25[["iterations"]][[1]][["Parameters"]])


# Extract target measures 
################################
      extract_measures_fnc <- function(simresults) {
      data <- list()
      
      for (i in 1:length(simresults[["iterations"]])) {
        df <- simresults[["iterations"]][[i]][["preds"]][["target_measures"]]
        df <- cbind(iteration = i, df)  # Add the iteration column
        combined_data[[i]] <- df
      }
      
      return(do.call(rbind, combined_data))
    }
  
  # Combine data for both datasets
  target_measures <- list(
    simresults_Yprev10Rprev50 = extract_measures_fnc(simresults_Yprev10Rprev50),
    simresults_Yprev10Rprev25 = extract_measures_fnc(simresults_Yprev10Rprev25),
    simresults_Yprev5Rprev50 = extract_measures_fnc(simresults_Yprev5Rprev50),
    simresults_Yprev5Rprev25 = extract_measures_fnc(simresults_Yprev5Rprev25)
  )
  

################################################################################
## Store n, bias 
################################################################################
## Bias here is how far away the predicted values of the outcome are from the true value

## The Root Mean Square Error RMSE
######################################
## RMSE aggregates error due to bias and variability 
## 0 = perfect imputation
## >0 = decreasing performance of the imputation (clinical relevance depends on range of predictor)
## Formula: 

## Calibration in the Large
######################################
# Calibration-in-the-large measures whether the overall predicted risk matches the observed risk. 
# Essentially, it checks if the average predicted probability is equal to the average observed outcome.
# For example, if a model predicts an average risk of 10% for a certain event, 
# calibration-in-the-large would be satisfied if the actual event rate is also 10%.



## Create summary of bias 
#####################################
# Define the number of iterations
num_iterations <- 20

  
  # Initialize empty lists to store bias summaries for each dataset
  bias_summaries <- list(
    simresults_Yprev10Rprev25 = data.frame(),
    simresults_Yprev10Rprev50 = data.frame(),
    simresults_Yprev5Rprev25 = data.frame(),
    simresults_Yprev5Rprev50 = data.frame()
  )

# Go through each dataset and iteration to calculate
for (i in 1:num_iterations) {
  # Loop through each dataset
  for (dataset_name in names(bias_summaries)) {
    dataset <- get(dataset_name)
  
  # Loop through each method
  for (method_id in c("CCA_val_data", "mean_val", "MI_val_data_noY", "MI_val_data_withY")) {
  
    # Extract true Y and estimated Y values
    true_Y <- dataset[["iterations"]][[i]][["preds"]][["preds_per_data_set"]][[method_id]][["Y"]]
    estimated_Y <- dataset[["iterations"]][[i]][["preds"]][["preds_per_data_set"]][[method_id]][["Prediction_Model"]]
    
    # Calculate number of observations (n)
    n <- length(true_Y)
    
    # Calculate bias
    bias <- sum(true_Y - estimated_Y) / n
    
    # Calculate MSE 
    mse <- sum((true_Y - estimated_Y)^2) / n
    
    # Calculate RMSE 
   rmse <- sqrt(mse)
    
    # Create a temporary row for the current iteration and dataset
    new_row <- data.frame(iteration = i, dataset = method_id, n = n, bias = bias, mse=mse, rmse=rmse)
    
    # Append the temporary row to the corresponding bias_summary data frame
    bias_summaries[[dataset_name]] <- rbind(bias_summaries[[dataset_name]], new_row)
  }
}
}
  
  
################################################################################
# Combine target measures and bias_summary
#################################################################################
  combined_summaries <- list()
  
  for (dataset_name in names(target_measures)) {
    target_measures_df <- as.data.frame(target_measures[[dataset_name]])
    bias_summary_df <- as.data.frame(bias_summaries[[dataset_name]])
    
    combined_df <- full_join(target_measures_df, bias_summary_df, by = c("iteration", "dataset"))
    
    combined_summaries[[dataset_name]] <- combined_df
  }
  

##############################################################################
## Calculate average across simulations 
##############################################################################
## Create summaries
 #############################################
  # Function to summarize the data
  summarize_data <- function(df) {
    df %>%
      group_by(dataset) %>%
      summarise(across(c(Cal_Int, Cal_Slope, AUC, Brier, bias, mse, rmse), 
                       list(AVG = mean, 
                            LCI = ~ quantile(.x, 0.025), 
                            UCI = ~ quantile(.x, 0.975)), 
                       .names = "{fn}-{col}"))
  }
  
  # Apply the summarization function to each data frame in the combined_summaries list
  summarized_summaries <- lapply(combined_summaries, summarize_data)
  


## Reshape long 
  ################################################################
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
    Method = case_when(dataset == "CCA_val_data" ~"Complete Case Analysis", 
                       dataset == "mean_val" ~"Mean Imputation",
                       dataset == "MI_val_data_noY" ~ "Multiple Imputation without Outcome",
                       dataset == "MI_val_data_withY" ~  "Multiple Imputation with Outcome"),
    Parameter = case_when(
      df == "simresults_Yprev10Rprev50" ~ "Yprev 10% Rprev 50%",
      df == "simresults_Yprev10Rprev25" ~ "Yprev 10% Rprev 25%",
      df == "simresults_Yprev5Rprev50" ~ "Yprev 5% Rprev 50%",
      df == "simresults_Yprev5Rprev25" ~ "Yprev 5% Rprev 25%") )
  
  # Add Scale Group
  simulation_parameters_long$scale_group <- ifelse(
    simulation_parameters_long$Metric %in% c("CalSlope", "AUC"), 
    "Group1", 
    "Group2"
  )
  

##############################################################################
## Plot AUC  
##############################################################################
## Four columns, onegraph 
  auc_plot <- ggplot(simulation_parameters_long %>% filter(Measure=="AUC"), 
         aes(x = AVG, y = Method, colour = Method)) +
    geom_point(size = 3) +
    geom_errorbar(aes(xmin = LCI, xmax = UCI), width = 0.2) +
    labs(y = NULL,
         x = NULL,
         title = "Performance Measures of Interest",
         colour = "Method\n(Mean, 95% CI)") +
    theme_minimal() + 
    facet_grid(Measure ~ Parameter, scales = "free_x") + 
  #  facet_wrap(Measure ~ Parameter, scales = "free_x") + 
    scale_colour_manual(values = c("Complete Case Analysis" = "blue", 
                                   "Mean Imputation" = "red", 
                                   "Multiple Imputation with Outcome" = "green",
                                   "Multiple Imputation without Outcome" = "purple")) +
    theme(legend.position = "right",
          strip.text.y = element_text(size = 14, angle = 0, hjust = 0.5, vjust=0.5),         
          strip.text.x = element_text(size = 14, hjust = 0),  # Align column strip text to the left
          axis.title.x = element_text(size = 14), 
          axis.title.y = element_text(size = 14), 
          axis.text.x = element_text(size = 12), 
          axis.text.y = element_blank(),  # Remove y-axis text
          axis.ticks.y = element_blank())  # Remove y-axis ticks
  

  
  
  ##############################################################################
  ## Plot Brier   
  ##############################################################################
  ## Four columns, onegraph 
  ## Note: Need to fix the axis of these 
brier_plot <- ggplot(simulation_parameters_long %>% filter(Measure=="Brier Score"), 
                     aes(x = AVG, y = Method, colour = Method)) +
    geom_point(size = 3) +
    geom_errorbar(aes(xmin = LCI, xmax = UCI), width = 0.2) +
    labs(y = NULL,
         x = NULL,
         title = "Performance Measures of Interest",
         colour = "Method\n(Mean, 95% CI)") +
    theme_minimal() + 
    facet_grid(Measure ~ Parameter, scales = "free_x") + 
    #  facet_wrap(Measure ~ Parameter, scales = "free_x") + 
    scale_colour_manual(values = c("Complete Case Analysis" = "blue", 
                                   "Mean Imputation" = "red", 
                                   "Multiple Imputation with Outcome" = "green",
                                   "Multiple Imputation without Outcome" = "purple")) +
    theme(legend.position = "right",
          strip.text.y = element_text(size = 14, angle = 0, hjust = 0.5, vjust=0.5),         
          strip.text.x = element_text(size = 14, hjust = 0),  # Align column strip text to the left
          axis.title.x = element_text(size = 14), 
          axis.title.y = element_text(size = 14), 
          axis.text.x = element_text(size = 12), 
          axis.text.y = element_blank(),  # Remove y-axis text
          axis.ticks.y = element_blank())  # Remove y-axis ticks
  
  
  
  
  
##############################################################################
## Plot average of estimates for each simulation and corresonding confidence intervals 
##############################################################################

## The below code plots each measure on the y axis with an individual x axis 
## for each measure. The colours are by the method for imputing the data 
#------------------------------------------------------------------------------
ggplot(simulation_parameters_long, 
       aes(x = AVG, y = Method, colour = Method)) +
  geom_point(size = 3) +
  geom_errorbar(aes(xmin = LCI, xmax = UCI), width = 0.2) +
  labs(y = NULL,
       x = NULL,
       title = "Performance Measures of Interest",
       colour = "Method\n(Mean, 95% CI)") +
  theme_minimal() + 
  facet_grid(Measure ~ Parameter, scales = "free_x") + 
  scale_colour_manual(values = c("Complete Case Analysis" = "blue", 
                                 "Mean Imputation" = "red", 
                                 "Multiple Imputation with Outcome" = "green",
                                 "Multiple Imputation without Outcome" = "purple")) +
  theme(legend.position = "right",
        strip.text = element_text(size = 14, hjust = 0),  # Align strip text to the left
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_blank(),  # Remove y-axis text
        axis.ticks.y = element_blank())  # Remove y-axis ticks

## Same plot but patterns and shapes instead 
#-----------------------------------------------------------------------------
ggplot(simulation_parameters_long, 
       aes(x = AVG, y = Method, shape = Method)) +
  geom_point(size = 3) +
  geom_errorbar(aes(xmin = LCI, xmax = UCI), width = 0.2) +
  labs(y = NULL,
       x = NULL,
       title = "Performance Measures of Interest",
       colour = "Method\n(Mean, 95% CI)") +
  theme_minimal() + 
  facet_wrap(~Measure, scales = "free_x", nrow = 7) + 
  scale_shape_manual(values = c("Complete Case Analysis" = 16, 
                                 "Mean Imputation" = 17, 
                                 "Multiple Imputation with Outcome" =18,
                                 "Multiple Imputation without Outcome" = 15)) +
  theme(legend.position = "right",
        strip.text = element_text(size = 14, hjust = 0),  # Align strip text to the left
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_blank(),  # Remove y-axis text
        axis.ticks.y = element_blank())  # Remove y-axis ticks



##############################################################################
## Plot average of estimates for each simulation and corresonding confidence intervals 
##############################################################################

ggplot(simulation_parameters, aes(y = Method, x = avg_Cal_Int, colour = Method)) +
  geom_point(size = 3) +
  geom_errorbar(aes(xmin = lci_Cal_Int, xmax = uci_Cal_Int), width = 0.2) +
  labs(y = "Method",
       x = "Calibration in the Large (Calibration Intercept)") +
  theme_minimal() + 
  scale_colour_manual(values = c("Complete Case Analysis" = "blue", 
                                 "Mean Imputation" = "red", 
                                 "Multiple Imputation with Outcome" = "green",
                                 "Multiple Imputation without Outcome" = "purple")) +
  theme(legend.position = "none",
        strip.text = element_text(size = 14), 
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12)) + 
  scale_x_continuous(limits = c(-0.21, 0.21), breaks = seq(-0.2, 0.2, by = 0.05))













################################################################################
# Plotting Calibration in the Large 
################################################################################
# The following code creates a graph of the average and the simulation individual points
iteration_parameters <- as.data.frame(iteration_parameters)
ggplot(data=iteration_parameters, 
       aes(x=iteration, y=Cal_Int, colour=Method)) + 
  geom_point() + 
  geom_hline(data = simulation_parameters, 
             aes(yintercept =  avg_Cal_Int, colour = Method), 
             linetype = "dashed", size = 1) +
  labs(x = "Iteration",
       y = "Calibration in the Large (Calibration Intercept)") +
  theme_minimal() + 
  facet_wrap(~Method, nrow=1) + 
  scale_colour_manual(values = c("Complete Case Analysis" = "blue", 
                                 "Mean Imputation" = "red", 
                                 "Multiple Imputation with Outcome" = "green",
                                 "Multiple Imputation without Outcome" = "purple")) +
  theme(legend.position = "none",
        strip.text = element_text(size = 14), 
        axis.title.x=element_text(size=14), 
        axis.title.y = element_text(size=14), 
        axis.text.x = element_text(size=12), 
        axis.text.y = element_text(size=12)) + 
  scale_y_continuous(limits = c(-0.21, 0.21), breaks = seq(-0.2, 0.2, by = 0.05))

