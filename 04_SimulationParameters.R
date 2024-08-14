################################################################################
## Simulation Performance Measures
################################################################################
setwd("C:\\Users\\maecj\\OneDrive - Nexus365\\A DPhil\\Simulation studies\\Programs\\Study 1\\SimulationStudy1_11Jun2024\\SimulationStudy")
load("Results_Nsim50_Yprev0.1_Rprev0.25_14Aug2024.Rdata")

## Combine Target Measures 
combined_data <- list()

# Loop through the iterations and combine the data frames
for (i in 1:length(simulation_results[["iterations"]])) {
  df <- simulation_results[["iterations"]][[i]][["preds"]][["target_measures"]]
  df <- cbind(iteration=i, df)  # Add the iteration column
  combined_data[[i]] <- df
  }

# Combine all data frames into one
target_measures <- do.call(rbind, combined_data)



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
num_iterations <- 50

# Define an empty data frame to store bias
bias_summary <- data.frame(
  iteration = integer(),  # Pre-allocate for efficiency
  dataset = character(),
  n = integer(),
  bias = numeric(), 
  mse=numeric(),
  rmse = numeric()
)


# Iterate over iterations
#####################################
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
    
    # Calculate MSE 
    mse <- sum((true_Y - estimated_Y)^2) / n
    
    # Calculate RMSE 
   rmse <- sqrt(mse)
    
    # Create a temporary row for the current iteration and dataset
    new_row <- data.frame(iteration = i, dataset = dataset_id, n = n, bias = bias, mse=mse, rmse=rmse)
    
    # Append the temporary row to the bias_summary data frame
    bias_summary <- rbind(bias_summary, new_row)
  }
}


# Combine target measures and bias_summary
library(dplyr)
iteration_parameters <- full_join(target_measures, bias_summary, by=c("iteration","dataset"))

iteration_parameters <- iteration_parameters %>%
                        mutate(Method = case_when(dataset=="CCA_val_data" ~ "Complete Case Analysis", 
                                                  dataset=="mean_val" ~ "Mean Imputation", 
                                                  dataset=="MI_val_data_noY" ~ "Multiple Imputation without Outcome", 
                                                  dataset=="MI_val_data_withY" ~ "Multiple Imputation with Outcome")) %>%
                        rename(AUVvar=AUC_var,
                               CalInt=Cal_Int,
                               CalIntVar=Cal_Int_var,
                               CalSlope=Cal_Slope, 
                               CalSlopeVar=Cal_Slope_var)
##############################################################################
## Calculate average  
##############################################################################

simulation_parameters <- iteration_parameters %>%
  group_by(Method) %>%
  summarise(across(c(CalInt, CalSlope, AUC, Brier, bias, mse, rmse), 
                   list(avg = mean, 
                        lci = ~ quantile(.x, 0.025), 
                        uci = ~ quantile(.x, 0.975)), 
                   .names = "{fn}_{col}"))

library(tidyr)
simulation_parameters_long <- simulation_parameters %>%
                                      pivot_longer(cols= -Method,
                                                   names_to = c(".value", "Metric"), 
                                                   names_sep = "_")


## Rename 
simulation_parameters_long <-simulation_parameters_long %>%
                                mutate(Measure =
                                         case_when(Metric=="CalInt" ~ "Calibration in the Large", 
                                                   Metric=="CalSlope" ~ "Calibration Slope", 
                                                   Metric=="AUC" ~ "AUC",
                                                   Metric=="Brier" ~ "Brier Score", 
                                                   Metric=="bias" ~ "Bias",
                                                   Metric=="mse" ~ "Mean Square Error",
                                                   Metric=="rmse" ~ "Root Mean Square Error"))

## Add Scale Group 
simulation_parameters_long$scale_group <- 
  ifelse(simulation_parameters_long$Metric %in% c("CalSlope", "AUC"), "Group1", "Group2")


  
##############################################################################
## Plot 
##############################################################################
### Library 
library(ggplot2)


##############################################################################
## Plot average of estimates for each simulation and corresonding confidence intervals 
##############################################################################

## The below code plots each measure on the y axis with an individual x axis 
## for each measure. The colours are by the method for imputing the data 

ggplot(simulation_parameters_long, 
       aes(x = avg, y = Method, colour = Method)) +
  geom_point(size = 3) +
  geom_errorbar(aes(xmin = lci, xmax = uci), width = 0.2) +
  labs(y = NULL,
       x = NULL,
       title = "Performance Measures of Interest",
       colour = "Method\n(Mean, 95% CI)") +
  theme_minimal() + 
  facet_wrap(~Measure, scales = "free_x", nrow = 7) + 
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

