################################################################################
## Simulation Performance Measures
################################################################################
setwd("C:\\Users\\maecj\\OneDrive - Nexus365\\A DPhil\\Simulation studies\\Programs\\Study 1\\SimulationStudy1_11Jun2024\\SimulationStudy")
load("Results_Yprev0.05_Rprev0.25_01Jul2024.Rdata")

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
num_iterations <- 3

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

##############################################################################
## Calculate average  
##############################################################################

simulation_parameters <- iteration_parameters %>%
                            group_by(dataset) %>%
                              summarise(across(Cal_Int:rmse, mean, .names = "avg_{col}"))
  
  
## Plot 
##############################################################################
# Code adapted from AG plot-results.R
library(ggplot2)
iteration_parameters <- as.data.frame(iteration_parameters)
ggplot(data=iteration_parameters, 
       aes(y=iteration, x=Cal_Int, colour=dataset)) + 
        geom_point() + 
        geom_vline(data = simulation_parameters, 
                   aes(xintercept =  avg_Cal_Int, colour = dataset), 
                   linetype = "dashed", size = 1) +
        labs(title = "Calibration in the Large for Each Iteration and Dataset",
             y = "Iteration",
             x = "Calibration Intercept") +
        theme_minimal()


+ 
  geom_errorbar(aes(xmin = min_estimates, xmax = max_estimates), width=.1) +
  geom_point(size = 3, stroke = 0.5) +
  guides(color = guide_legend(reverse = TRUE)) + 
  scale_shape_manual(values = c(8, 17, 16, 15)) +
  scale_color_brewer(palette = "Set1") +
  xlab("Mean Estimates") +
  ylab("Data Imputation Methods") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text = element_text(size=14),
        axis.title = element_text(size=16, face="bold"),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        strip.text = element_text(size = 16),
        panel.background = element_rect(fill = "gray90"),  # add background color to panels
        panel.spacing.x = unit(0.5, "lines")) +  # increase space between panels
  ggh4x::facet_grid2(~  target_measures, scales = "free_x", independent = "x")


