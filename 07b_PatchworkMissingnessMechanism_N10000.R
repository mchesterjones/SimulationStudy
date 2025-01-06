################################################################################
### Figures for Simulation Study Results 
#### Average and 95% CI 
################################################################################
## Created: 24Dec2024
## AUthor: Mae CHester-Jones
## Purpose: Create graphs comparing mechanisms directly filtering on sample size 
################################################################################
# NOTE: Wrote this on 04Jan to get new graph for bias, old graphs for AUC used in Overleaf
# ## library
library(dplyr)
library(ggplot2)
library(tidyr)
library(flextable)
library(rlang)

## Set working directory
setwd("C:\\Users\\maecj\\OneDrive - Nexus365\\A DPhil\\Simulation studies\\Programs\\Study 1\\SimulationStudy1_11Jun2024\\SimulationStudy\\Data")

## Load Datasets
load("MCAR_Combined_Long.Rdata")
MCAR_Long <- simulation_parameters_long 
load("MCAR_Combined.Rdata")
MCAR_Combined_data <- combined_df

load("MAR_Combined_Long.Rdata")
MAR_Long <- simulation_parameters_long 
load("MAR_Combined.Rdata")
MAR_Combined_data <- combined_df

load("MNAR_Combined_Long.Rdata")
MNAR_Long <- simulation_parameters_long
load("MNAR_Combined.Rdata")
MNAR_Combined_data <- combined_df

#Load nonmissing
setwd("C:\\Users\\maecj\\OneDrive - Nexus365\\A DPhil\\Simulation studies\\Programs\\Study 1\\SimulationStudy1_11Jun2024\\SimulationStudy\\Data")
load("Nomissing_10000_Combined_Long.Rdata")
load("Nomissing_10000_Combined.Rdata")

## Join by sample size 
##########################################################################
## Join by sample size 
data_mcar <- MCAR_Long %>% filter(samplesize=="N=10,000") %>%
  mutate(mechanism = "MCAR")
## %>%
##  filter(Method != "Validation data, no missingness")
data_mar <- MAR_Long %>% filter(samplesize=="N=10,000") %>%
  mutate(mechanism = "MAR") 
## %>%
##  filter(Method != "Validation data, no missingness")
data_mnar <- MNAR_Long %>% filter(samplesize=="N=10,000") %>%
  mutate(mechanism = "MNAR") 

data <- rbind(data_mar,data_mcar)
data <- rbind(data,data_mnar) %>%
  filter(Method != "Validation data, no missingness")

## Create factors for plotting
########################################################################
# As factor

data <- data %>%
  mutate(mechanism = factor(mechanism, 
                            levels=c("MCAR", "MAR", "MNAR") ),
         Method = factor(Method, 
                         levels = c("Complete Case Analysis", 
                                    "Mean Imputation", 
                                    "Multiple Imputation with Outcome",
                                    "Multiple Imputation without Outcome")))


### Graph Loop ###
##########################################################
# Create Loop for graph 
##########################################################
plot_fnc <- function(df, measure, combinedmeasure, parameter, x_scale_limits, x_scale_breaks, intercept, width) {
  ggplot( 
    data %>%
      filter(Measure == measure,
             Parameter == parameter),
    aes(x = AVG, y = mechanism, shape = Method, colour=mechanism )) +
    geom_point(size = 2) +
    geom_errorbar(aes(xmin = LCI, xmax = UCI), width = 0.4) +
    labs(
      y = NULL,
      x = NULL,
      colour = "Missing Data Method\n(Mean, 95% CI)"
    ) +
    geom_vline(xintercept = intercept, linetype = "dotted", color = "grey50", alpha = 0.8) +
    facet_wrap( ~ Method, ncol=1, as.table=TRUE) +
    scale_shape_manual(values = c(16, 17, 18,15)) +
    scale_x_continuous(limits = x_scale_limits, breaks = x_scale_breaks) +
    theme_minimal() +
    theme(
      legend.position = "right",
      axis.title.x = element_text(size = 14),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      strip.text = element_blank() # Remove facet labels
    )
  
}
################################################################################
## Set Parameters for each plot type 
################################################################################
################
auc_x_limits <- c(0.6, 0.90)
auc_x_breaks <- seq(0, 1, by = 0.1)

auc_params <- list(
  list(parameter ="Outcome prevalence 1% and Missingness 25%", x_limits = auc_x_limits, x_breaks = auc_x_breaks, width=0.001,
       intercept =no_missing_long$AVG[no_missing_long$Metric == "AUC"& no_missing_long$df=="simresults_Yprev1Rprev75"]),
  list(parameter ="Outcome prevalence 1% and Missingness 50%", x_limits = auc_x_limits, x_breaks = auc_x_breaks, width=0.001, 
       intercept =no_missing_long$AVG[no_missing_long$Metric == "AUC"& no_missing_long$df=="simresults_Yprev1Rprev75"]),
  list(parameter ="Outcome prevalence 1% and Missingness 75%", x_limits = auc_x_limits, x_breaks = auc_x_breaks, width=0.001, 
       intercept =no_missing_long$AVG[no_missing_long$Metric == "AUC"& no_missing_long$df=="simresults_Yprev1Rprev75"]),
  list(parameter ="Outcome prevalence 5% and Missingness 25%", x_limits = auc_x_limits, x_breaks = auc_x_breaks, width=0.001,
       intercept =no_missing_long$AVG[no_missing_long$Metric == "AUC"& no_missing_long$df=="simresults_Yprev5Rprev75"]),
  list(parameter ="Outcome prevalence 5% and Missingness 50%", x_limits = auc_x_limits, x_breaks = auc_x_breaks, width=0.001, 
       intercept =no_missing_long$AVG[no_missing_long$Metric == "AUC"& no_missing_long$df=="simresults_Yprev5Rprev75"]),
  list(parameter ="Outcome prevalence 5% and Missingness 75%", x_limits = auc_x_limits, x_breaks = auc_x_breaks, width=0.001, 
       intercept =no_missing_long$AVG[no_missing_long$Metric == "AUC"& no_missing_long$df=="simresults_Yprev5Rprev75"]),
  list(parameter ="Outcome prevalence 10% and Missingness 25%", x_limits = auc_x_limits, x_breaks = auc_x_breaks, width=0.001, 
       intercept =no_missing_long$AVG[no_missing_long$Metric == "AUC"& no_missing_long$df=="simresults_Yprev10Rprev75"]),
  list(parameter ="Outcome prevalence 10% and Missingness 50%", x_limits = auc_x_limits, x_breaks = auc_x_breaks, width=0.001, 
       intercept =no_missing_long$AVG[no_missing_long$Metric == "AUC"& no_missing_long$df=="simresults_Yprev10Rprev75"]),
  list(parameter ="Outcome prevalence 10% and Missingness 75%", x_limits = auc_x_limits, x_breaks = auc_x_breaks, width=0.001,
       intercept =no_missing_long$AVG[no_missing_long$Metric == "AUC"& no_missing_long$df=="simresults_Yprev10Rprev75"]))

### Bias
####################
bias_x_limits <- c(-0.03,0.025)
bias_x_breaks <- seq(-1,1,by=0.01)

bias_params <- list(
  list(parameter ="Outcome prevalence 1% and Missingness 25%", x_limits = bias_x_limits, x_breaks = bias_x_breaks, width=0.0001, 
       intercept =no_missing_long$AVG[no_missing_long$Metric == "bias"& no_missing_long$df=="simresults_Yprev1Rprev75"]),
  list(parameter ="Outcome prevalence 1% and Missingness 50%", x_limits = bias_x_limits, x_breaks = bias_x_breaks, width=0.0001, 
       intercept =no_missing_long$AVG[no_missing_long$Metric == "bias"& no_missing_long$df=="simresults_Yprev1Rprev75"]),
  list(parameter ="Outcome prevalence 1% and Missingness 75%", x_limits = bias_x_limits, x_breaks = bias_x_breaks, width=0.0001, 
       intercept =no_missing_long$AVG[no_missing_long$Metric == "bias"& no_missing_long$df=="simresults_Yprev1Rprev75"]),
  list(parameter ="Outcome prevalence 5% and Missingness 25%", x_limits = bias_x_limits, x_breaks = bias_x_breaks, width=0.0001, 
       intercept =no_missing_long$AVG[no_missing_long$Metric == "bias"& no_missing_long$df=="simresults_Yprev5Rprev75"]),
  list(parameter ="Outcome prevalence 5% and Missingness 50%", x_limits = bias_x_limits, x_breaks = bias_x_breaks, width=0.0001, 
       intercept =no_missing_long$AVG[no_missing_long$Metric == "bias"& no_missing_long$df=="simresults_Yprev5Rprev75"]),
  list(parameter ="Outcome prevalence 5% and Missingness 75%", x_limits = bias_x_limits, x_breaks = bias_x_breaks, width=0.0001,  
       intercept =no_missing_long$AVG[no_missing_long$Metric == "bias"& no_missing_long$df=="simresults_Yprev5Rprev75"]),
  list(parameter ="Outcome prevalence 10% and Missingness 25%", x_limits = bias_x_limits, x_breaks = bias_x_breaks, width=0.0001,  
       intercept =no_missing_long$AVG[no_missing_long$Metric == "bias"& no_missing_long$df=="simresults_Yprev10Rprev75"]),
  list(parameter ="Outcome prevalence 10% and Missingness 50%", x_limits = bias_x_limits, x_breaks = bias_x_breaks, width=0.0001, 
       intercept =no_missing_long$AVG[no_missing_long$Metric == "bias"& no_missing_long$df=="simresults_Yprev10Rprev75"]),
  list(parameter ="Outcome prevalence 10% and Missingness 75%", x_limits = bias_x_limits, x_breaks = bias_x_breaks, width=0.0001,  
       intercept =no_missing_long$AVG[no_missing_long$Metric == "bias"& no_missing_long$df=="simresults_Yprev10Rprev75"]))

## Calibration in the large
############################
cal_int_x_limits <- c(-1,0.5)
cal_int_x_breaks <- seq(-2, 2, by = 0.5)

citl_params <- list(
  list(parameter ="Outcome prevalence 1% and Missingness 25%", x_limits =cal_int_x_limits, x_breaks = cal_int_x_breaks, width=0.05,
       intercept =no_missing_long$AVG[no_missing_long$Metric == "Cal_Int"& no_missing_long$df=="simresults_Yprev1Rprev75"]),
  list(parameter ="Outcome prevalence 1% and Missingness 50%", x_limits =cal_int_x_limits, x_breaks = cal_int_x_breaks, width=0.05,
       intercept =no_missing_long$AVG[no_missing_long$Metric == "Cal_Int"& no_missing_long$df=="simresults_Yprev1Rprev75"]),
  list(parameter ="Outcome prevalence 1% and Missingness 75%", x_limits =cal_int_x_limits, x_breaks = cal_int_x_breaks, width=0.05,
       intercept =no_missing_long$AVG[no_missing_long$Metric == "Cal_Int"& no_missing_long$df=="simresults_Yprev1Rprev75"]),
  list(parameter ="Outcome prevalence 5% and Missingness 25%", x_limits =cal_int_x_limits, x_breaks = cal_int_x_breaks, width=0.05, 
       intercept =no_missing_long$AVG[no_missing_long$Metric == "Cal_Int"& no_missing_long$df=="simresults_Yprev5Rprev75"]),
  list(parameter ="Outcome prevalence 5% and Missingness 50%", x_limits =cal_int_x_limits, x_breaks = cal_int_x_breaks, width=0.05,
       intercept =no_missing_long$AVG[no_missing_long$Metric == "Cal_Int"& no_missing_long$df=="simresults_Yprev5Rprev75"]),
  list(parameter ="Outcome prevalence 5% and Missingness 75%", x_limits =cal_int_x_limits, x_breaks = cal_int_x_breaks, width=0.05, 
       intercept =no_missing_long$AVG[no_missing_long$Metric == "Cal_Int"& no_missing_long$df=="simresults_Yprev5Rprev75"]),
  list(parameter ="Outcome prevalence 10% and Missingness 25%", x_limits =cal_int_x_limits, x_breaks = cal_int_x_breaks, width=0.05, 
       intercept =no_missing_long$AVG[no_missing_long$Metric == "Cal_Int"& no_missing_long$df=="simresults_Yprev10Rprev75"]),
  list(parameter ="Outcome prevalence 10% and Missingness 50%", x_limits =cal_int_x_limits, x_breaks = cal_int_x_breaks, width=0.05, 
       intercept =no_missing_long$AVG[no_missing_long$Metric == "Cal_Int"& no_missing_long$df=="simresults_Yprev10Rprev75"]),
  list(parameter ="Outcome prevalence 10% and Missingness 75%", x_limits =cal_int_x_limits, x_breaks = cal_int_x_breaks, width=0.05, 
       intercept =no_missing_long$AVG[no_missing_long$Metric == "Cal_Int"& no_missing_long$df=="simresults_Yprev10Rprev75"]))


## Calibration Slope
###########################
cal_slope_x_limits=c(0.4,2.0)
cal_slope_x_breaks=seq(-1,4,by=0.25)

calslope_params <- list(
  list(parameter ="Outcome prevalence 1% and Missingness 25%", x_limits =cal_slope_x_limits, x_breaks = cal_slope_x_breaks, width=0.05,
       intercept =no_missing_long$AVG[no_missing_long$Metric == "Cal_Slope"& no_missing_long$df=="simresults_Yprev1Rprev75"]),
  list(parameter ="Outcome prevalence 1% and Missingness 50%", x_limits =cal_slope_x_limits, x_breaks = cal_slope_x_breaks, width=0.05,
       intercept =no_missing_long$AVG[no_missing_long$Metric == "Cal_Slope"& no_missing_long$df=="simresults_Yprev1Rprev75"]),
  list(parameter ="Outcome prevalence 1% and Missingness 75%", x_limits =cal_slope_x_limits, x_breaks = cal_slope_x_breaks, width=0.05,
       intercept =no_missing_long$AVG[no_missing_long$Metric == "Cal_Slope"& no_missing_long$df=="simresults_Yprev1Rprev75"]),
  list(parameter ="Outcome prevalence 5% and Missingness 25%", x_limits =cal_slope_x_limits, x_breaks = cal_slope_x_breaks, width=0.05, 
       intercept =no_missing_long$AVG[no_missing_long$Metric == "Cal_Slope"& no_missing_long$df=="simresults_Yprev5Rprev75"]),
  list(parameter ="Outcome prevalence 5% and Missingness 50%", x_limits =cal_slope_x_limits, x_breaks = cal_slope_x_breaks, width=0.05,
       intercept =no_missing_long$AVG[no_missing_long$Metric == "Cal_Slope"& no_missing_long$df=="simresults_Yprev5Rprev75"]),
  list(parameter ="Outcome prevalence 5% and Missingness 75%", x_limits =cal_slope_x_limits, x_breaks = cal_slope_x_breaks, width=0.05, 
       intercept =no_missing_long$AVG[no_missing_long$Metric == "Cal_Slope"& no_missing_long$df=="simresults_Yprev5Rprev75"]),
  list(parameter ="Outcome prevalence 10% and Missingness 25%", x_limits =cal_slope_x_limits, x_breaks = cal_slope_x_breaks, width=0.05, 
       intercept =no_missing_long$AVG[no_missing_long$Metric == "Cal_Slope"& no_missing_long$df=="simresults_Yprev10Rprev75"]),
  list(parameter ="Outcome prevalence 10% and Missingness 50%", x_limits =cal_slope_x_limits, x_breaks = cal_slope_x_breaks, width=0.05, 
       intercept =no_missing_long$AVG[no_missing_long$Metric == "Cal_Slope"& no_missing_long$df=="simresults_Yprev10Rprev75"]),
  list(parameter ="Outcome prevalence 10% and Missingness 75%", x_limits =cal_slope_x_limits, x_breaks = cal_slope_x_breaks, width=0.05, 
       intercept =no_missing_long$AVG[no_missing_long$Metric == "Cal_Slope"& no_missing_long$df=="simresults_Yprev10Rprev75"]))

## Scaled Brier
##########################
brierscl_x_limits <- c(-0.05,0.3)
brierscl_x_breaks <- seq(-0.5,0.5, by=0.1)

brierscl_params <- list(
  list(parameter ="Outcome prevalence 1% and Missingness 25%",x_limits = brierscl_x_limits, x_breaks = brierscl_x_breaks,width=0.0001,
       intercept =no_missing_long$AVG[no_missing_long$Metric == "Brier_scaled"& no_missing_long$df=="simresults_Yprev1Rprev75"]),
  list(parameter ="Outcome prevalence 1% and Missingness 50%",x_limits = brierscl_x_limits, x_breaks = brierscl_x_breaks,width=0.0001,
       intercept =no_missing_long$AVG[no_missing_long$Metric == "Brier_scaled"& no_missing_long$df=="simresults_Yprev1Rprev75"]),
  list(parameter ="Outcome prevalence 1% and Missingness 75%",x_limits = brierscl_x_limits, x_breaks = brierscl_x_breaks,width=0.0001,
       intercept =no_missing_long$AVG[no_missing_long$Metric == "Brier_scaled"& no_missing_long$df=="simresults_Yprev1Rprev75"]),
  list(parameter ="Outcome prevalence 5% and Missingness 25%",x_limits = brierscl_x_limits, x_breaks = brierscl_x_breaks,width=0.0001,
       intercept =no_missing_long$AVG[no_missing_long$Metric == "Brier_scaled"& no_missing_long$df=="simresults_Yprev5Rprev75"]),
  list(parameter ="Outcome prevalence 5% and Missingness 50%",x_limits = brierscl_x_limits, x_breaks = brierscl_x_breaks,width=0.0001,
       intercept =no_missing_long$AVG[no_missing_long$Metric == "Brier_scaled"& no_missing_long$df=="simresults_Yprev5Rprev75"]),
  list(parameter ="Outcome prevalence 5% and Missingness 75%",x_limits = brierscl_x_limits, x_breaks = brierscl_x_breaks,width=0.0001,
       intercept =no_missing_long$AVG[no_missing_long$Metric == "Brier_scaled"& no_missing_long$df=="simresults_Yprev5Rprev75"]),
  list(parameter ="Outcome prevalence 10% and Missingness 25%",x_limits = brierscl_x_limits, x_breaks = brierscl_x_breaks,width=0.0001,
       intercept =no_missing_long$AVG[no_missing_long$Metric == "Brier_scaled"& no_missing_long$df=="simresults_Yprev10Rprev75"]),
  list(parameter ="Outcome prevalence 10% and Missingness 50%",x_limits = brierscl_x_limits, x_breaks = brierscl_x_breaks,width=0.0001,
       intercept =no_missing_long$AVG[no_missing_long$Metric == "Brier_scaled"& no_missing_long$df=="simresults_Yprev10Rprev75"]),
  list(parameter ="Outcome prevalence 10% and Missingness 75%",x_limits = brierscl_x_limits, x_breaks = brierscl_x_breaks, width=0.0001,
       intercept =no_missing_long$AVG[no_missing_long$Metric == "Brier_scaled"& no_missing_long$df=="simresults_Yprev10Rprev75"]))


## RMSE
##########################
rmse_x_limits <- c(0,0.35)
rmse_x_breaks <- seq(0,0.5,by=0.1)

rmse_params <- list(
  list(parameter ="Outcome prevalence 1% and Missingness 25%", x_limits =rmse_x_limits, x_breaks = rmse_x_breaks, width=0.0001,
       intercept =no_missing_long$AVG[no_missing_long$Metric == "rmse"& no_missing_long$df=="simresults_Yprev1Rprev75"]),
  list(parameter ="Outcome prevalence 1% and Missingness 50%", x_limits =rmse_x_limits, x_breaks = rmse_x_breaks, width=0.0001,
       intercept =no_missing_long$AVG[no_missing_long$Metric == "rmse"& no_missing_long$df=="simresults_Yprev1Rprev75"]),
  list(parameter ="Outcome prevalence 1% and Missingness 75%", x_limits =rmse_x_limits, x_breaks = rmse_x_breaks, width=0.0001,
       intercept =no_missing_long$AVG[no_missing_long$Metric == "rmse"& no_missing_long$df=="simresults_Yprev1Rprev75"]),
  list(parameter ="Outcome prevalence 5% and Missingness 25%", x_limits =rmse_x_limits, x_breaks = rmse_x_breaks, width=0.0001,
       intercept =no_missing_long$AVG[no_missing_long$Metric == "rmse"& no_missing_long$df=="simresults_Yprev5Rprev75"]),
  list(parameter ="Outcome prevalence 5% and Missingness 50%", x_limits =rmse_x_limits, x_breaks = rmse_x_breaks, width=0.0001,
       intercept =no_missing_long$AVG[no_missing_long$Metric == "rmse"& no_missing_long$df=="simresults_Yprev5Rprev75"]),
  list(parameter ="Outcome prevalence 5% and Missingness 75%", x_limits =rmse_x_limits, x_breaks = rmse_x_breaks, width=0.0001,
       intercept =no_missing_long$AVG[no_missing_long$Metric == "rmse"& no_missing_long$df=="simresults_Yprev5Rprev75"]),
  list(parameter ="Outcome prevalence 10% and Missingness 25%", x_limits =rmse_x_limits, x_breaks = rmse_x_breaks, width=0.0001,
       intercept =no_missing_long$AVG[no_missing_long$Metric == "rmse"& no_missing_long$df=="simresults_Yprev10Rprev75"]),
  list(parameter ="Outcome prevalence 10% and Missingness 50%", x_limits =rmse_x_limits, x_breaks = rmse_x_breaks, width=0.0001,
       intercept =no_missing_long$AVG[no_missing_long$Metric == "rmse"& no_missing_long$df=="simresults_Yprev10Rprev75"]),
  list(parameter ="Outcome prevalence 10% and Missingness 75%", x_limits =rmse_x_limits, x_breaks = rmse_x_breaks, width=0.0001,
       intercept =no_missing_long$AVG[no_missing_long$Metric == "rmse"& no_missing_long$df=="simresults_Yprev10Rprev75"]))



################################################################################
## Set Measures and Sample Sizes
################################################################################
# Define measures 
measures <- list(
  AUC = list(measure = "AUC", combinedmeasure = "AUC", params=auc_params),
  Bias = list(measure = "Bias", combinedmeasure = "bias", params=bias_params), 
  CalInt = list(measure = "Calibration in the Large", combinedmeasure = "Cal_Int", params=citl_params),
  CalSlope = list( measure = "Calibration Slope", combinedmeasure = "Cal_Slope", params=calslope_params),
  Brier_scaled = list(measure = "Brier Score Scaled", combinedmeasure = "Brier_scaled", params=brierscl_params ),
  RMSE = list(measure = "Root Mean Square Error", combinedmeasure = "rmse", params=rmse_params )
)


################################################################################
## Create Plots 
################################################################################
# Initialize lists to store plots
plot_storage <- list()

# Nested loop for measures and sample sizes
for (measure_name in names(measures)) {
  # Initialize a list for the current measure
  plot_storage[[measure_name]] <- list()
  
  
  # Create plots for the current measure and sample size
  plots <- list()
  for (i in seq_along(measures[[measure_name]]$params)) {
    params <- measures[[measure_name]]$params[[i]]
    plot <- plot_fnc(
      df = data,
      measure = measures[[measure_name]]$measure,
      combinedmeasure = measures[[measure_name]]$combinedmeasure,
      parameter = params$parameter,
      x_scale_limits = params$x_limits,
      x_scale_breaks = params$x_breaks,
      width = params$width, 
      intercept= params$intercept
    )
    plots[[i]] <- plot
  }
  
  # Store plots in the appropriate sublist
  plot_storage[[measure_name]] <- plots
}


###############################################################################
## Creat Patchwork 
###############################################################################
library(patchwork)

## Set labels
##########################################################
label_blank <- ggplot() +
  theme_void() + # Remove axes and grid
  geom_text(aes(x = 0.5, y = 0.5, label = ""), size = 5, hjust = 0.5, vjust = 0.5)

label_top <-  ggplot() +
  theme_void() + # Remove axes and grid
  geom_text(aes(x = 0.5, y = 0.5, label = "Missingness in X1 (%)"), size = 5, hjust = 0.5, vjust = 0.5, fontface="bold")

label_left <-  ggplot() +
  theme_void() + # Remove axes and grid
  geom_text(aes(x = 0.5, y = 0.5, label = "Outcome Prevalence"), size = 5, hjust = 0.5, vjust = 0.5, angle=90, fontface="bold")

label_25 <- ggplot() +
  theme_void() + # Remove axes and grid
  geom_text(aes(x = 0.5, y = 0.5, label = "25%"), size = 5, hjust = 0.5, vjust = 0.5)

label_50 <- ggplot() +
  theme_void() + # Remove axes and grid
  geom_text(aes(x = 0.5, y = 0.5, label = "50%"), size = 5, hjust = 0.5, vjust = 0.5)

label_75 <- ggplot() +
  theme_void() + # Remove axes and grid
  geom_text(aes(x = 0.5, y = 0.5, label = "75%"), size = 5, hjust = 0.5, vjust = 0.5)

label_1 <- ggplot() +
  theme_void() + # Remove axes and grid
  geom_text(aes(x = 0.5, y = 0.5, label = "1%"), size = 5, hjust = 0.5, vjust = 0.5)

label_5 <- ggplot() +
  theme_void() + # Remove axes and grid
  geom_text(aes(x = 0.5, y = 0.5, label = "5%"), size = 5, hjust = 0.5, vjust = 0.5)

label_10 <- ggplot() +
  theme_void() + # Remove axes and grid
  geom_text(aes(x = 0.5, y = 0.5, label = "10%"), size = 5, hjust = 0.5, vjust = 0.5)


### Design Patchwork
###############################################
# Combine the two plots using patchwork
#         area(start_row, start_col, end_row, end_col)
#       Each number means the following:
#   
#           start_row: The starting row number where the plot begins.
#           start_col: The starting column number where the plot begins.
#           end_row: The ending row number where the plot ends.
#           end_col: The ending column number where the plot ends.


## Layout of the graphs
design <- c(
  area(1, 1, 1, 1),
  area(1, 2, 1, 2),
  area(1, 3, 1, 3), 
  area(1, 4, 1, 4),
  area(1, 5, 1, 5),
  ## 2nd row
  area(2, 1, 2, 1), # Top-left for blank
  area(2, 2, 2, 2), # Top-center for 25%
  area(2, 3, 2, 3), # Top-right for 50%
  area(2, 4, 2, 4), # Top-right for 75%
  area(2, 5, 2, 5), # Top-right for 75%
  
  ## 1st column (labels)
  area(3, 1, 3, 1), # 
  area(4, 1, 4, 1), # 
  area(5, 1, 5, 1), # 
  ## 2nd Column
  area(3, 2, 3, 2), # 1% 
  area(4, 2, 4, 2), # 5% 
  area(5, 2, 5, 2), # 10%
  ## 3rd column
  area(3, 3, 3, 3), #  outcome1_missingness25
  area(4, 3, 4, 3), # outcome5_missingness25
  area(5, 3, 5, 3), # outcome10_missingness25
  ## 4th Column
  area(3, 4, 3, 4), # outcome1_missingness50  
  area(4, 4, 4, 4), # outcome5_missingness50 
  area(5, 4, 5, 4), # outcome10_missingness50
  ## 5th Column
  area(3, 5, 3, 5), # outcome1_missingness75  
  area(4, 5, 4, 5), # outcome5_missingness75 
  area(5, 5, 5, 5) # outcome10_missingness75
)

## Function to create patchwork
#######################################
# Define a function to create patchwork layouts dynamically
create_patchwork <- function(plots, title){
  
  # Construct the patchwork layout
  patchwork <- label_blank + label_blank + label_blank + label_top + label_blank +
    # 2nd row
    label_blank + label_blank + label_25 + label_50 + label_75 +
    # 1st column
    label_blank + label_left + label_blank +
    # 2nd column
    label_1 + label_5 + label_10 +
    # 3rd column
    plots[[1]] + plots[[4]] + plots[[7]] +
    # 4th column
    plots[[2]] + plots[[5]] + plots[[8]] +
    # 5th column
    plots[[3]] + plots[[6]] + plots[[9]] +
    plot_layout(design = design, guides = "collect", axes = "collect_x",
                widths = c(1, 2, 10, 10, 10), heights = c(1, 1, 6, 6, 6)) +
    plot_annotation(
      title = title,
      subtitle = subtitle
    )
  return(patchwork)
}

### Measures and Subtitles 
###############################
measure_titles <- list(
  AUC = list(title= "Discrimination (AUC)",
             subtitle ="Higher scores indicate better discrimination with 0.5 indicating the model is no better than chance"),
  Bias = list(title = "Bias",
              subtitle = "Bias indicates the difference between observed and true outcomes, lower values are better"),
  CalInt = list(title="Calibration in the Large",
                subtitle="Where 0 indicates perfect calibration, positive values indicate risk underestimation and negative values indicate overestimation"),
  CalSlope = list(title="Calibration Slope",
                  subtitle="Where 1 indicates perfect calibration, >1 indicates underfitting and <1 indicate overfitting"),
  Brier_scaled = list(title="Scaled Brier Score",
                      subtitle="Score of 1 indicates perfect prediction, 0 or less than 0 indicates no better than baseline model"), 
  RMSE = list(title="Root Mean Square Error",
              subtitle="Lower RMSE indicates better model performance")
)


## Create patchwork of graphs
###################################
setwd("C:\\Users\\maecj\\OneDrive - Nexus365\\A DPhil\\Simulation studies\\Programs\\Study 1\\SimulationStudy1_11Jun2024\\SimulationStudy\\Plots")
patchwork_storage <- list()

for (measure_name in names(plot_storage)) {
  # Initialize a list for the current measure
  patchwork_storage[[measure_name]] <- list()
  
  # Get the plots for the current measure and sample size
  measure_plots <- plot_storage[[measure_name]]
  
  # Define the title and subtitle dynamically
  title <- paste(measure_titles[[measure_name]][["title"]], " across Missingness Mechanisms", sep = "")
  subtitle <- measure_titles[[measure_name]][["subtitle"]]
  
  # Create the patchwork
  patchwork <- create_patchwork(measure_plots, title)
  patchwork <- patchwork + plot_annotation(subtitle = subtitle)
  
  # Store the patchwork
  patchwork_storage[[measure_name]] <- patchwork
  
  # Save the patchwork as a PDF
  output_file <- paste0("Patchwork_MissingnessMechanism_10000_", measure_name, ".pdf")
  
  # Save the patchwork as a PDF (you can adjust the width and height as needed)
  ggsave(output_file, plot = patchwork, device = "pdf", width = 14, height = 8)
}

