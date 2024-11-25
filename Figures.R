################################################################################
### Figures for Simulation Study Results 
################################################################################
## Created: 24Nov2024
## AUthor: Mae CHester-Jones
## Purpose: Create graphs summarising the Simulatino Study Results
################################################################################
# 
# ## library
# library(dplyr)
# library(ggplot2)
# library(tidyr)
# library(flextable)
# library(rlang)
# 
# ## Set working directory
# setwd("C:\\Users\\maecj\\OneDrive - Nexus365\\A DPhil\\Simulation studies\\Programs\\Study 1\\SimulationStudy1_11Jun2024\\SimulationStudy\\Data")
# 
# ## Load required datasets
# ########################
# load("MCAR_500_Combined_Long_07Oct2024.Rdata")
# load("MCAR_500_Combined_07Oct2024.Rdata")

# 
# 
# ## Create missingness and prevalance groups
# ###############################################
# simulation_parameters_long <- simulation_parameters_long %>%
#   mutate(Prevalence = case_when(grepl("Outcome prevalence 1%", Parameter) ~ "1%",
#                                 grepl("Outcome prevalence 5%", Parameter) ~ "5%",
#                                 grepl("Outcome prevalence 10%", Parameter) ~ "10%"),
#          Missingness =  case_when(grepl("25%", Parameter) ~ "25%",
#                                   grepl("50%", Parameter) ~ "50%",
#                                   grepl("75%", Parameter) ~ "75%"))

################################################################################
# ## Brier Score 
# ################################################################################
# ggplot(simulation_parameters_long %>% filter(Measure=="Brier Score"),
#        aes(x=AVG, y=Method, colour=Method)) + 
#   geom_point(size=3) +
#   geom_errorbar(aes(xmin = LCI, xmax = UCI), width = 0.8 ) +   
#   geom_point(data = combined_df, aes(x = Brier, y = Method),               
#              shape =4, position = position_jitter(width = 0.0001), alpha = 0.5) + 
#   labs(y = NULL,
#        x = NULL,
#        colour = "Missing Data Method\n(Mean, 95% CI)") +
#   theme_minimal() + 
#   facet_grid( Prevalance~Missingness , scales = "fixed", switch="both", labeller = labeller(
#     Missingness = label_both,
#     Prevalance = label_both
#   )) +   
#   ## Add X scale 
#   scale_x_continuous(limits = c(0, 0.15), breaks = seq(0, 0.15, by = 0.025)) +     
#   ## Add Colour Scale
#   scale_colour_manual(values = c("Validation data, no missingness" = "grey",
#                                  "Complete Case Analysis" = "blue", 
#                                  "Mean Imputation" = "red", 
#                                  "Multiple Imputation with Outcome" = "green",
#                                  "Multiple Imputation without Outcome" = "purple")) +
#   labs(#caption = "Brier score ranges between 0 (perfect accuracy) and 1 (perfect inaccuracy). This x scale runs from 0 to 0.15.") + 
#   theme(legend.position = "right",
#         strip.text = element_text(size = 14),  # Customize strip text size
#         strip.placement = "outside",  # Place strip labels outside the plot area
#         strip.background = element_blank(),  # Remove strip background
#         axis.title.x = element_text(size = 14), 
#         axis.title.y = element_text(size = 14), 
#         axis.text.x = element_text(size = 12), 
#         axis.text.y = element_blank(),  # Remove y-axis text
#         axis.ticks.y = element_blank())  # Remove y-axis tick      


# ## Create the 9 graphs 
# ###############################################################################
# outcome1_missingness25 <- ggplot(simulation_parameters_long %>%
#                                    filter(Measure == "Brier Score",
#                                           Parameter == "Outcome prevalence 1% and Missingness 25%"),
#                                  aes(x = AVG, y = Method, colour = Method)) +
#   geom_point(size = 3) +
#   geom_errorbar(aes(xmin = LCI, xmax = UCI), width = 0.8) +
#   geom_point(data = combined_df %>%
#                filter(Parameter == "Outcome prevalence 1% and Missingness 25%"),
#              aes(x = Brier, y = Method),
#              shape = 4, position = position_jitter(width = 0.0001), alpha = 0.5) +
#   labs(
#     y = NULL,
#     x = NULL,
#     ##caption = "Brier score ranges between 0 (perfect accuracy) and 1 (perfect inaccuracy). This x scale runs from 0 to 0.15."
#     colour = "Missing Data Method\n(Mean, 95% CI)",
#   ) +
#   theme_minimal() +
#   scale_x_continuous(limits = c(0, 0.15), breaks = seq(0, 0.15, by = 0.05)) +  # Customize x-axis scale
#   scale_colour_manual(
#     values = c(
#       "Validation data, no missingness" = "grey",
#       "Complete Case Analysis" = "blue",
#       "Mean Imputation" = "red",
#       "Multiple Imputation with Outcome" = "green",
#       "Multiple Imputation without Outcome" = "purple"
#     )
#   ) +
#   theme(
#     legend.position = "right",
#     axis.title.x = element_text(size = 14),
#     axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, hjust = 1),
#     axis.text.y=element_blank(),  #remove y axis labels
#     axis.ticks.y=element_blank()  #remove y axis ticks
#   )
# 
# # Outcome 1% missingness 50%
# outcome1_missingness50 <- ggplot(simulation_parameters_long %>%
#                                    filter(Measure == "Brier Score",
#                                           Prevalence == "1%",
#                                           Missingness == "50%"),
#                                  aes(x = AVG, y = Method, colour = Method)) +
#   geom_point(size = 3) +
#   geom_errorbar(aes(xmin = LCI, xmax = UCI), width = 0.8) +
#   geom_point(data = combined_df %>%
#                filter(Parameter == "Outcome prevalence 1% and Missingness 50%"),
#              aes(x = Brier, y = Method),
#              shape = 4, position = position_jitter(width = 0.0001), alpha = 0.5) +
#   labs(
#     y = NULL,
#     x = NULL,
#     colour = "Missing Data Method\n(Mean, 95% CI)",
#     #caption = "Brier score ranges between 0 (perfect accuracy) and 1 (perfect inaccuracy). This x scale runs from 0 to 0.15."
#   ) +
#   theme_minimal() +
#   scale_x_continuous(limits = c(0, 0.15), breaks = seq(0, 0.15, by = 0.05)) +  # Customize x-axis scale
#   scale_colour_manual(
#     values = c(
#       "Validation data, no missingness" = "grey",
#       "Complete Case Analysis" = "blue",
#       "Mean Imputation" = "red",
#       "Multiple Imputation with Outcome" = "green",
#       "Multiple Imputation without Outcome" = "purple"
#     )
#   ) +
#   theme(
#     legend.position = "right",
#     axis.title.x = element_text(size = 14),
#     axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, hjust = 1),
#     axis.text.y=element_blank(),  #remove y axis labels
#     axis.ticks.y=element_blank()  #remove y axis ticks
#   )
# 
# # Outcome 1% Missingness 75%
# 
# outcome1_missingness75 <- ggplot(simulation_parameters_long %>%
#                                    filter(Measure == "Brier Score",
#                                           Prevalence == "1%",
#                                           Missingness == "75%"),
#                                  aes(x = AVG, y = Method, colour = Method)) +
#   geom_point(size = 3) +
#   geom_errorbar(aes(xmin = LCI, xmax = UCI), width = 0.8) +
#   geom_point(data = combined_df %>%
#                filter(Parameter == "Outcome prevalence 1% and Missingness 75%"),
#              aes(x = Brier, y = Method),
#              shape = 4, position = position_jitter(width = 0.0001), alpha = 0.5) +
#   labs(
#     y = NULL,
#     x = NULL,
#     colour = "Missing Data Method\n(Mean, 95% CI)",
#     #caption = "Brier score ranges between 0 (perfect accuracy) and 1 (perfect inaccuracy). This x scale runs from 0 to 0.15."
#   ) +
#   theme_minimal() +
#   scale_x_continuous(limits = c(0, 0.15), breaks = seq(0, 0.15, by = 0.05)) +  # Customize x-axis scale
#   scale_colour_manual(
#     values = c(
#       "Validation data, no missingness" = "grey",
#       "Complete Case Analysis" = "blue",
#       "Mean Imputation" = "red",
#       "Multiple Imputation with Outcome" = "green",
#       "Multiple Imputation without Outcome" = "purple"
#     )
#   ) +
#   theme(
#     legend.position = "right",
#     axis.title.x = element_text(size = 14),
#     axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, hjust = 1),
#     axis.text.y=element_blank(),  #remove y axis labels
#     axis.ticks.y=element_blank()  #remove y axis ticks
#   )
# 
# 
# ## Outcome 5%, Missingness 25% 
# outcome5_missingness25 <- ggplot(simulation_parameters_long %>%
#                                    filter(Measure == "Brier Score",
#                                           Prevalence == "5%",
#                                           Missingness == "25%"),
#                                  aes(x = AVG, y = Method, colour = Method)) +
#   geom_point(size = 3) +
#   geom_errorbar(aes(xmin = LCI, xmax = UCI), width = 0.8) +
#   geom_point(data = combined_df %>%
#                filter(Parameter == "Outcome prevalence 5% and Missingness 25%"),
#              aes(x = Brier, y = Method),
#              shape = 4, position = position_jitter(width = 0.0001), alpha = 0.5) +
#   labs(
#     y = NULL,
#     x = NULL,
#     colour = "Missing Data Method\n(Mean, 95% CI)",
#     #caption = "Brier score ranges between 0 (perfect accuracy) and 1 (perfect inaccuracy). This x scale runs from 0 to 0.15."
#   ) +
#   theme_minimal() +
#   scale_x_continuous(limits = c(0, 0.15), breaks = seq(0, 0.15, by = 0.05)) +  # Customize x-axis scale
#   scale_colour_manual(
#     values = c(
#       "Validation data, no missingness" = "grey",
#       "Complete Case Analysis" = "blue",
#       "Mean Imputation" = "red",
#       "Multiple Imputation with Outcome" = "green",
#       "Multiple Imputation without Outcome" = "purple"
#     )
#   ) +
#   theme(
#     legend.position = "right",
#     axis.title.x = element_text(size = 14),
#     axis.text.x =element_text(size = 12, angle = 90, vjust = 0.5, hjust = 1),
#     axis.text.y=element_blank(),  #remove y axis labels
#     axis.ticks.y=element_blank()  #remove y axis ticks
#   )
# 
# # Outcome 5% missingness 50%
# outcome5_missingness50 <- ggplot(simulation_parameters_long %>%
#                                    filter(Measure == "Brier Score",
#                                           Prevalence == "5%",
#                                           Missingness == "50%"),
#                                  aes(x = AVG, y = Method, colour = Method)) +
#   geom_point(size = 3) +
#   geom_errorbar(aes(xmin = LCI, xmax = UCI), width = 0.8) +
#   geom_point(data = combined_df %>%
#                filter(Parameter == "Outcome prevalence 5% and Missingness 50%"),
#              aes(x = Brier, y = Method),
#              shape = 4, position = position_jitter(width = 0.0001), alpha = 0.5) +
#   labs(
#     y = NULL,
#     x = NULL,
#     colour = "Missing Data Method\n(Mean, 95% CI)",
#     #caption = "Brier score ranges between 0 (perfect accuracy) and 1 (perfect inaccuracy). This x scale runs from 0 to 0.15."
#   ) +
#   theme_minimal() +
#   scale_x_continuous(limits = c(0, 0.15), breaks = seq(0, 0.15, by = 0.05)) +  # Customize x-axis scale
#   scale_colour_manual(
#     values = c(
#       "Validation data, no missingness" = "grey",
#       "Complete Case Analysis" = "blue",
#       "Mean Imputation" = "red",
#       "Multiple Imputation with Outcome" = "green",
#       "Multiple Imputation without Outcome" = "purple"
#     )
#   ) +
#   theme(
#     legend.position = "right",
#     axis.title.x = element_text(size = 14),
#     axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, hjust = 1),
#     axis.text.y=element_blank(),  #remove y axis labels
#     axis.ticks.y=element_blank()  #remove y axis ticks
#   )
# 
# # Outcome 5% Missingness 75%
# outcome5_missingness75 <- ggplot(simulation_parameters_long %>%
#                                    filter(Measure == "Brier Score",
#                                           Prevalence == "5%",
#                                           Missingness == "75%"),
#                                  aes(x = AVG, y = Method, colour = Method)) +
#   geom_point(size = 3) +
#   geom_errorbar(aes(xmin = LCI, xmax = UCI), width = 0.8) +
#   geom_point(data = combined_df %>%
#                filter(Parameter == "Outcome prevalence 5% and Missingness 75%"),
#              aes(x = Brier, y = Method),
#              shape = 4, position = position_jitter(width = 0.0001), alpha = 0.5) +
#   labs(
#     y = NULL,
#     x = NULL,
#     colour = "Missing Data Method\n(Mean, 95% CI)",
#     #caption = "Brier score ranges between 0 (perfect accuracy) and 1 (perfect inaccuracy). This x scale runs from 0 to 0.15."
#   ) +
#   theme_minimal() +
#   scale_x_continuous(limits = c(0, 0.15), breaks = seq(0, 0.15, by = 0.05)) +  # Customize x-axis scale
#   scale_colour_manual(
#     values = c(
#       "Validation data, no missingness" = "grey",
#       "Complete Case Analysis" = "blue",
#       "Mean Imputation" = "red",
#       "Multiple Imputation with Outcome" = "green",
#       "Multiple Imputation without Outcome" = "purple"
#     )
#   ) +
#   theme(
#     legend.position = "right",
#     axis.title.x = element_text(size = 14),
#     axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, hjust = 1),
#     axis.text.y=element_blank(),  #remove y axis labels
#     axis.ticks.y=element_blank()  #remove y axis ticks
#   )
# 
# 
# outcome10_missingness25 <- ggplot(simulation_parameters_long %>%
#                                    filter(Measure == "Brier Score",
#                                           Prevalence == "10%",
#                                           Missingness == "25%"),
#                                  aes(x = AVG, y = Method, colour = Method)) +
#   geom_point(size = 3) +
#   geom_errorbar(aes(xmin = LCI, xmax = UCI), width = 0.8) +
#   geom_point(data = combined_df %>%
#                filter(Parameter == "Outcome prevalence 10% and Missingness 25%"),
#              aes(x = Brier, y = Method),
#              shape = 4, position = position_jitter(width = 0.0001), alpha = 0.5) +
#   labs(
#     y = NULL,
#     x = NULL,
#     colour = "Missing Data Method\n(Mean, 95% CI)",
#     #caption = "Brier score ranges between 0 (perfect accuracy) and 1 (perfect inaccuracy). This x scale runs from 0 to 0.15."
#   ) +
#   theme_minimal() +
#   scale_x_continuous(limits = c(0, 0.15), breaks = seq(0, 0.15, by = 0.05)) +  # Customize x-axis scale
#   scale_colour_manual(
#     values = c(
#       "Validation data, no missingness" = "grey",
#       "Complete Case Analysis" = "blue",
#       "Mean Imputation" = "red",
#       "Multiple Imputation with Outcome" = "green",
#       "Multiple Imputation without Outcome" = "purple"
#     )
#   ) +
#   theme(
#     legend.position = "right",
#     axis.title.x = element_text(size = 14),
#     axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, hjust = 1),
#     axis.text.y=element_blank(),  #remove y axis labels
#     axis.ticks.y=element_blank()  #remove y axis ticks
#   )
# 
# # Outcome 10% missingness 50%
# outcome10_missingness50 <- ggplot(simulation_parameters_long %>%
#                                    filter(Measure == "Brier Score",
#                                           Prevalence == "10%",
#                                           Missingness == "50%"),
#                                  aes(x = AVG, y = Method, colour = Method)) +
#   geom_point(size = 3) +
#   geom_errorbar(aes(xmin = LCI, xmax = UCI), width = 0.8) +
#   geom_point(data = combined_df %>%
#                filter(Parameter == "Outcome prevalence 10% and Missingness 50%"),
#              aes(x = Brier, y = Method),
#              shape = 4, position = position_jitter(width = 0.0001), alpha = 0.5) +
#   labs(
#     y = NULL,
#     x = NULL,
#     colour = "Missing Data Method\n(Mean, 95% CI)",
#     #caption = "Brier score ranges between 0 (perfect accuracy) and 1 (perfect inaccuracy). This x scale runs from 0 to 0.15."
#   ) +
#   theme_minimal() +
#   scale_x_continuous(limits = c(0, 0.15), breaks = seq(0, 0.15, by = 0.05)) +  # Customize x-axis scale
#   scale_colour_manual(
#     values = c(
#       "Validation data, no missingness" = "grey",
#       "Complete Case Analysis" = "blue",
#       "Mean Imputation" = "red",
#       "Multiple Imputation with Outcome" = "green",
#       "Multiple Imputation without Outcome" = "purple"
#     )
#   ) +
#   theme(
#     legend.position = "right",
#     axis.title.x = element_text(size = 14),
#     axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, hjust = 1),
#     axis.text.y=element_blank(),  #remove y axis labels
#     axis.ticks.y=element_blank()  #remove y axis ticks
#   )
# 
# # Outcome 10% Missingness 75%
# outcome10_missingness75 <- ggplot(simulation_parameters_long %>%
#                                    filter(Measure == "Brier Score",
#                                           Prevalence == "10%",
#                                           Missingness == "75%"),
#                                  aes(x = AVG, y = Method, colour = Method)) +
#   geom_point(size = 3) +
#   geom_errorbar(aes(xmin = LCI, xmax = UCI), width = 0.8) +
#   geom_point(data = combined_df %>%
#                filter(Parameter == "Outcome prevalence 10% and Missingness 75%"),
#              aes(x = Brier, y = Method),
#              shape = 4, position = position_jitter(width = 0.0001), alpha = 0.5) +
#   labs(
#     y = NULL,
#     x = NULL,
#     colour = "Missing Data Method\n(Mean, 95% CI)",
#     #caption = "Brier score ranges between 0 (perfect accuracy) and 1 (perfect inaccuracy). This x scale runs from 0 to 0.15."
#   ) +
#   theme_minimal() +
#   scale_x_continuous(limits = c(0, 0.15), breaks = seq(0, 0.15, by = 0.05)) +  # Customize x-axis scale
#   scale_colour_manual(
#     values = c(
#       "Validation data, no missingness" = "grey",
#       "Complete Case Analysis" = "blue",
#       "Mean Imputation" = "red",
#       "Multiple Imputation with Outcome" = "green",
#       "Multiple Imputation without Outcome" = "purple"
#     )
#   ) +
#   theme(
#     legend.position = "right",
#     axis.title.x = element_text(size = 14),
#     axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, hjust = 1),
#     axis.text.y=element_blank(),  #remove y axis labels
#     axis.ticks.y=element_blank()  #remove y axis ticks
#   )



### Graph Loop ###
##########################################################
# Create Loop for graph 
##########################################################
plot_fnc <- function(df, measure, combinedmeasure, parameter, x_scale_limits, x_scale_breaks, width) {
  ggplot(df %>% 
           filter(Measure == measure,
                  Parameter == parameter),
         aes(x = AVG, y = Method, colour = Method)) +
    geom_point(size = 3) +
    geom_errorbar(aes(xmin = LCI, xmax = UCI), width = 0.8) +
    geom_point(data = combined_df %>%
                 filter(Parameter ==parameter),
               aes(x = !!sym(combinedmeasure), y = Method),
               shape = 4, position = position_jitter(width = width), alpha = 0.5) +
    labs(
      y = NULL,
      x = NULL,
      colour = "Missing Data Method\n(Mean, 95% CI)"
    ) +
    theme_minimal() +
    scale_x_continuous(limits = x_scale_limits, breaks = x_scale_breaks) +
    scale_colour_manual(
      values = c(
        "Validation data, no missingness" = "grey",
        "Complete Case Analysis" = "blue",
        "Mean Imputation" = "red",
        "Multiple Imputation with Outcome" = "green",
        "Multiple Imputation without Outcome" = "purple"
      )
    ) +
    theme(
      legend.position = "right",
      axis.title.x = element_text(size = 14),
      axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, hjust = 1),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
}
################################################################################
## Set Parameters for each plot type 
################################################################################
## AUC
################
auc_params <- list(
  list(parameter ="Outcome prevalence 1% and Missingness 25%", x_limits = c(0, 1), x_breaks = seq(0, 1, by = 0.25), width=0.001),
  list(parameter ="Outcome prevalence 1% and Missingness 50%", x_limits = c(0, 1), x_breaks = seq(0, 1, by = 0.25), width=0.001),
  list(parameter ="Outcome prevalence 1% and Missingness 75%", x_limits = c(0, 1), x_breaks = seq(0, 1, by = 0.25), width=0.001),
  list(parameter ="Outcome prevalence 5% and Missingness 25%", x_limits = c(0, 1), x_breaks = seq(0, 1, by = 0.25), width=0.001),
  list(parameter ="Outcome prevalence 5% and Missingness 50%", x_limits = c(0, 1), x_breaks = seq(0, 1, by = 0.25), width=0.001),
  list(parameter ="Outcome prevalence 5% and Missingness 75%", x_limits = c(0, 1), x_breaks = seq(0, 1, by = 0.25), width=0.001),
  list(parameter ="Outcome prevalence 10% and Missingness 25%", x_limits = c(0,1), x_breaks = seq(0, 1, by = 0.25), width=0.001),
  list(parameter ="Outcome prevalence 10% and Missingness 50%", x_limits = c(0,1), x_breaks = seq(0, 1, by = 0.25), width=0.001),
  list(parameter ="Outcome prevalence 10% and Missingness 75%", x_limits = c(0,1), x_breaks = seq(0, 1, by = 0.25), width=0.001))


## Generate and save plots
auc_plots <- list()
for (i in seq_along(auc_params)) {
  params <- auc_params[[i]]
  plot <- plot_fnc(
    df = simulation_parameters_long,
    measure = "AUC",
    combinedmeasure = "AUC", 
    parameter = params$parameter,
    x_scale_limits = params$x_limits,
    x_scale_breaks = params$x_breaks,
    width = params$width
  )
  auc_plots[[i]] <- plot
}

## auc 
auc_patchwork <- label_blank + label_25 + label_50 + label_75 + 
  label_1 + label_5 + label_10 +
  auc_plots[1] + auc_plots[4] + auc_plots[7] +
  auc_plots[2] + auc_plots[5] + auc_plots[8] +  
  auc_plots[4] + auc_plots[6] + auc_plots[9] +  
  plot_layout(design=design, guides = "collect", axes = "collect_x",
              widths=c(2,10,10,10), heights=c(1,6,6,6))


###############################################################################
## Bias 
########################################
## Set parameters for the plot for Brier
bias_params <- list(
  list(parameter ="Outcome prevalence 1% and Missingness 25%", x_limits = c(-0.1, 0.11), x_breaks = seq(-0.1, 0.1, by = 0.05), width=0.0001),
  list(parameter ="Outcome prevalence 1% and Missingness 50%", x_limits = c(-0.1, 0.11), x_breaks = seq(-0.1, 0.1, by = 0.05), width=0.0001),
  list(parameter ="Outcome prevalence 1% and Missingness 75%", x_limits = c(-0.1, 0.11), x_breaks = seq(-0.1, 0.1, by = 0.05), width=0.0001),
  list(parameter ="Outcome prevalence 5% and Missingness 25%", x_limits = c(-0.1, 0.11), x_breaks = seq(-0.1, 0.1, by = 0.05), width=0.0001),
  list(parameter ="Outcome prevalence 5% and Missingness 50%", x_limits = c(-0.1, 0.11), x_breaks = seq(-0.1, 0.1, by = 0.05), width=0.0001),
  list(parameter ="Outcome prevalence 5% and Missingness 75%", x_limits = c(-0.1, 0.11), x_breaks = seq(-0.1, 0.1, by = 0.05), width=0.0001),
  list(parameter ="Outcome prevalence 10% and Missingness 25%", x_limits = c(-0.1, 0.11), x_breaks = seq(-0.1, 0.1, by = 0.05), width=0.0001),
  list(parameter ="Outcome prevalence 10% and Missingness 50%", x_limits = c(-0.1, 0.11), x_breaks = seq(-0.1, 0.1, by = 0.05), width=0.0001),
  list(parameter ="Outcome prevalence 10% and Missingness 75%", x_limits = c(-0.1, 0.11), x_breaks = seq(-0.1, 0.1, by = 0.05), width=0.0001)
)

## Generate and save plots
bias_plots <- list()
for (i in seq_along(bias_params)) {
  params <- bias_params[[i]]
  plot <- plot_fnc(
    df = simulation_parameters_long,
    measure = "Bias",
    combinedmeasure = "bias",
    parameter = params$parameter,
    x_scale_limits = params$x_limits,
    x_scale_breaks = params$x_breaks,
    width = params$width
  )
  bias_plots[[i]] <- plot
}

###############################################################################
## calibratio in the large/ calibration Intercept
########################################
## Set parameters for the plot for Brier
citl_params <- list(
  list(parameter ="Outcome prevalence 1% and Missingness 25%", x_limits = c(-2.1, 2), x_breaks = seq(-2, 2, by = 0.5), width=0.05),
  list(parameter ="Outcome prevalence 1% and Missingness 50%", x_limits = c(-2.1, 2), x_breaks = seq(-2, 2, by = 0.5), width=0.05),
  list(parameter ="Outcome prevalence 1% and Missingness 75%", x_limits = c(-2.1, 2), x_breaks = seq(-2, 2, by = 0.5), width=0.05),
  list(parameter ="Outcome prevalence 5% and Missingness 25%", x_limits = c(-2.1, 2), x_breaks = seq(-2, 2, by = 0.5), width=0.05),
  list(parameter ="Outcome prevalence 5% and Missingness 50%", x_limits = c(-2.1, 2), x_breaks = seq(-2, 2, by = 0.5), width=0.05),
  list(parameter ="Outcome prevalence 5% and Missingness 75%", x_limits = c(-2.1, 2), x_breaks = seq(-2, 2, by = 0.5), width=0.05),
  list(parameter ="Outcome prevalence 10% and Missingness 25%", x_limits = c(-2.1, 2), x_breaks = seq(-2, 2, by = 0.5), width=0.05),
  list(parameter ="Outcome prevalence 10% and Missingness 50%", x_limits = c(-2.1, 2), x_breaks = seq(-2, 2, by = 0.5), width=0.05),
  list(parameter ="Outcome prevalence 10% and Missingness 75%", x_limits = c(-2.1, 2), x_breaks = seq(-2, 2, by = 0.5), width=0.05)
)

## Generate and save plots
citl_plots <- list()
for (i in seq_along(citl_params)) {
  params <- citl_params[[i]]
  plot <- plot_fnc(
    df = simulation_parameters_long,
    measure = "Calibration in the Large",
    combinedmeasure = "Cal_Int",
    parameter = params$parameter,
    x_scale_limits = params$x_limits,
    x_scale_breaks = params$x_breaks,
    width = params$width
  )
  citl_plots[[i]] <- plot
}


###############################################################################
## Calibration Slope
########################################
## Set parameters for the plot for Calibration Slope 
calslope_params <- list(
  list(parameter ="Outcome prevalence 1% and Missingness 25%", x_limits = c(-1.5, 5), x_breaks = seq(-1.5, 5, by = 0.5), width=0.0001),
  list(parameter ="Outcome prevalence 1% and Missingness 50%", x_limits = c(-1.5, 5), x_breaks = seq(-1.5, 5, by = 0.5), width=0.0001),
  list(parameter ="Outcome prevalence 1% and Missingness 75%", x_limits = c(-1.5, 5), x_breaks = seq(-1.5, 5, by = 0.5), width=0.0001),
  list(parameter ="Outcome prevalence 5% and Missingness 25%", x_limits = c(-1.5, 5), x_breaks = seq(-1.5, 5, by = 0.5), width=0.0001),
  list(parameter ="Outcome prevalence 5% and Missingness 50%", x_limits = c(-1.5, 5), x_breaks = seq(-1.5, 5, by = 0.5), width=0.0001),
  list(parameter ="Outcome prevalence 5% and Missingness 75%", x_limits = c(-1.5, 5), x_breaks = seq(-1.5, 5, by = 0.5), width=0.0001),
  list(parameter ="Outcome prevalence 10% and Missingness 25%", x_limits = c(-1.5, 5), x_breaks = seq(-1.5, 5, by = 0.5), width=0.0001),
  list(parameter ="Outcome prevalence 10% and Missingness 50%", x_limits = c(-1.5, 5), x_breaks = seq(-1.5, 5, by = 0.5), width=0.0001),
  list(parameter ="Outcome prevalence 10% and Missingness 75%", x_limits = c(-1.5, 5), x_breaks = seq(-1.5, 5, by = 0.5), width=0.0001)
)

## Generate and save plots
calslope_plots <- list()
for (i in seq_along(calslope_params)) {
  params <- calslope_params[[i]]
  plot <- plot_fnc(
    df = simulation_parameters_long,
    measure = "Calibration Slope",
    combinedmeasure = "Cal_Slope",
    parameter = params$parameter,
    x_scale_limits = params$x_limits,
    x_scale_breaks = params$x_breaks,
    width = params$width
  )
  calslope_plots[[i]] <- plot
}

###############################################################################
## Brier 
########################################
## Set parameters for the plot for Brier
plot_params <- list(
  list(parameter ="Outcome prevalence 1% and Missingness 25%", x_limits = c(0, 0.15), x_breaks = seq(0, 0.15, by = 0.05), width=0.0001),
  list(parameter ="Outcome prevalence 1% and Missingness 50%", x_limits = c(0, 0.15), x_breaks = seq(0, 0.15, by = 0.05), width=0.0001),
  list(parameter ="Outcome prevalence 1% and Missingness 75%", x_limits = c(0, 0.15), x_breaks = seq(0, 0.15, by = 0.05), width=0.0001),
  list(parameter ="Outcome prevalence 5% and Missingness 25%", x_limits = c(0, 0.15), x_breaks = seq(0, 0.15, by = 0.05), width=0.0001),
  list(parameter ="Outcome prevalence 5% and Missingness 50%", x_limits = c(0, 0.15), x_breaks = seq(0, 0.15, by = 0.05), width=0.0001),
  list(parameter ="Outcome prevalence 5% and Missingness 75%", x_limits = c(0, 0.15), x_breaks = seq(0, 0.15, by = 0.05), width=0.0001),
  list(parameter ="Outcome prevalence 10% and Missingness 25%", x_limits = c(0, 0.15), x_breaks = seq(0, 0.15, by = 0.05), width=0.0001),
  list(parameter ="Outcome prevalence 10% and Missingness 50%", x_limits = c(0, 0.15), x_breaks = seq(0, 0.15, by = 0.05), width=0.0001),
  list(parameter ="Outcome prevalence 10% and Missingness 75%", x_limits = c(0, 0.15), x_breaks = seq(0, 0.15, by = 0.05), width=0.0001)
)

## Generate and save plots
brier_plots <- list()
for (i in seq_along(plot_params)) {
  params <- plot_params[[i]]
  plot <- plot_fnc(
    df = simulation_parameters_long,
    measure = "Brier Score",
    parameter = params$parameter,
    x_scale_limits = params$x_limits,
    x_scale_breaks = params$x_breaks,
    width = params$width
  )
  brier_plots[[i]] <- plot
}

###############################################################################
## RMSE 
########################################
## Set parameters for the plot for RMSE
rmse_params <- list(
  list(parameter ="Outcome prevalence 1% and Missingness 25%", x_limits = c(0, 0.4), x_breaks = seq(0, 0.40, by = 0.1), width=0.0001),
  list(parameter ="Outcome prevalence 1% and Missingness 50%", x_limits = c(0, 0.4), x_breaks = seq(0, 0.4, by = 0.1), width=0.0001),
  list(parameter ="Outcome prevalence 1% and Missingness 75%", x_limits = c(0, 0.4), x_breaks = seq(0, 0.4, by = 0.1), width=0.0001),
  list(parameter ="Outcome prevalence 5% and Missingness 25%", x_limits = c(0, 0.4), x_breaks = seq(0, 0.4, by = 0.1), width=0.0001),
  list(parameter ="Outcome prevalence 5% and Missingness 50%", x_limits = c(0, 0.4), x_breaks = seq(0, 0.4, by = 0.1), width=0.0001),
  list(parameter ="Outcome prevalence 5% and Missingness 75%", x_limits = c(0, 0.4), x_breaks = seq(0, 0.4, by = 0.1), width=0.0001),
  list(parameter ="Outcome prevalence 10% and Missingness 25%", x_limits = c(0, 0.4), x_breaks = seq(0, 0.4, by = 0.1), width=0.0001),
  list(parameter ="Outcome prevalence 10% and Missingness 50%", x_limits = c(0, 0.4), x_breaks = seq(0, 0.4, by = 0.1), width=0.0001),
  list(parameter ="Outcome prevalence 10% and Missingness 75%", x_limits = c(0, 0.4), x_breaks = seq(0, 0.4, by = 0.1), width=0.0001)
)

## Generate and save plots
rmse_plots <- list()
for (i in seq_along(rmse_params)) {
  params <- rmse_params[[i]]
  plot <- plot_fnc(
    df = simulation_parameters_long,
    measure = "Root Mean Square Error",
    combinedmeasure = "rmse",
    parameter = params$parameter,
    x_scale_limits = params$x_limits,
    x_scale_breaks = params$x_breaks,
    width = params$width
  )
  rmse_plots[[i]] <- plot
}





# Combine
##########################################################
library(patchwork)

label_blank <- ggplot() +
  theme_void() + # Remove axes and grid
  geom_text(aes(x = 0.5, y = 0.5, label = ""), size = 5, hjust = 0.5, vjust = 0.5)

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
  area(1, 1, 1, 1), # Top-left for blank
  area(1, 2, 1, 2), # Top-center for 25%
  area(1, 3, 1, 3), # Top-right for 50%
  area(1, 4, 1, 4), # Top-right for 75%
  area(2, 1, 2, 1), # 1%
  area(3, 1, 3, 1), # 5%
  area(4, 1, 4, 1), # 10%
  area(2, 2, 2, 2), # outcome1_missingness25
  area(3, 2, 3, 2), # outcome5_missingness25
  area(4, 2, 4, 2), # outcome10_missingness25
  area(2, 3, 2, 3), # outcome1_missingness50
  area(3, 3, 3, 3), # outcome5_missingness50
  area(4, 3, 4, 3), # outcome10_missingness50
  area(2, 4, 2, 4), # outcome1_missingness75
  area(3, 4, 3, 4), # outcome5_missingness75
  area(4, 4, 4, 4) # outcome10_missingness75
)


## Plot 
## Bias
bias_patchwork <- label_blank + label_25 + label_50 + label_75 + 
  label_1 + label_5 + label_10 +
  bias_plots[1] + bias_plots[4] + bias_plots[7] +
  bias_plots[2] + bias_plots[5] + bias_plots[8] +  
  bias_plots[3] + bias_plots[6] + bias_plots[9] +  
  plot_layout(design=design, guides = "collect", axes = "collect_x",
              widths=c(2,10,10,10), heights=c(1,6,6,6))


## Calibration in the Large 
citl_patchwork <-  label_blank + label_25 + label_50 + label_75 + 
  label_1 + label_5 + label_10 +
  citl_plots[1] + citl_plots[4] + citl_plots[7] +
  citl_plots[2] + citl_plots[5] + citl_plots[8] +  
  citl_plots[3] + citl_plots[6] + citl_plots[9] +  
  plot_layout(design=design, guides = "collect", axes = "collect_x",
              widths=c(2,10,10,10), heights=c(1,6,6,6))

## Calibration Slope
calslope_patchwork <-  label_blank + label_25 + label_50 + label_75 + 
  label_1 + label_5 + label_10 +
  calslope_plots[1] + calslope_plots[4] + calslope_plots[7] +
  calslope_plots[2] + calslope_plots[5] + calslope_plots[8] +  
  calslope_plots[3] + calslope_plots[6] + calslope_plots[9] +  
  plot_layout(design=design, guides = "collect", axes = "collect_x",
              widths=c(2,10,10,10), heights=c(1,6,6,6))


## Brier
brier_patchwork <- label_blank + label_25 + label_50 + label_75 + 
  label_1 + label_5 + label_10 +
  brier_plots[1] + brier_plots[4] + brier_plots[7] +
  brier_plots[2] + brier_plots[5] + brier_plots[8] +  
  brier_plots[3] + brier_plots[6] + brier_plots[9] +  
  plot_layout(design=design, guides = "collect", axes = "collect_x",
              widths=c(2,10,10,10), heights=c(1,6,6,6))

## RMSE 
rmse_patchwork <- label_blank + label_25 + label_50 + label_75 + 
  label_1 + label_5 + label_10 +
  rmse_plots[1] + rmse_plots[4] + rmse_plots[7] +
  rmse_plots[2] + rmse_plots[5] + rmse_plots[8] +  
  rmse_plots[3] + rmse_plots[6] + rmse_plots[9] +  
  plot_layout(design=design, guides = "collect", axes = "collect_x",
              widths=c(2,10,10,10), heights=c(1,6,6,6))
