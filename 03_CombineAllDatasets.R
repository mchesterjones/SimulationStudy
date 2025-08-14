################################################################################
# libraryies
library(tidyverse)
################################################################################
### Directories 
results_dir <- "C:\\Users\\maecj\\Documents\\Simulation_Data_Study1\\"
development_dir <- "C:\\Users\\maecj\\OneDrive - Nexus365\\A DPhil\\Simulation studies\\Programs\\Study 1 Part 2\\Development Datasets\\"
programs_dir <- "C:\\Users\\maecj\\OneDrive - Nexus365\\A DPhil\\Simulation studies\\Programs\\Study 1 Part 2\\"
################################################################################

datasets <- list()
# Define parameter combinations
mechanisms <- c("MCAR", "MAR", "MNAR")
prev <- c(0.01,0.05, 0.10)
missing <-c("0.25","0.5","0.75")

# Load and store in list
for (mech in mechanisms) {
  for (p in prev) {
    for (miss in missing) {
      fname <- paste0(results_dir, mech,"_Nval_100000_Yprev_", p, "_Rprev_", miss, ".Rdata")
      load(fname)  # loads simulation_results
      key <- paste0(mech, "_", p, "_Rprev", miss)
      datasets[[key]] <- sim_res
    }
  }
}

## Define the number of iterations NOTE MANUAL
num_iterations <- 10
methods <- c("NM_val","CCA_val_data", "mean_val", "MI_val_data_noY", "MI_val_data_withY")



################################################################################
## Extract Target Measures  into one dataset
################################################################################
# Initialize empty list to collect target measures
target_measures_all <- list()

for (dataname in names(datasets)) {
  sim_result <- datasets[[dataname]]
  
  for (i in 1:num_iterations) {
    df <- sim_result[["iterations"]][[i]][["preds"]][["target_measures"]]
    
    df <- cbind(
      iteration = i,
      combination = dataname,
      df  # <- this already includes a "model" column
    )
    
    target_measures_all[[length(target_measures_all) + 1]] <- df
  }
}

# Combine into a single data frame
target_measures_df <- do.call(rbind, target_measures_all)




################################################################################
## Extract bias predictions into one dataset
################################################################################
# Initialize bias summary list
bias_summaries <- list()

for (dataname in names(datasets)) {
  sim_result <- datasets[[dataname]] 
  
  for (i in 1:num_iterations) {
    for (method_id in c("NM_val","CCA_val_data", "mean_val", "MI_val_data_noY", "MI_val_data_withY")) {
      
      true_Y <- sim_result[["iterations"]][[i]][["preds"]][["preds_per_data_set"]][[method_id]][["Y"]]
      estimated_Y <- sim_result[["iterations"]][[i]][["preds"]][["preds_per_data_set"]][[method_id]][["Prediction_Model"]]
      
      n <- length(true_Y)
      bias <- sum(true_Y - estimated_Y) / n
      mse <- sum((true_Y - estimated_Y)^2) / n
      rmse <- sqrt(mse)
      
      new_row <- data.frame(iteration = i, combination = dataname, dataset = method_id, n = n, bias = bias, mse = mse, rmse = rmse)
      
      if (is.null(bias_summaries[[dataname]])) {
        bias_summaries[[dataname]] <- new_row
      } else {
        bias_summaries[[dataname]] <- rbind(bias_summaries[[dataname]], new_row)
      }
    }
  }
}



# Combine all rows into a single data frame
bias_summary_df <- do.call(rbind, bias_summaries)


### JOIN BIAS AND TARGET
combined_df <- full_join(target_measures_df, bias_summary_df, by = c("iteration" = "iteration", 
                                                                     "combination" = "combination",
                                                                     "dataset"="dataset"))
combined_df <- combined_df %>%
  mutate(Method = factor(case_when(
    dataset== "NM_val" ~ "Fully Observed",
    dataset == "CCA_val_data" ~"Complete Case Analysis",
    dataset == "mean_val" ~"Mean Imputation", 
    dataset == "MI_val_data_withY" ~"MI with Y", 
    dataset == "MI_val_data_noY" ~ "MI no Y")),
    Missingness = factor(case_when(
      grepl("0.25", combination) ~ "75%",
      grepl("0.5", combination) ~ "50%",
      grepl("0.75", combination) ~ "25%")), 
    Prevalence = factor(case_when(
      grepl("0.01", combination) ~ "1%",
      grepl("0.05", combination) ~ "5%",
      grepl("0.1", combination) ~ "10%")), 
    samplesize = "N=100,000",
    Mechanism = factor(case_when(
      grepl("MCAR", combination) ~ "MCAR",
      grepl("MAR", combination) ~ "MAR",
      grepl("MNAR", combination) ~ "MNAR")))




################################################################################
## This next part of the code keeps combines the average 
###############################################################################

# Function to summarize the data
summarised_df <-   combined_df %>%
  group_by(dataset, Missingness, samplesize, Mechanism,Prevalence) %>%
  summarise(across(c(Cal_Int, Cal_Slope, AUC, Brier, Brier_scaled, bias, mse, rmse), 
                   list(AVG = ~ mean(.x, na.rm = TRUE),  
                        LCI = ~ quantile(.x, 0.025, na.rm = TRUE), 
                        UCI = ~ quantile(.x, 0.975, na.rm = TRUE), 
                        NACount = ~ sum(is.na(.x)))))

# Convert to long format
summarised_df_long <- summarised_df %>%
  pivot_longer(cols = ends_with(c("AVG", "LCI", "UCI", "NACount")),
               names_to = c("Measure", ".value"), 
               names_pattern = "^(.*)_(AVG|LCI|UCI|NACount)$")


## Create variables as factors
summarised_df_long <- summarised_df_long %>%
  mutate(Method = factor(case_when(
    dataset== "NM_val" ~ "Fully Observed",
    dataset == "CCA_val_data" ~"Complete Case Analysis",
    dataset == "mean_val" ~"Mean Imputation", 
    dataset == "MI_val_data_withY" ~"MI with Y", 
    dataset == "MI_val_data_noY" ~ "MI no Y")))

summarised_df_long <- summarised_df_long %>% 
  mutate(Measure =factor(Measure,
                         levels = c("AUC", "Brier_scaled", "Brier",
                                    "bias", "mse",
                                    "Cal_Int", "Cal_Slope", "rmse")))


# setwd("C:\\Users\\maecj\\OneDrive - Nexus365\\A DPhil\\Simulation studies\\Programs\\Study 1 Part 2\\Plotting Datasets")
# save(summarised_df_long,file= "Summarised_Long.Rdata")
# save(combined_df,file= "Combined_Alliterations.Rdata")


#################################################################################
## Check with Plots 
################################################################################
# individualriskdata <- df_long %>%
#  filter(samplesize == input$samplesize & Mechanism==input$missingness_mech)


## Target Measure Filtering
#######################################################
## AUC discimrination
ggplot(summarised_df_long %>% filter(Measure == "AUC" & Prevalence == "1%" & Mechanism =="MNAR"),
       aes(x = AVG, y = Method, colour = Method)) +
  geom_point(size = 3) +
  geom_errorbar(aes(xmin = LCI, xmax = UCI), width = 0.8) +
  geom_point(data = combined_df %>% filter(Prevalence == "1%" & Mechanism =="MNAR"), aes(x = AUC, y = Method),
             shape = 4, position = position_jitter(width = 0.0001), alpha = 0.5) +  # FIXED
  scale_x_continuous(limits=c(0.6, 1), breaks=seq(-1, 1, by=0.1)) +
  labs(title = "Discrimination",
       subtitle ="A value of 0.5 indicates no discrimination. 1 indicates perfect discrimination",
       x = "Sample Size",
       y = "Proportion of X1 Missing",
       colour = "Method\n(Mean, 95% CI)") +
  theme_minimal() +
  facet_grid(Missingness ~ samplesize,
             scales = "fixed",
             switch = "both") +
  theme(legend.position = "right",
        strip.text = element_text(size = 14),
        strip.placement = "outside",
        strip.background = element_blank(),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 10)),
        axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 10)),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())



























## Brier Scaled 
ggplot(summarised_df_long %>% filter(Measure == "Brier_scaled" & Prevalence == "1%" & Mechanism =="MNAR"),
       aes(x = AVG, y = Method, colour = Method)) +
  geom_point(size = 3) +
  geom_errorbar(aes(xmin = LCI, xmax = UCI), width = 0.8) +
  geom_point(data = combined_df %>% filter(Prevalence == "1%" & Mechanism =="MNAR"), aes(x = Brier_scaled, y = Method),
             shape = 4, position = position_jitter(width = 0.0001), alpha = 0.5) +
  scale_x_continuous(limits=c(-0.05, 0.3), breaks=seq(-1, 1, by=0.05)) +
  # geom_vline(aes(xintercept = 0), linetype = "dashed", colour = "grey30") +
  labs(title = "Brier Score Scaled",
       subtitle = " <0 worse than the null model",
       x = "Sample Size",
       y = "Proportion of X1 Missing",
       colour = "Method\n(Mean, 95% CI)") +
  theme_minimal() +
  facet_grid(Missingness ~ samplesize,
             scales = "fixed",
             switch = "both") +
  theme(legend.position = "right",
        strip.text = element_text(size = 14),
        strip.placement = "outside",
        strip.background = element_blank(),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 10)),
        axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 10)),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())








# Bias
#-------------------------------------------------------------------------------------------------
ggplot(summarised_df_long %>% filter(Measure == "bias"& Prevalence == "1%" & Mechanism =="MNAR"),
       aes(x = AVG, y = Method, colour = Method)) +
  geom_point(size = 3) +
  geom_errorbar(aes(xmin = LCI, xmax = UCI), width = 0.8) +
  geom_point(data = combined_df %>% filter(Prevalence == "1%" & Mechanism =="MNAR"), aes(x = bias, y = Method),
             shape = 4, position = position_jitter(width = 0.0001), alpha = 0.5) +
  labs(title = "Bias of predictions vs true outcome",
       x = "Sample Size",
       y = "Proportion of X1 Missing",
       colour = "Method\n(Mean, 95% CI)") +
  theme_minimal() +
  #   scale_x_continuous(limits=c(-0.05,0.05), breaks=seq(-0.06,0.06, by=0.01))+
  geom_vline(aes(xintercept = 0), linetype = "dashed", colour = "grey30") +
  
  facet_grid(Missingness ~ samplesize,
             scales = "fixed",
             switch = "both") +
  theme(legend.position = "right",
        strip.text = element_text(size = 14),
        strip.placement = "outside",
        strip.background = element_blank(),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 10)),
        axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 10)),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())


#------------------------------------------------------------------------------------------------
ggplot(summarised_df_long %>% filter(Measure == "bias"& Prevalence == "5%" & Mechanism =="MNAR"),
       aes(x = AVG, y = Method, colour = Method)) +
  geom_point(size = 3) +
  geom_errorbar(aes(xmin = LCI, xmax = UCI), width = 0.8) +
  geom_point(data = combined_df %>% filter(Prevalence == "5%" & Mechanism =="MNAR"), aes(x = bias, y = Method),
             shape = 4, position = position_jitter(width = 0.0001), alpha = 0.5) +
  labs(title = "Bias of predictions vs true outcome",
       x = "Sample Size",
       y = "Proportion of X1 Missing",
       colour = "Method\n(Mean, 95% CI)") +
  theme_minimal() +
  #   scale_x_continuous(limits=c(-0.05,0.05), breaks=seq(-0.06,0.06, by=0.01))+
  geom_vline(aes(xintercept = 0), linetype = "dashed", colour = "grey30") +
  
  facet_grid(Missingness ~ samplesize,
             scales = "fixed",
             switch = "both") +
  theme(legend.position = "right",
        strip.text = element_text(size = 14),
        strip.placement = "outside",
        strip.background = element_blank(),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 10)),
        axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 10)),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

## 10% Prev MNAR
#--------------------------------------------------------------------------------
ggplot(summarised_df_long %>% filter(Measure == "bias"& Prevalence == "10%" & Mechanism =="MNAR"),
       aes(x = AVG, y = Method, colour = Method)) +
  geom_point(size = 3) +
  geom_errorbar(aes(xmin = LCI, xmax = UCI), width = 0.8) +
  geom_point(data = combined_df %>% filter(Prevalence == "10%" & Mechanism =="MNAR"), aes(x = bias, y = Method),
             shape = 4, position = position_jitter(width = 0.0001), alpha = 0.5) +
  labs(title = "Bias of predictions vs true outcome",
       x = "Sample Size",
       y = "Proportion of X1 Missing",
       colour = "Method\n(Mean, 95% CI)") +
  theme_minimal() +
  #   scale_x_continuous(limits=c(-0.05,0.05), breaks=seq(-0.06,0.06, by=0.01))+
  geom_vline(aes(xintercept = 0), linetype = "dashed", colour = "grey30") +
  
  facet_grid(Missingness ~ samplesize,
             scales = "fixed",
             switch = "both") +
  theme(legend.position = "right",
        strip.text = element_text(size = 14),
        strip.placement = "outside",
        strip.background = element_blank(),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 10)),
        axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 10)),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())



## BIAS MCAR
#----------------------------------------------------------------------------------------
ggplot(summarised_df_long %>% filter(Measure == "bias"& Prevalence == "1%" & Mechanism =="MCAR"),
       aes(x = AVG, y = Method, colour = Method)) +
  geom_point(size = 3) +
  geom_errorbar(aes(xmin = LCI, xmax = UCI), width = 0.8) +
  geom_point(data = combined_df %>% filter(Prevalence == "1%" & Mechanism =="MCAR"), aes(x = bias, y = Method),
             shape = 4, position = position_jitter(width = 0.0001), alpha = 0.5) +
  labs(title = "Bias of predictions vs true outcome",
       x = "Sample Size",
       y = "Proportion of X1 Missing",
       colour = "Method\n(Mean, 95% CI)") +
  theme_minimal() +
  #   scale_x_continuous(limits=c(-0.05,0.05), breaks=seq(-0.06,0.06, by=0.01))+
  geom_vline(aes(xintercept = 0), linetype = "dashed", colour = "grey30") +
  
  facet_grid(Missingness ~ samplesize,
             scales = "fixed",
             switch = "both") +
  theme(legend.position = "right",
        strip.text = element_text(size = 14),
        strip.placement = "outside",
        strip.background = element_blank(),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 10)),
        axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 10)),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

## 5% Prev MCAR
#------------------------------------------------------------------------------------------------
ggplot(summarised_df_long %>% filter(Measure == "bias"& Prevalence == "5%" & Mechanism =="MCAR"),
       aes(x = AVG, y = Method, colour = Method)) +
  geom_point(size = 3) +
  geom_errorbar(aes(xmin = LCI, xmax = UCI), width = 0.8) +
  geom_point(data = combined_df %>% filter(Prevalence == "5%" & Mechanism =="MCAR"), aes(x = bias, y = Method),
             shape = 4, position = position_jitter(width = 0.0001), alpha = 0.5) +
  labs(title = "Bias of predictions vs true outcome",
       x = "Sample Size",
       y = "Proportion of X1 Missing",
       colour = "Method\n(Mean, 95% CI)") +
  theme_minimal() +
  #   scale_x_continuous(limits=c(-0.05,0.05), breaks=seq(-0.06,0.06, by=0.01))+
  geom_vline(aes(xintercept = 0), linetype = "dashed", colour = "grey30") +
  
  facet_grid(Missingness ~ samplesize,
             scales = "fixed",
             switch = "both") +
  theme(legend.position = "right",
        strip.text = element_text(size = 14),
        strip.placement = "outside",
        strip.background = element_blank(),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 10)),
        axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 10)),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

## 10% Prev MCAR
#--------------------------------------------------------------------------------
ggplot(summarised_df_long %>% filter(Measure == "bias"& Prevalence == "10%" & Mechanism =="MCAR"),
       aes(x = AVG, y = Method, colour = Method)) +
  geom_point(size = 3) +
  geom_errorbar(aes(xmin = LCI, xmax = UCI), width = 0.8) +
  geom_point(data = combined_df %>% filter(Prevalence == "10%" & Mechanism =="MCAR"), aes(x = bias, y = Method),
             shape = 4, position = position_jitter(width = 0.0001), alpha = 0.5) +
  labs(title = "Bias of predictions vs true outcome",
       x = "Sample Size",
       y = "Proportion of X1 Missing",
       colour = "Method\n(Mean, 95% CI)") +
  theme_minimal() +
  #   scale_x_continuous(limits=c(-0.05,0.05), breaks=seq(-0.06,0.06, by=0.01))+
  geom_vline(aes(xintercept = 0), linetype = "dashed", colour = "grey30") +
  
  facet_grid(Missingness ~ samplesize,
             scales = "fixed",
             switch = "both") +
  theme(legend.position = "right",
        strip.text = element_text(size = 14),
        strip.placement = "outside",
        strip.background = element_blank(),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 10)),
        axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 10)),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())



## BIAS MAR
#----------------------------------------------------------------------------------------
ggplot(summarised_df_long %>% filter(Measure == "bias"& Prevalence == "1%" & Mechanism =="MAR"),
       aes(x = AVG, y = Method, colour = Method)) +
  geom_point(size = 3) +
  geom_errorbar(aes(xmin = LCI, xmax = UCI), width = 0.8) +
  geom_point(data = combined_df %>% filter(Prevalence == "1%" & Mechanism =="MAR"), aes(x = bias, y = Method),
             shape = 4, position = position_jitter(width = 0.0001), alpha = 0.5) +
  labs(title = "Bias of predictions vs true outcome",
       x = "Sample Size",
       y = "Proportion of X1 Missing",
       colour = "Method\n(Mean, 95% CI)") +
  theme_minimal() +
  #   scale_x_continuous(limits=c(-0.05,0.05), breaks=seq(-0.06,0.06, by=0.01))+
  geom_vline(aes(xintercept = 0), linetype = "dashed", colour = "grey30") +
  
  facet_grid(Missingness ~ samplesize,
             scales = "fixed",
             switch = "both") +
  theme(legend.position = "right",
        strip.text = element_text(size = 14),
        strip.placement = "outside",
        strip.background = element_blank(),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 10)),
        axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 10)),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

## 5% Prev MAR
#------------------------------------------------------------------------------------------------
ggplot(summarised_df_long %>% filter(Measure == "bias"& Prevalence == "5%" & Mechanism =="MAR"),
       aes(x = AVG, y = Method, colour = Method)) +
  geom_point(size = 3) +
  geom_errorbar(aes(xmin = LCI, xmax = UCI), width = 0.8) +
  geom_point(data = combined_df %>% filter(Prevalence == "5%" & Mechanism =="MAR"), aes(x = bias, y = Method),
             shape = 4, position = position_jitter(width = 0.0001), alpha = 0.5) +
  labs(title = "Bias of predictions vs true outcome",
       x = "Sample Size",
       y = "Proportion of X1 Missing",
       colour = "Method\n(Mean, 95% CI)") +
  theme_minimal() +
  #   scale_x_continuous(limits=c(-0.05,0.05), breaks=seq(-0.06,0.06, by=0.01))+
  geom_vline(aes(xintercept = 0), linetype = "dashed", colour = "grey30") +
  
  facet_grid(Missingness ~ samplesize,
             scales = "fixed",
             switch = "both") +
  theme(legend.position = "right",
        strip.text = element_text(size = 14),
        strip.placement = "outside",
        strip.background = element_blank(),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 10)),
        axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 10)),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

## 10% Prev MAR
#--------------------------------------------------------------------------------
ggplot(summarised_df_long %>% filter(Measure == "bias"& Prevalence == "10%" & Mechanism =="MAR"),
       aes(x = AVG, y = Method, colour = Method)) +
  geom_point(size = 3) +
  geom_errorbar(aes(xmin = LCI, xmax = UCI), width = 0.8) +
  geom_point(data = combined_df %>% filter(Prevalence == "10%" & Mechanism =="MAR"), aes(x = bias, y = Method),
             shape = 4, position = position_jitter(width = 0.0001), alpha = 0.5) +
  labs(title = "Bias of predictions vs true outcome",
       x = "Sample Size",
       y = "Proportion of X1 Missing",
       colour = "Method\n(Mean, 95% CI)") +
  theme_minimal() +
  #   scale_x_continuous(limits=c(-0.05,0.05), breaks=seq(-0.06,0.06, by=0.01))+
  geom_vline(aes(xintercept = 0), linetype = "dashed", colour = "grey30") +
  
  facet_grid(Missingness ~ samplesize,
             scales = "fixed",
             switch = "both") +
  theme(legend.position = "right",
        strip.text = element_text(size = 14),
        strip.placement = "outside",
        strip.background = element_blank(),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 10)),
        axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 10)),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())














## Calibration in the large (intercept)
ggplot(summarised_df_long %>% filter(Measure == "Cal_Int"),
       aes(x = AVG, y = Method, colour = Method)) +
  geom_point(size = 3) +
  geom_errorbar(aes(xmin = LCI, xmax = UCI), width = 0.8) +
  geom_point(data = combined_df, aes(x = Cal_Int, y = Method),
             shape = 4, position = position_jitter(width = 0.0001), alpha = 0.5) +
  scale_x_continuous(limits=c(-0.75, 0.5), breaks=seq(-1, 1, by=0.25)) +
  geom_vline(aes(xintercept = 0), linetype = "dashed", colour = "grey30") +
  labs(title = "Calibration in the Large",
       x = "Sample Size",
       y = "Proportion of X1 Missing",
       colour = "Method\n(Mean, 95% CI)") +
  theme_minimal() +
  facet_grid(Missingness ~ samplesize,
             scales = "fixed",
             switch = "both") +
  theme(legend.position = "right",
        strip.text = element_text(size = 14),
        strip.placement = "outside",
        strip.background = element_blank(),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 10)),
        axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 10)),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

## Calibration slope 
#-----------------------------------------------------------
ggplot(summarised_df_long %>% filter(Measure == "Cal_Slope"),
       aes(x = AVG, y = Method, colour = Method)) +
  geom_point(size = 3) +
  geom_errorbar(aes(xmin = LCI, xmax = UCI), width = 0.8) +
  geom_point(data = combined_df, aes(x = Cal_Slope, y = Method),
             shape = 4, position = position_jitter(width = 0.0001), alpha = 0.5) +
  scale_x_continuous(limits=c(0.50, 1.5), breaks=seq(0.50, 2, by=0.25)) +
  geom_vline(aes(xintercept = 1), linetype = "dashed", colour = "grey30") +
  labs(title = "Calibration Slope",
       subtitle = "1 = perfect calibration;  < 1 suggest overfitting, > 1 suggest underfitting.",
       x = "Sample Size",
       y = "Proportion of X1 Missing",
       colour = "Method\n(Mean, 95% CI)") +
  geom_vline(aes(xintercept = 1), linetype = "dashed", colour = "grey30") +
  theme_minimal() +
  facet_grid(Missingness ~ samplesize,
             scales = "fixed",
             switch = "both") +
  theme(legend.position = "right",
        strip.text = element_text(size = 14),
        strip.placement = "outside",
        strip.background = element_blank(),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 10)),
        axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 10)),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())


# RMSE
#-----------------------------------------------------------------------------
ggplot(summarised_df_long %>% filter(Measure == "rmse"),
       aes(x = AVG, y = Method, colour = Method)) +
  geom_point(size = 3) +
  geom_errorbar(aes(xmin = LCI, xmax = UCI), width = 0.8) +
  geom_point(data = combined_df, aes(x = rmse, y = Method),
             shape = 4, position = position_jitter(width = 0.0001), alpha = 0.5) +
  #   scale_x_continuous(limits=c(0.2, 0.325), breaks=seq(0, 2, by=0.025)) +
  
  labs(title = "Root Mean Square Error",
       x = "Sample Size",
       y = "Proportion of Rows Missing",
       colour = "Method\n(Mean, 95% CI)") +
  theme_minimal() +
  facet_grid(Missingness ~ samplesize,
             scales = "fixed",
             switch = "both") +
  theme(legend.position = "right",
        strip.text = element_text(size = 14),
        strip.placement = "outside",
        strip.background = element_blank(),
        axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 10)),
        axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 10)),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())





