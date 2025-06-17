library(shiny) # Calls library Shiny 
library(ggplot2)  # Calls ggplot
library(tidyverse)
library(dplyr)
#setwd("C:\\Users\\maecj\\OneDrive - Nexus365\\A DPhil\\Simulation studies\\Programs\\Study 1\\SimulationStudy1_11Jun2024\\SimulationStudy\\Data")
setwd("C:\\Users\\maecj\\OneDrive - Nexus365\\A DPhil\\Simulation studies\\Programs\\Study 1\\SimulationStudy1_11Jun2024\\SimulationStudy\\App")

## Load required datasets
########################

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



## 1. Define User Interface UI i.e. the html which people can interact with
ui <- fluidPage( # fluidpage() is a layout function
  ## selectInput() is an input control that allows user to interact with the 
  ## app by providing a value
  # Select missingness mechanism
  selectInput("missingness_mech", "Missingness Mechanism",
              choices = c("MCAR","MAR", "MNAR")),
  
  # Inputs
  selectInput("samplesize", 
              label = "Sample Size", 
              choices = c("N=500", "N=10,000", "N=100,000")),
  
  # Select Measure
  selectInput("measure", "Measure",
              choices = c("AUC", 
                          "Brier Score Scaled", 
                          "Bias", 
                          "Calibration in the Large",
                          "Calibration Slope", 
                          "Root Mean Square Error")),
  
  
  ## Output controls 
  ## This tells Shiny where to put the output
  verbatimTextOutput("summary"),
  plotOutput("plot"))


# ## 2. Specify behaviour of the app by defining a server
## This is a type of reactive programming (recipe vs sandwich)
server <- function(input,output, session) {
  output$plot <- renderPlot({
    
    #  Select the appropriate dataset based on input parameters
    if (input$missingness_mech == "MCAR") {
      simulation_parameters_long <- MCAR_Long
      combined_df <- MCAR_Combined_data
    } else if (input$missingness_mech == "MAR") {
      simulation_parameters_long <- MAR_Long
      combined_df <- MAR_Combined_data
      
      } else if (input$missingness_mech == "MNAR") {
        simulation_parameters_long <- MNAR_Long
        combined_df <- MNAR_Combined_data

    }
    
    
    if (input$measure == "AUC") {
      measure_col <- "AUC"
      x_limits <- c(0, 1)
      x_breaks <- seq(0, 1, by = 0.25)
      jitter_width <- 0.01
    } else if (input$measure == "Brier Score Scaled") {
      measure_col <- "Brier_scaled"
      x_limits <- c(-0.6, 1)
      x_breaks <- seq(-0.6, 1, by = 0.1)
      jitter_width <- 0.01
    } else if (input$measure == "Bias") {
      measure_col <- "bias"
      x_limits <- c(-0.1, 0.11)
      x_breaks <- seq(-0.1, 0.1, by = 0.05)
      jitter_width <- 0.0001
    } else if (input$measure == "Calibration in the Large") { 
      measure_col <- "Cal_Int"
      x_limits <- c(-2.1, 2)
      x_breaks <- seq(-2, 2, by = 0.5)
      jitter_width=0.05
      
    } else if (input$measure == "Calibration Slope") { 
      measure_col <- "Cal_Slope"
      x_limits <-  c(-1.5, 5)
      x_breaks <- seq(-1.5, 5, by = 0.5)
      jitter_width=0.0001
      
    } else if (input$measure == "Root Mean Square Error") { 
      measure_col <- "rmse"
      x_limits <- c(0, 0.4)
      x_breaks <-  seq(0, 0.40, by = 0.1)
      jitter_width=0.0001
    }            
    
    ggplot(simulation_parameters_long %>%
             filter(Metric == measure_col & samplesize == input$samplesize),
           aes(x = AVG, y = Method, colour = Method)) +
      geom_point(size = 3) +
      geom_errorbar(aes(xmin = LCI, xmax = UCI), width = 0.8) +
      geom_point(data = combined_df %>% filter(samplesize == input$samplesize),
                 aes_string(x = measure_col, y = "Method"),
                 shape = 4,
                 position = position_jitter(width = jitter_width),
                 alpha = 0.5) +
      labs(y = NULL,
           x = NULL,
           colour = "Missing Data Method\n(Mean, 95% CI)") +
      theme_minimal() +
      facet_grid(Prevalence ~ Missingness, scales = "fixed", switch = "both") +
      scale_x_continuous(limits = x_limits, breaks = x_breaks) +
      ## Add Colour Scale
      scale_colour_manual(values = c("Validation data, no missingness" = "grey",
                                     "Complete Case Analysis" = "blue",
                                     "Mean Imputation" = "red",
                                     "Multiple Imputation with Outcome" = "green",
                                     "Multiple Imputation without Outcome" = "purple")) +
      theme(legend.position = "right",
            strip.text = element_text(size = 14),  # Customize strip text size
            strip.placement = "outside",  # Place strip labels outside the plot area
            strip.background = element_blank(),  # Remove strip background
            axis.title.x = element_text(size = 12),
            axis.title.y = element_text(size = 14),
            axis.text.x =  element_text(size = 14, angle = 90, hjust = 1),
            axis.text.y  = element_blank(),  # Remove y-axis text
            axis.ticks.y = element_blank())  # Remove y-axis tick
    
  })
  
  
  
  
}

# Executes and contructs a Shiny application from UI and server
shinyApp(ui,server)


# library(rsconnect)

rsconnect::deployApp(appName = "App",
                     appDir = "C:/Users/maecj/OneDrive - Nexus365/A DPhil/Simulation studies/Programs/Study 1/SimulationStudy1_11Jun2024/SimulationStudy/App/")
rsconnect::deployApp('/App')