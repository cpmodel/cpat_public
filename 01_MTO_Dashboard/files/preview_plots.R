

# Defining the preview plot function

preview_plot <- function (df, plot_class, plot_type) {
  
  years <- seq(2020, 2031)
  year <- 2023
  countries <- c("Germany", "Brazil")
  
  if (plot_class == "Bar" & plot_type != "Cost Burden Distribution") {
    get_barplot(
      df,
      year,
      countries,
      plot_type
    )$p
  } 
  else if (plot_type == "Cost Burden Distribution") {
    get_costdistnplot(
      df_cost_distn,
      countries
    )$p
  } 
  else {
    get_multiyear_plot(
      df,
      years,
      countries,
      plot_class,
      plot_type
    )$p
  }
  
}


# Including required libraries
library(tidyverse)
library(ggplot2)

# Get the file path of the current R file
filePath <- rstudioapi::getSourceEditorContext()$path

# Set the working directory to the directory containing the R file
setwd(dirname(filePath))


# Loading in the data
# Reading in the MT Outputs data
load("mt_dashboard.rda")

# Including the file with plot functions. If you want to edit hwo the plots look, please make changes in the file below
source("plot_functions.R")


# The function takes in three arguments - df, plot_class and plot_type
# If plot type is Cost Distribution, please pass df_costdistn to the function, otherwise just pass df

######
# Choose plot_class from: Bar, Area, Line

######
# Choose plot_type from:
# "Welfare Benefits",
# "Change in GHG Emissions",
# "Revenue Gains",
# "Price Increase",
# "Cost Burden Distribution",
# "Averted Deaths (Net)",
# "Averted Deaths % baseline",
# "Averted Deaths (Age Group)",
# "Road Fatalities"

######
# Default Start Year = 2023
# Default End Year = 2031
# Year for barplots = 2023
# Default Countries - Germany, Brazil




### Run Examples
### Please make sure you run all the lines above before trying the previews

preview_plot(df, "Line", "Revenue Gains")

preview_plot(df, "Area", "Healthcare Savings")

preview_plot(df_costdistn, "Bar", "Cost Burden Distribution")