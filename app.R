# --- project_root/app.R ---
# Development branch - test change
# Date: [today's date]
# Main entry point for the application

# Load necessary libraries
library(shiny)
library(shinyjs)   # For interactive tutorial and custom JS
library(bslib)
library(bsicons)
library(tidyr)
library(ggplot2)
library(DT)
library(plotly)
library(rintrojs)

# Source all components (global variables, UI layout, server logic)
source("global.R")
source("ui.R")
source("server.R")

# Run the Shiny application
shinyApp(ui = ui, server = server)