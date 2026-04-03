# Global configurations for Shiny app

# Load necessary libraries
library(shiny)

# Increase maximum upload size to 30 MB (default is 5 MB)
options(shiny.maxRequestSize = 30 * 1024^2)

# Global variables or configurations can be defined here

