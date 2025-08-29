# Load required libraries
library(shiny)

source("R/global.R")
source("R/ui.R")
source("R/server.R")

# Run the application
shinyApp(ui = ui, server = server)

