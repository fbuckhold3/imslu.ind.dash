library(shiny)

source("R/global.R")

# Load all module files
for (f in list.files("R/modules", full.names = TRUE, pattern = "\\.R$")) source(f)

source("R/ui.R")
source("R/server.R")

shinyApp(ui = ui, server = server)
