# ============================================================================
# ULTRA-SIMPLE GLOBAL.R - Using gmed::load_rdm_complete()
# ============================================================================

library(shiny)
library(shinyjs)
library(bslib)
library(DT)
library(dplyr)
library(plotly)      # Required for gmed assessment modules
library(ggplot2)     # Required for gmed assessment modules
library(purrr)       # Required for gmed assessment modules
library(tidyr)       # Required for gmed assessment modules
library(lubridate)   # Required for gmed assessment modules


library(gmed)

# Fix for Posit Connect graphics rendering
options(shiny.plot.res = 96)
options(repr.plot.width = 8, repr.plot.height = 6)

# Ensure graphics device settings work in server environment
if (!interactive()) {
  options(bitmapType = "cairo")
}

# ============================================================================
# CONFIGURATION - Secure for GitHub and Posit Connect
# ============================================================================

app_config <- list(
  rdm_token = Sys.getenv("RDM_TOKEN"),
  redcap_url = "https://redcapsurvey.slu.edu/api/"
)

# ============================================================================
# DATA LOADING - ULTRA-SIMPLE with load_rdm_complete
# ============================================================================

app_data_store <- NULL

load_app_data <- function() {
  if (is.null(app_data_store)) {
    message("Loading RDM 2.0 data using load_rdm_complete...")
    
    # Use your new clean function!
    complete_data <- load_rdm_complete(
      rdm_token = app_config$rdm_token, 
      verbose = TRUE
    )
    
    # DEBUG: Check what we actually got
    message("=== DEBUGGING DATA STRUCTURE ===")
    if (!is.null(complete_data$residents)) {
      message("Residents data columns: ", paste(names(complete_data$residents), collapse = ", "))
      if ("Level" %in% names(complete_data$residents)) {
        level_counts <- table(complete_data$residents$Level, useNA = "always")
        message("Resident Level distribution: ", paste(names(level_counts), "=", level_counts, collapse = ", "))
      }
    }
    
    if (!is.null(complete_data$all_forms$assessment)) {
      message("Assessment data columns: ", paste(names(complete_data$all_forms$assessment)[1:10], collapse = ", "), "...")
      if ("ass_level" %in% names(complete_data$all_forms$assessment)) {
        ass_level_counts <- table(complete_data$all_forms$assessment$ass_level, useNA = "always")
        message("Assessment ass_level distribution: ", paste(names(ass_level_counts), "=", ass_level_counts, collapse = ", "))
      }
    }
    message("=== END DEBUG ===")
    
    app_data_store <<- complete_data
   
    message("Data loaded successfully!")
    message("Residents: ", nrow(complete_data$residents))
    message("Assessment records: ", nrow(complete_data$all_forms$assessment))
    message("Data dictionary fields: ", nrow(complete_data$data_dict))
  }
  
  
  
  return(app_data_store)
}

# ============================================================================
# HELPER FUNCTIONS - Simplified
# ============================================================================

validate_access_code <- function(code) {
  if (is.null(code) || code == "") return(FALSE)
  
  data <- load_app_data()
  if (is.null(data$residents) || !"access_code" %in% names(data$residents)) return(FALSE)
  
  return(code %in% data$residents$access_code)
}

get_resident_by_code <- function(code) {
  data <- load_app_data()
  
  resident <- data$residents %>%
    filter(access_code == !!code) %>%
    slice(1)
  
  if (nrow(resident) == 0) return(NULL)
  as.list(resident)
}



# ============================================================================
# STARTUP
# ============================================================================

message("=== INDIVIDUAL DASHBOARD STARTUP ===")
message("GMED package loaded: ", "gmed" %in% loadedNamespaces())
message("load_rdm_complete available: ", exists("load_rdm_complete", where = "package:gmed"))
message("Ready to load data with clean pattern!")
