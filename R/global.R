# ============================================================================
# ULTRA-SIMPLE GLOBAL.R - Your working pattern, no package dependencies
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

# Install gmed from GitHub for Posit Connect deployment
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

remotes::install_github("fbuckhold3/gmed")
library(gmed)

# ============================================================================
# CONFIGURATION - Secure for GitHub and Posit Connect
# ============================================================================

# Configuration that works in both environments without exposing tokens
app_config <- list(
  rdm_token = Sys.getenv("RDM_TOKEN", ""),  # Posit Connect sets this
  redcap_url = "https://redcapsurvey.slu.edu/api/"
)

# Local development: use config.yml (gitignored)
if (app_config$rdm_token == "" && file.exists("config.yml")) {
  tryCatch({
    config_data <- config::get()
    app_config$rdm_token <- config_data$rdm_token
    message("Using token from config.yml (local development)")
  }, error = function(e) {
    message("Config file error: ", e$message)
  })
}

# Validate token exists
if (app_config$rdm_token == "") {
  stop("RDM_TOKEN not configured. Set environment variable (Posit Connect) or config.yml (local)")
}

message("Token configured securely")

# ============================================================================
# DATA LOADING - Your exact working pattern
# ============================================================================

app_data_store <- NULL

load_app_data <- function() {
  if (is.null(app_data_store)) {
    message("Loading RDM 2.0 data using proven pattern...")
    
    # Use the token from app_config
    data <- load_data_by_forms(rdm_token = app_config$rdm_token)
    message("Raw data loaded")
    
    # Calculate milestone medians from historical data BEFORE filtering
    historical_milestone_medians <- calculate_all_milestone_medians(data)
    message("Historical milestone medians calculated")
    
    # Filter archived residents for active data
    clean_data <- filter_archived_residents(data, verbose = TRUE)
    
    # Add levels to active residents
    data_with_levels <- add_level_at_time_to_forms(clean_data)  
    message("Added levels to active residents")
    
    # Prepare milestone app data from active residents
    mile_data <- prepare_milestone_app_data(data_with_levels)
    message("Prepared milestone data from active residents")
    
    # Extract residents and forms
    residents <- data_with_levels$forms$resident_data
    all_forms <- data_with_levels$forms
    
    if (is.null(residents)) {
      residents <- all_forms$residents %||% 
        all_forms$demographic_data %||%
        all_forms[[1]]
      message("Used alternative form for resident data")
    }
    
    # Ensure required columns exist (simplified)
    if (!is.null(residents)) {
      # Add name if missing
      if (!"name" %in% names(residents)) {
        name_alternatives <- c("Name", "resident_name", "full_name", "first_name")
        found_name_col <- intersect(name_alternatives, names(residents))[1]
        
        if (!is.na(found_name_col)) {
          residents$name <- residents[[found_name_col]]
        } else {
          residents$name <- paste("Resident", residents$record_id)
        }
      }
      
      # Add Level if missing  
      if (!"Level" %in% names(residents)) {
        level_alternatives <- c("level", "resident_level", "training_level", "pgy", "PGY", "year")
        found_level_col <- intersect(level_alternatives, names(residents))[1]
        
        if (!is.na(found_level_col)) {
          residents$Level <- residents[[found_level_col]]
        } else {
          residents$Level <- "Unknown"
        }
      }
    }
    
    # Get assessment data
    assessment_data <- all_forms$assessment %||% data.frame()
    
    # CRITICAL FIX: Ensure gmed module column names exist
    if (nrow(assessment_data) > 0) {
      # Check if ass_level column exists, if not try to create it
      if (!"ass_level" %in% names(assessment_data)) {
        # Try different possible level column names
        level_alternatives <- c("level_at_time", "Level", "level", "resident_level")
        found_level_col <- intersect(level_alternatives, names(assessment_data))[1]
        
        if (!is.na(found_level_col)) {
          assessment_data$ass_level <- assessment_data[[found_level_col]]
          message("Created ass_level from ", found_level_col)
        } else {
          # Fallback: create ass_level as "Unknown"
          assessment_data$ass_level <- "Unknown"
          message("Created ass_level as 'Unknown' - no level column found")
        }
      }
      
      # Ensure other required gmed columns exist
      if (!"ass_plus" %in% names(assessment_data)) {
        plus_cols <- grep("plus|strength|positive", names(assessment_data), ignore.case = TRUE, value = TRUE)
        if (length(plus_cols) > 0) {
          assessment_data$ass_plus <- assessment_data[[plus_cols[1]]]
          message("Created ass_plus from ", plus_cols[1])
        } else {
          assessment_data$ass_plus <- as.character(NA)
        }
      }
      
      if (!"ass_delta" %in% names(assessment_data)) {
        delta_cols <- grep("delta|improve|concern", names(assessment_data), ignore.case = TRUE, value = TRUE)
        if (length(delta_cols) > 0) {
          assessment_data$ass_delta <- assessment_data[[delta_cols[1]]]
          message("Created ass_delta from ", delta_cols[1])
        } else {
          assessment_data$ass_delta <- as.character(NA)
        }
      }
      
      message("Assessment data now has required columns for gmed modules")
    }
    
    # DEBUG: Check what's actually in the assessment data
    message("=== DEBUG ASSESSMENT DATA ===")
    message("Assessment data rows: ", nrow(assessment_data))
    if (nrow(assessment_data) > 0) {
      message("Assessment columns: ", paste(head(names(assessment_data), 20), collapse = ", "))
      
      # Check for level-related columns
      level_cols <- grep("level|ass_level", names(assessment_data), ignore.case = TRUE, value = TRUE)
      message("Level-related columns: ", paste(level_cols, collapse = ", "))
      
      # Check for resident 99 specifically
      resident_99_data <- assessment_data %>% filter(record_id == "99")
      message("Resident 99 assessment records: ", nrow(resident_99_data))
      if (nrow(resident_99_data) > 0) {
        message("Resident 99 columns: ", paste(names(resident_99_data), collapse = ", "))
      }
    }
    message("=== END DEBUG ===")
    
    app_data_store <<- list(
      residents = residents,
      all_forms = all_forms,
      assessment_data = assessment_data,
      milestone_data = mile_data,
      historical_medians = historical_milestone_medians,
      data_loaded = TRUE
    )
    
    message("Data loaded successfully!")
    message("Residents: ", nrow(residents))
    message("Assessment records: ", nrow(assessment_data))
  }
  
  return(app_data_store)
}

# ============================================================================
# HELPER FUNCTIONS
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

# NULL coalescing operator
if (!exists("%||%")) {
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
}

# ============================================================================
# STARTUP
# ============================================================================

message("=== INDIVIDUAL DASHBOARD STARTUP ===")
message("GMED package loaded: ", "gmed" %in% loadedNamespaces())
message("Ready to test with proven data loading pattern")