library(shiny)
library(redcapAPI)
library(ggplot2)
library(DT)
library(dplyr)
library(config)
library(imres)
library(bslib)
library(httr)

# Load environment variables and configuration
is_connect <- Sys.getenv("R_CONFIG_ACTIVE") == "posit_connect" || 
  !identical(Sys.getenv("CONNECT_APP"), "") || 
  !identical(Sys.getenv("CONNECT_SERVER"), "")

# Set API tokens based on environment
if (is_connect) {
  eval_token <- Sys.getenv("EVAL_TOKEN")
  rdm_token <- Sys.getenv("RDM_TOKEN")
  fac_token <- Sys.getenv("FAC_TOKEN")
  new_ass_token <- Sys.getenv("NEW_ASS_TOKEN")
} else {
  conf <- config::get(file = "config.yml")
  eval_token <- conf$eval_token
  rdm_token <- conf$rdm_token
  fac_token <- conf$fac_token
  new_ass_token <- conf$new_ass_token
}

url <- "https://redcapsurvey.slu.edu/api/"

# Function to safely pull resident data from REDCap API
get_resident_data <- function() {
  tryCatch({
    ass_dat <- full_api_pull(eval_token, url)
    ass_dat <- wrangle_assessment_data(ass_dat)
    rdm_dat <- forms_api_pull(rdm_token, url, 'resident_data', 'faculty_evaluation')
    return(create_res_data(ass_dat, rdm_dat))
  }, error = function(e) {
    cat("Error in API pull:", e$message, "\n")
    return(NULL)
  })
}

# Function to pull and process milestone data
get_milestone_data <- function() {
  tryCatch({
    miles <- get_all_milestones(rdm_token, url)
    miles <- fill_missing_resident_data(miles)
    p_miles <- process_milestones(miles, type = "program")
    s_miles <- process_milestones(miles, type = "self")
    return(list(p_miles = p_miles, s_miles = s_miles))
  }, error = function(e) {
    cat("Error in milestone API pull:", e$message, "\n")
    return(NULL)
  })
}

# Load resident and milestone data
resident_data <- get_resident_data()
milestone_data <- get_milestone_data()

p_miles <- milestone_data$p_miles
s_miles <- milestone_data$s_miles