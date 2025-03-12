library(shiny)
library(redcapAPI)
library(ggplot2)
library(DT)
library(dplyr)
library(config)
library(imres)
library(bslib)
library(httr)
library(gganimate)
library(stringr)
library(xml2)
library(fontawesome)
library(tidyr)
library(reactable)
library(htmltools)
library(data.table)
library(purrr)


# 1) Identify whether we are hosted
is_hosted <- Sys.getenv("EVAL_TOKEN") != ""

# 2) Load tokens from environment or config
if (is_hosted) {
  eval_token <- Sys.getenv("EVAL_TOKEN")
  rdm_token  <- Sys.getenv("RDM_TOKEN")
  fac_token  <- Sys.getenv("FAC_TOKEN")
  new_ass_token <- Sys.getenv("NEW_ASS_TOKEN")
  
  # Disable SSL verification in the hosted environment (NOT recommended for production)
  httr::set_config(httr::config(ssl_verifypeer = FALSE))
  
} else {
  conf <- config::get(file = "config.yml")
  eval_token <- conf$eval_token
  rdm_token  <- conf$rdm_token
  fac_token  <- conf$fac_token
  new_ass_token <- conf$new_ass_token
}

# Debug prints
cat("=== Checking the tokens exist in Sys.getenv() ===\n")
cat("EVAL_TOKEN is set? ", "EVAL_TOKEN" %in% names(Sys.getenv()), "\n")
cat("RDM_TOKEN is set?  ", "RDM_TOKEN"  %in% names(Sys.getenv()), "\n")
cat("FAC_TOKEN is set?  ", "FAC_TOKEN"  %in% names(Sys.getenv()), "\n")

# The RedCap URL
url <- "https://redcapsurvey.slu.edu/api/"

# Possibly a test call:
resp <- httr::POST(url)
cat("Initial POST() status:", httr::status_code(resp), "\n")

res <- httr::POST(
  url = "https://redcapsurvey.slu.edu/api/",
  body = list(
    token   = rdm_token,
    content = "record",
    format  = "json",
    forms   = c("resident_data","faculty_evaluation")
  ),
  encode = "form"
)

if (httr::status_code(res) != 200) {
  stop("REDCap API call failed! Status: ", httr::status_code(res))
}
jsonlite::fromJSON(httr::content(res, "text"))


tryCatch({
  # your API function, e.g. redcapAPI::redcap_read(...)
}, error = function(e) {
  cat("API request failed!\n")
  cat("URL was: ", your_url, "\n")
  cat("Token length: ", nchar(your_token), "\n")
  cat("Forms: ", paste(your_forms, collapse=", "), "\n")
  cat("Error message: ", e$message, "\n")
})

ass_dat <- full_api_pull(eval_token, url)
ass_dat <- wrangle_assessment_data(ass_dat)
rdm_dat <- forms_api_pull(rdm_token, url, 'resident_data', 'faculty_evaluation')


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


rdm_dict <- get_data_dict(rdm_token, url)
ass_dict <- get_data_dict(eval_token, url)
