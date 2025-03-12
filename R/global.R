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



# Simple approach: if EVAL_TOKEN is defined, we assume "hosted"
is_hosted <- Sys.getenv("EVAL_TOKEN") != ""

if (is_hosted) {
  # Use environment variables from the hosting service
  eval_token <- Sys.getenv("EVAL_TOKEN")
  rdm_token  <- Sys.getenv("RDM_TOKEN")
  fac_token  <- Sys.getenv("FAC_TOKEN")
  new_ass_token <- Sys.getenv("NEW_ASS_TOKEN")
} else {
  # Use local config.yml
  conf <- config::get(file = "config.yml")
  eval_token <- conf$eval_token
  rdm_token  <- conf$rdm_token
  fac_token  <- conf$fac_token
  new_ass_token <- conf$new_ass_token
}

cat("=== DEBUG: Dumping environment variable NAMES only ===\n")
all_env_vars <- names(Sys.getenv())
print(all_env_vars)

# If you just need to confirm that EVAL_TOKEN, RDM_TOKEN are set at all:
cat("=== Checking the tokens exist in Sys.getenv() ===\n")
cat("EVAL_TOKEN is set? ", "EVAL_TOKEN" %in% all_env_vars, "\n")
cat("RDM_TOKEN is set?  ", "RDM_TOKEN"  %in% all_env_vars, "\n")
cat("FAC_TOKEN is set?  ", "FAC_TOKEN"  %in% all_env_vars, "\n")


url <- "https://redcapsurvey.slu.edu/api/"


