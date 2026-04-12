library(shiny)
library(shinyjs)
library(bslib)
library(DT)
library(dplyr)
library(plotly)
library(ggplot2)
library(purrr)
library(tidyr)
library(lubridate)
library(httr)
library(jsonlite)
library(gmed)

# ── Graphics options ──────────────────────────────────────────────────────────
options(shiny.plot.res = 96)
if (!interactive()) options(bitmapType = "cairo")

# ── Configuration ─────────────────────────────────────────────────────────────
# Environment switch: set RDM_ENV=test in your .Renviron (or session) to point
# all reads AND writes at the test REDCap project.  Leave unset (or set to
# "production") for the live database.
#
#   Test mode:       Sys.setenv(RDM_ENV = "test")   # in R console before runApp()
#   Production mode: Sys.setenv(RDM_ENV = "production")  # or just restart R
#
# RDM_TEST_TOKEN must be set in .Renviron alongside RDM_TOKEN.

.rdm_env <- tolower(trimws(Sys.getenv("RDM_ENV", unset = "production")))

app_config <- list(
  env        = .rdm_env,
  is_test    = (.rdm_env == "test"),
  rdm_token  = if (.rdm_env == "test") Sys.getenv("RDM_TEST_TOKEN")
               else                    Sys.getenv("RDM_TOKEN"),
  redcap_url = "https://redcapsurvey.slu.edu/api/"
)

if (app_config$is_test) {
  message("\u26a0\ufe0f  [TEST MODE] Connected to test REDCap project. ",
          "Reads and writes go to RDM_TEST_TOKEN.")
} else {
  message("\u2705  [PRODUCTION] Connected to live REDCap project.")
}

# ── Utility operators (R < 4.4 doesn't have %||% in base) ────────────────────
`%||%` <- function(x, y) if (!is.null(x)) x else y

# ── Navigation blocks definition ──────────────────────────────────────────────
# Edit here to add / reorder / rename sections. Each list must have:
#   id    — used internally for routing (must be unique, no spaces)
#   label — shown on the home screen block
#   icon  — Bootstrap Icons name (https://icons.getbootstrap.com)
#   desc  — small subtitle on the block

resident_nav_blocks <- list(
  list(id = "evaluations",  label = "My Evaluations",         icon = "clipboard2-check-fill",  desc = "Evaluations received & feedback"),
  list(id = "learning",     label = "My Learning",            icon = "mortarboard-fill",        desc = "Goals, learning topics & exam prep"),
  list(id = "milestones",   label = "Milestones",             icon = "graph-up-arrow",          desc = "Competency progress & curves"),
  list(id = "scholarship",  label = "Scholarship & Teaching", icon = "award-fill",              desc = "Research, teaching & academic portfolio"),
  list(id = "faculty_eval", label = "Faculty Evaluations",    icon = "person-check-fill",       desc = "Evaluations you've completed"),
  list(id = "self_eval",    label = "Self Evaluations",       icon = "person-lines-fill",       desc = "Self-assessments & ILP"),
  list(id = "schedule",     label = "Schedule",               icon = "calendar3-fill",          desc = "Rotation schedule"),
  list(id = "resources",    label = "Program Resources",      icon = "grid-3x3-gap-fill",       desc = "Links, SharePoint & program tools")
)

# ── Data loading ───────────────────────────────────────────────────────────────
app_data_store <- NULL

load_app_data <- function() {
  if (is.null(app_data_store)) {
    message("Loading RDM 2.0 data...")
    app_data_store <<- load_rdm_complete(
      rdm_token    = app_config$rdm_token,
      verbose      = FALSE,
      raw_or_label = "raw"   # keep numeric codes so scale visualizations work
    )
    message("Data loaded — ", nrow(app_data_store$residents), " residents, ",
            nrow(app_data_store$all_forms$assessment), " assessments.")
  }
  app_data_store
}

# ── Shared UI helpers ──────────────────────────────────────────────────────────

# Placeholder card for sections not yet built
section_placeholder <- function(icon_name, msg = "Content coming soon") {
  div(
    class = "d-flex flex-column align-items-center justify-content-center py-5",
    style = "color: var(--ssm-text-muted);",
    tags$i(
      class = paste0("bi bi-", icon_name),
      style = "font-size: 3rem; opacity: 0.25; margin-bottom: 16px;"
    ),
    tags$p(msg, style = "font-size: 0.95rem; margin: 0;")
  )
}
