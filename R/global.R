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
library(future)
library(promises)

# Background workers for async Phase 2 data load
plan(multisession)

# ── Static assets from gmed package ──────────────────────────────────────────
# Serves milestone grid images as /milestones/<file>.png
local({
  ms_dir <- system.file("www", "milestones", package = "gmed")
  if (nzchar(ms_dir) && dir.exists(ms_dir))
    shiny::addResourcePath("milestones", ms_dir)
})

# ── Graphics options ──────────────────────────────────────────────────────────
options(shiny.plot.res = 96)
if (!interactive()) options(bitmapType = "cairo")

# ── Configuration ─────────────────────────────────────────────────────────────
# Single REDCap token. Locally, set RDM_TOKEN in .Renviron to your fake/dev
# project token. In production (Posit Connect), set RDM_TOKEN in the
# deployment environment variables to the live project token.
#
# R reads .Renviron once at session startup, so editing the file without
# restarting R leaves stale values in Sys.getenv(). Re-read the project-local
# .Renviron explicitly on every app launch so `runApp()` honors the current
# file on disk. (If you've done `Sys.setenv(RDM_TOKEN=...)` in the console and
# want that to win, comment out this block.)
local({
  proj_env <- file.path(getwd(), ".Renviron")
  if (file.exists(proj_env)) {
    readRenviron(proj_env)
    message("\U0001F501 Reloaded ", proj_env)
  }
})

app_config <- list(
  rdm_token  = Sys.getenv("RDM_TOKEN", unset = ""),
  fac_token  = Sys.getenv("FAC_TOKEN", unset = ""),
  redcap_url = "https://redcapsurvey.slu.edu/api/"
)

# Fail loudly if no token — prevents silent "saves" that hit REDCap with an
# empty token (REDCap rejects but the app can't always tell).
if (!nzchar(app_config$rdm_token)) {
  stop("No REDCap token available. Set RDM_TOKEN in .Renviron (local) ",
       "or in the deployment environment (Posit Connect).")
}

# Fingerprint + project identity probe — eyeball which REDCap project this
# runApp() is actually talking to. Hits /api/ with content=project once at
# startup. Never prints the token itself.
local({
  tok <- app_config$rdm_token
  fp  <- paste0(substr(tok, 1, 6), "\u2026",
                substr(tok, nchar(tok) - 3, nchar(tok)))
  pt  <- tryCatch({
    resp <- httr::POST(
      url  = app_config$redcap_url,
      body = list(token = tok, content = "project",
                  format = "json", returnFormat = "json"),
      encode = "form", httr::timeout(10))
    if (httr::status_code(resp) == 200) {
      j <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
      paste0(j$project_title %||% "(no title)",
             " [id=", j$project_id %||% "?", "]")
    } else paste0("(HTTP ", httr::status_code(resp), ")")
  }, error = function(e) paste0("(probe failed: ", e$message, ")"))
  message("\u2705 RDM connected \u2014 token ", fp, " \u2192 ", pt)
})

# ── Utility operators (R < 4.4 doesn't have %||% in base) ────────────────────
`%||%` <- function(x, y) if (!is.null(x)) x else y

# ── ABIM board risk — McDonald et al. 2020 nomogram ──────────────────────────
# Exported from gmed::abim_risk.R; thin wrappers kept here so existing module
# code that calls .pass_prob() / .risk_from_prob() continues to work unchanged.
.NOMOGRAM_PARAMS <- gmed::NOMOGRAM_PARAMS
.pass_prob       <- gmed::pass_prob
.risk_from_prob  <- gmed::risk_from_prob

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
  list(id = "schedule",     label = "Schedule",               icon = "calendar3-fill",          desc = "Rotation schedule",              disabled = TRUE),
  list(id = "resources",    label = "Program Resources",      icon = "grid-3x3-gap-fill",       desc = "Links, SharePoint & program tools", disabled = TRUE)
)

# ── Faculty roster loader (IMSLUFaculty REDCap project via FAC_TOKEN) ─────────
# Implementation lives in gmed::load_faculty_roster(); app just holds the store.
# Reset on every global.R load so re-running runApp() in the same R session
# doesn't surface stale rows from a previous token/project.
faculty_roster_store <- NULL

# ── Data stores ───────────────────────────────────────────────────────────────
# Phase 1: residents only (fast — used for login auth)
residents_store <- NULL

# Phase 2: full data (slow — loaded after Phase 1 completes)
app_data_store  <- NULL

# Sanity message so it's obvious the caches just got cleared.
message("\U0001F9F9 Cleared residents_store / app_data_store / faculty_roster_store")

# ── Phase 1: residents-only load (~4 s) ───────────────────────────────────────
# Fetches only the resident_data form so the login UI appears quickly.
# Returns the residents data frame (or NULL on error).
load_app_residents <- function() {
  if (is.null(residents_store)) {
    message("Phase 1: loading residents...")
    residents_store <<- gmed::load_rdm_residents_only(
      rdm_token  = app_config$rdm_token,
      redcap_url = app_config$redcap_url,
      raw_or_label = "raw"
    )
    if (!is.null(residents_store)) {
      message("Phase 1 done — ", nrow(residents_store), " active residents.")
    }
  }
  residents_store
}

# ── Phase 2: full data load (~25 s) ───────────────────────────────────────────
# Called from a future worker — returns a list so the .then() handler can
# assign results back into the main-process globals.
fetch_full_app_data <- function(rdm_token, redcap_url, fac_token) {
  rdm <- gmed::load_rdm_complete(
    rdm_token    = rdm_token,
    redcap_url   = redcap_url,
    verbose      = FALSE,
    raw_or_label = "raw"
  )
  fac <- gmed::load_faculty_roster(
    fac_token  = fac_token,
    redcap_url = redcap_url
  )
  list(rdm = rdm, fac = fac)
}

# Thin wrapper kept for compatibility / warm-start cache check
load_app_data <- function() {
  if (!is.null(app_data_store)) return(app_data_store)
  result <- fetch_full_app_data(
    rdm_token  = app_config$rdm_token,
    redcap_url = app_config$redcap_url,
    fac_token  = app_config$fac_token
  )
  app_data_store     <<- result$rdm
  faculty_roster_store <<- result$fac
  app_data_store
}

# ── Shared UI helpers ──────────────────────────────────────────────────────────

# Collapsible info note — use wherever a section needs explanation without
# cluttering the primary UI.  Renders as a small ⓘ summary that expands inline.
info_note <- function(..., summary = "About this section") {
  tags$details(
    class = "info-note",
    tags$summary(
      tags$i(class = "bi bi-info-circle me-1"),
      summary
    ),
    div(class = "info-note-body", ...)
  )
}

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
