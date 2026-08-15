# mod_schedule.R ─ Schedule
# Category-level rotation days (mod_rotation_summary) + a team-level
# drill-down beneath it (mod_team_summary) — both from amiontools. Team-level
# is additive, not a replacement: category stays the high-level view, team
# detail (Green/Yellow/MICU 1/etc.) is the deeper layer under it.
# amiontools must be installed (renv::install("fbuckhold3/amiontools")) —
# see the repo's own CLAUDE.md for why this app can't locally build packages.

mod_schedule_ui <- function(id) {
  ns <- NS(id)
  tagList(
    amiontools::mod_rotation_summary_ui(ns("category")),
    tags$hr(style = "margin: 24px 0;"),
    amiontools::mod_team_summary_ui(ns("team"))
  )
}

mod_schedule_server <- function(id, resident_id) {
  # Outer moduleServer() IS needed here (unlike a single-module passthrough)
  # because two amiontools child modules ("category" and "team") each need
  # their own sub-namespace beneath "schedule" — Shiny's nested-module
  # pattern: calling child moduleServer()s from inside a parent's callback
  # composes the namespaces automatically. Verified via headless test
  # (ids render as schedule-category-* / schedule-team-*, not double-nested).
  moduleServer(id, function(input, output, session) {
    amiontools::mod_rotation_summary_server(
      "category",
      resident_id = resident_id,
      rdm_token   = app_config$rdm_token,
      redcap_url  = app_config$redcap_url
    )
    amiontools::mod_team_summary_server(
      "team",
      resident_id = resident_id,
      rdm_token   = app_config$rdm_token,
      redcap_url  = app_config$redcap_url
    )
  })
}
