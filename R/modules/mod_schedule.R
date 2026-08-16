# mod_schedule.R ─ Schedule
# Category-level rotation days (mod_rotation_summary) + a team-level
# drill-down (mod_team_summary) + a time-allocation chart (mod_time_
# allocation) — all from amiontools, all additive layers in the same tab,
# not replacements of each other.
# amiontools must be installed (renv::install("fbuckhold3/amiontools")) —
# see the repo's own CLAUDE.md for why this app can't locally build packages.

mod_schedule_ui <- function(id) {
  ns <- NS(id)
  tagList(
    amiontools::mod_rotation_summary_ui(ns("category")),
    tags$hr(style = "margin: 24px 0;"),
    amiontools::mod_team_summary_ui(ns("team")),
    tags$hr(style = "margin: 24px 0;"),
    amiontools::mod_time_allocation_ui(ns("allocation"))
  )
}

mod_schedule_server <- function(id, resident_id) {
  # Outer moduleServer() needed - three child modules each need their own
  # sub-namespace beneath "schedule". Verified via headless test (ids render
  # as schedule-category-*/schedule-team-*/schedule-allocation-*, not
  # double-nested) - same pattern as the category+team addition before this.
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
    amiontools::mod_time_allocation_server(
      "allocation",
      resident_id = resident_id,
      rdm_token   = app_config$rdm_token,
      redcap_url  = app_config$redcap_url
    )
  })
}
