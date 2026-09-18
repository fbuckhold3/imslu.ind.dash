# mod_duty_hours.R ─ thin passthrough to amiontools::mod_duty_hour_page
#
# The full composition (calendar + confirm form + summary chart) now lives
# in amiontools (promoted 2026-09-19, same pattern already used for
# mod_schedule.R passing through to the Schedule-tab modules) — this file
# just supplies ind.dash's own token/URL sourcing convention.
#
# amiontools must be installed (renv::install("fbuckhold3/amiontools")) —
# see the repo's own CLAUDE.md for why this app can't locally build packages.

mod_duty_hours_ui <- function(id) {
  amiontools::mod_duty_hour_page_ui(id)
}

mod_duty_hours_server <- function(id, resident_id) {
  amiontools::mod_duty_hour_page_server(
    id, resident_id = resident_id,
    rdm_token  = app_config$rdm_token,
    redcap_url = app_config$redcap_url
  )
}
