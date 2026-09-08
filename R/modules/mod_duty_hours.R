# mod_duty_hours.R ─ Duty Hours (Phase 1: read-only preview)
# Thin passthrough to amiontools::mod_duty_hour_summary — Amion-derived
# default hours only (no moonlighting/at-home time yet, no editing/
# persistence yet). See amion_integration project notes 2026-09-07 for the
# full staged plan; this is Phase 1 only.
# amiontools must be installed (renv::install("fbuckhold3/amiontools")) —
# see the repo's own CLAUDE.md for why this app can't locally build packages.

mod_duty_hours_ui <- function(id) {
  amiontools::mod_duty_hour_summary_ui(id)
}

mod_duty_hours_server <- function(id, resident_id) {
  amiontools::mod_duty_hour_summary_server(
    id,
    resident_id = resident_id,
    rdm_token   = app_config$rdm_token,
    redcap_url  = app_config$redcap_url
  )
}
