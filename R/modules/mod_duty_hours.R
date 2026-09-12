# mod_duty_hours.R ─ Duty Hours (Phase 1 summary + Phase 2 confirm-flow)
# Composes amiontools::mod_duty_hour_summary (chart/flags — Amion defaults
# overlaid with any resident-confirmed/entered hours from RDM's
# duty_hour_log instrument) with the local mod_duty_hour_confirm (the
# confirm/edit queue, which writes to duty_hour_log via .rc_save()).
# See amion_integration project notes for the full staged plan.
# amiontools must be installed (renv::install("fbuckhold3/amiontools")) —
# see the repo's own CLAUDE.md for why this app can't locally build packages.

mod_duty_hours_ui <- function(id) {
  ns <- NS(id)
  tagList(
    mod_duty_hour_confirm_ui(ns("confirm")),
    tags$hr(style = "margin: 24px 0;"),
    amiontools::mod_duty_hour_summary_ui(ns("summary"))
  )
}

mod_duty_hours_server <- function(id, resident_id) {
  moduleServer(id, function(input, output, session) {
    mod_duty_hour_confirm_server("confirm", resident_id = resident_id)
    amiontools::mod_duty_hour_summary_server(
      "summary",
      resident_id = resident_id,
      rdm_token   = app_config$rdm_token,
      redcap_url  = app_config$redcap_url
    )
  })
}
