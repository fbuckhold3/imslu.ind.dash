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
  # double-nested).
  moduleServer(id, function(input, output, session) {

    # Shared fetch (2026-08-16): all three sections used to independently
    # re-fetch the same RDM crosswalk + full-year Amion data - measured
    # ~4x redundant fetches, roughly doubling load time. use_amion_data()
    # fetches once; each module reuses it via crosswalk_r/amion_r. Measured
    # 3.2x speedup (24.4s -> 7.6s) with byte-identical results.
    shared <- amiontools::use_amion_data(
      rdm_token  = app_config$rdm_token,
      redcap_url = app_config$redcap_url
    )

    amiontools::mod_rotation_summary_server(
      "category",
      resident_id = resident_id,
      rdm_token   = app_config$rdm_token,
      redcap_url  = app_config$redcap_url,
      crosswalk_r = shared$crosswalk,
      amion_r     = shared$amion
    )
    amiontools::mod_team_summary_server(
      "team",
      resident_id = resident_id,
      rdm_token   = app_config$rdm_token,
      redcap_url  = app_config$redcap_url,
      crosswalk_r = shared$crosswalk,
      amion_r     = shared$amion
    )
    amiontools::mod_time_allocation_server(
      "allocation",
      resident_id = resident_id,
      rdm_token   = app_config$rdm_token,
      redcap_url  = app_config$redcap_url,
      crosswalk_r = shared$crosswalk,
      amion_r     = shared$amion
    )
  })
}
