# mod_schedule.R ─ Schedule
# Rotation-days-by-category vs. class average, via amiontools::mod_rotation_summary.
# amiontools must be installed (renv::install("fbuckhold3/amiontools")) —
# see the repo's own CLAUDE.md for why this app can't locally build packages.

mod_schedule_ui <- function(id) {
  amiontools::mod_rotation_summary_ui(id)
}

mod_schedule_server <- function(id, resident_id) {
  # Thin passthrough — NOT wrapped in its own moduleServer(id, ...). amiontools'
  # mod_rotation_summary_server() already calls moduleServer(id, ...) internally,
  # so wrapping it here again would double-namespace and break the id match
  # against mod_schedule_ui()'s direct (unwrapped) call to the amiontools UI.
  amiontools::mod_rotation_summary_server(
    id,
    resident_id = resident_id,
    rdm_token   = app_config$rdm_token,
    redcap_url  = app_config$redcap_url
  )
}
