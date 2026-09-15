# mod_duty_hours.R ─ Duty Hours (Phase 1 summary + Phase 2 calendar/confirm)
#
# Orchestrator: owns the shared reactives (entries_r, amion_blocks_r,
# refresh, selected_date) that mod_duty_hour_calendar and
# mod_duty_hour_confirm both need and must stay in sync on — a save in the
# confirm form bumps `refresh`, which both entries_r() and the calendar's
# coloring depend on; a calendar click sets `selected_date`, which the
# confirm form reads to decide what to show. Composes:
#   - mod_duty_hour_calendar : month grid, click a day to select it
#   - mod_duty_hour_confirm  : the edit form for the selected (or oldest
#     unconfirmed) day, writes to duty_hour_log
#   - amiontools::mod_duty_hour_summary : the read-only weekly chart/flags
#     (Amion defaults merged with whatever's been saved)
#
# amiontools must be installed (renv::install("fbuckhold3/amiontools")) —
# see the repo's own CLAUDE.md for why this app can't locally build packages.

mod_duty_hours_ui <- function(id) {
  ns <- NS(id)
  tagList(
    mod_duty_hour_calendar_ui(ns("calendar")),
    tags$hr(style = "margin: 20px 0;"),
    mod_duty_hour_confirm_ui(ns("confirm")),
    tags$hr(style = "margin: 24px 0;"),
    amiontools::mod_duty_hour_summary_ui(ns("summary"))
  )
}

mod_duty_hours_server <- function(id, resident_id) {
  moduleServer(id, function(input, output, session) {
    refresh <- reactiveVal(0)
    selected_date <- reactiveVal(NULL)

    # Shared crosswalk/Amion fetch (amiontools::use_amion_data(), the same
    # helper the Schedule tab already uses) — threaded into amion_blocks_r
    # AND the summary sub-module below. Without this, each of
    # build_duty_hour_summary()'s two call sites here independently
    # re-fetches the full 503-resident crosswalk + full-year Amion pull
    # from scratch — found live 2026-09-15 (Fred: slow load, "doom loop"
    # in the console) — 2-3 redundant multi-second fetches per page load,
    # the exact problem already solved once before for mod_schedule.R.
    shared <- amiontools::use_amion_data(
      rdm_token  = app_config$rdm_token,
      redcap_url = app_config$redcap_url
    )

    entries_r <- reactive({
      refresh()
      req(resident_id())
      amiontools::pull_duty_hour_log(app_config$rdm_token, app_config$redcap_url,
                                     record_id = resident_id())
    })

    # Amion-only defaults (entries = data.frame() skips the overlay) — both
    # children need the RAW defaults (to find gaps / color cells), not the
    # already-merged view build_duty_hour_summary() normally returns.
    amion_blocks_r <- reactive({
      req(resident_id())
      summ <- amiontools::build_duty_hour_summary(
        rdm_token = app_config$rdm_token, redcap_url = app_config$redcap_url,
        crosswalk = shared$crosswalk(), amion = shared$amion(),
        entries = data.frame()
      )
      summ$duty_blocks |> dplyr::filter(record_id == resident_id())
    })

    mod_duty_hour_calendar_server("calendar", resident_id = resident_id,
                                  entries_r = entries_r, amion_blocks_r = amion_blocks_r,
                                  selected_date = selected_date)
    mod_duty_hour_confirm_server("confirm", resident_id = resident_id,
                                 entries_r = entries_r, amion_blocks_r = amion_blocks_r,
                                 refresh = refresh, selected_date = selected_date)
    amiontools::mod_duty_hour_summary_server(
      "summary",
      resident_id = resident_id,
      rdm_token   = app_config$rdm_token,
      redcap_url  = app_config$redcap_url,
      crosswalk_r = shared$crosswalk,
      amion_r     = shared$amion,
      entries_r   = entries_r
    )
  })
}
