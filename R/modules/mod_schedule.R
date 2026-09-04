# mod_schedule.R ─ Schedule
# Category-level rotation days (mod_rotation_summary) + the same, broken out
# by training year actually lived (mod_rotation_by_year) + a team-level
# drill-down incl. off-days (mod_team_summary) + a time-allocation chart
# (mod_time_allocation) + conference attendance reconciliation
# (mod_attendance_reconciliation) + a day-by-day detail log
# (mod_daily_detail) — all from amiontools, all additive layers in the same
# tab.
# amiontools must be installed (renv::install("fbuckhold3/amiontools")) —
# see the repo's own CLAUDE.md for why this app can't locally build packages.

mod_schedule_ui <- function(id) {
  ns <- NS(id)
  tagList(
    amiontools::mod_rotation_summary_ui(ns("category")),
    tags$hr(style = "margin: 24px 0;"),
    amiontools::mod_rotation_by_year_ui(ns("by_year")),
    tags$hr(style = "margin: 24px 0;"),
    amiontools::mod_team_summary_ui(ns("team")),
    tags$hr(style = "margin: 24px 0;"),
    amiontools::mod_time_allocation_ui(ns("allocation")),
    tags$hr(style = "margin: 24px 0;"),
    amiontools::mod_attendance_reconciliation_ui(ns("attendance")),
    tags$hr(style = "margin: 24px 0;"),
    amiontools::mod_daily_detail_ui(ns("daily"))
  )
}

mod_schedule_server <- function(id, resident_id) {
  # Outer moduleServer() needed - six child modules each need their own
  # sub-namespace beneath "schedule". Verified via headless test (ids render
  # as schedule-category-*/schedule-team-*/schedule-allocation-*/
  # schedule-daily-*/schedule-by_year-*/schedule-attendance-*, not
  # double-nested).
  moduleServer(id, function(input, output, session) {

    # Shared live-fetch reactives: defined unconditionally, but Shiny
    # reactives are lazy -- wiring these costs nothing until something
    # actually calls shared$crosswalk()/shared$amion(). mod_daily_detail
    # always needs them (its data is deliberately excluded from the REDCap
    # cache -- ~12 MB, blows the field-size ceiling -- so it's always a
    # live fetch); the 3 aggregate modules below only fall through to them
    # on a cache miss, so this stays a single live fetch either way, never
    # a redundant second one.
    shared <- amiontools::use_amion_data(
      rdm_token  = app_config$rdm_token,
      redcap_url = app_config$redcap_url
    )

    # Cache-first for the 3 aggregate sections: try the REDCap app_cache
    # record (written weekly by rdm-data-refresh's refresh_amion.qmd)
    # before touching Amion/RDM live. NULL on a cache miss/stale/malformed
    # payload -> fall back to the shared live-fetch path, unchanged from
    # before this caching work.
    cached <- amiontools::use_amion_data_cached(
      rdm_token  = app_config$rdm_token,
      redcap_url = app_config$redcap_url
    )

    # Same cache-first pattern, separate cache field -- the expected-
    # conference calendar the attendance-reconciliation section needs.
    cached_calendar <- amiontools::use_expected_calendar_cached(
      rdm_token  = app_config$rdm_token,
      redcap_url = app_config$redcap_url
    )

    if (!is.null(cached)) {
      amiontools::mod_rotation_summary_server(
        "category", resident_id = resident_id,
        rdm_token = app_config$rdm_token, redcap_url = app_config$redcap_url,
        summary_r = cached$rotation
      )
      amiontools::mod_team_summary_server(
        "team", resident_id = resident_id,
        rdm_token = app_config$rdm_token, redcap_url = app_config$redcap_url,
        summary_r = cached$team
      )
      amiontools::mod_time_allocation_server(
        "allocation", resident_id = resident_id,
        rdm_token = app_config$rdm_token, redcap_url = app_config$redcap_url,
        summary_r = cached$talloc
      )
    } else {
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
    }

    amiontools::mod_daily_detail_server(
      "daily",
      resident_id = resident_id,
      rdm_token   = app_config$rdm_token,
      redcap_url  = app_config$redcap_url,
      crosswalk_r = shared$crosswalk,
      amion_r     = shared$amion
    )

    # Amion/RDM side is cache-fed when available (the questions log stays
    # live regardless -- see mod_attendance_reconciliation.R for why).
    # Falls back to the shared live fetch (same one mod_daily_detail uses,
    # so never a redundant second fetch) on a cache miss.
    amiontools::mod_attendance_reconciliation_server(
      "attendance",
      resident_id = resident_id,
      rdm_token   = app_config$rdm_token,
      redcap_url  = app_config$redcap_url,
      crosswalk_r = shared$crosswalk,
      amion_r     = shared$amion,
      expected_calendar_r = cached_calendar
    )

    # The current AY's contribution to this table is literally the same
    # data as `cached$rotation` above -- feed it straight in and this
    # section costs nothing extra on a cache hit (no live fetch, no
    # recompute). Falls back to the shared live crosswalk/amion (same ones
    # mod_daily_detail uses, so still never a second live fetch) on a
    # cache miss.
    amiontools::mod_rotation_by_year_server(
      "by_year",
      resident_id = resident_id,
      rdm_token   = app_config$rdm_token,
      redcap_url  = app_config$redcap_url,
      crosswalk_r = shared$crosswalk,
      amion_r     = shared$amion,
      cached_rotation_r = if (!is.null(cached)) cached$rotation else NULL
    )
  })
}
