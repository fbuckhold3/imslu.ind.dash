# mod_duty_hour_confirm.R ─ Duty Hours: confirm/edit flow (Phase 2)
#
# A queue of unconfirmed past/current days (Amion default pre-filled, one
# at a time, oldest first) the resident steps through to confirm-as-shown
# or edit + add moonlighting/at-home hours + notes. Writes to RDM's
# duty_hour_log repeating instrument via .rc_save() (defined in
# mod_self_eval.R, reused here — same generic writer scholarship/self-eval
# already use).
#
# Future days are never in the queue — they haven't happened yet, so
# there's nothing to confirm (see amiontools' duty_hour_summary.R header).
# A day already covered by a saved entry (confirmed OR entered) drops out
# of the queue automatically once amiontools::pull_duty_hour_log() reflects
# the save — no separate "mark done" bookkeeping needed.
#
# amiontools must be installed (renv::install("fbuckhold3/amiontools")) —
# see this repo's own CLAUDE.md for why this app can't locally build
# packages.

.dh_hhmm_to_colon <- function(x) {
  ifelse(is.na(x) | x == "", "", paste0(substr(x, 1, 2), ":", substr(x, 3, 4)))
}
.dh_colon_to_hhmm <- function(x) {
  x <- gsub(":", "", x)
  ifelse(is.na(x) | x == "", NA_character_, x)
}

mod_duty_hour_confirm_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("body"))
}

mod_duty_hour_confirm_server <- function(id, resident_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    refresh <- reactiveVal(0)

    entries_r <- reactive({
      refresh()
      req(resident_id())
      amiontools::pull_duty_hour_log(app_config$rdm_token, app_config$redcap_url,
                                     record_id = resident_id())
    })

    # Amion-only defaults (entries = data.frame() skips the overlay) — the
    # queue needs the RAW defaults, not the already-merged view, since the
    # whole point is finding dates NOT yet covered by a saved entry.
    amion_blocks <- reactive({
      req(resident_id())
      summ <- amiontools::build_duty_hour_summary(
        rdm_token = app_config$rdm_token, redcap_url = app_config$redcap_url,
        entries = data.frame()
      )
      summ$duty_blocks |> dplyr::filter(record_id == resident_id())
    })

    queue <- reactive({
      db <- amion_blocks()
      covered <- entries_r()$dh_date
      if (nrow(db) == 0) return(db[0, ])
      db |>
        dplyr::filter(Date <= Sys.Date(), !(Date %in% covered)) |>
        dplyr::group_by(Date) |>
        dplyr::summarise(
          category   = dplyr::first(category),
          Hours      = { h <- Hours[!is.na(Hours)]; if (length(h) == 0) NA_real_ else sum(h) },
          start_hhmm = { s <- block_start[!is.na(block_start)]; if (length(s) == 0) NA_character_ else s[which.min(amiontools::to_int_time(s))] },
          end_hhmm   = { e <- block_end[!is.na(block_end)];   if (length(e) == 0) NA_character_ else e[which.max(amiontools::to_int_time(e))] },
          .groups = "drop"
        ) |>
        dplyr::arrange(Date)
    })

    current_day <- reactive({
      q <- queue()
      if (nrow(q) == 0) NULL else q[1, ]
    })

    output$body <- renderUI({
      day <- current_day()
      n_remaining <- nrow(queue())

      if (is.null(day)) {
        return(tagList(
          h5("Duty Hours — Confirm"),
          p(class = "text-success", "✓ All caught up! No pending days to confirm.")
        ))
      }

      tagList(
        h5("Duty Hours — Confirm"),
        p(class = "text-muted small",
          sprintf("%d day(s) need confirmation. Showing the oldest: %s.",
                 n_remaining, format(day$Date, "%A, %B %d, %Y"))),
        fluidRow(
          column(6, selectInput(ns("category"), "Category",
                                choices = names(amiontools::DUTY_HOUR_CATEGORY_UI_CHOICES),
                                selected = if (day$category %in% names(amiontools::DUTY_HOUR_CATEGORY_UI_CHOICES))
                                             day$category else "Elective")),
          column(3, textInput(ns("start_time"), "Start time", value = .dh_hhmm_to_colon(day$start_hhmm), placeholder = "HH:MM")),
          column(3, textInput(ns("end_time"), "End time", value = .dh_hhmm_to_colon(day$end_hhmm), placeholder = "HH:MM"))
        ),
        fluidRow(
          column(4, numericInput(ns("hours"), "Total hours", value = if (is.na(day$Hours)) NA else day$Hours, min = 0, max = 24, step = 0.5)),
          column(4, numericInput(ns("moonlighting"), "Moonlighting hours (this date)", value = NA, min = 0, max = 24, step = 0.5)),
          column(4, numericInput(ns("home_hours"), "At-home chart-review hours (this date)", value = NA, min = 0, max = 24, step = 0.5))
        ),
        textAreaInput(ns("notes"), "Notes (optional)", rows = 2),
        div(class = "d-flex gap-2 mt-2",
          actionButton(ns("save_next"), "Save & Next", class = "btn btn-primary"),
          if (!is.na(day$Hours))
            actionButton(ns("confirm_as_shown"), "Confirm as shown", class = "btn btn-outline-secondary")
        )
      )
    })

    .build_and_save <- function(day, category, start_colon, end_colon, hours_val, moon_val, home_val, notes) {
      entries <- entries_r()
      existing <- entries$redcap_repeat_instance[entries$dh_date == day$Date]
      instance <- if (length(existing) > 0) existing[1]
                  else max(c(0, entries$redcap_repeat_instance), na.rm = TRUE) + 1

      unchanged <- !is.na(day$Hours) &&
        identical(category, day$category) &&
        identical(start_colon, .dh_hhmm_to_colon(day$start_hhmm)) &&
        identical(end_colon, .dh_hhmm_to_colon(day$end_hhmm)) &&
        isTRUE(all.equal(hours_val, day$Hours)) &&
        (is.na(moon_val) || moon_val == 0) &&
        (is.na(home_val) || home_val == 0)
      source_label <- if (unchanged) "Resident confirmed" else "Resident entered"

      fields <- list(
        dh_date = as.character(day$Date),
        dh_category = unname(amiontools::DUTY_HOUR_CATEGORY_CODES[category]),
        dh_start_time = start_colon,
        dh_end_time = end_colon,
        dh_hours = hours_val,
        dh_moonlighting_hours = moon_val,
        dh_home_hours = home_val,
        dh_source = unname(amiontools::DUTY_HOUR_SOURCE_CODES[source_label]),
        dh_notes = notes,
        dh_confirmed_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      )
      result <- .rc_save(resident_id(), "duty_hour_log", instance, fields)
      if (isTRUE(result$success)) {
        refresh(refresh() + 1)
        showNotification("Saved.", type = "message", duration = 2)
      } else {
        showNotification(paste("Save failed:", result$message), type = "error", duration = NULL)
      }
    }

    observeEvent(input$save_next, {
      day <- current_day()
      req(day)
      .build_and_save(
        day, input$category, input$start_time, input$end_time,
        suppressWarnings(as.numeric(input$hours)),
        suppressWarnings(as.numeric(input$moonlighting)),
        suppressWarnings(as.numeric(input$home_hours)),
        input$notes
      )
    })

    observeEvent(input$confirm_as_shown, {
      day <- current_day()
      req(day)
      req(!is.na(day$Hours))
      .build_and_save(
        day, day$category, .dh_hhmm_to_colon(day$start_hhmm), .dh_hhmm_to_colon(day$end_hhmm),
        day$Hours, NA_real_, NA_real_, NULL
      )
    })
  })
}
