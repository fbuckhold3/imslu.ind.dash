# mod_attendance.R ─ Noon Conference Attendance (after-the-fact entry)
#
# Real-time check-in still happens in the separate imslu.at.noon app. This
# module only covers the case where a resident wasn't able to check in live
# (e.g. arrived late) and wants to log it afterward from their own
# dashboard. Writes to the same REDCap `questions` repeating instrument
# at.noon uses. There's no separate "real-time vs after-the-fact" flag —
# q_entry_timestamp (recorded automatically at save time) combined with the
# resident-picked q_date tells that story: same-day submission looks like
# real-time, a timestamp days after q_date means it was added later.
#
# Conference types and rotation lists mirror imslu.at.noon/global.R
# (conference_type_choices / rotation_choices_for_conference) — duplicated
# here intentionally rather than shared via gmed, to keep this change
# scoped to the two app repos.
#
# rdm_data() is only refreshed on login (Phase 2/3 load) — it does not
# automatically pick up a row just written via .rc_save(). display_data()
# below patches in rows saved earlier in the current session so the
# overview stats, heatmap, and history table update immediately without
# waiting for a full reload.

.att_sluh_rotation_choices <- c(
  "Red" = "1",
  "Green" = "2",
  "White" = "3",
  "Yellow" = "4",
  "Diamond" = "5",
  "Gold" = "6",
  "MICU" = "7",
  "Bronze" = "8",
  "Cardiology" = "9",
  "Bridge / Acute Care" = "10",
  "Consults - SLUH" = "11",
  "Elective / Clinics CSM" = "12"
)

.att_va_rotation_choices <- c(
  "VA A" = "13",
  "VA B" = "14",
  "VA C" = "15",
  "VA D" = "16",
  "VA Clinics or Consults" = "17"
)

.att_conference_type_choices <- c(
  "SLUH Noon Conference" = "1",
  "VA Noon Conference"   = "2",
  "Afternoon School"     = "3",
  "SLUH Grand Rounds"    = "4"
)

# Conference types that count toward the July-1 attendance percentage.
# Afternoon School is tracked separately as a simple counter, not a rate.
.att_percentage_conference_types <- c("1", "2", "4")

# Team/rotation question is hidden entirely for Afternoon School ("3").
.att_rotation_choices_for_conference <- function(conf_code) {
  if (identical(conf_code, "1") || identical(conf_code, "4")) .att_sluh_rotation_choices
  else if (identical(conf_code, "2")) .att_va_rotation_choices
  else c()
}

# Most recent July 1 on or before `today` — same academic-year convention
# used elsewhere in this ecosystem (gmed's level-at-time calculations).
.att_academic_year_start <- function(today) {
  yr <- as.numeric(format(today, "%Y"))
  mo <- as.numeric(format(today, "%m"))
  if (mo >= 7) as.Date(paste0(yr, "-07-01")) else as.Date(paste0(yr - 1, "-07-01"))
}

# Small calendar-heatmap: rows = Mon-Fri, columns = the last `n_weeks`
# weeks (oldest -> newest, ending this week). Attended weekdays get a
# filled checkmark cell so the status isn't color-only; upcoming weekdays
# are outlined/blank; everything else is a muted "no entry" cell.
.att_build_heatmap <- function(attended_dates, today, n_weeks = 9) {
  iso_wday <- as.integer(format(today, "%u"))  # Mon=1..Sun=7
  monday_this_week <- today - (iso_wday - 1)
  week_starts <- monday_this_week - 7 * seq(n_weeks - 1, 0)
  day_labels <- c("Mon", "Tue", "Wed", "Thu", "Fri")
  attended_chr <- format(attended_dates, "%Y-%m-%d")

  month_cells <- lapply(seq_along(week_starts), function(i) {
    ws <- week_starts[i]
    show_label <- i == 1 || format(ws, "%m") != format(week_starts[i - 1], "%m")
    tags$td(style = "font-size:0.65rem; color:#6c757d; text-align:center; padding-bottom:2px;",
            if (show_label) format(ws, "%b") else "")
  })

  day_rows <- lapply(seq_along(day_labels), function(wd_i) {
    row_cells <- lapply(seq_along(week_starts), function(i) {
      d <- week_starts[i] + (wd_i - 1)
      if (d > today) {
        cell <- div(style = "width:18px; height:18px; border-radius:3px; background:#ffffff; border:1px dashed #dee2e6;",
                     title = paste(format(d, "%a, %b %d"), "— upcoming"))
      } else if (format(d, "%Y-%m-%d") %in% attended_chr) {
        cell <- div(style = "width:18px; height:18px; border-radius:3px; background:#198754; display:flex; align-items:center; justify-content:center;",
                     title = paste(format(d, "%a, %b %d"), "— attended"),
                     tags$span(style = "color:#fff; font-size:10px; line-height:1;", "✓"))
      } else {
        cell <- div(style = "width:18px; height:18px; border-radius:3px; background:#e9ecef;",
                     title = paste(format(d, "%a, %b %d"), "— no entry"))
      }
      tags$td(style = "padding:2px;", cell)
    })
    tags$tr(
      tags$td(style = "font-size:0.7rem; color:#6c757d; padding-right:6px; text-align:right; white-space:nowrap;", day_labels[wd_i]),
      row_cells
    )
  })

  legend <- div(style = "display:flex; flex-wrap:wrap; gap:16px; margin-top:10px; font-size:0.78rem; color:#6c757d;",
    div(style = "display:flex; align-items:center; gap:5px;",
      div(style = "width:14px; height:14px; border-radius:3px; background:#198754; display:flex; align-items:center; justify-content:center;",
          tags$span(style = "color:#fff; font-size:9px;", "✓")),
      "Attended (Noon Conference / Grand Rounds)"),
    div(style = "display:flex; align-items:center; gap:5px;",
      div(style = "width:14px; height:14px; border-radius:3px; background:#e9ecef;"), "No entry"),
    div(style = "display:flex; align-items:center; gap:5px;",
      div(style = "width:14px; height:14px; border-radius:3px; background:#fff; border:1px dashed #dee2e6;"), "Upcoming")
  )

  tagList(
    div(style = "overflow-x:auto;",
      tags$table(style = "border-collapse:collapse;",
        tags$tbody(tags$tr(tags$td(""), month_cells), day_rows))
    ),
    legend
  )
}

mod_attendance_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$p(
      class = "text-muted mb-3", style = "font-size:0.9rem;",
      "Log attendance you weren't able to submit in real time — e.g. you arrived late. ",
      "Real-time check-in still happens in the ", tags$strong("Noon Conference"),
      " app (see Program Resources)."
    ),
    uiOutput(ns("attendance_overview")),
    uiOutput(ns("add_button_panel")),
    uiOutput(ns("add_form_panel")),
    h6(class = "mt-4 mb-2", style = "color:#003d5c; font-weight:700; font-size:0.95rem;",
       "Your Attendance History"),
    DT::dataTableOutput(ns("history_dt"))
  )
}

# rdm_data    : reactive() -> list with $all_forms$questions (all residents or
#               this resident's rows, depending on load phase)
# resident_id : reactive() -> record_id to save under / filter history by
mod_attendance_server <- function(id, rdm_data, resident_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ss             <- reactiveValues(save = NULL)
    form_open      <- reactiveVal(FALSE)
    local_new_rows <- reactiveVal(NULL)  # rows saved this session, ahead of the next full reload

    base_data <- reactive({
      req(resident_id())
      df <- tryCatch(rdm_data()$all_forms$questions, error = function(e) NULL)
      if (is.null(df) || nrow(df) == 0) return(NULL)
      df <- df[as.character(df$record_id) == as.character(resident_id()), , drop = FALSE]
      if (nrow(df) == 0) NULL else df
    })

    # base_data() (from rdm_data(), refreshed only on login) patched with
    # anything saved earlier in this session but not yet reflected there.
    display_data <- reactive({
      base  <- base_data()
      extra <- local_new_rows()
      if (is.null(base) && is.null(extra)) return(NULL)
      if (is.null(base))  return(extra)
      if (is.null(extra)) return(base)
      cols <- union(names(base), names(extra))
      for (cn in setdiff(cols, names(base)))  base[[cn]]  <- NA
      for (cn in setdiff(cols, names(extra))) extra[[cn]] <- NA
      rbind(base[cols], extra[cols])
    })

    observeEvent(input$btn_add,    { form_open(TRUE);  ss$save <- NULL })
    observeEvent(input$btn_cancel, { form_open(FALSE); ss$save <- NULL })

    output$add_button_panel <- renderUI({
      if (form_open()) return(NULL)
      div(class = "mb-3",
        actionButton(ns("btn_add"), "+ Log Attendance", class = "btn btn-sm",
          style = "background:#003d5c; color:#fff; border:none; padding:4px 14px; font-size:0.85rem;"))
    })

    output$add_form_panel <- renderUI({
      if (!form_open()) return(NULL)

      lbl <- function(text)
        tags$label(text, class = "form-label fw-semibold", style = "font-size:0.92rem; color:#2c3e50;")

      div(class = "card border-0 shadow-sm mb-3", style = "border-radius:8px;",
        div(class = "card-header border-0 d-flex align-items-center gap-2",
            style = "background:#f8fafc; border-radius:8px 8px 0 0; padding:12px 18px;",
          tags$i(class = "bi bi-calendar2-plus-fill", style = "color:#003d5c; font-size:1rem;"),
          tags$span(style = "font-weight:700; color:#003d5c; font-size:0.95rem;",
                     "Log Attendance After the Fact")),
        div(class = "card-body",
          div(class = "mb-3", lbl("Conference date"),
            dateInput(ns("att_date"), label = NULL, value = Sys.Date() - 1, max = Sys.Date(),
                      daysofweekdisabled = c(0, 6), width = "200px")),
          div(class = "mb-3", lbl("Conference"),
            selectInput(ns("att_conference_type"), label = NULL,
              choices = c("-- select --" = "", .att_conference_type_choices),
              selectize = FALSE, width = "100%")),
          div(class = "mb-3", lbl("Team / rotation"),
            selectInput(ns("att_rotation"), label = NULL,
              choices = c("-- select conference first --" = ""), selectize = FALSE, width = "100%")),
          uiOutput(ns("dup_warning")),
          div(class = "d-flex align-items-center gap-2 mt-3",
            actionButton(ns("btn_save"), "Save", class = "btn btn-sm",
              style = "background:#003d5c; color:#fff; border:none; padding:6px 18px;"),
            actionButton(ns("btn_cancel"), "Cancel",
              class = "btn btn-sm btn-outline-secondary", style = "padding:6px 14px;"),
            uiOutput(ns("save_status")))
        )
      )
    })

    # Rotation choices follow the picked conference type; hidden entirely
    # (empty choices) for Afternoon School.
    observeEvent(input$att_conference_type, {
      req(nzchar(input$att_conference_type))
      choices <- .att_rotation_choices_for_conference(input$att_conference_type)
      updateSelectInput(session, "att_rotation",
        choices = if (length(choices) == 0) c("-- not needed for this conference --" = "") else choices,
        selected = character(0))
    }, ignoreInit = TRUE)

    output$dup_warning <- renderUI({
      req(input$att_date, nzchar(input$att_conference_type))
      df <- display_data()
      if (is.null(df)) return(NULL)
      dup <- df[as.character(df$q_date) == format(input$att_date, "%Y-%m-%d") &
                  as.character(df$q_conference_type) == input$att_conference_type, ]
      if (nrow(dup) > 0) {
        div(class = "alert alert-warning py-2 px-3 mb-0", style = "font-size:0.85rem;",
          tags$i(class = "bi bi-exclamation-triangle-fill me-1"),
          "You already have an attendance record for this date and conference. Saving will add another entry.")
      } else NULL
    })

    output$save_status <- renderUI({
      r <- ss$save
      if (is.null(r)) return(NULL)
      if (isTRUE(r$success))
        tags$span(class = "text-success", style = "font-size:0.82rem;",
          tags$i(class = "bi bi-check-circle-fill me-1"), paste("Saved", r$ts))
      else
        tags$span(class = "text-danger", style = "font-size:0.82rem;",
          tags$i(class = "bi bi-exclamation-triangle-fill me-1"), r$message)
    })

    observeEvent(input$btn_save, {
      req(resident_id())
      d    <- input$att_date
      conf <- input$att_conference_type
      rot  <- input$att_rotation

      if (is.null(d) || length(d) == 0 || is.na(d)) {
        ss$save <- list(success = FALSE, message = "Please select a date."); return()
      }
      if (d > Sys.Date()) {
        ss$save <- list(success = FALSE, message = "Date can't be in the future."); return()
      }
      if (is.null(conf) || !nzchar(conf)) {
        ss$save <- list(success = FALSE, message = "Please select which conference."); return()
      }
      needs_rotation <- length(.att_rotation_choices_for_conference(conf)) > 0
      if (needs_rotation && (is.null(rot) || !nzchar(rot))) {
        ss$save <- list(success = FALSE, message = "Please select your team/rotation."); return()
      }

      fields <- list(
        q_date              = format(d, "%Y-%m-%d"),
        q_conference_type   = conf,
        q_rotation          = if (needs_rotation) rot else "",
        q_answer            = "",
        q_entry_timestamp   = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      )

      df <- display_data()
      inst <- if (!is.null(df) && nrow(df) > 0)
        max(suppressWarnings(as.integer(df$redcap_repeat_instance)), na.rm = TRUE) + 1L else 1L

      result <- .rc_save(resident_id(), "questions", inst, fields)
      ss$save <- result

      if (isTRUE(result$success)) {
        new_row <- as.data.frame(
          c(list(record_id = as.character(resident_id()),
                 redcap_repeat_instrument = "questions",
                 redcap_repeat_instance   = as.character(inst)),
            fields),
          stringsAsFactors = FALSE, check.names = FALSE)
        local_new_rows(rbind(local_new_rows(), new_row))
        form_open(FALSE)
      }
    })

    # ── Overview: July-1 percentage, Afternoon School counter, heatmap ─────
    attendance_stats <- reactive({
      today  <- Sys.Date()
      july1  <- .att_academic_year_start(today)
      all_days <- seq(july1, today, by = "day")
      weekday_seq <- all_days[!weekdays(all_days) %in% c("Saturday", "Sunday")]

      df <- display_data()
      attended_dates    <- as.Date(character(0))
      afternoon_count   <- 0L
      if (!is.null(df) && nrow(df) > 0) {
        d <- df
        d$.date <- suppressWarnings(as.Date(as.character(d$q_date)))
        d <- d[!is.na(d$.date) & d$.date >= july1 & d$.date <= today, , drop = FALSE]
        qual <- d[as.character(d$q_conference_type) %in% .att_percentage_conference_types, , drop = FALSE]
        attended_dates  <- unique(qual$.date)
        afternoon_count <- sum(as.character(d$q_conference_type) == "3", na.rm = TRUE)
      }

      list(
        july1           = july1,
        today           = today,
        n_weekdays      = length(weekday_seq),
        n_attended      = length(attended_dates),
        pct             = if (length(weekday_seq) > 0) round(length(attended_dates) / length(weekday_seq) * 100) else 0,
        attended_dates  = attended_dates,
        afternoon_count = afternoon_count
      )
    })

    output$attendance_overview <- renderUI({
      st <- attendance_stats()
      div(class = "card border-0 shadow-sm mb-3", style = "border-radius:8px;",
        div(class = "card-body",
          div(class = "d-flex flex-wrap gap-4 align-items-end mb-3",
            div(
              div(style = "font-size:0.78rem; color:#6c757d;",
                  paste0("Noon Conference + Grand Rounds — since ", format(st$july1, "%b %d"))),
              div(style = "font-size:1.8rem; font-weight:700; color:#003d5c; line-height:1.2;",
                paste0(st$pct, "%"),
                tags$span(style = "font-size:0.85rem; font-weight:400; color:#6c757d; margin-left:6px;",
                          paste0("(", st$n_attended, " of ", st$n_weekdays, " weekdays)")))
            ),
            div(
              div(style = "font-size:0.78rem; color:#6c757d;",
                  paste0("Afternoon School — since ", format(st$july1, "%b %d"))),
              div(style = "font-size:1.8rem; font-weight:700; color:#003d5c; line-height:1.2;",
                  st$afternoon_count)
            )
          ),
          .att_build_heatmap(st$attended_dates, st$today)
        )
      )
    })

    output$history_dt <- DT::renderDataTable({
      df <- display_data()
      rot_labels <- c(.att_sluh_rotation_choices, .att_va_rotation_choices)
      rot_labels <- setNames(names(rot_labels), unname(rot_labels))
      conf_labels <- setNames(names(.att_conference_type_choices), unname(.att_conference_type_choices))

      if (is.null(df)) {
        show_df <- data.frame(Date = character(0), Conference = character(0),
                               Team = character(0), Logged = character(0),
                               stringsAsFactors = FALSE)
      } else {
        show_df <- data.frame(
          Date       = df$q_date,
          Conference = unname(ifelse(df$q_conference_type %in% names(conf_labels),
                                      conf_labels[df$q_conference_type], "—")),
          Team       = unname(ifelse(df$q_rotation %in% names(rot_labels),
                                      rot_labels[df$q_rotation], ifelse(nzchar(df$q_rotation %||% ""), df$q_rotation, "—"))),
          Logged     = if ("q_entry_timestamp" %in% names(df)) df$q_entry_timestamp else "—",
          stringsAsFactors = FALSE
        )
        show_df <- show_df[order(show_df$Date, decreasing = TRUE), , drop = FALSE]
      }

      DT::datatable(
        show_df, rownames = FALSE, options = list(pageLength = 10, dom = "tp"),
        class = "compact stripe"
      )
    })
  })
}
