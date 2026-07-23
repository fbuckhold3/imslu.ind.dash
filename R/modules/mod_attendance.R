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

# Team/rotation question is hidden entirely for Afternoon School ("3").
.att_rotation_choices_for_conference <- function(conf_code) {
  if (identical(conf_code, "1") || identical(conf_code, "4")) .att_sluh_rotation_choices
  else if (identical(conf_code, "2")) .att_va_rotation_choices
  else c()
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

    ss        <- reactiveValues(save = NULL)
    form_open <- reactiveVal(FALSE)

    existing_data <- reactive({
      req(resident_id())
      df <- tryCatch(rdm_data()$all_forms$questions, error = function(e) NULL)
      if (is.null(df) || nrow(df) == 0) return(NULL)
      df <- df[as.character(df$record_id) == as.character(resident_id()), , drop = FALSE]
      if (nrow(df) == 0) NULL else df
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
      df <- existing_data()
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

      df <- existing_data()
      inst <- if (!is.null(df) && nrow(df) > 0)
        max(suppressWarnings(as.integer(df$redcap_repeat_instance)), na.rm = TRUE) + 1L else 1L

      result <- .rc_save(resident_id(), "questions", inst, fields)
      ss$save <- result

      if (isTRUE(result$success)) form_open(FALSE)
    })

    output$history_dt <- DT::renderDataTable({
      df <- existing_data()
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
