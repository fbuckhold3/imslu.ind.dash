# mod_scholarship.R ─ Scholarship & Teaching Portfolio
# Shows existing scholarship entries in a table and provides an "+ Add Entry"
# form. Saves to REDCap scholarship instrument (repeating, additive pattern).

mod_scholarship_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("entries_panel")),
    uiOutput(ns("add_form_panel"))
  )
}

mod_scholarship_server <- function(id, rdm_data, resident_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── local cache — starts from loaded data, appended on each save ─────────
    # (rdm_data() is frozen at startup; this cache stays live)
    schol_local <- reactiveVal(NULL)

    observe({
      req(rdm_data(), resident_id())
      if (!is.null(schol_local())) return()   # already seeded
      schol <- rdm_data()$all_forms$scholarship
      rows  <- if (!is.null(schol) && nrow(schol) > 0)
                 schol[schol$record_id == resident_id(), , drop = FALSE]
               else data.frame()
      schol_local(rows)
    })

    my_scholarship <- reactive({
      schol_local()
    })

    # ── type map ──────────────────────────────────────────────────────────────
    .TYPE_LABELS <- c(
      "1" = "QI / Improvement Project",
      "2" = "Patient Safety",
      "3" = "Research",
      "4" = "Presentation",
      "5" = "Publication",
      "6" = "Education / Teaching",
      "7" = "Committee Work"
    )

    # ── save state ─────────────────────────────────────────────────────────────
    ss <- reactiveValues(save = NULL)

    # ── show form toggle ───────────────────────────────────────────────────────
    show_form <- reactiveVal(FALSE)
    observeEvent(input$btn_add, { show_form(TRUE); ss$save <- NULL })
    observeEvent(input$btn_cancel, { show_form(FALSE); ss$save <- NULL })

    # ── entries table ──────────────────────────────────────────────────────────
    output$entries_panel <- renderUI({
      df <- my_scholarship()

      table_content <- if (is.null(df) || nrow(df) == 0) {
        div(class = "text-muted fst-italic", style = "font-size:0.85rem; padding:16px 0;",
            tags$i(class = "bi bi-inbox me-2"),
            "No scholarship entries yet — click below to add your first entry.")
      } else {
        rows <- lapply(seq_len(nrow(df)), function(i) {
          row      <- df[i, ]
          get_fv   <- function(f) if (f %in% names(row)) { v <- row[[f]][1]; if (is.na(v)) "" else as.character(v) } else ""
          type_val <- get_fv("schol_type")
          type_lbl <- .TYPE_LABELS[type_val] %||% paste0("Type ", type_val)
          inst     <- get_fv("redcap_repeat_instance")

          # Description/title: use best available field
          desc <- get_fv("schol_qi")
          if (!nzchar(desc)) desc <- get_fv("schol_cit")
          if (!nzchar(desc)) desc <- get_fv("schol_comm")
          if (!nzchar(desc)) desc <- get_fv("schol_pres_conf")
          if (!nzchar(desc)) desc <- get_fv("schol_res")
          if (nchar(desc) > 80) desc <- paste0(substr(desc, 1, 77), "...")

          # Date: attempt schol_date or fall back to instance#
          date_val <- get_fv("schol_date")
          date_lbl <- if (nzchar(date_val)) date_val else paste0("Entry #", inst)

          tags$tr(
            tags$td(style = "font-size:0.82rem; color:#6c757d; white-space:nowrap;", date_lbl),
            tags$td(style = "font-size:0.82rem;",
              tags$span(
                class = "badge",
                style = "background:#e3eef8; color:#003d5c; font-size:0.75rem; font-weight:600;",
                type_lbl)),
            tags$td(style = "font-size:0.82rem; color:#2c3e50;", desc)
          )
        })

        div(class = "table-responsive",
          tags$table(class = "table table-sm mb-0",
            tags$thead(
              tags$tr(
                tags$th(style = "font-size:0.75rem; color:#6c757d; font-weight:600; width:120px;", "Date / Entry"),
                tags$th(style = "font-size:0.75rem; color:#6c757d; font-weight:600; width:160px;", "Type"),
                tags$th(style = "font-size:0.75rem; color:#6c757d; font-weight:600;", "Description / Title")
              )
            ),
            tags$tbody(rows)
          )
        )
      }

      div(
        div(class = "card border-0 shadow-sm mb-3",
            style = "border-radius:8px;",
          div(class = "card-header border-0 d-flex align-items-center justify-content-between gap-2",
              style = "background:#f8fafc; border-radius:8px 8px 0 0; padding:12px 18px;",
            div(class = "d-flex align-items-center gap-2",
              tags$i(class = "bi bi-award-fill", style = "color:#003d5c; font-size:1rem;"),
              tags$span(style = "font-weight:700; color:#003d5c; font-size:0.95rem;",
                        "Scholarship & Teaching Portfolio")),
            if (!isTRUE(show_form()))
              actionButton(ns("btn_add"), "+ Add Entry",
                class = "btn btn-sm",
                style = "background:#003d5c; color:#fff; border:none; padding:4px 14px; font-size:0.82rem;")),
          div(class = "card-body", table_content))
      )
    })

    # ── entry form ─────────────────────────────────────────────────────────────
    output$add_form_panel <- renderUI({
      if (!isTRUE(show_form())) return(NULL)

      div(class = "card border-0 shadow-sm mb-3",
          style = "border-radius:8px;",
        div(class = "card-header border-0 d-flex align-items-center gap-2",
            style = "background:#f8fafc; border-radius:8px 8px 0 0; padding:12px 18px;",
          tags$i(class = "bi bi-plus-circle-fill", style = "color:#003d5c; font-size:1rem;"),
          tags$span(style = "font-weight:700; color:#003d5c; font-size:0.95rem;",
                    "Add Scholarship Entry")),
        div(class = "card-body",
          tags$p(class = "text-muted mb-3", style = "font-size:0.82rem;",
                 "Log research, presentations, teaching, and academic activities."),

          # Type selector
          div(class = "mb-3",
            tags$label("Activity type", class = "form-label fw-semibold",
                       style = "font-size:0.85rem; color:#2c3e50;"),
            selectInput(ns("schol_type"), label = NULL,
              choices = c("-- select type --" = "",
                          "QI / Improvement Project" = "1",
                          "Patient Safety"           = "2",
                          "Research"                 = "3",
                          "Presentation"             = "4",
                          "Publication"              = "5",
                          "Education / Teaching"     = "6",
                          "Committee Work"           = "7"),
              selected = "", selectize = FALSE, width = "100%")),

          # Conditional fields
          uiOutput(ns("schol_type_fields")),

          # Save / cancel buttons
          div(class = "d-flex align-items-center gap-2 mt-3",
            actionButton(ns("btn_save"), "Save Entry",
              class = "btn btn-sm",
              style = "background:#003d5c; color:#fff; border:none; padding:6px 18px;"),
            actionButton(ns("btn_cancel"), "Cancel",
              class = "btn btn-sm btn-outline-secondary",
              style = "padding:6px 14px;"),
            uiOutput(ns("save_status")))
        )
      )
    })

    # ── conditional fields by type ─────────────────────────────────────────────
    output$schol_type_fields <- renderUI({
      type_val <- input$schol_type
      if (is.null(type_val) || !nzchar(type_val)) return(NULL)

      common_desc <- function(field_id, label_text, rows = 3)
        div(class = "mb-3",
          tags$label(label_text, class = "form-label fw-semibold",
                     style = "font-size:0.85rem; color:#2c3e50;"),
          tags$textarea(id = ns(field_id), class = "form-control",
                        rows = rows, style = "font-size:0.88rem; resize:vertical;", ""))

      mentor_status <- tagList(
        div(class = "mb-3",
          tags$label("Mentor / supervisor", class = "form-label fw-semibold",
                     style = "font-size:0.85rem; color:#2c3e50;"),
          tags$input(type = "text", id = ns("schol_res_mentor"),
                     class = "form-control form-control-sm",
                     placeholder = "Name of mentor or supervisor")),
        div(class = "mb-3",
          tags$label("Project status", class = "form-label fw-semibold",
                     style = "font-size:0.85rem; color:#2c3e50;"),
          selectInput(ns("schol_res_status"), label = NULL,
            choices = c("-- select --" = "",
                        "Planning"    = "1",
                        "In Progress" = "2",
                        "Completed"   = "3",
                        "Submitted"   = "4",
                        "Published"   = "5"),
            selected = "", selectize = FALSE, width = "100%")))

      if (type_val %in% c("1", "2", "3")) {
        # QI / Safety / Research
        tagList(
          common_desc("schol_qi",
            if (type_val == "1") "Describe the QI / improvement project"
            else if (type_val == "2") "Describe the patient safety activity"
            else "Describe the research project"),
          mentor_status)

      } else if (type_val == "4") {
        # Presentation
        tagList(
          div(class = "mb-3",
            tags$label("Conference / venue", class = "form-label fw-semibold",
                       style = "font-size:0.85rem; color:#2c3e50;"),
            tags$input(type = "text", id = ns("schol_pres_conf"),
                       class = "form-control form-control-sm",
                       placeholder = "e.g., ACP National Meeting 2025")),
          div(class = "mb-3",
            tags$label("Presentation type", class = "form-label fw-semibold",
                       style = "font-size:0.85rem; color:#2c3e50;"),
            selectInput(ns("schol_pres_type"), label = NULL,
              choices = c("-- select --" = "",
                          "Poster"       = "1",
                          "Oral"         = "2",
                          "Workshop"     = "3",
                          "Grand Rounds" = "4"),
              selected = "", selectize = FALSE, width = "100%")),
          common_desc("schol_qi", "Brief description (optional)", rows = 2))

      } else if (type_val == "5") {
        # Publication
        tagList(
          div(class = "mb-3",
            tags$label("Full citation", class = "form-label fw-semibold",
                       style = "font-size:0.85rem; color:#2c3e50;"),
            tags$textarea(id = ns("schol_cit"), class = "form-control",
                          rows = 3, style = "font-size:0.88rem; resize:vertical;",
                          placeholder = "Authors, Title, Journal, Year, Vol(Issue):pages", "")),
          common_desc("schol_qi", "Additional notes (optional)", rows = 2))

      } else if (type_val == "6") {
        # Education / Teaching
        common_desc("schol_qi", "Describe the teaching activity")

      } else if (type_val == "7") {
        # Committee Work
        tagList(
          div(class = "mb-3",
            tags$label("Committee name", class = "form-label fw-semibold",
                       style = "font-size:0.85rem; color:#2c3e50;"),
            tags$input(type = "text", id = ns("schol_comm"),
                       class = "form-control form-control-sm",
                       placeholder = "e.g., Patient Safety Committee")),
          div(class = "mb-3",
            tags$label("Committee level", class = "form-label fw-semibold",
                       style = "font-size:0.85rem; color:#2c3e50;"),
            selectInput(ns("schol_comm_type"), label = NULL,
              choices = c("-- select --" = "",
                          "Local"    = "1",
                          "Regional" = "2",
                          "National" = "3"),
              selected = "", selectize = FALSE, width = "100%")),
          common_desc("schol_qi", "Description / role (optional)", rows = 2))
      }
    })

    # ── save status ────────────────────────────────────────────────────────────
    output$save_status <- renderUI({
      r <- ss$save
      if (is.null(r)) return(NULL)
      if (isTRUE(r$success))
        tags$span(class = "text-success", style = "font-size:0.8rem;",
          tags$i(class = "bi bi-check-circle-fill me-1"),
          paste("Saved", r$ts))
      else
        tags$span(class = "text-danger", style = "font-size:0.8rem;",
          tags$i(class = "bi bi-exclamation-triangle-fill me-1"),
          r$message)
    })

    # ── save handler ───────────────────────────────────────────────────────────
    observeEvent(input$btn_save, {
      req(resident_id())
      type_val <- input$schol_type %||% ""
      if (!nzchar(type_val)) {
        ss$save <- list(success = FALSE,
                        message = "Please select an activity type before saving.")
        return()
      }

      fv <- function(id) { v <- input[[id]]; if (is.null(v)) "" else as.character(v) }

      fields <- list(
        schol_type       = type_val,
        schol_qi         = fv("schol_qi"),
        schol_res_mentor = fv("schol_res_mentor"),
        schol_res_status = fv("schol_res_status"),
        schol_pres_conf  = fv("schol_pres_conf"),
        schol_pres_type  = fv("schol_pres_type"),
        schol_cit        = fv("schol_cit"),
        schol_comm       = fv("schol_comm"),
        schol_comm_type  = fv("schol_comm_type")
      )

      # Get next instance number (additive pattern)
      df <- tryCatch({
        schol <- rdm_data()$all_forms$scholarship
        if (!is.null(schol)) schol[schol$record_id == resident_id(), , drop = FALSE]
        else NULL
      }, error = function(e) NULL)

      next_inst <- if (!is.null(df) && nrow(df) > 0) {
        existing_insts <- suppressWarnings(as.integer(df$redcap_repeat_instance))
        max(existing_insts, na.rm = TRUE) + 1L
      } else 1L

      result <- .rc_save(resident_id(), "scholarship", next_inst, fields)
      ss$save <- result

      if (isTRUE(result$success)) {
        # Append to local cache so table updates immediately (no app restart needed)
        new_row <- as.data.frame(
          c(list(record_id               = resident_id(),
                 redcap_repeat_instrument = "scholarship",
                 redcap_repeat_instance  = as.character(next_inst),
                 schol_date              = format(Sys.Date(), "%Y-%m-%d")),
            lapply(fields, as.character)),
          stringsAsFactors = FALSE, check.names = FALSE)
        schol_local(dplyr::bind_rows(schol_local() %||% data.frame(), new_row))
        show_form(FALSE)
      }
    })

  }) # end moduleServer
}
