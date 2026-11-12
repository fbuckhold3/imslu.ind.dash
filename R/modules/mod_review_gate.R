# mod_review_gate.R ─ data-readiness gate and Submit for the self-evaluation
#
# Rules and counts live in reviewkit; duty and conference data in
# amiontools. This module is the screen: it shows where the resident stands,
# embeds the two places to fix it, and on Submit re-checks live data and
# freezes the snapshot on this period's s_eval row. A saved
# s_e_snapshot_json is what "submitted" means.
#
# The live check takes ~10 s (RDM + Amion), so it runs in a future worker.
#
# Lock (Fred, 2026-11-12): until duty hours, the conference log and faculty
# evaluations (FAC_EVAL_MIN this academic year) all pass, the self-eval
# sections stay hidden. The module returns locked_r for mod_self_eval. A
# coach's gate override unlocks all three. Faculty evaluations are counted
# from the live check, so finishing some and re-checking unlocks at once.

FAC_EVAL_MIN <- 10L

mod_review_gate_ui <- function(id) {
  ns <- NS(id)
  conditionalPanel(
    condition = "output.gate_show", ns = ns,
    div(class = "card border-0 shadow-sm mt-3 mb-3", style = "border-radius:8px;",
      div(class = "card-body",
        tags$h5(class = "mb-1", "Before you submit: your data"),
        tags$p(class = "text-muted mb-3",
               "Your duty hours and conference log need to be current since ",
               textOutput(ns("window_start"), inline = TRUE),
               ", and you need at least ", FAC_EVAL_MIN, " faculty evaluations this academic year. ",
               "The self-evaluation opens once all three are done."),
        uiOutput(ns("status")),
        bslib::accordion(
          id = ns("fix"), open = FALSE, class = "mt-3",
          bslib::accordion_panel(
            "Log conference days", value = "conference",
            amiontools::mod_conference_catchup_ui(ns("catchup"))
          ),
          bslib::accordion_panel(
            "Confirm duty hours", value = "duty",
            amiontools::mod_duty_hour_page_ui(ns("duty"))
          )
        )
      )
    )
  )
}

mod_review_gate_submit_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("submit_panel"))
}

# enabled_r: TRUE when the selected period is an open period that uses the gate.
# sections_done_r: TRUE when every section of that period is complete.
# saved_json_r: the period's stored s_e_snapshot_json, or NA/NULL.
# save_snapshot: function(json) -> list(success, message); writes the snapshot.
# override_r: the period's gate override, list(reason, by, date), or NULL.
#   An overridden period can be submitted with the gate failing; the
#   snapshot still stores the real numbers.
mod_review_gate_server <- function(id, resident_id, enabled_r, sections_done_r,
                                   saved_json_r, save_snapshot,
                                   override_r = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    state <- reactiveVal(NULL)
    load_error <- reactiveVal(NULL)
    submitting <- reactiveVal(FALSE)
    submit_msg <- reactiveVal(NULL)

    token <- app_config$rdm_token
    url   <- app_config$redcap_url

    task <- ExtendedTask$new(function(rid, token, url) {
      future_promise({
        inputs <- reviewkit::fetch_review_inputs(token, url, record_ids = rid)
        w <- reviewkit::review_window()
        a <- inputs$activity
        a <- a[as.character(a$record_id) == rid & a$Date >= w$start & a$Date <= w$end &
                 a$conf_expected != "None" & !a$conf_attended & !a$conf_not_attended,
               c("Date", "conf_expected", "Rotation"), drop = FALSE]
        list(snapshot = reviewkit::build_review_snapshot(rid, inputs$forms, inputs$activity, w),
             unlogged = a)
      })
    })

    recheck <- function() {
      rid <- isolate(resident_id())
      if (is.null(rid) || !nzchar(as.character(rid))) return(invisible())
      if (isolate(task$status()) != "running") {
        load_error(NULL)
        task$invoke(as.character(rid), token, url)
      }
      invisible()
    }

    submitted <- reactive({
      j <- saved_json_r()
      length(j) == 1 && !is.na(j) && nzchar(trimws(j))
    })
    show_gate <- reactive(isTRUE(enabled_r()) && !submitted())

    output$gate_show <- reactive(show_gate())
    outputOptions(output, "gate_show", suspendWhenHidden = FALSE)
    output$window_start <- renderText(trimws(format(reviewkit::review_window()$start, "%B %e")))

    observeEvent(resident_id(), {
      state(NULL); load_error(NULL); submit_msg(NULL); submitting(FALSE)
    })

    # Every module starts at login, so wait until this panel is actually
    # on screen: the check loads all of RDM plus Amion. The key doesn't
    # exist until the self-eval tab has rendered.
    on_screen <- reactive(isFALSE(session$clientData[[paste0("output_", ns("status"), "_hidden")]]))

    # First look at an open period: run the check once.
    observeEvent(list(show_gate(), on_screen()), {
      if (isTRUE(show_gate()) && on_screen() && is.null(state())) recheck()
    })
    observeEvent(input$recheck, recheck())

    observeEvent(task$status(), {
      st <- task$status()
      if (st == "success") {
        r <- task$result()
        n_fac <- suppressWarnings(as.integer(r$snapshot$faculty_evals_submitted))
        if (length(n_fac) != 1 || is.na(n_fac)) n_fac <- 0L
        state(list(snapshot = r$snapshot, unlogged = r$unlogged,
                   gate = reviewkit::evaluate_gate(r$snapshot),
                   fac = list(n = n_fac, pass = n_fac >= FAC_EVAL_MIN)))
        if (isTRUE(submitting())) finish_submit()
      } else if (st == "error") {
        msg <- tryCatch({ task$result(); "" }, error = function(e) conditionMessage(e))
        message("[review_gate] check failed: ", msg)
        load_error("We couldn't load your schedule just now. Please try again in a minute.")
        if (isTRUE(submitting())) {
          submitting(FALSE)
          submit_msg(list(ok = FALSE, text = "Not submitted: we couldn't check your data. Please try again."))
        }
      }
    })

    finish_submit <- function() {
      submitting(FALSE)
      s <- state()
      if (!isTRUE(sections_done_r()) || isTRUE(submitted())) return(invisible())
      if (!(isTRUE(s$gate$pass) && isTRUE(s$fac$pass)) && is.null(override_r())) {
        failing <- c(if (!s$gate$duty$pass) s$gate$duty$message,
                     if (!s$gate$conference$pass) s$gate$conference$message,
                     if (!isTRUE(s$fac$pass)) fac_message(s$fac))
        submit_msg(list(ok = FALSE, text = paste("Not submitted yet.", paste(failing, collapse = " "))))
        return(invisible())
      }
      res <- save_snapshot(reviewkit::snapshot_to_json(s$snapshot))
      submit_msg(if (isTRUE(res$success)) NULL else
        list(ok = FALSE, text = paste("Not submitted:", res$message %||% "the save failed. Please try again.")))
      invisible()
    }

    observeEvent(input$submit, {
      req(isTRUE(enabled_r()), isTRUE(sections_done_r()), !isTRUE(submitted()))
      if (isTRUE(submitting())) return()
      submit_msg(NULL)
      submitting(TRUE)
      recheck()
    })

    # ── Fixing it, without leaving the self-eval ────────────────────────
    saved_days <- amiontools::mod_conference_catchup_server(
      "catchup", resident_id = resident_id,
      days_r = reactive({ req(state()); state()$unlogged }),
      rdm_token = token, redcap_url = url
    )
    observeEvent(saved_days(), recheck(), ignoreInit = TRUE)

    # Queue holds exactly the days the gate counts: scheduled (not off)
    # days in the gate window (Oct 1 in AY 2026-27, Jul 1 after) through
    # yesterday.
    amiontools::mod_duty_hour_page_server(
      "duty", resident_id = resident_id, rdm_token = token, redcap_url = url,
      queue_from    = function() reviewkit::review_window()$start,
      queue_through = function() reviewkit::review_window()$end,
      skip_off_days = TRUE
    )

    # ── Lock ────────────────────────────────────────────────────────────
    # reactiveVal, so the self-eval form re-renders only when the lock
    # actually flips (a re-check that changes nothing must not redraw it).
    locked <- reactiveVal(TRUE)
    observe({
      s <- state()
      v <- isTRUE(show_gate()) && is.null(override_r()) &&
        (is.null(s) || !(isTRUE(s$gate$pass) && isTRUE(s$fac$pass)))
      if (!identical(v, isolate(locked()))) locked(v)
    })

    fac_message <- function(f) {
      if (isTRUE(f$pass)) sprintf("Faculty evaluations: %d this academic year.", f$n)
      else sprintf("Faculty evaluations: %d of %d this academic year. Complete %d more on the Faculty Evaluations page.",
                   f$n, FAC_EVAL_MIN, FAC_EVAL_MIN - f$n)
    }

    # ── Display ─────────────────────────────────────────────────────────
    check_row <- function(ok, text) {
      div(class = "d-flex align-items-start gap-2 mb-1",
        tags$i(class = paste0("bi ", if (ok) "bi-check-circle-fill text-success"
                                     else "bi-exclamation-circle-fill text-danger")),
        tags$span(text))
    }

    override_note <- function() {
      o <- override_r()
      if (is.null(o)) return(NULL)
      who  <- if (!is.na(o$by)) paste0(" by ", o$by) else ""
      when <- if (!is.na(o$date)) paste0(" on ", trimws(format(as.Date(o$date), "%B %e"))) else ""
      div(class = "mb-3 py-2 px-3",
          style = "background:#e8f5e9; border-left:5px solid #2e7d32; border-radius:8px;",
        tags$div(style = "font-weight:700; color:#1b5e20;",
                 tags$i(class = "bi bi-unlock-fill me-1"),
                 paste0("The data check was cleared for you", who, when, ".")),
        if (!is.na(o$reason)) tags$div(style = "color:#2e7d32;", o$reason),
        tags$div(class = "small text-muted mt-1",
                 "You can submit even if the items below aren't current. Please still update what you can."))
    }

    output$status <- renderUI({
      running <- task$status() == "running"
      s <- state()
      err <- load_error()
      recheck_btn <- actionButton(ns("recheck"), "Re-check my data",
                                  class = "btn btn-outline-secondary btn-sm mt-2",
                                  disabled = if (running) NA else NULL)
      if (is.null(s)) {
        return(tagList(
          if (!is.null(err)) tags$p(class = "text-danger mb-1", err)
          else tags$p(class = "text-muted mb-1", "Checking your duty hours and conference log. This can take up to a minute."),
          if (!is.null(err)) recheck_btn))
      }
      n <- s$gate$nudges
      n <- n[n$item != "Faculty evaluations submitted", , drop = FALSE]
      tagList(
        override_note(),
        check_row(s$gate$duty$pass, s$gate$duty$message),
        check_row(s$gate$conference$pass, s$gate$conference$message),
        check_row(isTRUE(s$fac$pass), fac_message(s$fac)),
        if (!isTRUE(s$fac$pass))
          tags$a(href = "#", class = "small ms-4",
                 onclick = "Shiny.setInputValue('nav_block','faculty_eval',{priority:'event'});return false;",
                 "Go to Faculty Evaluations ", tags$i(class = "bi bi-arrow-right")),
        tags$div(class = "mt-3 small text-muted", "Also worth a look (these don't block submission):"),
        tags$ul(class = "small mb-1",
          lapply(seq_len(nrow(n)), function(i)
            tags$li(tags$strong(if (is.na(n$value[i])) "\u2014" else format(n$value[i])), " ", n$item[i], " · ", n$detail[i]))),
        if (!is.null(err)) tags$p(class = "text-danger mb-1", err),
        recheck_btn,
        if (running) tags$span(class = "small text-muted ms-2", "Checking…")
      )
    })

    output$submit_panel <- renderUI({
      req(isTRUE(enabled_r()))
      if (isTRUE(submitted())) {
        snap <- tryCatch(reviewkit::snapshot_from_json(saved_json_r()), error = function(e) NULL)
        when <- if (!is.null(snap)) format(as.Date(substr(snap$built_at, 1, 10)), "%B %e, %Y") else NULL
        return(div(class = "mt-3 py-3 px-3",
          style = "background:#e8f5e9; border-left:5px solid #2e7d32; border-radius:8px;",
          tags$div(style = "font-weight:700; color:#1b5e20;",
                   tags$i(class = "bi bi-check-circle-fill me-1"),
                   "Self-evaluation submitted", if (!is.null(when)) paste0(" on ", when)),
          tags$div(style = "color:#2e7d32;",
                   "You can still revise your answers until this period closes.")))
      }
      if (!isTRUE(sections_done_r())) {
        return(tags$p(class = "text-muted mt-3", "Finish every section above, then submit here."))
      }
      m <- submit_msg()
      busy <- isTRUE(submitting())
      div(class = "card border-0 shadow-sm mt-3", style = "border-radius:8px;",
        div(class = "card-body",
          tags$h5(class = "mb-1", "All sections complete"),
          tags$p(class = "text-muted mb-2",
                 "Submitting checks your duty hours and conference log one more time, ",
                 "then saves your data for your coach."),
          actionButton(ns("submit"), if (busy) "Checking…" else "Submit self-evaluation",
                       class = "btn btn-primary", disabled = if (busy) NA else NULL),
          if (!is.null(m)) tags$p(class = paste0("mt-2 mb-0 ", if (m$ok) "text-success" else "text-danger"), m$text)
        ))
    })

    list(locked_r = locked)
  })
}
