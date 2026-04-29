# mod_faculty_eval.R ─ Faculty Evaluations Done
#
# Section 1: Summary strip + PGY year progress chart + faculty name chips
# Section 2: Native multi-step evaluation form (no iframe)
#            Uses faculty_roster_store (loaded at startup via FAC_TOKEN).
#            Submits directly to REDCap via app_config credentials.

.FE_GOALS <- c("1" = 25L, "2" = 25L, "3" = 25L)   # 25 per year, all levels

# ── REDCap submission helpers ─────────────────────────────────────────────────

.fe_next_instance <- function(resident_id) {
  tryCatch({
    resp <- httr::POST(
      app_config$redcap_url,
      body = list(
        token = app_config$rdm_token, content = "record",
        action = "export", format = "json", type = "flat",
        records    = as.character(resident_id),
        fieldNames = "record_id,redcap_repeat_instrument,redcap_repeat_instance",
        rawOrLabel = "raw", rawOrLabelHeaders = "raw",
        exportCheckboxLabel = "false", exportSurveyFields = "false",
        exportDataAccessGroups = "false", returnFormat = "json"
      ),
      encode = "form"
    )
    if (httr::status_code(resp) == 200) {
      dat <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
      if (is.data.frame(dat) && nrow(dat) > 0) {
        fe <- dat[!is.na(dat$redcap_repeat_instrument) &
                    dat$redcap_repeat_instrument == "faculty_evaluation", ]
        if (nrow(fe) > 0) {
          inst <- suppressWarnings(as.numeric(fe$redcap_repeat_instance))
          return(max(inst[!is.na(inst)], 0L) + 1L)
        }
      }
    }
    1L
  }, error = function(e) 1L)
}

.fe_submit_eval <- function(resident_id, pgy, eval_data) {
  next_inst <- .fe_next_instance(resident_id)
  full_data <- c(
    list(
      record_id                   = as.character(resident_id),
      redcap_repeat_instrument    = "faculty_evaluation",
      redcap_repeat_instance      = as.character(next_inst),
      fac_eval_date               = format(Sys.Date(), "%Y-%m-%d"),
      fac_eval_level              = as.character(pgy),
      faculty_evaluation_complete = "2"
    ),
    eval_data
  )
  rc_df <- as.data.frame(
    lapply(full_data, function(x) if (is.null(x)) NA_character_ else as.character(x)),
    stringsAsFactors = FALSE
  )
  resp <- httr::POST(
    app_config$redcap_url,
    body = list(
      token = app_config$rdm_token, content = "record",
      format = "json", type = "flat",
      data = jsonlite::toJSON(rc_df, auto_unbox = TRUE)
    ),
    encode = "form"
  )
  if (httr::status_code(resp) != 200)
    stop("REDCap error (", httr::status_code(resp), "): ",
         httr::content(resp, "text", encoding = "UTF-8"))
  invisible(full_data)
}

.fe_submit_pending <- function(name, type_code, rot) {
  tok <- app_config$fac_token
  if (!nzchar(tok)) stop("FAC_TOKEN not configured")
  next_inst <- tryCatch({
    r <- httr::POST(
      app_config$redcap_url,
      body = list(
        token = tok, content = "record", action = "export",
        format = "json", records = "1",
        fieldNames = "record_id,redcap_repeat_instrument,redcap_repeat_instance",
        rawOrLabel = "raw", rawOrLabelHeaders = "raw",
        exportCheckboxLabel = "false", exportSurveyFields = "false",
        exportDataAccessGroups = "false", returnFormat = "json"
      ),
      encode = "form"
    )
    if (httr::status_code(r) == 200) {
      d <- jsonlite::fromJSON(httr::content(r, "text", encoding = "UTF-8"))
      if (is.data.frame(d) && nrow(d) > 0) {
        pq <- d[!is.na(d$redcap_repeat_instrument) &
                  d$redcap_repeat_instrument == "pending_queue", ]
        if (nrow(pq) > 0) {
          inst <- suppressWarnings(as.numeric(pq$redcap_repeat_instance))
          return(max(inst[!is.na(inst)], 0L) + 1L)
        }
      }
    }
    1L
  }, error = function(e) 1L)
  rc_df <- data.frame(
    record_id = "1", redcap_repeat_instrument = "pending_queue",
    redcap_repeat_instance = as.character(next_inst),
    pend_name = name, pend_fac_fell = type_code, pend_rot = rot,
    pending_queue_complete = "2", stringsAsFactors = FALSE
  )
  resp <- httr::POST(
    app_config$redcap_url,
    body = list(
      token = tok, content = "record", format = "json", type = "flat",
      data = jsonlite::toJSON(rc_df, auto_unbox = TRUE)
    ),
    encode = "form"
  )
  if (httr::status_code(resp) != 200)
    stop("Pending queue error: ", httr::content(resp, "text"))
  invisible(TRUE)
}

# ── Rating question UI (namespaced for Shiny modules) ────────────────────────
# Buttons set a hidden input via feRate() JS; server reads input[[field]].
.fe_rating_q <- function(ns, field, label, left_lbl, right_lbl) {
  nid <- ns(field)
  div(
    class = "mb-4",
    tags$p(label, style = "font-weight:600; font-size:0.88rem; color:#2d3748; margin-bottom:8px;"),
    div(
      style = "display:flex; align-items:center; gap:10px; background:#f8fafc;
               border-radius:10px; padding:10px 14px;",
      div(style = "font-size:0.7rem; color:#888; flex:0 0 auto; max-width:100px;", left_lbl),
      div(
        style = "display:flex; gap:6px; flex:1; justify-content:center;",
        lapply(1:5, function(i)
          tags$button(
            type = "button", id = paste0(nid, "_b", i),
            style = "width:36px; height:36px; border-radius:50%; border:2px solid #dde5ed;
                     background:white; font-weight:700; font-size:0.82rem; cursor:pointer;
                     transition:all 0.15s ease; padding:0; line-height:1;",
            onclick = sprintf("feRate('%s',%d)", nid, i),
            as.character(i)
          )
        )
      ),
      div(style = "font-size:0.7rem; color:#888; flex:0 0 auto; max-width:100px; text-align:right;", right_lbl)
    ),
    tags$input(type = "hidden", id = nid, name = nid, value = "")
  )
}

# ── Empty plotly placeholder ──────────────────────────────────────────────────
.fe_empty_plot <- function(msg = "No evaluations yet") {
  plot_ly(data = data.frame(x = numeric(0), y = numeric(0)),
          x = ~x, y = ~y, type = "scatter", mode = "markers") %>%
    layout(
      annotations = list(list(
        text = msg, x = 0.5, y = 0.5, xref = "paper", yref = "paper",
        showarrow = FALSE, font = list(size = 13, color = "#adb5bd")
      )),
      xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
      margin = list(l=0,r=0,t=0,b=0),
      plot_bgcolor  = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)"
    ) %>%
    plotly::config(displayModeBar = FALSE)
}

# ═════════════════════════════════════════════════════════════════════════════
# UI
# ═════════════════════════════════════════════════════════════════════════════
mod_faculty_eval_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # JS for rating buttons — singleton prevents duplicate injection
    singleton(tags$head(tags$script(HTML("
      function feRate(qId, val) {
        for (var i = 1; i <= 5; i++) {
          var b = document.getElementById(qId + '_b' + i);
          if (!b) continue;
          var on = (i === val);
          b.style.background  = on ? '#0066a1' : 'white';
          b.style.color       = on ? 'white'   : '#2d3748';
          b.style.borderColor = on ? '#0066a1' : '#dde5ed';
        }
        var h = document.getElementById(qId);
        if (h) h.value = val;
        Shiny.setInputValue(qId, val, {priority: 'event'});
      }
    ")))),

    # ── Summary strip ─────────────────────────────────────────────────────────
    uiOutput(ns("summary_strip")),

    # ── PGY year chart ────────────────────────────────────────────────────────
    div(class = "mt-3",
      div(class = "gmed-card",
        div(class = "card-body",
          tags$p(class = "text-muted mb-2",
                 style = "font-size:0.75rem; text-transform:uppercase; letter-spacing:.07em;",
                 "Progress by PGY Year (Goal: 25 / year)"),
          plotlyOutput(ns("chart_by_pgy"), height = "200px")
        )
      )
    ),

    # ── Faculty chips ─────────────────────────────────────────────────────────
    div(class = "mt-3",
      div(class = "gmed-card",
        div(class = "card-body",
          tags$p(class = "text-muted mb-2",
                 style = "font-size:0.75rem; text-transform:uppercase; letter-spacing:.07em;",
                 "Faculty Evaluated"),
          uiOutput(ns("faculty_chips"))
        )
      )
    ),

    # ── Evaluation form ───────────────────────────────────────────────────────
    div(class = "mt-4",
      div(class = "gmed-card",
        div(class = "card-body",
          tags$p(class = "fw-semibold mb-3",
                 style = "font-size:0.9rem; color:var(--ssm-primary-blue);",
                 tags$i(class = "bi bi-pencil-square me-2"),
                 "Submit a Faculty Evaluation"),
          uiOutput(ns("eval_form"))
        )
      )
    )
  )
}

# ═════════════════════════════════════════════════════════════════════════════
# Server
# ═════════════════════════════════════════════════════════════════════════════
mod_faculty_eval_server <- function(id, rdm_data, resident_id, faculty_roster_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Local eval cache (seeded once from rdm_data) ──────────────────────────
    local_evals <- reactiveVal(NULL)

    observe({
      req(rdm_data(), resident_id())
      if (!is.null(local_evals())) return()
      fe   <- rdm_data()$all_forms$faculty_evaluation
      mine <- if (!is.null(fe) && nrow(fe) > 0)
                fe[fe$record_id == resident_id(), , drop = FALSE]
              else data.frame()
      local_evals(mine)
    })

    my_evals <- reactive({ local_evals() %||% data.frame() })

    # ── PGY info ──────────────────────────────────────────────────────────────
    pgy_info <- reactive({
      req(rdm_data(), resident_id())
      res <- rdm_data()$residents[rdm_data()$residents$record_id == resident_id(), ]
      req(nrow(res) > 0)
      gy <- suppressWarnings(as.numeric(res$grad_yr[1]))
      tp <- suppressWarnings(as.numeric(res$type[1]))
      if (is.na(gy) || gy < 2000) {
        dd <- rdm_data()$data_dict
        if (!is.null(dd)) {
          r <- dd[dd$field_name == "grad_yr", ]
          if (nrow(r) > 0 && !is.na(r$select_choices_or_calculations[1])) {
            pairs <- strsplit(trimws(strsplit(r$select_choices_or_calculations[1], "\\|")[[1]]), ",\\s*")
            ch <- setNames(trimws(sapply(pairs, `[[`, 2)), trimws(sapply(pairs, `[[`, 1)))
            gy <- suppressWarnings(as.numeric(ch[as.character(res$grad_yr[1])]))
          }
        }
      }
      if (is.na(gy)) return(list(pgy = NA_integer_, goal = 25L, grad_yr = NA_real_))
      info <- tryCatch(calculate_pgy_and_period(gy, tp), error = function(e) NULL)
      pgy  <- if (!is.null(info)) info$pgy_year %||% 1L else 1L
      list(pgy = as.integer(pgy),
           goal = as.integer(.FE_GOALS[as.character(pgy)] %||% 25L),
           grad_yr = gy)
    })

    # ── Form state ────────────────────────────────────────────────────────────
    # eval_step: "search" | "form1" | "form2" | "form3" | "done"
    eval_step    <- reactiveVal("search")
    sel_faculty  <- reactiveVal(NULL)     # selected faculty row
    eval_type_rv <- reactiveVal(NULL)     # "attending" | "fellow"
    fac_results  <- reactiveVal(NULL)     # current search hits
    saved_rot    <- reactiveVal("")       # att_rot saved across step transitions

    reset_form <- function() {
      sel_faculty(NULL); eval_type_rv(NULL)
      fac_results(NULL); saved_rot(""); eval_step("search")
    }

    # ── Summary strip ─────────────────────────────────────────────────────────
    output$summary_strip <- renderUI({
      df   <- my_evals()
      n    <- nrow(df)
      info <- pgy_info()
      pgy  <- info$pgy
      goal <- info$goal

      n30 <- if (n > 0 && "fac_eval_date" %in% names(df)) {
        dates <- suppressWarnings(as.Date(df$fac_eval_date))
        sum(!is.na(dates) & dates >= Sys.Date() - 30)
      } else 0L

      pct <- if (goal > 0L) round(n / goal * 100) else 0L
      col <- if (n >= goal)      "var(--ssm-success-green)"
             else if (pct >= 60) "var(--ssm-secondary-blue)"
             else                "var(--ssm-warning-orange)"

      stat <- function(value, label, icon, color = "var(--ssm-primary-blue)") {
        div(class = "col-sm-4",
          div(class = "card text-center border-0 shadow-sm py-3 px-2",
              style = "background:#fff; border-radius:8px;",
              tags$i(class = paste0("bi bi-", icon),
                     style = paste0("font-size:1.5rem; color:", color, "; opacity:.85;")),
              tags$p(class = "mb-0 mt-2",
                     style = "font-size:1.8rem; font-weight:700; color:var(--ssm-primary-blue); line-height:1.1;",
                     value),
              tags$p(class = "mb-0 mt-1",
                     style = "font-size:0.72rem; color:var(--ssm-text-muted); text-transform:uppercase; letter-spacing:.07em;",
                     label)
          )
        )
      }

      div(class = "row g-3",
          stat(n, "Total Submitted", "person-check-fill", col),
          stat(if (!is.na(pgy)) paste0(n, " / ", goal) else as.character(n),
               if (!is.na(pgy)) paste0("PGY", pgy, " Goal (", goal, ")") else "Goal",
               "bullseye", col),
          stat(n30, "Last 30 Days", "calendar-check-fill",
               if (n30 > 0) "var(--ssm-secondary-blue)" else "var(--ssm-text-muted)")
      )
    })

    # ── PGY year chart ────────────────────────────────────────────────────────
    output$chart_by_pgy <- renderPlotly({
      df   <- my_evals()
      info <- pgy_info()
      gy   <- info$grad_yr
      if (is.na(gy)) return(.fe_empty_plot("PGY year data unavailable"))

      goal  <- 25L
      today <- Sys.Date()
      wins  <- lapply(1:3, function(p) list(
        pgy = p, label = paste0("PGY ", p),
        start = as.Date(paste0(gy - (4L - p), "-07-01")),
        end   = as.Date(paste0(gy - (3L - p), "-06-30"))
      ))
      wins <- Filter(function(w) w$start <= today, wins)
      if (length(wins) == 0) return(.fe_empty_plot("Residency not yet started"))

      all_dates <- suppressWarnings(as.Date(df$fac_eval_date))
      dated     <- all_dates[!is.na(all_dates)]

      counts <- sapply(wins, function(w) sum(dated >= w$start & dated <= w$end))
      labels  <- sapply(wins, `[[`, "label")
      cur_pgy <- info$pgy

      # Evals with no date OR a date outside every PGY window
      in_any_window <- Reduce(`|`, lapply(wins, function(w) !is.na(all_dates) & all_dates >= w$start & all_dates <= w$end))
      n_historical  <- nrow(df) - sum(in_any_window, na.rm = TRUE)

      # Append historical bar if any exist
      if (n_historical > 0) {
        labels <- c(labels, "Historical /\nUndated")
        counts <- c(counts, n_historical)
      }

      cols <- c(
        sapply(seq_along(wins), function(i) {
          if (counts[i] >= goal)             "#2e7d32"
          else if (wins[[i]]$pgy == cur_pgy) "#0066a1"
          else                               "#adb5bd"
        }),
        if (n_historical > 0) "#c8a97e" else character(0)   # warm tan for historical
      )

      bar_text <- c(
        paste0(counts[seq_along(wins)], " / ", goal),
        if (n_historical > 0) as.character(n_historical) else character(0)
      )

      plot_ly() %>%
        add_trace(
          x = labels, y = counts, type = "bar",
          marker = list(color = cols),
          text = bar_text, textposition = "outside",
          textfont = list(size = 12, color = "#2d3748"),
          hovertemplate = paste0("<b>%{x}</b><br>%{y} evaluations<extra></extra>")
        ) %>%
        layout(
          shapes = list(list(
            type = "line", xref = "paper", yref = "y",
            x0 = 0, x1 = 1, y0 = goal, y1 = goal,
            line = list(color = "#d32f2f", width = 2, dash = "dash")
          )),
          annotations = list(list(
            xref = "paper", yref = "y", x = 0.99, y = goal,
            text = paste0("Goal: ", goal),
            xanchor = "right", yanchor = "bottom",
            showarrow = FALSE, font = list(size = 10, color = "#d32f2f")
          )),
          xaxis = list(title = "", fixedrange = TRUE),
          yaxis = list(title = "", rangemode = "tozero",
                       range = list(0, max(goal * 1.35, max(counts, 0) * 1.35, 5)),
                       dtick = 5, fixedrange = TRUE),
          margin = list(l=10, r=10, t=20, b=10),
          plot_bgcolor  = "rgba(0,0,0,0)",
          paper_bgcolor = "rgba(0,0,0,0)"
        ) %>%
        plotly::config(displayModeBar = FALSE)
    })

    # ── Faculty chips ─────────────────────────────────────────────────────────
    output$faculty_chips <- renderUI({
      df <- my_evals()
      if (nrow(df) == 0 || !"fac_fell_name" %in% names(df))
        return(tags$p(class = "text-muted fst-italic", style = "font-size:0.85rem;",
                      "No evaluations submitted yet."))
      nms <- sort(unique(df$fac_fell_name[!is.na(df$fac_fell_name) & nzchar(df$fac_fell_name)]))
      if (length(nms) == 0)
        return(tags$p(class = "text-muted fst-italic", style = "font-size:0.85rem;",
                      "No faculty names recorded."))
      div(lapply(nms, function(nm) {
        cnt <- sum(df$fac_fell_name == nm, na.rm = TRUE)
        tags$span(
          style = "display:inline-block; background:#e8f0f7; color:#003d5c;
                   border-radius:20px; padding:4px 14px; font-size:0.8rem;
                   font-weight:600; margin:3px;",
          nm,
          if (cnt > 1) tags$span(
            style = "margin-left:5px; background:#b8d0e8; color:#003d5c;
                     border-radius:10px; padding:1px 7px;
                     font-size:0.68rem; font-weight:700; vertical-align:middle;",
            paste0(cnt, "\u00d7")
          )
        )
      }))
    })

    # ── Search results (separate output so typing doesn't lose focus) ─────────
    output$search_results <- renderUI({
      term <- trimws(input$fac_search %||% "")
      if (nchar(term) < 2)
        return(tags$p(style = "font-size:0.82rem; color:#888; margin:0;",
                      "Type at least 2 characters\u2026"))
      roster <- faculty_roster_r()
      if (is.null(roster) || nrow(roster) == 0)
        return(tags$p(style = "color:#c00; font-size:0.82rem;", "Faculty roster unavailable."))

      hits <- roster[grepl(term, roster$fac_name, ignore.case = TRUE), , drop = FALSE]
      hits <- head(hits, 10)
      fac_results(hits)

      if (nrow(hits) == 0)
        return(tags$p(style = "font-size:0.82rem; color:#888; margin:0;",
                      "No faculty found \u2014 use \u201cCan\u2019t find?\u201d below."))

      div(
        class = "list-group mt-2",
        lapply(seq_len(nrow(hits)), function(i) {
          fac   <- hits[i, ]
          lbl   <- fac$fac_name
          if (!is.null(fac$fac_div) && !is.na(fac$fac_div) && nzchar(fac$fac_div) &&
              !grepl("^[0-9]+$", as.character(fac$fac_div)))
            lbl <- paste0(lbl, " \u2014 ", fac$fac_div)
          if (!is.null(fac$fac_fell) && !is.na(fac$fac_fell))
            lbl <- paste0(lbl, " (",
                          if (fac$fac_fell == 1) "Attending" else if (fac$fac_fell == 2) "Fellow" else "", ")")
          tags$button(
            type = "button",
            class = "list-group-item list-group-item-action py-2",
            style = "font-size:0.88rem; text-align:left;",
            onclick = sprintf("Shiny.setInputValue('%s',%d,{priority:'event'})", ns("fac_pick"), i),
            lbl
          )
        })
      )
    })

    # ── Evaluation form (multi-step) ──────────────────────────────────────────
    rot_choices <- c(
      "Choose rotation\u2026" = "", "SLUH Floors" = "1", "VA Floors" = "2",
      "Continuity Clinic" = "3", "Night Float" = "4", "Acute Care" = "5",
      "Bridge Clinic" = "6", "Bronze" = "7", "Cards (inpatient)" = "8",
      "MICU" = "9", "Specialty Specific Rotation" = "10", "Outpatient Clinic" = "11"
    )

    output$eval_form <- renderUI({
      step <- eval_step()

      # ── SEARCH ───────────────────────────────────────────────────────────────
      if (step == "search") {
        return(tagList(
          div(class = "mb-2",
              textInput(ns("fac_search"), label = NULL,
                        placeholder = "Type name to search faculty\u2026",
                        width = "100%")),
          uiOutput(ns("search_results")),
          tags$hr(style = "margin:14px 0 10px;"),
          div(
            class = "form-check",
            tags$input(type = "checkbox", class = "form-check-input",
                       id = ns("show_pending"),
                       onclick = sprintf(
                         "document.getElementById('%s').style.display=this.checked?'block':'none'",
                         ns("pending_panel")
                       )),
            tags$label(class = "form-check-label fst-italic",
                       `for` = ns("show_pending"),
                       style = "font-size:0.83rem; color:#666;",
                       "Can\u2019t find the attending or fellow? Add them here.")
          ),
          div(id = ns("pending_panel"), style = "display:none; margin-top:12px;",
              div(class = "border rounded p-3",
                  style = "background:#f9fafb;",
                  fluidRow(
                    column(6, textInput(ns("pend_name"), "Full Name",
                                        placeholder = "Full name")),
                    column(6, selectInput(ns("pend_type"), "Type",
                                          choices = c("Choose\u2026" = "",
                                                      "Attending" = "1",
                                                      "Fellow" = "2")))
                  ),
                  textInput(ns("pend_rot"), "Where did you work together?",
                            placeholder = "e.g., Bronze, VA, MICU\u2026",
                            width = "100%"),
                  actionButton(ns("add_pending"), "Create Evaluation",
                               class = "btn btn-warning btn-sm"),
                  tags$p(class = "mb-0 mt-2",
                         style = "font-size:0.76rem; color:#888;",
                         "This person will be added for administrative review.")
              )
          )
        ))
      }

      fac   <- sel_faculty()
      etype <- eval_type_rv()

      # ── FORM 1: Type / rotation / plus / delta ───────────────────────────────
      if (step == "form1") {
        return(tagList(
          div(class = "alert alert-primary py-2 px-3 mb-3",
              style = "border-radius:8px; font-size:0.9rem;",
              tags$i(class = "bi bi-person-badge me-1"),
              tags$strong(if (!is.null(fac)) fac$fac_name else "Faculty")),

          # Type selection
          if (is.null(etype)) {
            div(class = "mb-3",
                tags$p(style = "font-weight:600; font-size:0.88rem; margin-bottom:8px;",
                       "Are you evaluating an attending or fellow?"),
                div(class = "d-flex gap-2",
                    actionButton(ns("sel_attending"), "Attending",
                                 class = "btn btn-outline-primary btn-sm"),
                    actionButton(ns("sel_fellow"), "Fellow",
                                 class = "btn btn-outline-success btn-sm")
                )
            )
          } else {
            div(class = "mb-3",
                div(class = "badge",
                    style = "background:#e8f5e9; color:#2e7d32; font-size:0.82rem;
                             padding:6px 14px; border-radius:20px; font-weight:600;",
                    if (etype == "attending") "Evaluating: Attending" else "Evaluating: Fellow"))
          },

          # Fellow type dropdown (fellows only)
          if (!is.null(etype) && etype == "fellow")
            div(class = "mb-3",
                selectInput(ns("fell_eval"), "Fellow evaluation type:",
                            choices = c("Choose\u2026" = "",
                                        "As a consultant" = "1",
                                        "Inpatient service (cards, bronze, ICU)" = "2",
                                        "Consult service (ID, Nephro)" = "3",
                                        "Outpatient rotation" = "4"))),

          # Rotation (attendings only)
          if (!is.null(etype) && etype == "attending")
            div(class = "mb-3",
                selectInput(ns("att_rot"), "Rotation:", choices = rot_choices)),

          div(class = "mb-3",
              textAreaInput(ns("plus"), "What went well? (Plus)", rows = 3,
                            placeholder = "Positive aspects of working with this faculty member\u2026")),
          div(class = "mb-3",
              textAreaInput(ns("delta"), "What could be improved? (Delta)", rows = 3,
                            placeholder = "Areas for improvement or development\u2026")),

          div(class = "d-flex gap-2",
              actionButton(ns("back_to_search"), "Back",
                           class = "btn btn-outline-secondary btn-sm"),
              actionButton(ns("continue_form1"), "Continue",
                           class = "btn btn-primary btn-sm"))
        ))
      }

      is_short <- saved_rot() %in% c("4", "5", "6", "11")

      # ── FORM 2: Rating scales part 1 (attendings) ────────────────────────────
      if (step == "form2") {
        qs <- if (is_short) {
          tagList(
            .fe_rating_q(ns, "approachability",
                         "How approachable is the attending to address questions?",
                         "Difficult / intimidating", "Easy to approach"),
            .fe_rating_q(ns, "ques_clin_des",
                         "Does the attending ask questions about your clinical decisions?",
                         "Never", "Always"),
            .fe_rating_q(ns, "feedback",
                         "The attending provided feedback that helped me improve",
                         "Never", "Always \u2014 actionable")
          )
        } else {
          tagList(
            .fe_rating_q(ns, "approachability",
                         "How approachable is the attending to address questions?",
                         "Difficult / intimidating", "Easy to approach"),
            .fe_rating_q(ns, "respect",
                         "Is the attending respectful of all team members?",
                         "Not respectful", "Consistently respectful"),
            .fe_rating_q(ns, "bedside_manner",
                         "Does the attending role model good bedside manner?",
                         "Does not role model", "Consistently models"),
            .fe_rating_q(ns, "time_teaching",
                         "Does the attending ensure time for teaching?",
                         "Never", "Always"),
            .fe_rating_q(ns, "ques_clin_des",
                         "Does the attending ask questions about your clinical decisions?",
                         "Never", "Always"),
            .fe_rating_q(ns, "autonomy",
                         "The attending allowed me autonomy in clinical decision making",
                         "Never allowed", "Consistently allowed"),
            .fe_rating_q(ns, "feedback",
                         "The attending provided feedback that helped me improve",
                         "Never", "Always \u2014 actionable"),
            .fe_rating_q(ns, "organ",
                         "Clinical sessions were organized",
                         "Consistently disorganized", "Consistently organized")
          )
        }
        return(tagList(
          tags$h6("Teaching Evaluation \u2014 Part 1",
                  style = "font-weight:700; color:var(--ssm-primary-blue); margin-bottom:16px;"),
          qs,
          div(class = "d-flex gap-2 mt-2",
              actionButton(ns("back_to_form1"), "Back",
                           class = "btn btn-outline-secondary btn-sm"),
              actionButton(ns("continue_form2"), "Continue",
                           class = "btn btn-primary btn-sm"))
        ))
      }

      # ── FORM 3: Final selects + overall rating + submit ───────────────────────
      if (step == "form3") {
        return(tagList(
          tags$h6("Teaching Evaluation \u2014 Part 2",
                  style = "font-weight:700; color:var(--ssm-primary-blue); margin-bottom:16px;"),

          if (!is_short)
            div(class = "mb-3",
                selectInput(ns("att_ext_tea"),
                            "The attending gave teaching outside of formal rounds:",
                            choices = c("Choose\u2026" = "",
                                        "Strongly agree" = "1", "Agree" = "2",
                                        "Neither agree nor disagree" = "3",
                                        "Disagree" = "4", "Strongly disagree" = "5"))),

          div(class = "mb-3",
              selectInput(ns("att_give_feed"),
                          "How did the attending give feedback outside of rounds?",
                          choices = c("Choose\u2026" = "",
                                      "Verbal feedback only" = "1",
                                      "Written feedback only" = "2",
                                      "Both oral and written" = "3",
                                      "I did not receive any feedback" = "4"))),

          div(class = "mb-3",
              selectInput(ns("eval_done_q"),
                          "Do you know if the attending completed an evaluation for you?",
                          choices = c("Choose\u2026" = "",
                                      "Yes" = "1", "No" = "2", "I don\u2019t know" = "3"))),

          .fe_rating_q(ns, "att_overall",
                       "Overall teaching rating for this attending:",
                       "Needs improvement", "Outstanding"),

          div(class = "d-flex gap-2 mt-2",
              actionButton(ns("back_to_form2"), "Back",
                           class = "btn btn-outline-secondary btn-sm"),
              actionButton(ns("submit_eval"), "Submit Evaluation",
                           class = "btn btn-success btn-sm"))
        ))
      }

      # ── DONE ─────────────────────────────────────────────────────────────────
      if (step == "done") {
        return(div(
          class = "text-center py-4",
          tags$i(class = "bi bi-check-circle-fill",
                 style = "font-size:3rem; color:var(--ssm-success-green);"),
          tags$h5(class = "mt-3 mb-1", "Evaluation Submitted!"),
          tags$p(class = "text-muted mb-4", style = "font-size:0.88rem;",
                 "Thank you for your feedback."),
          div(class = "d-flex gap-2 justify-content-center",
              actionButton(ns("eval_another"), "Evaluate Another",
                           class = "btn btn-primary btn-sm"),
              actionButton(ns("eval_done"),    "Done",
                           class = "btn btn-outline-secondary btn-sm"))
        ))
      }
    })

    # ── Faculty pick ──────────────────────────────────────────────────────────
    observeEvent(input$fac_pick, {
      hits <- fac_results()
      req(!is.null(hits), input$fac_pick >= 1L, input$fac_pick <= nrow(hits))
      fac <- hits[input$fac_pick, ]
      sel_faculty(fac)
      # Auto-detect type from fac_fell field
      etype <- NULL
      if ("fac_fell" %in% names(fac) && !is.na(fac$fac_fell)) {
        ff <- suppressWarnings(as.integer(fac$fac_fell))
        if (!is.na(ff)) etype <- if (ff == 1L) "attending" else if (ff == 2L) "fellow" else NULL
      }
      eval_type_rv(etype)
      eval_step("form1")
    })

    # ── Type buttons ──────────────────────────────────────────────────────────
    observeEvent(input$sel_attending, { eval_type_rv("attending") })
    observeEvent(input$sel_fellow,    { eval_type_rv("fellow") })

    # ── Back buttons ──────────────────────────────────────────────────────────
    observeEvent(input$back_to_search, { reset_form() })
    observeEvent(input$back_to_form1,  { eval_step("form1") })
    observeEvent(input$back_to_form2,  { eval_step("form2") })

    # ── Continue from form1 ───────────────────────────────────────────────────
    observeEvent(input$continue_form1, {
      etype <- eval_type_rv()
      if (is.null(etype)) {
        showNotification("Please select Attending or Fellow", type = "warning"); return()
      }
      plus  <- trimws(input$plus  %||% "")
      delta <- trimws(input$delta %||% "")
      if (!nzchar(plus))  { showNotification("Please fill in what went well (Plus)",    type = "warning"); return() }
      if (!nzchar(delta)) { showNotification("Please fill in what could improve (Delta)", type = "warning"); return() }

      if (etype == "attending") {
        rot <- input$att_rot %||% ""
        if (!nzchar(rot)) { showNotification("Please select a rotation", type = "warning"); return() }
        saved_rot(rot)
        eval_step("form2")
      } else {
        if (is.null(input$fell_eval) || !nzchar(input$fell_eval))
          { showNotification("Please select the fellow evaluation type", type = "warning"); return() }
        do_submit()
      }
    })

    # ── Continue from form2 ───────────────────────────────────────────────────
    observeEvent(input$continue_form2, {
      is_short <- saved_rot() %in% c("4", "5", "6", "11")
      required <- if (is_short) c("approachability", "ques_clin_des", "feedback")
                  else c("approachability", "respect", "bedside_manner", "time_teaching",
                         "ques_clin_des", "autonomy", "feedback", "organ")
      missing <- required[sapply(required, function(f) is.null(input[[f]]) || input[[f]] == "")]
      if (length(missing) > 0) {
        showNotification("Please answer all rating questions", type = "warning"); return()
      }
      eval_step("form3")
    })

    # ── Submit from form3 ─────────────────────────────────────────────────────
    observeEvent(input$submit_eval, {
      is_short <- saved_rot() %in% c("4", "5", "6", "11")
      req_rate <- c("att_overall",
                    if (is_short) c("approachability", "ques_clin_des", "feedback")
                    else c("approachability", "respect", "bedside_manner", "time_teaching",
                           "ques_clin_des", "autonomy", "feedback", "organ"))
      req_sel  <- c("att_give_feed", "eval_done_q", if (!is_short) "att_ext_tea")
      all_req  <- c(req_rate, req_sel)
      missing  <- all_req[sapply(all_req, function(f) is.null(input[[f]]) || input[[f]] == "")]
      if (length(missing) > 0) {
        showNotification("Please answer all questions before submitting", type = "warning"); return()
      }
      do_submit()
    })

    # ── Core submission ───────────────────────────────────────────────────────
    do_submit <- function() {
      fac   <- sel_faculty()
      etype <- eval_type_rv()
      info  <- pgy_info()
      rid   <- resident_id()
      is_short <- saved_rot() %in% c("4", "5", "6", "11")

      tryCatch({
        ed <- list(
          fac_fell_name = fac$fac_name,
          att_or_fell   = if (etype == "attending") "1" else "2",
          plus          = input$plus,
          delta         = input$delta
        )
        if (etype == "fellow") {
          ed$att_rot   <- input$fell_eval
          ed$fell_eval <- input$fell_eval
        } else {
          ed$att_rot        <- saved_rot()
          ed$approachability <- input$approachability
          ed$ques_clin_des   <- input$ques_clin_des
          ed$feedback        <- input$feedback
          ed$att_give_feed   <- input$att_give_feed
          ed$eval_done       <- input$eval_done_q
          ed$att_overall     <- input$att_overall
          if (!is_short) {
            ed$respect        <- input$respect
            ed$bedside_manner <- input$bedside_manner
            ed$time_teaching  <- input$time_teaching
            ed$autonomy       <- input$autonomy
            ed$organ          <- input$organ
            ed$att_ext_tea    <- input$att_ext_tea
          }
        }

        .fe_submit_eval(rid, info$pgy, ed)

        # Update local cache so chart + chips refresh without full reload
        new_row <- data.frame(
          record_id     = as.character(rid),
          fac_fell_name = fac$fac_name,
          fac_eval_date = format(Sys.Date(), "%Y-%m-%d"),
          att_or_fell   = if (etype == "attending") "1" else "2",
          stringsAsFactors = FALSE
        )
        local_evals(dplyr::bind_rows(local_evals() %||% data.frame(), new_row))

        eval_step("done")
      }, error = function(e) {
        showNotification(paste("Submission failed:", e$message), type = "error", duration = 8)
      })
    }

    # ── Pending faculty ───────────────────────────────────────────────────────
    observeEvent(input$add_pending, {
      name <- trimws(input$pend_name %||% "")
      type <- input$pend_type %||% ""
      rot  <- trimws(input$pend_rot %||% "")
      if (!nzchar(name)) { showNotification("Please enter a name", type = "warning"); return() }
      if (!nzchar(type)) { showNotification("Please select Attending or Fellow", type = "warning"); return() }
      tryCatch({
        .fe_submit_pending(name, type, rot)
        showNotification(paste(name, "added for review"), type = "message")
        pending_fac <- data.frame(
          fac_name = name, fac_div = NA_character_,
          fac_fell = as.integer(type), stringsAsFactors = FALSE
        )
        sel_faculty(pending_fac)
        eval_type_rv(if (type == "1") "attending" else "fellow")
        eval_step("form1")
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })

    # ── Post-submission ───────────────────────────────────────────────────────
    observeEvent(input$eval_another, { reset_form() })
    observeEvent(input$eval_done,    { reset_form() })

  })
}
