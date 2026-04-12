# mod_evaluations.R ─ My Evaluations
# Layout:
#   1. Summary strip  — total evals, unique faculty, date range + per-year chips
#   2. CC Completion  — mod_cc_completion (accordion, PGY-aware)
#   3. Eval Feedback  — mod_eval_feedback (type counts + filters + plus/delta + scores)

mod_evaluations_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # 1 ── Summary strip ────────────────────────────────────────────────────────
    uiOutput(ns("summary_strip")),

    # 2 ── CC Completion ────────────────────────────────────────────────────────
    div(class = "mt-4",
      mod_cc_completion_ui(ns("cc"))
    ),

    # 3 ── Eval Feedback (breakdown + written feedback + scores) ────────────────
    div(class = "mt-4",
      mod_eval_feedback_ui(ns("feedback"))
    )
  )
}

mod_evaluations_server <- function(id, rdm_data, resident_id) {
  moduleServer(id, function(input, output, session) {

    # ── Normalise instrument casing ───────────────────────────────────────────
    assessment_data <- reactive({
      req(rdm_data())
      df <- rdm_data()$all_forms$assessment
      if (!is.null(df) && "redcap_repeat_instrument" %in% names(df))
        df$redcap_repeat_instrument <- tolower(trimws(df$redcap_repeat_instrument))
      df
    })

    data_dict_r <- reactive({
      req(rdm_data())
      rdm_data()$data_dict
    })

    my_assessments <- reactive({
      req(assessment_data(), resident_id())
      assessment_data() %>%
        dplyr::filter(
          record_id == resident_id(),
          tolower(trimws(redcap_repeat_instrument)) == "assessment"
        )
    })

    # ── 1. Summary strip ──────────────────────────────────────────────────────
    output$summary_strip <- renderUI({
      df        <- my_assessments()
      n_evals   <- if (!is.null(df)) nrow(df) else 0
      n_faculty <- if (n_evals > 0)
        length(unique(df$ass_faculty[!is.na(df$ass_faculty)])) else 0

      acad_year_label <- function(d) {
        d <- as.Date(d); yr <- as.integer(format(d, "%Y"))
        mo <- as.integer(format(d, "%m"))
        start <- ifelse(mo >= 7L, yr, yr - 1L)
        paste0(start, "\u2013", start + 1L)
      }

      year_chips <- if (n_evals > 0) {
        dates <- as.Date(df$ass_date[!is.na(df$ass_date)])
        dates <- dates[!is.na(dates)]
        if (length(dates) > 0) {
          tbl <- sort(table(acad_year_label(dates)), decreasing = TRUE)
          div(style = "margin-top:6px;",
            lapply(names(tbl), function(yr)
              tags$span(
                style = paste0("display:inline-block; background:#e8f0f7; color:#003d5c;",
                               "border-radius:20px; padding:3px 12px; font-size:0.78rem;",
                               "font-weight:600; margin:2px;"),
                paste0(yr, ": ", tbl[[yr]])
              )
            )
          )
        }
      }

      stat_card <- function(value, label, icon_name,
                            color = "var(--ssm-primary-blue)", extra = NULL) {
        div(class = "col-sm-4",
          div(
            class = "card text-center border-0 shadow-sm py-3 px-2",
            style = "background:#fff; border-radius:8px;",
            tags$i(class  = paste0("bi bi-", icon_name),
                   style  = paste0("font-size:1.6rem; color:", color, "; opacity:0.8;")),
            tags$p(class  = "mb-0 mt-2",
                   style  = "font-size:1.9rem; font-weight:700; color:var(--ssm-primary-blue); line-height:1.1;",
                   value),
            tags$p(class  = "mb-0 mt-1",
                   style  = "font-size:0.75rem; color:var(--ssm-text-muted); text-transform:uppercase; letter-spacing:.07em;",
                   label),
            extra
          )
        )
      }

      div(class = "row g-3",
        stat_card(n_evals,   "Total Evaluations", "clipboard2-check-fill",
                  extra = year_chips),
        stat_card(n_faculty, "Unique Faculty",    "people-fill"),
        stat_card(
          {
            dates <- as.Date(df$ass_date[!is.na(df$ass_date)])
            dates <- dates[!is.na(dates)]
            if (length(dates) > 0)
              paste0(format(min(dates), "%b %Y"), " \u2013 ", format(max(dates), "%b %Y"))
            else "\u2014"
          },
          "Date Range", "calendar3-fill",
          color = "var(--ssm-secondary-blue)"
        )
      )
    })

    # ── 2. CC Completion ──────────────────────────────────────────────────────
    resident_info_r <- reactive({
      req(rdm_data(), resident_id())
      info <- rdm_data()$residents %>% dplyr::filter(record_id == resident_id())
      if (nrow(info) == 0) return(info)
      calculate_resident_level(info)
    })

    mod_cc_completion_server(
      id            = "cc",
      rdm_data      = assessment_data,
      record_id     = resident_id,
      resident_data = resident_info_r
    )

    # ── 3. Eval Feedback module ───────────────────────────────────────────────
    mod_eval_feedback_server(
      id              = "feedback",
      assessment_data = assessment_data,
      record_id       = resident_id,
      data_dict       = data_dict_r
    )

  })
}
