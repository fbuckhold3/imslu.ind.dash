# mod_milestones.R ─ Milestone Viewer
#
# Top: Period selector (defaults to most recent) + Self & ACGME spiders side-by-side
# Bottom: Individual progression chart — raw 1-9 y-axis, checkboxes for Self/ACGME
#
# Progression methodology: Park et al. (2021) JAMA Network Open
#   Quadratic program growth curve + 95% CI ribbon; individual observed + linear projection

# ── Constants ─────────────────────────────────────────────────────────────────

.MILE_PERIODS <- c(
  "1" = "Mid Intern",
  "2" = "End Intern",
  "3" = "Mid PGY2",
  "4" = "End PGY2",
  "5" = "Mid PGY3",
  "6" = "Graduating"
)

.PERIOD_ORD <- unname(.MILE_PERIODS)

.PERIOD_NUM <- c(
  "Mid Intern"  = 1,
  "End Intern"  = 2,
  "Mid PGY2"    = 3,
  "End PGY2"    = 4,
  "Mid PGY3"    = 5,
  "Graduating"  = 6
)

.MILE_LABELS <- c(
  rep_pc1   = "PC1: History",
  rep_pc2   = "PC2: Physical Examination",
  rep_pc3   = "PC3: Clinical Reasoning",
  rep_pc4   = "PC4: Patient Management \u2014 Inpatient",
  rep_pc5   = "PC5: Patient Management \u2014 Outpatient",
  rep_pc6   = "PC6: Digital Health",
  rep_mk1   = "MK1: Applied Foundational Sciences",
  rep_mk2   = "MK2: Therapeutic Knowledge",
  rep_mk3   = "MK3: Knowledge of Diagnostic Testing",
  rep_sbp1  = "SBP1: Patient Safety and Quality Improvement",
  rep_sbp2  = "SBP2: System Navigation for Patient-Centered Care",
  rep_sbp3  = "SBP3: Physician Role in Health Care Systems",
  rep_pbl1  = "PBL1: Evidence-Based and Informed Practice",
  rep_pbl2  = "PBL2: Reflective Practice and Commitment to Personal Growth",
  rep_prof1 = "PROF1: Professional Behavior",
  rep_prof2 = "PROF2: Ethical Principles",
  rep_prof3 = "PROF3: Accountability/Conscientiousness",
  rep_prof4 = "PROF4: Knowledge of Systemic and Individual Factors of Well-Being",
  rep_ics1  = "ICS1: Patient- and Family-Centered Communication",
  rep_ics2  = "ICS2: Interprofessional and Team Communication",
  rep_ics3  = "ICS3: Communication within Health Care Systems",
  rep_pc1_self   = "PC1: History (Self)",
  rep_pc2_self   = "PC2: Physical Examination (Self)",
  rep_pc3_self   = "PC3: Clinical Reasoning (Self)",
  rep_pc4_self   = "PC4: Patient Management \u2014 Inpatient (Self)",
  rep_pc5_self   = "PC5: Patient Management \u2014 Outpatient (Self)",
  rep_pc6_self   = "PC6: Digital Health (Self)",
  rep_mk1_self   = "MK1: Applied Foundational Sciences (Self)",
  rep_mk2_self   = "MK2: Therapeutic Knowledge (Self)",
  rep_mk3_self   = "MK3: Knowledge of Diagnostic Testing (Self)",
  rep_sbp1_self  = "SBP1: Patient Safety and Quality Improvement (Self)",
  rep_sbp2_self  = "SBP2: System Navigation for Patient-Centered Care (Self)",
  rep_sbp3_self  = "SBP3: Physician Role in Health Care Systems (Self)",
  rep_pbl1_self  = "PBL1: Evidence-Based and Informed Practice (Self)",
  rep_pbl2_self  = "PBL2: Reflective Practice and Commitment to Personal Growth (Self)",
  rep_prof1_self = "PROF1: Professional Behavior (Self)",
  rep_prof2_self = "PROF2: Ethical Principles (Self)",
  rep_prof3_self = "PROF3: Accountability/Conscientiousness (Self)",
  rep_prof4_self = "PROF4: Knowledge of Systemic and Individual Factors of Well-Being (Self)",
  rep_ics1_self  = "ICS1: Patient- and Family-Centered Communication (Self)",
  rep_ics2_self  = "ICS2: Interprofessional and Team Communication (Self)",
  rep_ics3_self  = "ICS3: Communication within Health Care Systems (Self)",
  acgme_pc1   = "PC1: History",
  acgme_pc2   = "PC2: Physical Examination",
  acgme_pc3   = "PC3: Clinical Reasoning",
  acgme_pc4   = "PC4: Patient Management \u2014 Inpatient",
  acgme_pc5   = "PC5: Patient Management \u2014 Outpatient",
  acgme_pc6   = "PC6: Digital Health",
  acgme_mk1   = "MK1: Applied Foundational Sciences",
  acgme_mk2   = "MK2: Therapeutic Knowledge",
  acgme_mk3   = "MK3: Knowledge of Diagnostic Testing",
  acgme_sbp1  = "SBP1: Patient Safety and Quality Improvement",
  acgme_sbp2  = "SBP2: System Navigation for Patient-Centered Care",
  acgme_sbp3  = "SBP3: Physician Role in Health Care Systems",
  acgme_pbl1  = "PBL1: Evidence-Based and Informed Practice",
  acgme_pbl2  = "PBL2: Reflective Practice and Commitment to Personal Growth",
  acgme_prof1 = "PROF1: Professional Behavior",
  acgme_prof2 = "PROF2: Ethical Principles",
  acgme_prof3 = "PROF3: Accountability/Conscientiousness",
  acgme_prof4 = "PROF4: Knowledge of Systemic and Individual Factors of Well-Being",
  acgme_ics1  = "ICS1: Patient- and Family-Centered Communication",
  acgme_ics2  = "ICS2: Interprofessional and Team Communication",
  acgme_ics3  = "ICS3: Communication within Health Care Systems"
)

# ── Column helpers ─────────────────────────────────────────────────────────────

.mile_label <- function(col) {
  lbl <- .MILE_LABELS[col]
  if (!is.na(lbl)) lbl else col
}

.mile_choices <- function(cols) {
  lbls <- vapply(cols, .mile_label, character(1))
  stats::setNames(cols, lbls)
}

# rep_pc1 / acgme_pc1 / rep_pc1_self → "PC1";  rep_pbl1 → "PBLI1"
.col_to_comp_code <- function(col) {
  bare <- sub("^(rep|acgme)_", "", col)
  bare <- sub("_self$", "", bare)
  dom  <- gsub("\\d+$",   "", bare)
  num  <- gsub("^[a-z]+", "", bare)
  switch(dom,
    "pc"   = paste0("PC",   num),
    "mk"   = paste0("MK",   num),
    "sbp"  = paste0("SBP",  num),
    "pbl"  = paste0("PBLI", num),
    "prof" = paste0("PROF", num),
    "ics"  = paste0("ICS",  num),
    NULL
  )
}

# Pre-filter form_data to one period label before passing to spider function
.filter_period <- function(form_data, period_label) {
  code <- names(.MILE_PERIODS)[.MILE_PERIODS == period_label]
  if (!length(code)) return(form_data)
  pf <- if ("acgme_mile_period"      %in% names(form_data)) "acgme_mile_period"
        else if ("prog_mile_period_self" %in% names(form_data)) "prog_mile_period_self"
        else if ("prog_mile_period"      %in% names(form_data)) "prog_mile_period"
        else return(form_data)
  form_data[!is.na(form_data[[pf]]) &
              as.character(suppressWarnings(as.integer(form_data[[pf]]))) == code,
            , drop = FALSE]
}

# Periods where a resident has data in a given form
.avail_periods_for <- function(form_data, res_id, period_field) {
  if (is.null(form_data) || nrow(form_data) == 0) return(character(0))
  if (!period_field %in% names(form_data))         return(character(0))
  mine  <- form_data[as.character(form_data$record_id) == as.character(res_id), ]
  if (nrow(mine) == 0) return(character(0))
  codes <- sort(unique(suppressWarnings(as.integer(mine[[period_field]]))))
  codes <- codes[!is.na(codes)]
  lbls  <- .MILE_PERIODS[as.character(codes)]
  lbls[!is.na(lbls)]
}

# ── Read-only ILP behavioral anchor table ─────────────────────────────────────

.mile_anchor_table <- function(comp_code, dd) {
  tbl <- tryCatch(.get_milestone_table(comp_code, dd), error = function(e) NULL)
  if (is.null(tbl) || nrow(tbl) == 0) return(NULL)

  lv_styles <- c(
    "1" = "background:#e8f5e9; color:#1b5e20;",
    "2" = "background:#e3f2fd; color:#0d47a1;",
    "3" = "background:#fff3e0; color:#e65100;",
    "4" = "background:#fce4ec; color:#880e4f;",
    "5" = "background:#ede7f6; color:#4a148c;"
  )
  lv_labels <- c("1" = "Novice", "2" = "Adv Beginner", "3" = "Competent",
                 "4" = "Proficient", "5" = "Expert")

  div(
    class = "mt-2 mb-2",
    tags$p(
      style = "font-size:0.68rem; font-weight:700; text-transform:uppercase;
               letter-spacing:.06em; color:#6c757d; margin-bottom:4px;",
      paste0(comp_code, " \u2014 Behavioral Anchors")
    ),
    div(
      class = "table-responsive",
      tags$table(
        class = "table table-bordered mb-0",
        style = "font-size:0.68rem; min-width:520px;",
        tags$thead(
          tags$tr(
            tags$th(style = "width:22px; text-align:center; background:#f8fafc;
                             color:#6c757d; padding:3px;", "#"),
            lapply(as.character(1:5), function(l)
              tags$th(style = paste0(lv_styles[[l]],
                                     " text-align:center; padding:3px 5px;"),
                      lv_labels[[l]])
            )
          )
        ),
        tags$tbody(
          lapply(seq_len(nrow(tbl)), function(i) {
            tags$tr(
              tags$td(style = "text-align:center; font-weight:700; color:#9e9e9e;
                               padding:3px;",
                      as.character(tbl$Row[i])),
              lapply(as.character(1:5), function(l)
                tags$td(style = "vertical-align:top; padding:4px 5px;
                                 line-height:1.3; color:#444;",
                        tbl[[paste0("Level_", l)]][i])
              )
            )
          })
        )
      )
    )
  )
}

# ── Combined progression plot ─────────────────────────────────────────────────
#
# Plots raw scores on a fixed 1-9 y-axis.  Each dataset gets its own dashed
# graduation-target reference line (Self target=7, ACGME target=4).
#
# datasets: named list; each entry:
#   data    - data.frame
#   col     - milestone column
#   system  - "rep" or "acgme"
#   label   - legend label
#   color   - hex color
#   rgba_rb - fill color for CI ribbon
#   rgba_pr - color for projected line
# ─────────────────────────────────────────────────────────────────────────────
.milestone_prog_plot_combined <- function(datasets, resident_id,
                                          resident_name = NULL) {
  if (!length(datasets)) return(NULL)

  .period_field <- function(data) {
    if ("acgme_mile_period"      %in% names(data)) "acgme_mile_period"
    else if ("prog_mile_period_self" %in% names(data)) "prog_mile_period_self"
    else "prog_mile_period"
  }

  name_lbl        <- if (!is.null(resident_name) && nzchar(resident_name))
                       resident_name else "Resident"
  p               <- plotly::plot_ly()
  has_data        <- FALSE
  all_periods_obs <- character(0)

  for (nm in names(datasets)) {
    ds  <- datasets[[nm]]
    dat <- ds$data
    col <- ds$col
    if (is.null(dat) || nrow(dat) == 0 || !col %in% names(dat)) next

    pf    <- .period_field(dat)
    color <- ds$color

    df <- tryCatch(data.frame(
      record_id   = as.character(dat$record_id),
      period_code = as.character(suppressWarnings(as.integer(dat[[pf]]))),
      score       = suppressWarnings(as.numeric(dat[[col]])),
      stringsAsFactors = FALSE
    ), error = function(e) NULL)
    if (is.null(df)) next

    df$period_name <- .MILE_PERIODS[df$period_code]
    df$period_num  <- .PERIOD_NUM[df$period_name]
    df <- df[!is.na(df$score) & !is.na(df$period_name), , drop = FALSE]
    if (nrow(df) == 0) next
    has_data <- TRUE

    periods_present <- intersect(.PERIOD_ORD, unique(df$period_name))
    all_periods_obs <- union(all_periods_obs, periods_present)
    pred_nums       <- .PERIOD_NUM[periods_present]

    # Quadratic program growth curve
    prog_fit <- tryCatch(
      lm(score ~ period_num + I(period_num^2), data = df),
      error = function(e) NULL
    )
    if (is.null(prog_fit) || anyNA(coef(prog_fit)))
      prog_fit <- tryCatch(lm(score ~ period_num, data = df), error = function(e) NULL)

    if (!is.null(prog_fit)) {
      pred_df <- data.frame(period_num = pred_nums, period_name = periods_present)
      prog_ci <- tryCatch(
        predict(prog_fit, newdata = pred_df, interval = "confidence", level = 0.95),
        error = function(e) NULL
      )
      if (!is.null(prog_ci)) {
        prog_curve <- data.frame(
          period_name = factor(periods_present, levels = .PERIOD_ORD),
          fit = prog_ci[, "fit"],
          lwr = prog_ci[, "lwr"],
          upr = prog_ci[, "upr"]
        )
        prog_curve <- prog_curve[order(prog_curve$period_name), ]
        x_prog <- as.character(prog_curve$period_name)

        p <- p %>%
          plotly::add_ribbons(
            x          = x_prog,
            ymin       = prog_curve$lwr,
            ymax       = prog_curve$upr,
            fillcolor  = ds$rgba_rb,
            line       = list(color = "transparent", width = 0),
            name       = paste0(ds$label, " \u2014 cohort 95% CI"),
            showlegend = FALSE,
            hoverinfo  = "skip",
            legendgroup = nm
          ) %>%
          plotly::add_trace(
            x    = x_prog,
            y    = prog_curve$fit,
            type = "scatter", mode = "lines",
            line = list(color = color, width = 1.5, dash = "dot"),
            name = paste0(ds$label, " \u2014 cohort curve"),
            showlegend  = TRUE,
            hoverinfo   = "skip",
            legendgroup = nm
          )
      }
    }

    # Individual resident scores
    ind_rows <- df[df$record_id == as.character(resident_id), , drop = FALSE]
    if (nrow(ind_rows) > 0) {
      ind <- aggregate(score ~ period_name + period_num,
                       data = ind_rows, FUN = mean, na.rm = TRUE)
      ind$period_name <- factor(ind$period_name, levels = .PERIOD_ORD)
      ind <- ind[order(ind$period_name), ]

      # Linear projection if ≥3 observed periods
      if (nrow(ind) >= 3) {
        ind_fit <- tryCatch(lm(score ~ period_num, data = ind), error = function(e) NULL)
        if (!is.null(ind_fit)) {
          proj_nums   <- seq(min(ind$period_num), 6, by = 1)
          proj_names  <- names(.PERIOD_NUM)[match(proj_nums, .PERIOD_NUM)]
          proj_names  <- proj_names[!is.na(proj_names)]
          proj_nums   <- .PERIOD_NUM[proj_names]
          proj_scores <- predict(ind_fit, newdata = data.frame(period_num = proj_nums))
          ind_proj    <- data.frame(
            period_name = factor(proj_names, levels = .PERIOD_ORD),
            score = proj_scores
          )
          ind_proj <- ind_proj[order(ind_proj$period_name), ]
          p <- p %>%
            plotly::add_trace(
              x    = as.character(ind_proj$period_name),
              y    = ind_proj$score,
              type = "scatter", mode = "lines",
              line = list(color = ds$rgba_pr, width = 2, dash = "dot"),
              name = paste0(name_lbl, " \u2014 ", ds$label, " (projected)"),
              showlegend  = TRUE,
              hoverinfo   = "skip",
              legendgroup = nm
            )
        }
      }

      p <- p %>%
        plotly::add_trace(
          x      = as.character(ind$period_name),
          y      = ind$score,
          type   = "scatter", mode = "lines+markers",
          name   = paste0(name_lbl, " \u2014 ", ds$label),
          line   = list(color = color, width = 3),
          marker = list(color = color, size = 10,
                        line = list(color = "#ffffff", width = 2)),
          text   = paste0(name_lbl, " (", ds$label, ")",
                          "<br>Period: ", ind$period_name,
                          "<br>Score: ", round(ind$score, 1)),
          hoverinfo   = "text",
          legendgroup = nm
        )
    }

  }

  if (!has_data) return(NULL)

  x_all <- intersect(.PERIOD_ORD, all_periods_obs)

  # Single graduation target at Level 7 (both scales share this after conversion)
  p <- p %>%
    plotly::add_trace(
      x         = x_all,
      y         = rep(7, length(x_all)),
      type      = "scatter", mode = "lines",
      line      = list(color = "#dc3545", width = 1.5, dash = "dash"),
      name      = "Graduation Target (Level 7)",
      showlegend = TRUE,
      hoverinfo  = "skip"
    )

  p %>%
    plotly::layout(
      xaxis = list(
        title         = FALSE,
        categoryorder = "array",
        categoryarray = x_all,
        tickfont      = list(size = 11, color = "#2d3748"),
        tickangle     = -30,
        gridcolor     = "#e2e8f0",
        linecolor     = "#cbd5e0"
      ),
      yaxis = list(
        title     = list(text = "Score (1\u20139)",
                         font = list(size = 12, color = "#1a202c")),
        range     = c(0.5, 9.5),
        dtick     = 1,
        tickfont  = list(size = 11, color = "#1a202c"),
        gridcolor = "#e2e8f0",
        linecolor = "#cbd5e0",
        zeroline  = FALSE
      ),
      legend = list(
        orientation = "h",
        x           = 0.5,
        xanchor     = "center",
        y           = -0.32,
        font        = list(size = 10, color = "#1a202c"),
        bgcolor     = "rgba(255,255,255,0.9)"
      ),
      plot_bgcolor  = "#ffffff",
      paper_bgcolor = "#ffffff",
      margin        = list(l = 50, r = 20, t = 15, b = 95),
      hovermode     = "closest"
    ) %>%
    plotly::config(displayModeBar = FALSE)
}


# ═════════════════════════════════════════════════════════════════════════════
# UI
# ═════════════════════════════════════════════════════════════════════════════
mod_milestones_ui <- function(id) {
  ns <- NS(id)
  tagList(

    # ── Period selector ───────────────────────────────────────────────────────
    div(
      class = "d-flex align-items-center gap-2 mb-3 flex-wrap",
      tags$span("Assessment period:",
                style = "font-size:0.9rem; font-weight:600; color:#003d5c; white-space:nowrap;"),
      uiOutput(ns("period_pills"))
    ),

    info_note(
      summary = "About assessment periods",
      tags$p("Milestones are assessed at up to seven points across residency: ",
             tags$strong("Entering Residency, Mid-Intern, End Intern, Mid-PGY2, End PGY2, Mid-PGY3,"),
             " and ", tags$strong("Graduating"), "."),
      tags$p("Select any period above to view the radar charts for that snapshot. ",
             "Only periods with data will appear as options.")
    ),

    # ── Spider plots: Self + ACGME side by side ───────────────────────────────
    info_note(
      summary = "How to read these radar charts",
      tags$p("Each axis represents one of the six ACGME competency domains ",
             "(Patient Care, Medical Knowledge, Practice-Based Learning, Interpersonal Communication, ",
             "Professionalism, Systems-Based Practice). Scores are on a 1\u20139 scale."),
      tags$p(tags$strong("Left (blue):"), " your ", tags$em("self-reported"), " ratings from the Self-Evaluation form."),
      tags$p(tags$strong("Right (amber):"), " ratings ", tags$em("submitted to the ACGME"), " by the program on your behalf. ",
             "These reflect the program's composite assessment and may differ from self-reported scores."),
      tags$p("The shaded polygon shows your profile; the dashed line shows the cohort median for context.")
    ),
    div(
      class = "row g-3 mb-3",
      div(class = "col-lg-6",
        div(class = "gmed-card h-100",
          div(class = "card-body",
            tags$p(class = "text-muted mb-2",
                   style = "font-size:0.75rem; text-transform:uppercase; letter-spacing:.07em;",
                   "Self Assessment \u2014 Radar"),
            plotlyOutput(ns("spider_self"), height = "380px")
          )
        )
      ),
      div(class = "col-lg-6",
        div(class = "gmed-card h-100",
          div(class = "card-body",
            tags$p(class = "text-muted mb-2",
                   style = "font-size:0.75rem; text-transform:uppercase; letter-spacing:.07em;",
                   "Milestones Reported to the ACGME \u2014 Radar"),
            plotlyOutput(ns("spider_acgme"), height = "380px")
          )
        )
      )
    ),

    # ── Individual Progression ────────────────────────────────────────────────
    div(
      class = "gmed-card",
      div(
        class = "card-body",
        info_note(
          summary = "About the progression chart",
          tags$p("This chart tracks a single milestone over time across all periods you have data for."),
          tags$p("The ", tags$strong("shaded ribbon"), " shows the expected growth curve for the program cohort ",
                 "(quadratic fit, Park et al. 2021 ", tags$em("JAMA Network Open"), "), with a 95% confidence interval. ",
                 "The ", tags$strong("graduation target line"), " at Level 7 reflects the ACGME benchmark for completing residents."),
          tags$p("Your individual points are connected by a line. If you have 3 or more observations, ",
                 "a linear projection is shown to estimate your trajectory toward graduation.")
        ),
        div(
          class = "d-flex align-items-center flex-wrap gap-3 mb-2",
          tags$span("Individual Progression",
                    style = "font-weight:700; font-size:0.9rem; color:#003d5c;"),
          div(
            class = "ms-auto d-flex align-items-center flex-wrap gap-3",
            div(
              class = "form-check form-check-inline mb-0",
              tags$input(
                type    = "checkbox", class = "form-check-input",
                id      = ns("show_self"), checked = "checked",
                onclick = sprintf(
                  "Shiny.setInputValue('%s',this.checked,{priority:'event'})",
                  ns("show_self"))
              ),
              tags$label(class = "form-check-label", `for` = ns("show_self"),
                         style = "font-size:0.82rem;", "Self-Evaluation")
            ),
            div(
              class = "form-check form-check-inline mb-0",
              tags$input(
                type    = "checkbox", class = "form-check-input",
                id      = ns("show_acgme"), checked = "checked",
                onclick = sprintf(
                  "Shiny.setInputValue('%s',this.checked,{priority:'event'})",
                  ns("show_acgme"))
              ),
              tags$label(class = "form-check-label", `for` = ns("show_acgme"),
                         style = "font-size:0.82rem;",
                         "Milestones Reported to the ACGME")
            ),
            uiOutput(ns("prog_selector"))
          )
        ),
        uiOutput(ns("mile_desc")),
        plotlyOutput(ns("curve_combined"), height = "360px")
      )
    )
  )
}


# ═════════════════════════════════════════════════════════════════════════════
# Server
# ═════════════════════════════════════════════════════════════════════════════
mod_milestones_server <- function(id, rdm_data, resident_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Milestone workflow ────────────────────────────────────────────────────
    milestone_wf <- reactive({
      req(rdm_data())
      pre <- rdm_data()$milestone_workflow
      if (!is.null(pre)) return(pre)
      tryCatch(
        create_milestone_workflow_from_dict(
          all_forms     = rdm_data()$all_forms,
          data_dict     = rdm_data()$data_dict,
          resident_data = rdm_data()$residents,
          verbose       = FALSE
        ),
        error = function(e) { message("Milestone workflow error: ", e$message); NULL }
      )
    })

    # ── Raw form data ─────────────────────────────────────────────────────────
    self_data_r <- reactive({
      req(rdm_data()); rdm_data()$all_forms$milestone_selfevaluation_c33c
    })
    acgme_data_r <- reactive({
      req(rdm_data()); rdm_data()$all_forms$acgme_miles
    })
    data_dict_r <- reactive({
      req(rdm_data()); rdm_data()$data_dict
    })

    # ── Resident name ─────────────────────────────────────────────────────────
    res_name_r <- reactive({
      req(rdm_data(), resident_id())
      nr <- rdm_data()$residents
      if (is.null(nr) || !"name" %in% names(nr)) return(NULL)
      nm <- nr$name[as.character(nr$record_id) == as.character(resident_id())]
      if (length(nm) > 0 && !is.na(nm[1]) && nzchar(nm[1])) nm[1] else NULL
    })

    # ── Available periods ─────────────────────────────────────────────────────
    avail_self_periods <- reactive({
      req(self_data_r(), resident_id())
      .avail_periods_for(self_data_r(), resident_id(), "prog_mile_period_self")
    })
    avail_acgme_periods <- reactive({
      req(acgme_data_r(), resident_id())
      .avail_periods_for(acgme_data_r(), resident_id(), "acgme_mile_period")
    })

    # Union across both sources, canonical order
    avail_any_periods <- reactive({
      intersect(.PERIOD_ORD, union(avail_self_periods(), avail_acgme_periods()))
    })

    # Selected period: defaults to most recent (last in canonical order)
    selected_period <- reactive({
      ps <- avail_any_periods()
      if (!length(ps)) return(NULL)
      p  <- input$period_pick
      if (!is.null(p) && p %in% ps) p else ps[length(ps)]
    })

    # ── Period pills ──────────────────────────────────────────────────────────
    output$period_pills <- renderUI({
      ps  <- avail_any_periods()
      sel <- selected_period()
      if (!length(ps))
        return(tags$span(class = "text-muted fst-italic",
                         style = "font-size:0.82rem;",
                         "No milestone data available yet."))
      div(
        class = "d-flex gap-1 flex-wrap",
        lapply(ps, function(p) {
          active <- identical(p, sel)
          tags$button(
            type    = "button",
            class   = if (active) "btn btn-sm btn-primary"
                      else        "btn btn-sm btn-outline-secondary",
            style   = "font-size:0.78rem; padding:3px 10px;",
            onclick = sprintf(
              "Shiny.setInputValue('%s','%s',{priority:'event'})",
              ns("period_pick"), p),
            p
          )
        })
      )
    })

    # ── Medians helper ────────────────────────────────────────────────────────
    .get_medians <- function(wf, key_pattern) {
      if (is.null(wf)) return(NULL)
      k <- names(wf)[grepl(key_pattern, names(wf), ignore.case = TRUE)]
      if (!length(k)) return(NULL)
      wf[[k[1]]]$medians
    }

    # ── Self spider ───────────────────────────────────────────────────────────
    output$spider_self <- renderPlotly({
      req(self_data_r(), resident_id(), selected_period())
      pd <- .filter_period(self_data_r(), selected_period())
      tryCatch(
        create_enhanced_milestone_spider_plot(
          milestone_data = pd,
          median_data    = .get_medians(milestone_wf(), "rep.*self"),
          resident_id    = resident_id(),
          period_text    = selected_period(),
          milestone_type = "self",
          resident_data  = rdm_data()$residents
        ),
        error = function(e)
          section_placeholder("exclamation-triangle",
                               paste("Radar unavailable:", e$message))
      )
    })

    # ── ACGME spider ──────────────────────────────────────────────────────────
    output$spider_acgme <- renderPlotly({
      req(acgme_data_r(), resident_id(), selected_period())
      pd <- .filter_period(acgme_data_r(), selected_period())
      tryCatch(
        create_enhanced_milestone_spider_plot(
          milestone_data = pd,
          median_data    = .get_medians(milestone_wf(), "acgme"),
          resident_id    = resident_id(),
          period_text    = selected_period(),
          milestone_type = "acgme",
          resident_data  = rdm_data()$residents
        ),
        error = function(e)
          section_placeholder("exclamation-triangle",
                               paste("Radar unavailable:", e$message))
      )
    })

    # ── Progression: milestone selector ──────────────────────────────────────
    .mile_cols <- function(data, type_prefix) {
      if (is.null(data)) return(character(0))
      pat <- switch(type_prefix,
        "rep"   = "^rep_[a-z]+\\d+$",
        "self"  = "^rep_[a-z]+\\d+_self$",
        "acgme" = "^acgme_[a-z]+\\d+$",
        "^rep_[a-z]+\\d+$"
      )
      grep(pat, names(data), value = TRUE)
    }

    output$prog_selector <- renderUI({
      # Use self data for the base REP columns; fall back to ACGME cols
      sd   <- self_data_r()
      cols <- .mile_cols(sd, "self")
      # Strip _self so label/value align with the base col (rep_pc1)
      base_cols <- sub("_self$", "", cols)
      if (!length(base_cols)) return(NULL)
      selectInput(ns("sel_mile"), NULL,
                  choices  = .mile_choices(base_cols),
                  selected = base_cols[1],
                  width    = "220px")
    })

    # ── Behavioral anchor table ───────────────────────────────────────────────
    output$mile_desc <- renderUI({
      col <- input$sel_mile
      req(!is.null(col), nzchar(col))
      comp <- .col_to_comp_code(col)
      dd   <- data_dict_r()
      if (is.null(comp) || is.null(dd)) return(NULL)
      .mile_anchor_table(comp, dd)
    })

    # ── Checkbox state ────────────────────────────────────────────────────────
    show_self_r  <- reactive({ v <- input$show_self;  if (is.null(v)) TRUE else isTRUE(v) })
    show_acgme_r <- reactive({ v <- input$show_acgme; if (is.null(v)) TRUE else isTRUE(v) })

    # ── Combined progression chart ────────────────────────────────────────────
    output$curve_combined <- renderPlotly({
      col <- input$sel_mile
      req(!is.null(col), nzchar(col))
      req(show_self_r() || show_acgme_r())

      self_col  <- paste0(col, "_self")
      acgme_col <- sub("^rep_", "acgme_", col)

      datasets <- list()

      if (show_self_r()) {
        sd <- self_data_r()
        if (!is.null(sd) && self_col %in% names(sd))
          datasets[["self"]] <- list(
            data    = sd,
            col     = self_col,
            system  = "rep",
            label   = "Self-Evaluation",
            color   = "#003d5c",
            rgba_rb = "rgba(0,61,92,0.12)",
            rgba_pr = "rgba(0,61,92,0.45)"
          )
      }

      if (show_acgme_r()) {
        ad <- acgme_data_r()
        if (!is.null(ad) && acgme_col %in% names(ad))
          datasets[["acgme"]] <- list(
            data    = ad,
            col     = acgme_col,
            system  = "acgme",
            label   = "Milestones Reported to the ACGME",
            color   = "#d97706",
            rgba_rb = "rgba(217,119,6,0.12)",
            rgba_pr = "rgba(217,119,6,0.45)"
          )
      }

      # renderPlotly requires a plotly object — returning a shiny.tag (e.g.
      # from section_placeholder) throws 'no applicable method for
      # plotly_build'. Use an empty plotly with a centered annotation instead.
      empty_plot <- function(msg) {
        plotly::plot_ly() |>
          plotly::layout(
            xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
            annotations = list(list(
              x = 0.5, y = 0.5, xref = "paper", yref = "paper",
              text = msg, showarrow = FALSE,
              font = list(size = 14, color = "#607d8b"))))
      }

      if (!length(datasets))
        return(empty_plot("Select at least one dataset above."))

      tryCatch(
        .milestone_prog_plot_combined(
          datasets      = datasets,
          resident_id   = resident_id(),
          resident_name = res_name_r()
        ),
        error = function(e) empty_plot(paste("Progression unavailable:", e$message))
      )
    })

  })
}
