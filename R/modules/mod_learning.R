# mod_learning.R ─ My Learning
# Layout:
#   1. Learning Goals      — Self-Eval (inc. intern prep) + ILP goals (pill tabs)
#   2. Topics & Styles     — Checkbox-decoded from most recent s_eval
#   3. ITE / Board Scores  — % correct trajectory (primary) → risk banner
#                            → percentile trajectory (colored) → specialty table
#   4. Daily Question Log  — Rate stats + rotation breakdown
#
# s_e_ field notes:
#   Intern-specific (ume annotation): s_e_ume_goal1/2/3, s_e_prep_1-17, s_e_ume_concern
#   Universal: s_e_plus, s_e_delta, s_e_topic_sel/learn_style, s_e_step3, s_e_board_*
#   pgy*_tot_correct = % correct (absolute, ABIM risk driver)
#   pgy*_total_ile   = national percentile (relative)

# ── Helpers ───────────────────────────────────────────────────────────────────
# .NOMOGRAM_PARAMS / .pass_prob / .risk_from_prob defined in global.R (shared)

# Generate nomogram curve data for plotting (20–100% at 0.5% steps)
.nomogram_curves <- function() {
  do.call(rbind, lapply(1:3, function(pgy) {
    pcts <- seq(20, 100, by = 0.5)
    data.frame(
      ITE_Pct  = pcts,
      PGY      = pgy,
      PGY_lbl  = paste0("PGY-", pgy),
      Pass_Prob = sapply(pcts, function(x) .pass_prob(x, pgy))
    )
  }))
}

# Decode REDCap checkbox columns (uses exported parse_redcap_choices from gmed)
.decode_checkboxes <- function(row, prefix, choices_str) {
  if (is.null(choices_str) || is.na(choices_str) || !nzchar(trimws(choices_str)))
    return(character(0))
  cm <- parse_redcap_choices(choices_str)
  if (length(cm) == 0) return(character(0))
  selected <- character(0)
  for (code in names(cm)) {
    col <- paste0(prefix, "___", code)
    if (col %in% names(row)) {
      val <- row[[col]][1]
      if (!is.na(val) && as.character(val) == "1")
        selected <- c(selected, unname(cm[code]))
    }
  }
  selected
}

# ─────────────────────────────────────────────────────────────────────────────
# UI
# ─────────────────────────────────────────────────────────────────────────────
mod_learning_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("goals_section")),
    div(class = "mt-4", uiOutput(ns("topics_section"))),
    div(class = "mt-4", uiOutput(ns("ite_section"))),
    div(class = "mt-4", uiOutput(ns("questions_section")))
  )
}

# ─────────────────────────────────────────────────────────────────────────────
# Server
# ─────────────────────────────────────────────────────────────────────────────
mod_learning_server <- function(id, rdm_data, resident_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Data reactives ────────────────────────────────────────────────────────
    s_eval_r <- reactive({
      req(rdm_data(), resident_id())
      df <- rdm_data()$all_forms$s_eval
      if (is.null(df) || nrow(df) == 0) return(NULL)
      df %>% dplyr::filter(record_id == resident_id())
    })
    ilp_r <- reactive({
      req(rdm_data(), resident_id())
      df <- rdm_data()$all_forms$ilp
      if (is.null(df) || nrow(df) == 0) return(NULL)
      df %>% dplyr::filter(record_id == resident_id())
    })
    test_r <- reactive({
      req(rdm_data(), resident_id())
      df <- rdm_data()$all_forms$test_data
      if (is.null(df) || nrow(df) == 0) df <- rdm_data()$residents
      if (is.null(df) || nrow(df) == 0) return(NULL)
      td <- df %>% dplyr::filter(record_id == resident_id())
      if (nrow(td) == 0) return(td)
      # If multiple rows (repeating instrument), take the most recent instance
      if ("redcap_repeat_instance" %in% names(td) && nrow(td) > 1) {
        td <- td %>%
          dplyr::mutate(.inst = suppressWarnings(as.integer(
            .data$redcap_repeat_instance))) %>%
          dplyr::arrange(dplyr::desc(.inst)) %>%
          dplyr::slice(1) %>%
          dplyr::select(-.inst)
      }
      td
    })
    questions_r <- reactive({
      req(rdm_data(), resident_id())
      df <- rdm_data()$all_forms$questions
      if (is.null(df) || nrow(df) == 0) return(NULL)
      df %>% dplyr::filter(record_id == resident_id())
    })
    data_dict_r <- reactive({ req(rdm_data()); rdm_data()$data_dict })

    # Chronological period order: 7=Entering (oldest) → 1=Mid-Intern → 2=End-Intern
    #   → 3=Mid-PGY2 → 4=End-PGY2 → 5=Mid-PGY3 → 6=Graduating (newest)
    # Sorting by this order (descending) gives the most recent eval regardless of
    # how REDCap assigned redcap_repeat_instance values.
    .PERIOD_CHRON <- c("7"=0L, "1"=1L, "2"=2L, "3"=3L, "4"=4L, "5"=5L, "6"=6L)

    recent_seva <- reactive({
      df <- s_eval_r()
      if (is.null(df) || nrow(df) == 0) return(NULL)
      df %>%
        dplyr::mutate(.chron = unname(.PERIOD_CHRON[as.character(s_e_period)]),
                      .chron = dplyr::coalesce(.chron, -1L)) %>%
        dplyr::arrange(dplyr::desc(.chron)) %>%
        dplyr::slice(1) %>%
        dplyr::select(-.chron)
    })

    # ── Helpers ───────────────────────────────────────────────────────────────
    period_label <- function(code) {
      dplyr::case_when(
        as.character(code) == "7" ~ "Entering Residency",
        as.character(code) == "1" ~ "Mid-Intern",
        as.character(code) == "2" ~ "End of Intern Year",
        as.character(code) == "3" ~ "Mid-PGY2",
        as.character(code) == "4" ~ "End of PGY2",
        as.character(code) == "5" ~ "Mid-PGY3",
        as.character(code) == "6" ~ "Graduating",
        TRUE ~ paste0("Period ", code)
      )
    }

    sec_lbl <- function(ic, txt, sz = "0.72rem")
      tags$p(style = paste0("font-size:", sz, "; font-weight:700; text-transform:uppercase;",
                             " letter-spacing:.07em; color:#6c757d; margin-bottom:8px;"),
             tags$i(class = paste0("bi bi-", ic, " me-1")), txt)

    card_hdr <- function(ic, title, extra = NULL)
      div(class = "card-header border-0 d-flex align-items-center gap-2",
          style = "background:#f8fafc; border-radius:8px 8px 0 0; padding:14px 18px;",
          tags$i(class = paste0("bi bi-", ic),
                 style = "color:#003d5c; font-size:1.1rem;"),
          tags$span(style = "font-weight:700; color:#003d5c; font-size:1rem;", title),
          extra)

    info_banner <- function(...)
      div(class = "alert alert-info mb-3 py-2 px-3",
          style = "font-size:0.82rem; border-left:4px solid #0d6efd;",
          tags$i(class = "bi bi-info-circle me-1"), ...)

    period_badge <- function(code, bg = "#e8f0f7", col = "#003d5c")
      div(class = "mb-3",
          tags$span(style = paste0("background:", bg, "; color:", col,
                                   "; border-radius:20px; padding:3px 12px;",
                                   " font-size:0.78rem; font-weight:600;"),
                    paste0("Most Recent: ", period_label(code))))

    # =========================================================================
    # 1.  LEARNING GOALS
    # =========================================================================
    output$goals_section <- renderUI({
      seva <- recent_seva()
      # s_e_prep_1-17 are UME-annotated preparedness fields filled only for the
      # Entering Residency (period 7) evaluation.
      is_entering <- !is.null(seva) && as.character(seva$s_e_period[1]) == "7"

      # ── Self-Eval panel ───────────────────────────────────────────────────
      seva_panel <- if (!is.null(seva)) {
        plbl <- period_label(seva$s_e_period[1])

        # Intern preparedness display (s_e_prep_1-17, ume-annotated, period-7 only)
        prep_section <- if (is_entering) {
          prep_labels <- c(
            "1"  = "Answer questions from interdisciplinary services",
            "2"  = "Comfort with junior medical students",
            "3"  = "Obtaining consent for procedures",
            "4"  = "Personal organization / day-to-day tasks",
            "5"  = "Writing orders and prescriptions",
            "6"  = "Looking up evidence-based recommendations",
            "7"  = "Presenting patients (organized, hypothesis-driven)",
            "8"  = "Documenting encounters efficiently",
            "9"  = "Providing and receiving handoffs",
            "10" = "Recognizing urgent/emergent care needs",
            "11" = "Delivering bad news / challenging communication",
            "12" = "Recognizing when to ask for help",
            "13" = "Managing ICU patients (vents, pressors)",
            "14" = "Managing inpatient patients",
            "15" = "Managing primary care clinic patients",
            "16" = "Calling consults",
            "17" = "Writing orders and prescriptions"
          )
          scale_lbl <- c("1"="Not prepared","2"="Slightly","3"="Moderately","4"="Very","5"="Extremely")
          scale_col <- c("1"="#d62728","2"="#ff7f0e","3"="#f5c518","4"="#afd44e","5"="#27ae60")

          prep_rows <- lapply(names(prep_labels), function(n) {
            fld <- paste0("s_e_prep_", n)
            val <- if (fld %in% names(seva)) as.character(seva[[fld]][1]) else NA
            if (is.na(val) || val == "") return(NULL)
            lbl_t <- prep_labels[[n]]
            col   <- scale_col[val]
            lbl_v <- scale_lbl[val]
            if (is.na(col)) col <- "#adb5bd"
            if (is.na(lbl_v)) lbl_v <- val
            tags$div(
              style = "display:flex; align-items:center; gap:10px; margin-bottom:5px;",
              tags$span(style = paste0("min-width:10px; width:10px; height:10px; border-radius:50%;",
                                       " background:", col, "; display:inline-block; flex-shrink:0;")),
              tags$span(style = "font-size:0.88rem; color:#2c3e50; flex:1;", lbl_t),
              tags$span(style = paste0("font-size:0.75rem; font-weight:600; color:", col, ";",
                                       " white-space:nowrap;"), lbl_v)
            )
          })
          prep_rows <- Filter(Negate(is.null), prep_rows)
          if (length(prep_rows) > 0)
            div(class = "mb-3",
              sec_lbl("clipboard-check", "Preparedness Ratings (Entering Residency)"),
              div(prep_rows))
        }

        is_period_7 <- as.character(seva$s_e_period[1]) == "7"

        # UME goals (s_e_ume_goal1/2/3) are only filled in during the Entering
        # Residency (period 7) assessment.  For all other periods the goals live
        # in the ILP — direct the resident there instead.
        goals_block <- if (is_period_7) {
          goals <- Filter(function(g) !is.na(g) && nzchar(trimws(as.character(g))),
            list(if ("s_e_ume_goal1" %in% names(seva)) seva$s_e_ume_goal1[1] else NA,
                 if ("s_e_ume_goal2" %in% names(seva)) seva$s_e_ume_goal2[1] else NA,
                 if ("s_e_ume_goal3" %in% names(seva)) seva$s_e_ume_goal3[1] else NA))
          if (length(goals) > 0)
            div(class = "mb-3",
              sec_lbl("bullseye", "Learning Goals (Entering Residency)"),
              tags$ol(class = "mb-0 ps-4",
                style = "font-size:0.95rem; color:#2c3e50; line-height:1.8;",
                lapply(goals, function(g) tags$li(as.character(g))))
            )
          else
            tags$p(class="text-muted fst-italic small",
                   "No learning goals recorded for this period.")
        } else {
          # Non-period-7: goals are tracked in the ILP — show a gentle redirect
          div(class="alert alert-light border mb-3 py-2 px-3",
              style="font-size:0.84rem; border-left:4px solid #6f42c1 !important;",
              tags$i(class="bi bi-map-fill me-1", style="color:#6f42c1;"),
              tags$strong("Learning goals"), " for this period are tracked in your ",
              tags$strong("Individual Learning Plan"),
              " — see the ", tags$em("ILP Goals"), " tab above.")
        }

        plus_raw  <- if ("s_e_plus"  %in% names(seva)) seva$s_e_plus[1]  else NA
        delta_raw <- if ("s_e_delta" %in% names(seva)) seva$s_e_delta[1] else NA
        has_plus  <- !is.na(plus_raw)  && nzchar(trimws(as.character(plus_raw)))
        has_delta <- !is.na(delta_raw) && nzchar(trimws(as.character(delta_raw)))

        div(
          period_badge(seva$s_e_period[1]),
          prep_section,
          goals_block,
          if (has_plus || has_delta)
            div(class = "row g-3 mt-1",
              div(class = "col-md-6",
                sec_lbl("plus-circle-fill", "What Went Well"),
                tags$p(style = "font-size:1rem; color:#1a6b3a; line-height:1.75; margin:0;",
                  if (has_plus) as.character(plus_raw)
                  else tags$em(class="text-muted", "None recorded"))
              ),
              div(class = "col-md-6",
                sec_lbl("arrow-up-circle-fill", "Areas to Develop"),
                tags$p(style = "font-size:1rem; color:#c0392b; line-height:1.75; margin:0;",
                  if (has_delta) as.character(delta_raw)
                  else tags$em(class="text-muted", "None recorded"))
              )
            )
        )
      } else {
        tags$p(class="text-muted fst-italic", "No self-evaluation data available.")
      }

      # ── ILP panel ─────────────────────────────────────────────────────────
      ilp <- ilp_r()
      recent_ilp <- if (!is.null(ilp) && nrow(ilp) > 0) {
        ilp %>%
          dplyr::arrange(
            dplyr::desc(suppressWarnings(as.numeric(year_resident))),
            dplyr::desc(suppressWarnings(as.numeric(redcap_repeat_instance)))) %>%
          dplyr::slice(1)
      } else NULL

      dcfg <- list(
        pcmk    = list(label="Patient Care / Medical Knowledge",
                       icon="heart-pulse-fill", color="#0d6efd",
                       goal="goal_pcmk",           how="how_pcmk"),
        sbppbl  = list(label="Systems-Based Practice / PBL",
                       icon="diagram-3-fill",   color="#198754",
                       goal="goal_sbppbl",          how="how_sbppbl"),
        profics = list(label="Professionalism / ICS",
                       icon="people-fill",      color="#6f42c1",
                       goal="goal_subcomp_profics", how="how_profics")
      )
      ilp_panel <- if (!is.null(recent_ilp)) {
        cards <- Filter(Negate(is.null), lapply(names(dcfg), function(dk) {
          cfg   <- dcfg[[dk]]
          g_val <- if (cfg$goal %in% names(recent_ilp)) recent_ilp[[cfg$goal]][1] else NA
          h_val <- if (cfg$how  %in% names(recent_ilp)) recent_ilp[[cfg$how]][1]  else NA
          has_g <- !is.na(g_val) && nzchar(trimws(as.character(g_val)))
          has_h <- !is.na(h_val) && nzchar(trimws(as.character(h_val)))
          if (!has_g && !has_h) return(NULL)
          div(class="col-md-4 mb-2",
            div(style=paste0("background:#fff; border-left:4px solid ",cfg$color,";",
                             " border-radius:0 6px 6px 0; padding:14px; height:100%;",
                             " box-shadow:0 1px 4px rgba(0,0,0,.06);"),
              tags$p(style=paste0("font-size:0.72rem; font-weight:700; text-transform:uppercase;",
                                  " letter-spacing:.07em; color:",cfg$color,"; margin-bottom:8px;"),
                tags$i(class=paste0("bi bi-",cfg$icon," me-1")), cfg$label),
              if (has_g) tags$p(style="font-size:0.93rem; font-weight:600; color:#2c3e50; margin-bottom:6px;",
                                as.character(g_val)),
              if (has_h) tags$p(style="font-size:0.87rem; color:#5d6d7e; line-height:1.6; margin:0;",
                tags$strong("Plan: "), as.character(h_val))
            )
          )
        }))
        div(
          period_badge(recent_ilp$year_resident[1], "#f0ebf8", "#6f42c1"),
          if (length(cards) > 0) div(class="row g-2", cards)
          else tags$p(class="text-muted fst-italic small", "No ILP goals recorded.")
        )
      } else tags$p(class="text-muted fst-italic", "No ILP data available.")

      div(class="card border-0 shadow-sm", style="border-radius:8px;",
        card_hdr("bullseye", "Learning Goals"),
        div(class="card-body",
          info_banner(
            "Your most recent self-evaluation is shown below. ",
            tags$strong("Learning goals"),
            " for PGY2+ residents live in the ILP — use the tabs to switch. ",
            "The Entering Residency evaluation also includes preparedness ratings."
          ),
          tags$ul(class="nav nav-pills mb-3",
            tags$li(class="nav-item",
              tags$a(class="nav-link active py-1 px-3", style="font-size:0.83rem;",
                `data-bs-toggle`="pill", href=paste0("#",ns("tab_seva")),
                tags$i(class="bi bi-person-lines-fill me-1"), "Self-Eval Goals")),
            tags$li(class="nav-item",
              tags$a(class="nav-link py-1 px-3", style="font-size:0.83rem;",
                `data-bs-toggle`="pill", href=paste0("#",ns("tab_ilp")),
                tags$i(class="bi bi-map-fill me-1"), "ILP Goals"))
          ),
          div(class="tab-content",
            div(class="tab-pane fade show active", id=ns("tab_seva"), seva_panel),
            div(class="tab-pane fade",             id=ns("tab_ilp"),  ilp_panel)
          )
        )
      )
    })

    # =========================================================================
    # 2.  TOPICS & LEARNING STYLES
    # =========================================================================
    output$topics_section <- renderUI({
      seva <- recent_seva()
      dd   <- data_dict_r()

      decode_field <- function(prefix) {
        if (is.null(seva) || is.null(dd)) return(character(0))
        row <- dd %>% dplyr::filter(field_name == sub("___.*","",prefix))
        # use the base field name without ___
        base <- prefix
        row2 <- dd %>% dplyr::filter(field_name == base)
        if (nrow(row2) > 0)
          .decode_checkboxes(seva, base, row2$select_choices_or_calculations[1])
        else character(0)
      }

      topics_labels <- decode_field("s_e_topic_sel")
      styles_labels <- decode_field("s_e_learn_style")

      # Handle "other" free text
      if (!is.null(seva)) {
        oth_t <- if ("s_e_topic_oth" %in% names(seva)) seva$s_e_topic_oth[1] else NA
        if (!is.na(oth_t) && nzchar(trimws(as.character(oth_t))))
          topics_labels <- c(topics_labels[topics_labels != "Other"],
                             paste0("Other: ", trimws(oth_t)))
        oth_s <- if ("s_e_learn_oth" %in% names(seva)) seva$s_e_learn_oth[1] else NA
        if (!is.na(oth_s) && nzchar(trimws(as.character(oth_s))))
          styles_labels <- c(styles_labels[styles_labels != "Other"],
                             paste0("Other: ", trimws(oth_s)))
      }

      mk_chips <- function(labels, bg_col, ic) {
        if (length(labels) == 0)
          return(tags$p(class="text-muted fst-italic", "None identified"))
        div(style="display:flex; flex-wrap:wrap; gap:10px;",
          lapply(labels, function(lbl)
            tags$span(
              style=paste0("display:inline-block; background:", bg_col, "; color:#fff;",
                           " border-radius:8px; padding:8px 16px; font-size:0.9rem;",
                           " font-weight:600; line-height:1.4;"),
              tags$i(class=paste0("bi bi-",ic," me-1")), lbl
            )
          )
        )
      }

      div(class="card border-0 shadow-sm", style="border-radius:8px;",
        card_hdr("lightbulb-fill", "Topics & Learning Styles"),
        div(class="card-body",
          info_banner("Topics you feel least confident about and your preferred ",
                      "learning experiences, from your most recent self-evaluation."),
          if (!is.null(seva))
            period_badge(seva$s_e_period[1]),
          div(class="row g-4",
            div(class="col-md-6",
              sec_lbl("exclamation-triangle-fill",
                      "Topics Least Comfortable With", "0.78rem"),
              mk_chips(topics_labels, "#e67e22", "exclamation-circle-fill")
            ),
            div(class="col-md-6",
              sec_lbl("star-fill", "Preferred Learning Experiences", "0.78rem"),
              mk_chips(styles_labels, "#0066a1", "check-circle-fill")
            )
          )
        )
      )
    })

    # =========================================================================
    # 3.  ITE / BOARD SCORES  (McDonald et al. 2020 nomogram)
    # =========================================================================
    .SPEC_MAP <- c(
      total_ile="Total", cards_ile="Cardiology", endo_ile="Endocrinology",
      gi_ile="Gastroenterology", gim_ile="General Internal Medicine",
      geri_ile="Geriatrics", hemonc_ile="Hematology / Oncology",
      id_ile="Infectious Disease", nephro_ile="Nephrology", neuro_ile="Neurology",
      pulm_ccm_ile="Pulmonary / Critical Care", rheum_ile="Rheumatology",
      hvc_ile="High Value Care"
    )

    .pct_vec <- function(td) {
      vals <- c("1"=suppressWarnings(as.numeric(td[["pgy1_tot_correct"]][1])),
                "2"=suppressWarnings(as.numeric(td[["pgy2_tot_correct"]][1])),
                "3"=suppressWarnings(as.numeric(td[["pgy3_tot_correct"]][1])))
      vals[!is.na(vals)]
    }
    .ile_vec <- function(td) {
      vals <- c("1"=suppressWarnings(as.numeric(td[["pgy1_total_ile"]][1])),
                "2"=suppressWarnings(as.numeric(td[["pgy2_total_ile"]][1])),
                "3"=suppressWarnings(as.numeric(td[["pgy3_total_ile"]][1])))
      vals[!is.na(vals)]
    }

    # Plot 1: ABIM pass probability nomogram — 3 curves + resident's points
    # Renders even when no resident pct data (shows reference curves only)
    output$ite_nomogram_plot <- plotly::renderPlotly({
      td   <- test_r()
      if (is.null(td) || nrow(td) == 0) return(NULL)
      pcts <- .pct_vec(td)  # may be length-0 → curves only

      curves <- .nomogram_curves()
      curve_cols  <- c("1"="#4e79a7", "2"="#59a14f", "3"="#f28e2b")
      curve_dashes <- c("1"="solid",  "2"="dash",    "3"="dot")

      fig <- plotly::plot_ly()

      # Risk zone fills
      fig <- fig %>%
        plotly::add_ribbons(x=c(20,100), ymin=0, ymax=0.5,
          fillcolor="rgba(211,47,47,0.06)", line=list(width=0),
          hoverinfo="none", showlegend=FALSE) %>%
        plotly::add_ribbons(x=c(20,100), ymin=0.5, ymax=0.75,
          fillcolor="rgba(230,81,0,0.06)", line=list(width=0),
          hoverinfo="none", showlegend=FALSE)

      # Reference lines
      fig <- fig %>%
        plotly::add_segments(x=20, xend=100, y=0.75, yend=0.75,
          line=list(color="#e65100", dash="dash", width=1),
          name="75% threshold", showlegend=TRUE) %>%
        plotly::add_segments(x=20, xend=100, y=0.50, yend=0.50,
          line=list(color="#d32f2f", dash="dash", width=1),
          name="50% threshold", showlegend=TRUE)

      # Nomogram curves (PGY-1, PGY-2, PGY-3)
      for (pgy in 1:3) {
        cd <- curves[curves$PGY == pgy, ]
        fig <- fig %>%
          plotly::add_trace(data=cd, x=~ITE_Pct, y=~Pass_Prob,
            type="scatter", mode="lines",
            line=list(color=curve_cols[[as.character(pgy)]],
                      dash=curve_dashes[[as.character(pgy)]], width=2),
            name=paste0("PGY-",pgy," curve"),
            hovertemplate=paste0("PGY-",pgy," | %{x:.0f}% correct<br>",
                                 "P(pass) = %{y:.1%}<extra></extra>"))
      }

      # Resident's actual points (each on their own PGY curve)
      for (y in names(pcts)) {
        pct_v  <- pcts[[y]]
        prob_v <- .pass_prob(pct_v, as.integer(y))
        risk   <- .risk_from_prob(prob_v)
        fig <- fig %>%
          plotly::add_trace(x=pct_v, y=prob_v,
            type="scatter", mode="markers",
            marker=list(size=14, color=risk$color,
                        line=list(color="white", width=2), symbol="diamond"),
            name=paste0("Your PGY-",y," score"),
            hovertemplate=paste0("PGY-",y,"<br>",
                                 round(pct_v,1),"% correct<br>",
                                 "P(pass ABIM) = ", round(prob_v*100,1),"%<br>",
                                 risk$level, "<extra></extra>"))
      }

      fig %>% plotly::layout(
        xaxis=list(title="ITE % Correct", range=c(20,100),
                   showgrid=TRUE, gridcolor="#f0f0f0", dtick=10),
        yaxis=list(title="Probability of Passing ABIM",
                   range=c(0,1), tickformat=".0%",
                   showgrid=TRUE, gridcolor="#f0f0f0"),
        legend=list(orientation="h", x=0, y=-0.3),
        plot_bgcolor="rgba(0,0,0,0)", paper_bgcolor="rgba(0,0,0,0)",
        margin=list(t=10, b=60)
      )
    })

    # Plot 2: Percentile trajectory (secondary)
    output$ite_ile_plot <- plotly::renderPlotly({
      td  <- test_r()
      if (is.null(td) || nrow(td) == 0) return(NULL)
      iles <- .ile_vec(td)
      if (length(iles) == 0) return(NULL)
      yrs    <- as.integer(names(iles))
      pt_col <- dplyr::case_when(
        iles < 20 ~ "#d32f2f", iles < 50 ~ "#e65100", TRUE ~ "#2e7d32")

      plotly::plot_ly() %>%
        plotly::add_ribbons(x=c(1,3), ymin=0,  ymax=20,
          fillcolor="rgba(211,47,47,0.08)",  line=list(width=0),
          hoverinfo="none", showlegend=FALSE) %>%
        plotly::add_ribbons(x=c(1,3), ymin=20, ymax=50,
          fillcolor="rgba(230,81,0,0.07)",   line=list(width=0),
          hoverinfo="none", showlegend=FALSE) %>%
        plotly::add_ribbons(x=c(1,3), ymin=50, ymax=100,
          fillcolor="rgba(46,125,50,0.05)",  line=list(width=0),
          hoverinfo="none", showlegend=FALSE) %>%
        plotly::add_segments(x=1, xend=3, y=50, yend=50,
          line=list(color="#e65100", dash="dash", width=1.2),
          name="50th %ile", showlegend=TRUE) %>%
        plotly::add_segments(x=1, xend=3, y=20, yend=20,
          line=list(color="#d32f2f", dash="dash", width=1.2),
          name="20th %ile", showlegend=TRUE) %>%
        plotly::add_trace(x=yrs, y=unname(iles), type="scatter",
          mode="lines+markers",
          line=list(color="#2F5496", width=2.5),
          marker=list(size=11, color=unname(pt_col),
                      line=list(color="white", width=2)),
          hovertemplate="PGY%{x}<br>%{y:.0f}th percentile<extra></extra>",
          name="Percentile", showlegend=FALSE) %>%
        plotly::layout(
          xaxis=list(title="PGY Year", tickvals=c(1,2,3),
                     ticktext=c("PGY1","PGY2","PGY3"),
                     range=c(0.8,3.2), showgrid=FALSE),
          yaxis=list(title="National Percentile", range=c(0,100),
                     showgrid=TRUE, gridcolor="#f0f0f0"),
          legend=list(orientation="h", x=0, y=-0.3),
          plot_bgcolor="rgba(0,0,0,0)", paper_bgcolor="rgba(0,0,0,0)",
          margin=list(t=10, b=50)
        )
    })

    output$ite_section <- renderUI({
      td      <- test_r()
      has_pct <- !is.null(td) && nrow(td) > 0 &&
        any(sapply(c("pgy1_tot_correct","pgy2_tot_correct","pgy3_tot_correct"),
                   function(f) f %in% names(td) && !is.na(td[[f]][1]) && td[[f]][1] != ""))
      has_ile <- !is.null(td) && nrow(td) > 0 &&
        any(sapply(c("pgy1_total_ile","pgy2_total_ile","pgy3_total_ile"),
                   function(f) f %in% names(td) && !is.na(td[[f]][1]) && td[[f]][1] != ""))

      if (!has_pct && !has_ile) {
        return(div(class="card border-0 shadow-sm", style="border-radius:8px;",
          card_hdr("clipboard2-data-fill", "ITE / Board Exam Scores"),
          div(class="card-body d-flex flex-column align-items-center py-4",
            style="color:#adb5bd;",
            tags$i(class="bi bi-clipboard2-data",
                   style="font-size:2.5rem; opacity:0.35; margin-bottom:12px;"),
            tags$p("No ITE scores recorded yet.", style="font-size:0.92rem; margin:0;")
          )
        ))
      }

      pct_vec  <- if (!is.null(td)) .pct_vec(td) else c()

      avail_yrs <- Filter(function(y) {
        f <- paste0("pgy", y, "_total_ile")
        f %in% names(td) && !is.na(td[[f]][1]) && td[[f]][1] != ""
      }, c("1","2","3"))

      # Per-year summary cards — each with % correct, pass probability, risk
      yr_cards <- lapply(c("1","2","3"), function(y) {
        pct_f <- paste0("pgy", y, "_tot_correct")
        ile_f <- paste0("pgy", y, "_total_ile")
        pct_v <- if (pct_f %in% names(td)) suppressWarnings(as.numeric(td[[pct_f]][1])) else NA
        ile_v <- if (ile_f %in% names(td)) suppressWarnings(as.numeric(td[[ile_f]][1])) else NA
        if (is.na(pct_v) && is.na(ile_v)) return(NULL)

        prob  <- if (!is.na(pct_v)) .pass_prob(pct_v, as.integer(y)) else NA
        risk  <- .risk_from_prob(prob)

        prob_txt <- if (!is.na(prob)) {
          if      (prob >= 0.90) "Strong position for board success"
          else if (prob >= 0.75) "Favorable \u2014 continue preparation"
          else if (prob >= 0.50) "Moderate risk \u2014 targeted prep recommended"
          else                   "Significant risk \u2014 intervention needed"
        } else ""

        ile_col <- if (!is.na(ile_v)) {
          if (ile_v < 20) "#d32f2f" else if (ile_v < 50) "#e65100" else "#2e7d32"
        } else "#adb5bd"

        div(class="col-md-4 mb-2",
          div(style=paste0("background:#fff; border-left:5px solid ",risk$color,";",
                           " border-radius:0 8px 8px 0; padding:16px;",
                           " box-shadow:0 1px 4px rgba(0,0,0,.07);"),
            tags$p(style=paste0("font-size:0.7rem; font-weight:700; text-transform:uppercase;",
                               " letter-spacing:.07em; color:#6c757d; margin-bottom:8px;"),
                   paste0("PGY-", y)),
            if (!is.na(pct_v))
              tagList(
                tags$p(class="mb-0",
                  style=paste0("font-size:2rem; font-weight:700; color:",risk$color,
                               "; line-height:1;"),
                  paste0(round(pct_v,1),"%")),
                tags$p(class="mb-1", style="font-size:0.72rem; color:#6c757d;",
                       "ITE % correct")
              ),
            if (!is.na(prob))
              tagList(
                tags$hr(style="margin:8px 0;"),
                tags$p(class="mb-0",
                  style=paste0("font-size:1.35rem; font-weight:700; color:",risk$color,
                               "; line-height:1;"),
                  paste0(round(prob*100,0),"% pass probability")),
                tags$p(class="mb-1", style="font-size:0.75rem; color:#6c757d;",
                       "McDonald et al. 2020 nomogram"),
                tags$p(class="mb-0",
                  style=paste0("font-size:0.78rem; color:",risk$color,"; font-weight:600;"),
                  prob_txt)
              ),
            if (!is.na(ile_v))
              tags$p(class="mb-0 mt-2",
                style=paste0("font-size:0.83rem; color:",ile_col,"; font-weight:600;"),
                paste0(round(ile_v,0),"th national percentile"))
          )
        )
      })
      yr_cards <- Filter(Negate(is.null), yr_cards)

      # Specialty percentile table
      spec_rows <- lapply(names(.SPEC_MAP), function(suffix) {
        vals <- sapply(avail_yrs, function(y) {
          f <- paste0("pgy", y, "_", suffix)
          if (f %in% names(td)) suppressWarnings(as.numeric(td[[f]][1])) else NA
        })
        if (all(is.na(vals))) return(NULL)
        c(Specialty=unname(.SPEC_MAP[suffix]),
          setNames(as.character(vals), paste0("PGY",avail_yrs)))
      })
      spec_rows <- Filter(Negate(is.null), spec_rows)

      spec_tbl <- if (length(spec_rows) > 0 && length(avail_yrs) > 0) {
        df      <- as.data.frame(do.call(rbind, spec_rows), stringsAsFactors=FALSE)
        yr_ths  <- lapply(avail_yrs, function(y)
                    tags$th(paste0("PGY",y), style="text-align:center;"))
        ile_cell <- function(vs) {
          v <- suppressWarnings(as.numeric(vs))
          if (is.na(v)) return(tags$td("\u2014", style="text-align:center; color:#adb5bd;"))
          bg <- if (v<20) "#fde8e8" else if (v<50) "#fff3e0" else "#e8f5e9"
          fc <- if (v<20) "#c0392b" else if (v<50) "#7f4a00" else "#1a6b3a"
          tags$td(paste0(round(v,0),"%"),
                  style=paste0("text-align:center; background:",bg,
                               "; color:",fc,"; font-weight:600;"))
        }
        tags$table(
          class="table table-sm table-bordered mb-0", style="font-size:0.83rem;",
          tags$thead(tags$tr(tags$th("Specialty"), yr_ths)),
          tags$tbody(lapply(seq_len(nrow(df)), function(i) {
            is_tot <- df[i,"Specialty"] == "Total"
            cells  <- lapply(colnames(df)[-1], function(col) ile_cell(df[i,col]))
            tags$tr(class=if(is_tot) "table-primary fw-bold" else "",
              tags$td(df[i,"Specialty"]), cells)
          }))
        )
      } else NULL

      div(class="card border-0 shadow-sm", style="border-radius:8px;",
        card_hdr("clipboard2-data-fill", "ITE / Board Exam Scores"),
        div(class="card-body",
          info_banner(
            "Pass probability uses the ",
            tags$strong("McDonald et al. 2020"), " (", tags$em("Academic Medicine"), ")",
            " logistic regression nomogram — the same published tool used nationally to predict ",
            "ABIM pass rates from ITE % correct by PGY year. ",
            "National percentile is shown for comparison; yellow = <50th, red = <20th."
          ),

          # Per-year cards (% correct + pass probability + risk)
          if (length(yr_cards) > 0)
            div(class="row g-3 mb-4", yr_cards),

          # Nomogram chart — always shown; dots only appear when % correct is available
          div(class="mb-4",
            sec_lbl("graph-up-arrow", "ABIM Pass Probability Nomogram"),
            if (has_pct)
              tags$p(class="text-muted small mb-2",
                "All three PGY curves are shown. Your scores are plotted as diamonds ",
                "on their corresponding curve. Hover for exact probability.")
            else
              div(class="alert alert-light border mb-3 py-2 px-3",
                  style="font-size:0.8rem; border-left:3px solid #adb5bd;",
                tags$i(class="bi bi-info-circle me-1"),
                "Your ITE ", tags$strong("% correct"), " has not been entered yet — ",
                "the three reference curves are shown so you can see what score ",
                "corresponds to each risk level. Your dot will appear once % correct is recorded."),
            plotly::plotlyOutput(ns("ite_nomogram_plot"), height="320px")
          ),

          # Percentile trajectory (secondary)
          if (has_ile)
            div(class="mb-4",
              sec_lbl("bar-chart-fill", "National Percentile Trajectory"),
              tags$p(class="text-muted small mb-2",
                "Orange = below 50th percentile. Red = below 20th."),
              plotly::plotlyOutput(ns("ite_ile_plot"), height="200px")
            ),

          # Specialty table
          if (!is.null(spec_tbl))
            div(sec_lbl("table", "Specialty Breakdown (Percentile)"), spec_tbl)
        )
      )
    })

    # =========================================================================
    # 4.  DAILY QUESTION LOG
    # =========================================================================
    ROT_LKP <- c("1"="Red","2"="Green","3"="White","4"="Yellow","5"="Diamond",
                 "6"="Gold","7"="MICU","8"="Bronze","9"="Cardiology",
                 "10"="Bridge/Acute","11"="Consults-SLUH","12"="Elective/Clinics",
                 "13"="VA-A","14"="VA-B","15"="VA-C","16"="VA-D","17"="VA Clinics")

    output$questions_section <- renderUI({
      df <- questions_r()
      n_q <- if (!is.null(df) && nrow(df) > 0) nrow(df) else 0L

      div(class="card border-0 shadow-sm", style="border-radius:8px;",
        card_hdr("journal-text", "Daily Question Log",
          tags$span(class="ms-auto badge",
            style=paste0("background:#e8f0f7; color:#003d5c; font-size:0.75rem;",
                         " font-weight:600; border-radius:20px; padding:4px 10px;"),
            paste0(format(n_q, big.mark=","), " entries"))),
        div(class="card-body",
          if (n_q == 0L)
            div(class="d-flex flex-column align-items-center py-4", style="color:#adb5bd;",
              tags$i(class="bi bi-journal-x",
                     style="font-size:2.5rem; opacity:0.35; margin-bottom:12px;"),
              tags$p("No question log entries yet.", style="font-size:0.92rem; margin:0;")
            )
          else {
            dates  <- as.Date(df$q_date[!is.na(df$q_date)])
            today  <- Sys.Date()

            # Stat calculations
            wks_all <- length(unique(lubridate::floor_date(dates, "week")))
            rate_pw  <- if (wks_all > 0) round(n_q / wks_all, 1) else 0

            last_mo_dates <- dates[dates >= (today - 30)]
            last_mo_n     <- length(last_mo_dates)

            yr_start      <- as.Date(paste0(
              if (as.numeric(format(today,"%m")) >= 7) format(today,"%Y") else
                as.character(as.numeric(format(today,"%Y")) - 1), "-07-01"))
            yr_n          <- sum(dates >= yr_start, na.rm=TRUE)

            # Rotation breakdown
            rot_raw <- as.character(df$q_rotation)
            rot_lbl <- unname(ROT_LKP[rot_raw])
            rot_lbl[is.na(rot_lbl)] <- paste0("Rotation ", rot_raw[is.na(rot_lbl)])
            rot_counts <- sort(table(rot_lbl), decreasing=TRUE)
            max_cnt <- max(rot_counts, na.rm=TRUE)

            stat_card <- function(val, lbl, ic, col)
              div(class="col-4",
                div(class="card border-0 shadow-sm text-center py-3",
                  style="border-radius:8px; background:#fff;",
                  tags$i(class=paste0("bi bi-",ic),
                         style=paste0("font-size:1.3rem; color:",col,"; opacity:0.85;")),
                  tags$p(class="mb-0 mt-1",
                    style="font-size:1.8rem; font-weight:700; color:var(--ssm-primary-blue);",
                    val),
                  tags$p(class="mb-0",
                    style="font-size:0.72rem; text-transform:uppercase; letter-spacing:.07em; color:#6c757d;",
                    lbl)
                )
              )

            tagList(
              info_banner("Self-reported daily practice questions. ",
                "Program target is 4+ per week."),

              # Stat strip
              div(class="row g-3 mb-4",
                stat_card(format(rate_pw, nsmall=1), "Per Week Avg",
                          "bar-chart-fill", "#003d5c"),
                stat_card(last_mo_n, "Last 30 Days",
                          "calendar-month-fill", "#0066a1"),
                stat_card(yr_n, "This Acad. Year",
                          "calendar-check-fill", "#27ae60")
              ),

              # Rotation breakdown
              if (length(rot_counts) > 0)
                div(
                  sec_lbl("geo-alt-fill", "Questions by Rotation"),
                  div(style="max-width:480px;",
                    lapply(names(rot_counts), function(rname) {
                      cnt   <- rot_counts[[rname]]
                      pct   <- round(cnt / max_cnt * 100)
                      div(style="display:flex; align-items:center; gap:10px; margin-bottom:6px;",
                        tags$span(style=paste0("min-width:130px; font-size:0.85rem;",
                                               " font-weight:600; color:#2c3e50;"), rname),
                        div(style=paste0("flex:1; background:#e9ecef; border-radius:4px; height:16px;",
                                         " overflow:hidden;"),
                          div(style=paste0("width:",pct,"%; height:100%;",
                                           " background:#0066a1; border-radius:4px;",
                                           " transition:width .3s;"))),
                        tags$span(style="font-size:0.83rem; color:#6c757d; min-width:24px;",
                                  cnt)
                      )
                    })
                  )
                )
            )
          }
        )
      )
    })

  })
}
