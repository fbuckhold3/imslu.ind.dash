# mod_self_eval.R — Embedded Self-Evaluation
#
# Eight improvements over v1:
#   1. Clear completion status in period table (color-coded rows + section counts)
#   2. Per-section completion checklist at top of each form
#   3. Milestone self-assessment via gmed mod_miles_rating module
#   4. ILP goals as subcompetency dropdowns (not free text)
#   5. Recent evaluations reference panel in reflection section
#   6. Auto-populate career/track from most recent previous s_eval
#   7. Previous learning topics shown before checkbox selection
#   8. Section progress card (same as #2 — unified with checklist)

# ── Constants ─────────────────────────────────────────────────────────────────

.PREP_LABELS <- c(
  "1"="Answer questions from interdisciplinary services",
  "2"="Comfort with junior medical students",
  "3"="Obtaining consent for procedures",
  "4"="Personal organization / day-to-day tasks",
  "5"="Writing orders and prescriptions",
  "6"="Looking up evidence-based recommendations",
  "7"="Presenting patients (organized, hypothesis-driven)",
  "8"="Documenting encounters efficiently",
  "9"="Providing and receiving handoffs",
  "10"="Recognizing urgent/emergent care needs",
  "11"="Delivering bad news / challenging communication",
  "12"="Recognizing when to ask for help",
  "13"="Managing ICU patients (vents, pressors)",
  "14"="Managing inpatient patients",
  "15"="Managing primary care clinic patients",
  "16"="Calling consults",
  "17"="Completing documentation on time"
)
.PREP_SCALE <- c("1"="Not at all prepared","2"="Slightly","3"="Moderately",
                 "4"="Very","5"="Extremely prepared")

# ILP goal subcompetency choices — grouped by the 3 ILP domains
# NOTE: no empty-string names (R parser rejects "" = ""); blank option added in .ilp_goal_select()
.SUBCOMP_CHOICES <- list(
  pcmk = c(
    "PC1 — History-taking / interviewing"        = "PC1 — History-taking / interviewing",
    "PC2 — Physical examination"                 = "PC2 — Physical examination",
    "PC3 — Clinical reasoning / synthesis"       = "PC3 — Clinical reasoning / synthesis",
    "PC4 — Diagnosis and management"             = "PC4 — Diagnosis and management",
    "PC5 — Procedures"                           = "PC5 — Procedures",
    "PC6 — Transitions of care / discharge"      = "PC6 — Transitions of care / discharge",
    "MK1 — Core medical knowledge"               = "MK1 — Core medical knowledge",
    "MK2 — Evidence-based medicine"              = "MK2 — Evidence-based medicine",
    "MK3 — Basic science application"            = "MK3 — Basic science application"
  ),
  sbppbl = c(
    "SBP1 — Patient safety / error prevention"   = "SBP1 — Patient safety / error prevention",
    "SBP2 — Quality improvement"                 = "SBP2 — Quality improvement",
    "SBP3 — System navigation / advocacy"        = "SBP3 — System navigation / advocacy",
    "PBLI1 — Self-directed learning"             = "PBLI1 — Self-directed learning",
    "PBLI2 — Teaching and education"             = "PBLI2 — Teaching and education"
  ),
  profics = c(
    "PROF1 — Professional conduct / reliability" = "PROF1 — Professional conduct / reliability",
    "PROF2 — Ethics and accountability"          = "PROF2 — Ethics and accountability",
    "PROF3 — Self-awareness / well-being"        = "PROF3 — Self-awareness / well-being",
    "PROF4 — Diversity, equity, inclusion"       = "PROF4 — Diversity, equity, inclusion",
    "ICS1 — Communication with patients/families"= "ICS1 — Communication with patients/families",
    "ICS2 — Communication with the care team"    = "ICS2 — Communication with the care team",
    "ICS3 — Documentation"                       = "ICS3 — Documentation"
  )
)

# Sections per period (for checklist)
.PERIOD_SECTIONS <- list(
  # Period 7: concerns shown but NOT tracked (optional); milestones + background added
  "7" = c(goals="Learning Goals", background="Background Questions",
          prep="Preparedness Ratings", topics="Topics & Learning Styles",
          milestones="Milestone Self-Assessment"),
  # Periods 1-5: boards section added
  "1" = c(reflection="Self-Reflection", career="Career Planning",
          topics="Topics & Learning Styles", feedback="Program Feedback",
          boards="Boards & ITE", milestones="Milestone Self-Assessment", ilp="ILP Goals"),
  "2" = c(reflection="Self-Reflection", career="Career Planning",
          topics="Topics & Learning Styles", feedback="Program Feedback",
          boards="Boards & ITE", milestones="Milestone Self-Assessment", ilp="ILP Goals"),
  "3" = c(reflection="Self-Reflection", career="Career Planning",
          topics="Topics & Learning Styles", feedback="Program Feedback",
          boards="Boards & ITE", milestones="Milestone Self-Assessment", ilp="ILP Goals"),
  "4" = c(reflection="Self-Reflection", career="Career Planning",
          topics="Topics & Learning Styles", feedback="Program Feedback",
          boards="Boards & ITE", milestones="Milestone Self-Assessment", ilp="ILP Goals"),
  "5" = c(reflection="Self-Reflection", career="Career Planning",
          topics="Topics & Learning Styles", feedback="Program Feedback",
          boards="Boards & ITE", milestones="Milestone Self-Assessment", ilp="ILP Goals"),
  # Period 6: board + alumni added
  "6" = c(reflection="Self-Reflection", board="Board Preparation",
          alumni="Graduation Info", topics="Topics & Learning Styles",
          feedback="Program Feedback", milestones="Milestone Self-Assessment")
)

# ── Non-reactive helpers ───────────────────────────────────────────────────────

.rc_save <- function(record_id, instrument, instance, fields) {
  tryCatch({
    row <- as.data.frame(
      c(list(record_id = as.character(record_id),
             redcap_repeat_instrument = instrument,
             redcap_repeat_instance   = as.character(instance)),
        lapply(fields, function(x)
          if (is.null(x) || length(x) == 0 || (length(x)==1 && is.na(x))) ""
          else as.character(x))),
      stringsAsFactors = FALSE, check.names = FALSE)
    resp <- httr::POST(
      url  = app_config$redcap_url,
      body = list(token = app_config$rdm_token, content = "record",
                  format = "json", type = "flat",
                  overwriteBehavior = "overwrite",
                  data = jsonlite::toJSON(row, auto_unbox = TRUE),
                  returnContent = "ids", returnFormat = "json"),
      encode = "form", httr::timeout(30))
    if (httr::status_code(resp) == 200)
      list(success=TRUE,  ts=format(Sys.time(), "%b %d %I:%M %p"))
    else
      list(success=FALSE, message=paste0("REDCap error (HTTP ",
             httr::status_code(resp),"): ",
             httr::content(resp,"text",encoding="UTF-8")))
  }, error = function(e) list(success=FALSE, message=paste("Error:", e$message)))
}

# Save to non-repeating resident_data instrument
.rc_save_resident <- function(record_id, fields) {
  tryCatch({
    row <- as.data.frame(
      c(list(record_id = as.character(record_id)),
        lapply(fields, function(x)
          if (is.null(x)||length(x)==0||(length(x)==1&&is.na(x))) ""
          else as.character(x))),
      stringsAsFactors=FALSE, check.names=FALSE)
    resp <- httr::POST(
      url  = app_config$redcap_url,
      body = list(token=app_config$rdm_token, content="record", format="json",
                  type="flat", overwriteBehavior="overwrite",
                  data=jsonlite::toJSON(row, auto_unbox=TRUE),
                  returnContent="ids", returnFormat="json"),
      encode="form", httr::timeout(30))
    if (httr::status_code(resp)==200)
      list(success=TRUE, ts=format(Sys.time(),"%b %d %I:%M %p"))
    else
      list(success=FALSE, message=paste0("REDCap error (",httr::status_code(resp),"): ",
             httr::content(resp,"text",encoding="UTF-8")))
  }, error=function(e) list(success=FALSE, message=paste("Error:",e$message)))
}

.checked_codes <- function(data_row, prefix) {
  if (is.null(data_row) || nrow(data_row) == 0) return(character(0))
  cols <- names(data_row)[startsWith(names(data_row), paste0(prefix,"___"))]
  Filter(nchar, sapply(cols, function(col) {
    v <- data_row[[col]][1]
    if (!is.na(v) && as.character(v) == "1") sub(paste0(prefix,"___"),"",col) else ""
  }))
}

.checkbox_fields <- function(prefix, choices, selected_codes) {
  setNames(lapply(names(choices), function(code)
    if (code %in% selected_codes) "1" else "0"),
    paste0(prefix,"___",names(choices)))
}

.dd_choices <- function(dd, fld) {
  if (is.null(dd)) return(NULL)
  r <- dd[dd$field_name == fld, ]
  if (nrow(r)==0 || is.na(r$select_choices_or_calculations[1])) return(NULL)
  parse_redcap_choices(r$select_choices_or_calculations[1])
}

.fv <- function(row, fld) {
  if (is.null(row) || nrow(row)==0 || !(fld %in% names(row))) return("")
  v <- row[[fld]][1]; if (is.na(v)) "" else as.character(v)
}

# Check if a named section is complete given current cached data
# res = resident_data row (for background/alumni sections stored outside s_eval)
.section_complete <- function(section, sr, ir, ms_data, res = NULL) {
  has <- function(row, fld) {
    if (is.null(row) || nrow(row)==0) return(FALSE)
    v <- if (fld %in% names(row)) row[[fld]][1] else NA
    !is.na(v) && nzchar(trimws(as.character(v)))
  }
  any_checked <- function(row, prefix) {
    if (is.null(row) || nrow(row)==0) return(FALSE)
    cols <- names(row)[startsWith(names(row), paste0(prefix,"___"))]
    any(sapply(cols, function(col) {
      v <- row[[col]][1]; !is.na(v) && as.character(v) == "1"
    }))
  }
  switch(section,
    goals      = has(sr, "s_e_ume_goal1"),
    background = has(res, "hs_mo"),            # in resident_data
    prep       = has(sr, "s_e_prep_1"),
    topics     = any_checked(sr, "s_e_topic_sel"),
    concerns   = TRUE,                         # optional — always counts as done
    reflection = has(sr, "s_e_plus") || has(sr, "s_e_delta"),
    career     = any_checked(sr, "s_e_career_path"),
    feedback   = has(sr, "s_e_prog_plus") || has(sr, "s_e_prog_delta"),
    boards     = has(sr, "s_e_step3"),         # periods 1-5
    milestones = {
      if (is.null(ms_data) || nrow(ms_data)==0) return(FALSE)
      has(ms_data, "rep_pc1_self")
    },
    ilp        = has(ir, "goal_pcmk") && has(ir, "goal_sbppbl") &&
                 has(ir, "goal_subcomp_profics"),
    board      = has(sr, "s_e_step3") || has(sr, "s_e_board_plan"),  # period 6
    alumni     = has(res, "grad_email"),       # in resident_data
    FALSE
  )
}

# ── Period completion (for status table) ──────────────────────────────────────

.period_status <- function(seva, ilp, ms_data, period, res = NULL) {
  p  <- as.character(period)
  sr <- if (!is.null(seva) && nrow(seva)>0) seva[as.character(seva$s_e_period)==p,] else NULL
  ir <- if (!is.null(ilp)  && nrow(ilp)>0)  ilp[as.character(ilp$year_resident)==p,] else NULL
  md <- if (!is.null(ms_data) && nrow(ms_data)>0) {
    rows <- ms_data[as.character(ms_data$redcap_repeat_instance)==p,]
    if (nrow(rows)>0) rows else NULL
  } else NULL

  secs <- .PERIOD_SECTIONS[[p]]
  if (is.null(secs)) return(list(status="not_started", done=0L, total=0L))
  done  <- sum(sapply(names(secs), function(s) .section_complete(s, sr, ir, md, res)))
  total <- length(secs)
  status <- if (done == total) "complete"
            else if (done > 0) "in_progress"
            else "not_started"
  list(status=status, done=as.integer(done), total=as.integer(total))
}

# ── UI component helpers ──────────────────────────────────────────────────────

.sec_card <- function(..., title, icon, id=NULL)
  div(class="card border-0 shadow-sm mb-3", style="border-radius:8px;", id=id,
    div(class="card-header border-0 d-flex align-items-center gap-2",
        style="background:#f8fafc; border-radius:8px 8px 0 0; padding:12px 18px;",
        tags$i(class=paste0("bi bi-",icon), style="color:#003d5c; font-size:1rem;"),
        tags$span(style="font-weight:700; color:#003d5c; font-size:0.95rem;", title)),
    div(class="card-body", ...))

.save_btn <- function(ns, id, label="Save Section")
  div(class="d-flex align-items-center gap-2 mt-3",
    actionButton(ns(id), label, class="btn btn-sm",
      style="background:#003d5c; color:#fff; border:none; padding:6px 18px;"),
    uiOutput(ns(paste0(id,"_status"))))

.save_status_ui <- function(result) {
  if (is.null(result)) return(NULL)
  if (isTRUE(result$success))
    tags$span(class="text-success", style="font-size:0.8rem;",
      tags$i(class="bi bi-check-circle-fill me-1"), paste("Saved", result$ts))
  else
    tags$span(class="text-danger", style="font-size:0.8rem;",
      tags$i(class="bi bi-exclamation-triangle-fill me-1"), result$message)
}

.ta <- function(inputId, label, value="", rows=3, placeholder="")
  div(class="mb-3",
    if (!is.null(label) && nzchar(label))
      tags$label(label, class="form-label fw-semibold",
                 style="font-size:0.85rem; color:#2c3e50;"),
    tags$textarea(id=inputId, class="form-control", rows=rows,
                  placeholder=placeholder,
                  style="font-size:0.88rem; resize:vertical;", value))

# Preparedness rating matrix — replaces 17 individual dropdowns
.prep_matrix_ui <- function(ns, sr) {
  scale_labels <- c("1"="Not prepared","2"="Slightly","3"="Moderately","4"="Very","5"="Extremely")
  header_row <- tags$tr(
    tags$th(style="width:45%; font-size:0.78rem; font-weight:600; color:#6c757d;", "Skill area"),
    lapply(names(scale_labels), function(v)
      tags$th(style="text-align:center; font-size:0.72rem; color:#6c757d; font-weight:500; min-width:68px;",
              scale_labels[[v]]))
  )
  body_rows <- lapply(names(.PREP_LABELS), function(n) {
    cur <- .fv(sr, paste0("s_e_prep_",n))
    tags$tr(
      style=paste0("background:", if (as.integer(n)%%2==0) "#f8fafc" else "white", ";"),
      tags$td(style="font-size:0.83rem; color:#2c3e50; padding:6px 8px;", .PREP_LABELS[[n]]),
      lapply(names(scale_labels), function(v)
        tags$td(style="text-align:center; padding:4px;",
          tags$input(type="radio",
            name=ns(paste0("s_e_prep_",n)),
            id=paste0(ns(paste0("s_e_prep_",n)),"_",v),
            value=v,
            class="form-check-input",
            style="cursor:pointer; width:1.1em; height:1.1em;",
            if (cur==v) list(checked="checked") else list())))
    )
  })
  div(class="table-responsive",
    tags$table(class="table table-bordered mb-0",
               style="font-size:0.82rem;",
      tags$thead(header_row),
      tags$tbody(body_rows)))
}

# ITE score panel + board self-report questions (periods 1-5)
.ite_board_section_ui <- function(ns, sr, dd, ite_data, pgy) {
  # ITE percent correct for current PGY year
  pgy_fld <- paste0("pgy", pgy, "_tot_correct")
  ite_pct  <- if (!is.null(ite_data) && nrow(ite_data)>0 && pgy_fld %in% names(ite_data)) {
    v <- ite_data[[pgy_fld]][1]; if (!is.na(v) && nzchar(v)) as.numeric(v) else NA
  } else NA_real_

  ite_panel <- if (!is.na(ite_pct)) {
    risk_col   <- if (ite_pct >= 70) "#2e7d32" else if (ite_pct >= 60) "#e65100" else "#c62828"
    risk_label <- if (ite_pct >= 70) "On track" else if (ite_pct >= 60) "Monitor" else "At risk"
    div(class="alert mb-3 py-2 px-3",
        style=paste0("background:#f8fafc; border-left:4px solid ", risk_col, "; font-size:0.83rem;"),
      div(class="d-flex align-items-center gap-3",
        div(style="text-align:center; min-width:60px;",
          tags$span(style=paste0("font-size:1.5rem; font-weight:700; color:", risk_col),
                    paste0(round(ite_pct,1),"%")),
          tags$p(style="font-size:0.68rem; color:#6c757d; margin:0;", "ITE % correct")),
        div(style="flex:1;",
          tags$p(style="margin:0; font-weight:600; font-size:0.85rem; color:#003d5c;",
                 paste0("PGY", pgy, " ACP ITE Score")),
          tags$p(style="margin:0; font-size:0.78rem; color:#6c757d;",
                 paste0("Board readiness: "),
                 tags$span(style=paste0("font-weight:700; color:", risk_col), risk_label))))
    )
  } else {
    div(class="alert alert-light mb-3 py-2 px-3",
        style="font-size:0.82rem; border-left:4px solid #adb5bd;",
        tags$i(class="bi bi-info-circle me-1"),
        "ITE score not yet available. Complete your ACP ITE to see your score here.")
  }

  # Board yesno + MKSAP fields
  mksap_ch <- .dd_choices(dd, "s_e_mksap_comp")
  yn_row <- function(id, label, val) {
    div(class="mb-2 d-flex align-items-center gap-3",
      tags$label(label, style="font-size:0.83rem; color:#2c3e50; min-width:280px; margin:0;"),
      div(class="btn-group btn-group-sm",
        tags$input(type="radio", class="btn-check", name=ns(id),
                   id=paste0(ns(id),"_1"), value="1",
                   if (val=="1") list(checked="checked") else list()),
        tags$label(class=paste0("btn btn-outline-success",
                                if (val=="1") " active" else ""),
                   `for`=paste0(ns(id),"_1"), "Yes"),
        tags$input(type="radio", class="btn-check", name=ns(id),
                   id=paste0(ns(id),"_0"), value="0",
                   if (val=="0") list(checked="checked") else list()),
        tags$label(class=paste0("btn btn-outline-secondary",
                                if (val=="0") " active" else ""),
                   `for`=paste0(ns(id),"_0"), "No")))
  }

  tagList(
    ite_panel,
    yn_row("s_e_step3", "Have you completed Step 3 / USMLE?", .fv(sr,"s_e_step3")),
    yn_row("s_e_step3_contact", "Have you sent your score to the program?",
           .fv(sr,"s_e_step3_contact")),
    yn_row("s_e_step3_date_set", "Have you set a Step 3 exam date?",
           .fv(sr,"s_e_step3_date_set")),
    div(class="mb-2",
      tags$label("Step 3 scheduled date (if applicable)",
                 style="font-size:0.83rem; color:#2c3e50;"),
      tags$input(type="date", id=ns("s_e_step3_date"),
                 class="form-control form-control-sm", style="max-width:200px;",
                 value=.fv(sr,"s_e_step3_date"))),
    div(class="mt-3 mb-2 pt-2", style="border-top:1px solid #f0f0f0;",
      yn_row("s_e_board_concern",
             "Do you have concerns about failing boards?", .fv(sr,"s_e_board_concern")),
      yn_row("s_e_board_help",
             "Have you discussed this with the program?", .fv(sr,"s_e_board_help")),
      .ta(ns("s_e_board_discu"), "Who have you spoken with and what steps are being taken?",
          .fv(sr,"s_e_board_discu"), rows=2)),
    if (!is.null(mksap_ch)) div(class="mb-2",
      tags$label("MKSAP completion",
                 style="font-size:0.83rem; color:#2c3e50; font-weight:600;"),
      tags$select(id=ns("s_e_mksap_comp"), class="form-select form-select-sm",
                  style="max-width:340px;",
        tags$option(value="", if(.fv(sr,"s_e_mksap_comp")=="") "-- select --"),
        lapply(names(mksap_ch), function(v)
          tags$option(value=v,
            selected=if(.fv(sr,"s_e_mksap_comp")==v) NA else NULL,
            mksap_ch[[v]]))))
  )
}

.section_hdr <- function(icon, title, subtitle=NULL)
  div(class="mb-3",
    tags$h6(style="color:#003d5c; font-weight:700; margin-bottom:2px;",
      tags$i(class=paste0("bi bi-",icon," me-2")), title),
    if (!is.null(subtitle))
      tags$p(class="text-muted", style="font-size:0.8rem; margin:0;", subtitle))

# Section progress card (items 2 & 8)
.section_checklist_card <- function(sections, sr, ir, ms_data, res = NULL) {
  if (is.null(sections)) return(NULL)
  items <- lapply(names(sections), function(s) {
    done <- .section_complete(s, sr, ir, ms_data, res)
    div(class="d-flex align-items-center gap-2",
        style="min-width:140px;",
      (if (done) tags$i(class="bi bi-check-circle-fill", style="color:#2e7d32; font-size:0.9rem;")
       else       tags$i(class="bi bi-circle",           style="color:#adb5bd; font-size:0.9rem;")),
      tags$span(style=paste0("font-size:0.8rem; color:",
                             if(done) "#1a6b3a" else "#6c757d"), sections[[s]]))
  })
  n_done  <- sum(sapply(names(sections), function(s) .section_complete(s, sr, ir, ms_data, res)))
  n_total <- length(sections)
  all_done <- n_done == n_total
  div(class="card border-0 mb-4",
      style=paste0("background:", if(all_done) "#f0faf4" else "#f8fafc",
                   "; border-left:3px solid ",
                   if(all_done) "#2e7d32" else if(n_done>0) "#e65100" else "#adb5bd",
                   " !important; border-radius:6px;"),
    div(class="card-body py-2 px-3",
      div(class="d-flex align-items-center justify-content-between mb-2",
        tags$span(style="font-size:0.72rem; font-weight:700; text-transform:uppercase; letter-spacing:.07em; color:#6c757d;",
          tags$i(class="bi bi-list-check me-1"), "Section Progress"),
        tags$span(style=paste0("font-size:0.8rem; font-weight:700; color:",
                               if(all_done) "#2e7d32" else "#6c757d"),
          paste0(n_done,"/",n_total," complete"),
          if(all_done) tags$i(class="bi bi-stars ms-1", style="color:#2e7d32;"))),
      div(class="d-flex flex-wrap gap-3", items)))
}

# ── UI ────────────────────────────────────────────────────────────────────────

mod_self_eval_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("status_table")),
    div(class="mt-3", uiOutput(ns("period_form")))
  )
}

# ── Server ────────────────────────────────────────────────────────────────────

mod_self_eval_server <- function(id, rdm_data, resident_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── data reactives ────────────────────────────────────────────────────────
    resident_r <- reactive({
      req(rdm_data(), resident_id())
      df <- rdm_data()$residents
      if (is.null(df)) return(NULL)
      df[df$record_id == resident_id(), ]
    })

    period_info_r <- reactive({
      res <- resident_r(); req(res, nrow(res)>0)
      gy  <- suppressWarnings(as.numeric(res$grad_yr[1]))
      tp  <- suppressWarnings(as.numeric(res$type[1]))
      if (is.na(gy)) {
        dd <- rdm_data()$data_dict
        ch <- .dd_choices(dd, "grad_yr")
        if (!is.null(ch)) gy <- suppressWarnings(as.numeric(ch[as.character(res$grad_yr[1])]))
      }
      if (is.na(gy)) return(NULL)
      calculate_pgy_and_period(gy, tp)
    })

    # Period name reactive (for milestone module)
    period_name_r <- reactive({
      p <- local$sel_period; if (is.null(p)) return(NULL)
      c("7"="Entering Residency","1"="Mid Intern","2"="End Intern",
        "3"="Mid PGY2","4"="End PGY2","5"="Mid PGY3","6"="Graduating")[[p]] %||%
        paste0("Period ", p)
    })

    data_dict_r  <- reactive({ req(rdm_data()); rdm_data()$data_dict })
    evals_r      <- reactive({
      req(rdm_data(), resident_id())
      df <- rdm_data()$assessment %||% rdm_data()$all_forms$assessment
      if (is.null(df) || nrow(df)==0) return(NULL)
      df[df$record_id == resident_id(), ]
    })
    ite_data_r <- reactive({
      req(rdm_data(), resident_id())
      df <- rdm_data()$all_forms$test_data %||% rdm_data()$test_data
      if (is.null(df) || nrow(df)==0) return(NULL)
      df[df$record_id == resident_id(), ]
    })

    # Local live cache
    local <- reactiveValues(seva=NULL, ilp=NULL, ms=NULL, ready=FALSE, sel_period=NULL)

    observe({
      req(rdm_data(), resident_id(), !local$ready)
      rid <- resident_id()
      df_s <- rdm_data()$all_forms$s_eval
      df_i <- rdm_data()$all_forms$ilp
      df_m <- rdm_data()$all_forms$milestone_selfevaluation_c33c %||%
              rdm_data()$milestone_selfevaluation_c33c
      local$seva  <- if (!is.null(df_s)) df_s[df_s$record_id==rid,] else data.frame()
      local$ilp   <- if (!is.null(df_i)) df_i[df_i$record_id==rid,] else data.frame()
      local$ms    <- if (!is.null(df_m)) df_m[df_m$record_id==rid,] else data.frame()
      local$ready <- TRUE
      pi <- isolate(period_info_r())
      if (!is.null(pi) && !is.na(pi$period_number))
        local$sel_period <- as.character(pi$period_number)
    })

    sel_seva <- reactive({
      p <- local$sel_period
      if (is.null(p)||is.null(local$seva)||nrow(local$seva)==0) return(data.frame())
      local$seva[as.character(local$seva$s_e_period)==p,]
    })
    sel_ilp <- reactive({
      p <- local$sel_period
      if (is.null(p)||is.null(local$ilp)||nrow(local$ilp)==0) return(data.frame())
      local$ilp[as.character(local$ilp$year_resident)==p,]
    })
    sel_ms <- reactive({
      p <- local$sel_period
      if (is.null(p)||is.null(local$ms)||nrow(local$ms)==0) return(data.frame())
      local$ms[as.character(local$ms$redcap_repeat_instance)==p,]
    })

    # Previous period's s_eval for auto-populate (#6) and previous topics (#7)
    .PERIOD_CHRON <- c("7"=0L,"1"=1L,"2"=2L,"3"=3L,"4"=4L,"5"=5L,"6"=6L)
    prev_seva_r <- reactive({
      p <- local$sel_period; if (is.null(p)) return(data.frame())
      df <- local$seva; if (is.null(df)||nrow(df)==0) return(data.frame())
      cur_chron <- .PERIOD_CHRON[p] %||% -1L
      df2 <- df %>%
        dplyr::mutate(.chron = unname(.PERIOD_CHRON[as.character(s_e_period)])) %>%
        dplyr::filter(.chron < cur_chron) %>%
        dplyr::arrange(dplyr::desc(.chron)) %>%
        dplyr::slice(1) %>%
        dplyr::select(-.chron)
      df2
    })

    # ── milestone module (initialised once, period reactive) ──────────────────
    milestone_result <- mod_miles_rating_server("milestone_rating", period = period_name_r)

    observeEvent(milestone_result$done(), {
      req(isTRUE(milestone_result$done()), local$sel_period)
      scores <- milestone_result$scores()
      descs  <- milestone_result$desc()
      p      <- as.integer(local$sel_period)

      field_map <- get_milestone_field_mapping_rdm2("milestone_selfevaluation_c33c")
      fields    <- list(prog_mile_date   = format(Sys.Date(),"%Y-%m-%d"),
                        prog_mile_period = as.character(p))
      for (key in names(scores)) {
        rdm_fld <- field_map[key]
        if (!is.na(rdm_fld)) {
          fields[[rdm_fld]] <- as.character(scores[[key]])
          if (!is.null(descs[[key]]) && nzchar(trimws(descs[[key]])))
            fields[[paste0(rdm_fld,"_desc")]] <- descs[[key]]
        }
      }
      res <- .rc_save(resident_id(), "milestone_selfevaluation_c33c", p, fields)
      ss$milestones <- res
      if (res$success) {
        existing <- local$ms
        if (is.null(existing)) existing <- data.frame()
        row_idx <- which(as.character(existing$redcap_repeat_instance)==as.character(p))
        new_row <- as.data.frame(c(list(record_id=resident_id(),
                                        redcap_repeat_instance=as.character(p)),
                                   lapply(fields, as.character)),
                                 stringsAsFactors=FALSE, check.names=FALSE)
        if (length(row_idx)>0) existing[row_idx[1],] <- new_row[1,]
        else existing <- dplyr::bind_rows(existing, new_row)
        local$ms <- existing
      }
    })

    # ── save state ────────────────────────────────────────────────────────────
    ss <- reactiveValues(
      goals=NULL, background=NULL, prep=NULL, topics=NULL, concerns=NULL,
      reflection=NULL, career=NULL, feedback=NULL, ilp=NULL,
      boards=NULL, board=NULL, alumni=NULL, milestones=NULL)

    # Cache merge helpers
    .merge_seva <- function(period, fields) {
      p <- as.character(period); df <- local$seva
      idx <- if (!is.null(df)&&nrow(df)>0) which(as.character(df$s_e_period)==p) else integer(0)
      if (length(idx)>0) { for (fld in names(fields)) df[idx[1],fld] <- as.character(fields[[fld]]) }
      else df <- dplyr::bind_rows(df, as.data.frame(c(
        list(record_id=resident_id(), s_e_period=p, redcap_repeat_instance=p),
        lapply(fields,as.character)), stringsAsFactors=FALSE, check.names=FALSE))
      local$seva <- df
    }
    .merge_ilp <- function(period, fields) {
      p <- as.character(period); df <- local$ilp
      idx <- if (!is.null(df)&&nrow(df)>0) which(as.character(df$year_resident)==p) else integer(0)
      if (length(idx)>0) { for (fld in names(fields)) df[idx[1],fld] <- as.character(fields[[fld]]) }
      else df <- dplyr::bind_rows(df, as.data.frame(c(
        list(record_id=resident_id(), year_resident=p, redcap_repeat_instance=p),
        lapply(fields,as.character)), stringsAsFactors=FALSE, check.names=FALSE))
      local$ilp <- df
    }

    # ── save handlers ─────────────────────────────────────────────────────────
    output$save_goals_status      <- renderUI(.save_status_ui(ss$goals))
    output$save_background_status <- renderUI(.save_status_ui(ss$background))
    output$save_prep_status       <- renderUI(.save_status_ui(ss$prep))
    output$save_topics_status     <- renderUI(.save_status_ui(ss$topics))
    output$save_concerns_status   <- renderUI(.save_status_ui(ss$concerns))
    output$save_reflection_status <- renderUI(.save_status_ui(ss$reflection))
    output$save_career_status     <- renderUI(.save_status_ui(ss$career))
    output$save_feedback_status  <- renderUI(.save_status_ui(ss$feedback))
    output$save_ilp_status       <- renderUI(.save_status_ui(ss$ilp))
    output$save_boards_status    <- renderUI(.save_status_ui(ss$boards))
    output$save_board_status     <- renderUI(.save_status_ui(ss$board))
    output$save_alumni_status    <- renderUI(.save_status_ui(ss$alumni))
    output$milestone_save_status <- renderUI(.save_status_ui(ss$milestones))

    observeEvent(input$save_goals, {
      req(local$sel_period=="7")
      f <- list(s_e_period="7", s_e_ume_goal1=input$s_e_ume_goal1%||%"",
                s_e_ume_goal2=input$s_e_ume_goal2%||%"",
                s_e_ume_goal3=input$s_e_ume_goal3%||%"")
      res <- .rc_save(resident_id(),"s_eval",7,f); ss$goals <- res
      if (res$success) .merge_seva(7,f)
    })
    observeEvent(input$save_prep, {
      req(local$sel_period=="7")
      f <- c(list(s_e_period="7"), setNames(
        lapply(names(.PREP_LABELS), function(n) input[[paste0("s_e_prep_",n)]]%||%""),
        paste0("s_e_prep_",names(.PREP_LABELS))))
      res <- .rc_save(resident_id(),"s_eval",7,f); ss$prep <- res
      if (res$success) .merge_seva(7,f)
    })
    observeEvent(input$save_topics, {
      p <- local$sel_period; req(p); dd <- data_dict_r()
      tc <- .dd_choices(dd,"s_e_topic_sel"); sc <- .dd_choices(dd,"s_e_learn_style")
      f <- c(list(s_e_period=p,
                  s_e_topic_oth=input$s_e_topic_oth%||%"",
                  s_e_learn_oth=input$s_e_learn_oth%||%""),
             if(!is.null(tc)) .checkbox_fields("s_e_topic_sel",tc,input$s_e_topic_sel%||%character(0)),
             if(!is.null(sc)) .checkbox_fields("s_e_learn_style",sc,input$s_e_learn_style%||%character(0)))
      res <- .rc_save(resident_id(),"s_eval",as.integer(p),f); ss$topics <- res
      if (res$success) .merge_seva(p,f)
    })
    observeEvent(input$save_concerns, {
      req(local$sel_period=="7")
      f <- list(s_e_period="7", s_e_ume_concern=input$s_e_ume_concern%||%"")
      res <- .rc_save(resident_id(),"s_eval",7,f); ss$concerns <- res
      if (res$success) .merge_seva(7,f)
    })
    # Background questions → resident_data (non-repeating)
    observeEvent(input$save_background, {
      f <- list(hs_mo     = input$hs_mo     %||% "",
                college_mo= input$college_mo %||% "",
                med_mo    = input$med_mo     %||% "",
                slusom    = input$slusom     %||% "")
      res <- .rc_save_resident(resident_id(), f); ss$background <- res
    })
    # Boards section → s_eval (periods 1-5)
    observeEvent(input$save_boards, {
      p <- local$sel_period; req(p %in% as.character(1:5))
      f <- list(s_e_period=p,
                s_e_step3         = input$s_e_step3          %||% "",
                s_e_step3_contact = input$s_e_step3_contact   %||% "",
                s_e_step3_date_set= input$s_e_step3_date_set  %||% "",
                s_e_step3_date    = input$s_e_step3_date       %||% "",
                s_e_board_concern = input$s_e_board_concern    %||% "",
                s_e_board_help    = input$s_e_board_help       %||% "",
                s_e_board_discu   = input$s_e_board_discu      %||% "",
                s_e_mksap_comp    = input$s_e_mksap_comp       %||% "")
      res <- .rc_save(resident_id(),"s_eval",as.integer(p),f); ss$boards <- res
      if (res$success) .merge_seva(p,f)
    })
    observeEvent(input$save_reflection, {
      p <- local$sel_period; req(p %in% as.character(1:6))
      f <- list(s_e_period=p, s_e_plus=input$s_e_plus%||%"", s_e_delta=input$s_e_delta%||%"")
      res <- .rc_save(resident_id(),"s_eval",as.integer(p),f); ss$reflection <- res
      if (res$success) .merge_seva(p,f)
    })
    observeEvent(input$save_career, {
      p <- local$sel_period; req(p %in% as.character(1:5)); dd <- data_dict_r()
      cp <- .dd_choices(dd,"s_e_career_path"); fl <- .dd_choices(dd,"s_e_fellow")
      tr <- .dd_choices(dd,"s_e_track_type")
      f <- c(list(s_e_period=p),
             if(!is.null(cp)) .checkbox_fields("s_e_career_path",cp,input$s_e_career_path%||%character(0)),
             if(!is.null(fl)) .checkbox_fields("s_e_fellow",fl,input$s_e_fellow%||%character(0)),
             if(!is.null(tr)) .checkbox_fields("s_e_track_type",tr,input$s_e_track_type%||%character(0)),
             list(s_e_discussion=input$s_e_discussion%||%""))
      res <- .rc_save(resident_id(),"s_eval",as.integer(p),f); ss$career <- res
      if (res$success) .merge_seva(p,f)
    })
    observeEvent(input$save_feedback, {
      p <- local$sel_period; req(p %in% as.character(1:6))
      f <- list(s_e_period=p, s_e_prog_plus=input$s_e_prog_plus%||%"",
                s_e_prog_delta=input$s_e_prog_delta%||%"",
                s_e_progfeed=input$s_e_progfeed%||%"")
      res <- .rc_save(resident_id(),"s_eval",as.integer(p),f); ss$feedback <- res
      if (res$success) .merge_seva(p,f)
    })
    observeEvent(input$save_ilp, {
      p <- local$sel_period; req(p %in% as.character(1:5))
      f <- list(year_resident=p,
                goal_pcmk=input$goal_pcmk%||%"", how_pcmk=input$how_pcmk%||%"",
                goal_sbppbl=input$goal_sbppbl%||%"", how_sbppbl=input$how_sbppbl%||%"",
                goal_subcomp_profics=input$goal_subcomp_profics%||%"",
                how_profics=input$how_profics%||%"")
      res <- .rc_save(resident_id(),"ilp",as.integer(p),f); ss$ilp <- res
      if (res$success) .merge_ilp(p,f)
    })
    observeEvent(input$save_board, {
      req(local$sel_period=="6")
      f <- list(s_e_period="6",
                s_e_step3         = input$s_e_step3          %||% "",
                s_e_step3_contact = input$s_e_step3_contact   %||% "",
                s_e_board_concern = input$s_e_board_concern    %||% "",
                s_e_board_help    = input$s_e_board_help       %||% "",
                s_e_board_discu   = input$s_e_board_discu      %||% "",
                s_e_mksap_comp    = input$s_e_mksap_comp       %||% "",
                s_e_board_plan    = input$s_e_board_plan        %||% "")
      res <- .rc_save(resident_id(),"s_eval",6,f); ss$board <- res
      if (res$success) .merge_seva(6,f)
    })
    # Alumni / graduation data → resident_data (non-repeating)
    observeEvent(input$save_alumni, {
      f <- list(grad_spec          = input$grad_spec           %||% "",
                res_alumni_position= input$res_alumni_position  %||% "",
                res_alumni_academic= input$res_alumni_academic  %||% "",
                chief              = input$chief               %||% "",
                ssm                = input$ssm                 %||% "",
                mo_prac            = input$mo_prac             %||% "",
                rural              = input$rural               %||% "",
                und_urban          = input$und_urban           %||% "",
                grad_email         = input$grad_email          %||% "",
                grad_phone         = input$grad_phone          %||% "")
      res <- .rc_save_resident(resident_id(), f); ss$alumni <- res
    })

    # ── period selection ──────────────────────────────────────────────────────
    observeEvent(input$select_period, {
      local$sel_period <- input$select_period
      for (n in names(reactiveValuesToList(ss))) ss[[n]] <- NULL
    })

    # ── status table (#7 — high-contrast redesign, clearly separate from form) ──
    output$status_table <- renderUI({
      req(local$ready)
      res_row <- resident_r(); if (is.null(res_row)||nrow(res_row)==0) return(NULL)
      tp  <- suppressWarnings(as.numeric(res_row$type[1]))
      if (!is.na(tp) && tp==3)
        return(div(class="alert alert-warning",
                   tags$i(class="bi bi-exclamation-triangle me-2"),
                   "Self-evaluation not available for this record."))
      periods  <- if (!is.na(tp) && tp==1) c(7L,1L,2L) else c(7L,1L,2L,3L,4L,5L,6L)
      pi       <- period_info_r()
      active_p <- if (!is.null(pi)&&!is.na(pi$period_number))
                    as.character(pi$period_number) else NULL

      period_names <- c("7"="Entering Residency","1"="Mid-Intern","2"="End Intern Year",
                        "3"="Mid-PGY2","4"="End PGY2","5"="Mid-PGY3","6"="Graduating")

      # Count how many periods are fully complete
      n_complete <- sum(sapply(periods, function(p) {
        ps <- .period_status(local$seva, local$ilp, local$ms, p, res_row)
        ps$status == "complete"
      }))

      rows <- lapply(periods, function(p) {
        p_str <- as.character(p)
        ps    <- .period_status(local$seva, local$ilp, local$ms, p, res_row)
        is_sel <- isTRUE(local$sel_period == p_str)
        is_act <- isTRUE(active_p == p_str)

        # Colors per status
        status_col <- switch(ps$status,
          complete    = list(border="#2e7d32", bg="#f0faf4", badge_bg="#c8e6c9", text="#1a6b3a"),
          in_progress = list(border="#bf360c", bg="#fff8f5", badge_bg="#ffe0b2", text="#bf360c"),
                        list(border="#9e9e9e", bg="#fafafa",  badge_bg="#eeeeee", text="#757575"))

        icon_el <- switch(ps$status,
          complete    = tags$i(class="bi bi-check-circle-fill",
                               style=paste0("color:",status_col$border,"; font-size:1rem;")),
          in_progress = tags$i(class="bi bi-circle-half",
                               style=paste0("color:",status_col$border,"; font-size:1rem;")),
          tags$i(class="bi bi-circle", style="color:#bdbdbd; font-size:1rem;"))

        badge_txt <- if (ps$total>0) paste0(ps$done,"/",ps$total) else ""

        row_bg <- if (is_sel) "#e3eef8" else status_col$bg
        row_border <- if (is_sel) "#0066a1" else status_col$border
        row_outline <- if (is_sel) "2px solid #0066a1" else "1px solid #e8e8e8"

        div(style=paste0("cursor:pointer; display:flex; align-items:center; gap:10px;",
                         " padding:9px 14px; border-radius:7px; margin-bottom:4px;",
                         " background:", row_bg, ";",
                         " outline:", row_outline, ";",
                         " border-left: 4px solid ", row_border, ";"),
            onclick=sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})",
                            ns("select_period"), p_str),
          div(style="width:20px; text-align:center; flex-shrink:0;", icon_el),
          div(style="flex:1; min-width:0;",
            tags$span(style=paste0("font-size:0.87rem; font-weight:",
                                   if(is_sel||is_act)"700" else "500",
                                   "; color:", if(is_sel)"#003d5c" else "#2c3e50"),
              period_names[[p_str]]),
            if (is_act) tags$span(style="font-size:0.68rem; background:#0066a1; color:#fff;
                                        border-radius:10px; padding:1px 7px; margin-left:6px;",
                                  "Current"),
            if (isTRUE(pi$is_prestart) && p_str=="7")
              tags$span(style="font-size:0.68rem; background:#6f42c1; color:#fff;
                               border-radius:10px; padding:1px 7px; margin-left:4px;",
                        "Early access")),
          if (nzchar(badge_txt))
            tags$span(style=paste0("font-size:0.72rem; font-weight:700;",
                                   " background:", status_col$badge_bg,
                                   "; color:", status_col$text,
                                   "; border-radius:20px; padding:2px 9px; white-space:nowrap;"),
                      badge_txt))
      })

      # Overall progress summary bar
      n_tot <- length(periods)
      pct   <- round(100 * n_complete / n_tot)
      prog_col <- if (pct==100) "#2e7d32" else if (pct>0) "#bf360c" else "#9e9e9e"

      div(
        # ── Completion overview panel ──────────────────────────────────────────
        div(style=paste0("background:#1a2e42; border-radius:10px 10px 0 0;",
                         " padding:14px 18px 10px;"),
          div(style="display:flex; align-items:center; justify-content:space-between;",
            div(
              tags$p(style="margin:0; font-size:0.65rem; font-weight:700; letter-spacing:.1em;
                           color:#7fb3d3; text-transform:uppercase;",
                     "Self-Evaluation"),
              tags$p(style="margin:0; font-size:1rem; font-weight:700; color:#ffffff;",
                     "Completion Overview")),
            tags$span(style=paste0("font-size:1.4rem; font-weight:800; color:",
                                   if(pct==100)"#81c784" else "#f9a825"),
                      paste0(pct, "%"))),
          div(style="margin-top:8px; background:rgba(255,255,255,0.15); border-radius:4px; height:5px;",
            div(style=paste0("width:", pct, "%; height:100%; border-radius:4px; background:",
                             if(pct==100)"#81c784" else "#f9a825", ";")))),

        # ── Period rows ────────────────────────────────────────────────────────
        div(style="background:#ffffff; border:1px solid #e0e0e0; border-top:none;
                   border-radius:0 0 10px 10px; padding:10px 12px 8px;",
          tags$p(style="font-size:0.73rem; color:#757575; margin:0 0 8px 2px;",
                 tags$i(class="bi bi-hand-index me-1"), "Click any period to open its evaluation"),
          rows),

        # ── Separator before form ──────────────────────────────────────────────
        div(style="margin-top:16px; margin-bottom:4px; display:flex; align-items:center; gap:10px;",
          div(style="flex:1; height:1px; background:#e0e0e0;"),
          tags$span(style="font-size:0.72rem; font-weight:700; color:#9e9e9e;
                           text-transform:uppercase; letter-spacing:.08em; white-space:nowrap;",
                    tags$i(class="bi bi-chevron-double-down me-1"),
                    "Self-Evaluation Form"),
          div(style="flex:1; height:1px; background:#e0e0e0;"))
      )
    })

    # ── period form ───────────────────────────────────────────────────────────
    output$period_form <- renderUI({
      req(local$ready, local$sel_period)
      p    <- local$sel_period
      sr   <- sel_seva(); ir <- sel_ilp(); md <- sel_ms()
      dd   <- data_dict_r()
      psr  <- prev_seva_r()
      ev   <- evals_r()
      res  <- resident_r()
      ite  <- ite_data_r()
      pi   <- period_info_r()
      pgy  <- if (!is.null(pi)) as.integer(pi$pgy_year) else 1L
      secs <- .PERIOD_SECTIONS[[p]]
      chk  <- .section_checklist_card(secs, sr, ir, md, res)

      if (p=="7")      .form_p7(ns, p, sr, dd, psr, res, chk)
      else if (p=="6") .form_p6(ns, p, sr, dd, psr, ev, res, chk, ite)
      else             .form_std(ns, p, sr, ir, dd, psr, ev, chk, ite, pgy)
    })

    # Milestone UI lives outside renderUI so the server stays initialized
    output$milestone_ui <- renderUI({ mod_miles_rating_ui(ns("milestone_rating")) })

  }) # end moduleServer
}

# ── Form helpers ──────────────────────────────────────────────────────────────

# Recent evaluations reference panel (#5) — shows all available fields
.eval_reference_panel <- function(ev) {
  if (is.null(ev) || nrow(ev)==0) return(NULL)

  # Sort by date descending, show up to 5 most recent
  ev_sorted <- ev %>%
    dplyr::mutate(.dt = suppressWarnings(as.Date(ass_date, "%Y-%m-%d"))) %>%
    dplyr::arrange(dplyr::desc(.dt)) %>%
    dplyr::slice(1:min(5, dplyr::n())) %>%
    dplyr::select(-.dt)

  level_map <- c("1"="Intern","2"="PGY2","3"="PGY3","4"="Graduated","5"="Rotator")

  items <- lapply(seq_len(nrow(ev_sorted)), function(i) {
    row   <- ev_sorted[i,]
    fget  <- function(f) if (f %in% names(row)) .fv(row,f) else ""
    dt    <- fget("ass_date"); fac   <- fget("ass_faculty")
    spec  <- fget("ass_specialty"); rot <- fget("ass_rotator")
    lev   <- fget("ass_level"); plus  <- fget("ass_plus"); delta <- fget("ass_delta")

    lev_label <- if (nzchar(lev)) level_map[lev] %||% lev else ""
    meta_parts <- Filter(nzchar, c(fac, spec, rot, lev_label, dt))

    div(class="mb-3 pb-2", style="border-bottom:1px solid #eef0f3;",
      tags$p(class="mb-1",
             style="font-size:0.72rem; font-weight:700; color:#003d5c; text-transform:uppercase; letter-spacing:.05em;",
             paste(meta_parts, collapse=" · ")),
      if (nzchar(plus))
        div(class="mb-1 d-flex gap-2",
          tags$i(class="bi bi-plus-circle-fill flex-shrink-0 mt-1",
                 style="color:#2e7d32; font-size:0.85rem;"),
          tags$p(class="mb-0", style="font-size:0.83rem; color:#1a6b3a;", plus)),
      if (nzchar(delta))
        div(class="d-flex gap-2",
          tags$i(class="bi bi-arrow-repeat flex-shrink-0 mt-1",
                 style="color:#c0392b; font-size:0.85rem;"),
          tags$p(class="mb-0", style="font-size:0.83rem; color:#c0392b;", delta)))
  })

  total <- nrow(ev)
  shown <- min(5, total)
  tags$details(
    tags$summary(
      style="cursor:pointer; font-size:0.82rem; color:#0066a1; font-weight:600; padding:8px 0; list-style:none;",
      tags$i(class="bi bi-clipboard2-check me-1"),
      sprintf("Review recent evaluations — %d shown of %d total (click to expand)", shown, total)),
    div(class="mt-2 ps-2 pe-1 py-1",
        style="background:#f8fafc; border-radius:6px; max-height:400px; overflow-y:auto;",
        items))
}

# ILP goal select with fallback hint for pre-existing free text
.ilp_goal_select <- function(ns, field, label, choices, existing_val) {
  matched <- nzchar(existing_val) && existing_val %in% choices
  tagList(
    tags$label(label, class="form-label fw-semibold", style="font-size:0.82rem; color:#2c3e50;"),
    tags$select(
      id=ns(field), class="form-select form-select-sm mb-1",
      # always prepend a blank sentinel (R parser rejects "" = "" in c() / list())
      tags$option(value="", selected=if (!matched) NA else NULL, "-- select --"),
      lapply(names(choices), function(v)
        tags$option(value=v, selected=if (matched && v==existing_val) NA else NULL, v))),
    if (!matched && nzchar(existing_val))
      tags$p(class="text-muted", style="font-size:0.75rem; margin:0 0 8px;",
             tags$i(class="bi bi-info-circle me-1"),
             "Previously entered: ", tags$em(existing_val)))
}

# ── Period 7 form ─────────────────────────────────────────────────────────────

.form_p7 <- function(ns, p, sr, dd, psr, res, chk) {
  tc <- .dd_choices(dd,"s_e_topic_sel"); sc <- .dd_choices(dd,"s_e_learn_style")
  sel_t <- .checked_codes(sr,"s_e_topic_sel"); sel_s <- .checked_codes(sr,"s_e_learn_style")

  # Yes/No helper for background questions
  yn_q <- function(id, label, val)
    div(class="mb-2 d-flex align-items-center gap-3 flex-wrap",
      tags$label(label, style="font-size:0.84rem; color:#2c3e50; min-width:320px; margin:0;"),
      div(class="btn-group btn-group-sm",
        tags$input(type="radio", class="btn-check", name=ns(id),
                   id=paste0(ns(id),"_1"), value="1",
                   if (.fv(res,id)=="1") list(checked="checked") else list()),
        tags$label(class=paste0("btn btn-outline-success",
                                if (.fv(res,id)=="1") " active" else ""),
                   `for`=paste0(ns(id),"_1"), "Yes"),
        tags$input(type="radio", class="btn-check", name=ns(id),
                   id=paste0(ns(id),"_0"), value="0",
                   if (.fv(res,id)=="0") list(checked="checked") else list()),
        tags$label(class=paste0("btn btn-outline-secondary",
                                if (.fv(res,id)=="0") " active" else ""),
                   `for`=paste0(ns(id),"_0"), "No")))

  div(
    div(class="alert alert-info mb-3 py-2 px-3",
        style="font-size:0.83rem; border-left:4px solid #0d6efd;",
        tags$i(class="bi bi-info-circle me-1"),
        tags$strong("Entering Residency"),
        " — Complete each section below. You can save and return at any time."),
    chk,

    .sec_card(title="Learning Goals", icon="bullseye",
      tags$p(class="text-muted", style="font-size:0.82rem;",
             "What are your 3 main learning goals entering residency?"),
      .ta(ns("s_e_ume_goal1"),"Goal 1",.fv(sr,"s_e_ume_goal1"),rows=2,
          placeholder="e.g., Become comfortable leading a code"),
      .ta(ns("s_e_ume_goal2"),"Goal 2",.fv(sr,"s_e_ume_goal2"),rows=2),
      .ta(ns("s_e_ume_goal3"),"Goal 3",.fv(sr,"s_e_ume_goal3"),rows=2),
      .save_btn(ns,"save_goals")),

    # Background questions → saved to resident_data
    .sec_card(title="Background Questions", icon="geo-alt-fill",
      tags$p(class="text-muted mb-3", style="font-size:0.82rem;",
             "These help us track our program's regional impact — one answer per question."),
      yn_q("hs_mo",     "Did any of your high school education take place in Missouri?",     .fv(res,"hs_mo")),
      yn_q("college_mo","Did any of your college / undergraduate education take place in Missouri?", .fv(res,"college_mo")),
      yn_q("med_mo",    "Did you attend medical school in Missouri?",                        .fv(res,"med_mo")),
      yn_q("slusom",    "Are you a Saint Louis University School of Medicine (SLUSOM) graduate?", .fv(res,"slusom")),
      .save_btn(ns,"save_background")),

    # Preparedness ratings — compact matrix instead of 17 dropdowns
    .sec_card(title="Preparedness Ratings", icon="clipboard-check",
      tags$p(class="text-muted mb-3", style="font-size:0.82rem;",
             "Rate your preparedness for each skill area. Be honest — this is for your benefit!"),
      .prep_matrix_ui(ns, sr),
      .save_btn(ns,"save_prep")),

    .form_topics_section(ns, sr, dd, tc, sc, sel_t, sel_s, psr),

    # Milestone self-assessment
    .sec_card(title="Milestone Self-Assessment", icon="graph-up-arrow",
      div(class="alert alert-light border mb-3 py-1 px-2",
          style="font-size:0.78rem; border-left:3px solid #198754 !important;",
          tags$i(class="bi bi-info-circle me-1"),
          "Rate yourself on each ACGME milestone. Ratings of 4+ require a brief description."),
      uiOutput(ns("milestone_ui")),
      uiOutput(ns("milestone_save_status"))),

    # Concerns — optional (not tracked for completion)
    .sec_card(title="Concerns / Additional Notes", icon="chat-text",
      div(class="alert alert-light border mb-3 py-1 px-2",
          style="font-size:0.78rem; border-left:3px solid #6c757d !important;",
          tags$i(class="bi bi-info-circle me-1"),
          "Optional — anything you'd like the program to know before you start."),
      .ta(ns("s_e_ume_concern"),
          "Any concerns or questions for the program?",
          .fv(sr,"s_e_ume_concern"), rows=4),
      .save_btn(ns,"save_concerns"))
  )
}

# ── Standard periods (1-5) ────────────────────────────────────────────────────

.form_std <- function(ns, p, sr, ir, dd, psr, ev, chk, ite_data = NULL, pgy = 1L) {
  tc <- .dd_choices(dd,"s_e_topic_sel"); sc <- .dd_choices(dd,"s_e_learn_style")
  cp <- .dd_choices(dd,"s_e_career_path"); fl <- .dd_choices(dd,"s_e_fellow")
  tr <- .dd_choices(dd,"s_e_track_type")

  sel_t  <- .checked_codes(sr,"s_e_topic_sel"); sel_s  <- .checked_codes(sr,"s_e_learn_style")
  sel_cp <- .checked_codes(sr,"s_e_career_path")

  # Auto-populate career from previous s_eval if current is empty (#6)
  if (length(sel_cp)==0 && !is.null(psr) && nrow(psr)>0)
    sel_cp <- .checked_codes(psr,"s_e_career_path")
  sel_fl <- .checked_codes(sr,"s_e_fellow")
  if (length(sel_fl)==0 && !is.null(psr) && nrow(psr)>0)
    sel_fl <- .checked_codes(psr,"s_e_fellow")
  sel_tr <- .checked_codes(sr,"s_e_track_type")
  if (length(sel_tr)==0 && !is.null(psr) && nrow(psr)>0)
    sel_tr <- .checked_codes(psr,"s_e_track_type")
  was_prepopulated <- (
    length(.checked_codes(sr,"s_e_career_path"))==0 &&
    (!is.null(psr) && nrow(psr)>0) &&
    length(sel_cp)>0)

  div(
    chk,

    # 1. Self-Reflection
    .sec_card(title="Self-Reflection", icon="person-lines-fill",
      .eval_reference_panel(ev),
      div(class="mt-2",
        .section_hdr("plus-circle-fill","What went well this period?"),
        .ta(ns("s_e_plus"),NULL,.fv(sr,"s_e_plus"),rows=3,
            placeholder="Strengths, successes, moments you're proud of...")),
      .section_hdr("arrow-up-circle-fill","Areas to develop?"),
      .ta(ns("s_e_delta"),NULL,.fv(sr,"s_e_delta"),rows=3,
          placeholder="What do you want to work on next period?"),
      .save_btn(ns,"save_reflection")),

    # 2. Career Planning
    if (!is.null(cp)||!is.null(fl)) .sec_card(
      title="Career Planning", icon="briefcase-medical",
      if (was_prepopulated) div(
        class="alert alert-light border mb-3 py-1 px-2",
        style="font-size:0.78rem; border-left:3px solid #6f42c1 !important;",
        tags$i(class="bi bi-magic me-1", style="color:#6f42c1;"),
        "Pre-filled from your previous evaluation — update as needed."),
      if (!is.null(cp)) div(class="mb-3",
        tags$label("Career path interests", class="form-label fw-semibold",
                   style="font-size:0.85rem;"),
        checkboxGroupInput(ns("s_e_career_path"),NULL,
          choices=setNames(names(cp),unname(cp)), selected=sel_cp, inline=TRUE)),
      if (!is.null(fl)) div(class="mb-3",
        tags$label("Fellowship interests", class="form-label fw-semibold",
                   style="font-size:0.85rem;"),
        checkboxGroupInput(ns("s_e_fellow"),NULL,
          choices=setNames(names(fl),unname(fl)), selected=sel_fl, inline=TRUE)),
      if (!is.null(tr)) div(class="mb-3",
        tags$label("Track pursuit", class="form-label fw-semibold",
                   style="font-size:0.85rem;"),
        checkboxGroupInput(ns("s_e_track_type"),NULL,
          choices=setNames(names(tr),unname(tr)), selected=sel_tr, inline=TRUE)),
      .ta(ns("s_e_discussion"),"Discussion topics for your coach",
          .fv(sr,"s_e_discussion"),rows=2),
      .save_btn(ns,"save_career")),

    # 3. Topics & Learning Styles
    .form_topics_section(ns, sr, dd, tc, sc, sel_t, sel_s, psr),

    # 4. Program Feedback
    .sec_card(title="Program Feedback", icon="chat-dots-fill",
      div(class="alert alert-light border mb-3 py-1 px-2",
          style="font-size:0.78rem; border-left:3px solid #0d6efd !important;",
          tags$i(class="bi bi-shield-check me-1"),
          "Shared with program leadership — not your evaluating faculty."),
      .ta(ns("s_e_prog_plus"),"What is the program doing well?",
          .fv(sr,"s_e_prog_plus"),rows=2),
      .ta(ns("s_e_prog_delta"),"What should the program improve?",
          .fv(sr,"s_e_prog_delta"),rows=2),
      .ta(ns("s_e_progfeed"),"Other comments",.fv(sr,"s_e_progfeed"),rows=2),
      .save_btn(ns,"save_feedback")),

    # 5. Boards & ITE
    .sec_card(title="Boards & ITE", icon="clipboard2-pulse-fill",
      .ite_board_section_ui(ns, sr, dd, ite_data, pgy),
      .save_btn(ns, "save_boards", "Save Board Info")),

    # 6. Milestone Self-Assessment
    .sec_card(title="Milestone Self-Assessment", icon="graph-up-arrow",
      div(class="alert alert-light border mb-3 py-1 px-2",
          style="font-size:0.78rem; border-left:3px solid #198754 !important;",
          tags$i(class="bi bi-info-circle me-1"),
          "Rate yourself on each ACGME milestone. Ratings of 4+ require a brief description."),
      uiOutput(ns("milestone_ui")),
      uiOutput(ns("milestone_save_status"))),

    # 7. ILP Goals
    .sec_card(title="Individual Learning Plan Goals", icon="map-fill",
      div(class="alert alert-light border mb-3 py-1 px-2",
          style="font-size:0.8rem; border-left:4px solid #6f42c1 !important;",
          tags$i(class="bi bi-info-circle me-1", style="color:#6f42c1;"),
          "Select the specific subcompetency you want to target for each domain."),
      div(class="row g-3",
        div(class="col-md-4",
          div(style="border-left:4px solid #0d6efd; padding-left:10px;",
            tags$p(style="font-weight:700; font-size:0.8rem; color:#0d6efd; margin-bottom:8px;",
                   tags$i(class="bi bi-heart-pulse-fill me-1"), "Patient Care / Med Knowledge"),
            .ilp_goal_select(ns,"goal_pcmk","Subcompetency goal",
                             .SUBCOMP_CHOICES$pcmk, .fv(ir,"goal_pcmk")),
            .ta(ns("how_pcmk"),"How / Plan",.fv(ir,"how_pcmk"),rows=2))),
        div(class="col-md-4",
          div(style="border-left:4px solid #198754; padding-left:10px;",
            tags$p(style="font-weight:700; font-size:0.8rem; color:#198754; margin-bottom:8px;",
                   tags$i(class="bi bi-diagram-3-fill me-1"), "Systems / Practice-Based Learning"),
            .ilp_goal_select(ns,"goal_sbppbl","Subcompetency goal",
                             .SUBCOMP_CHOICES$sbppbl, .fv(ir,"goal_sbppbl")),
            .ta(ns("how_sbppbl"),"How / Plan",.fv(ir,"how_sbppbl"),rows=2))),
        div(class="col-md-4",
          div(style="border-left:4px solid #6f42c1; padding-left:10px;",
            tags$p(style="font-weight:700; font-size:0.8rem; color:#6f42c1; margin-bottom:8px;",
                   tags$i(class="bi bi-people-fill me-1"), "Professionalism / Interpersonal"),
            .ilp_goal_select(ns,"goal_subcomp_profics","Subcompetency goal",
                             .SUBCOMP_CHOICES$profics, .fv(ir,"goal_subcomp_profics")),
            .ta(ns("how_profics"),"How / Plan",.fv(ir,"how_profics"),rows=2)))),
      .save_btn(ns,"save_ilp","Save ILP Goals"))
  )
}

# ── Period 6 (Graduating) ────────────────────────────────────────────────────

.form_p6 <- function(ns, p, sr, dd, psr, ev, res, chk, ite_data = NULL) {
  tc <- .dd_choices(dd,"s_e_topic_sel"); sc <- .dd_choices(dd,"s_e_learn_style")
  mksap_ch <- .dd_choices(dd,"s_e_mksap_comp")
  grad_spec_ch <- .dd_choices(dd,"grad_spec")
  sel_t <- .checked_codes(sr,"s_e_topic_sel"); sel_s <- .checked_codes(sr,"s_e_learn_style")

  # PGY3 ITE panel for graduation period
  ite_pct <- tryCatch({
    if (!is.null(ite_data)&&nrow(ite_data)>0&&"pgy3_tot_correct"%in%names(ite_data)) {
      v <- ite_data[["pgy3_tot_correct"]][1]
      if (!is.na(v)&&nzchar(as.character(v))) as.numeric(v) else NA_real_
    } else NA_real_
  }, error=function(e) NA_real_)

  yn_row <- function(id, label, val)
    div(class="mb-2 d-flex align-items-center gap-3 flex-wrap",
      tags$label(label, style="font-size:0.83rem; color:#2c3e50; min-width:280px; margin:0;"),
      div(class="btn-group btn-group-sm",
        tags$input(type="radio", class="btn-check", name=ns(id),
                   id=paste0(ns(id),"_1"), value="1",
                   if (val=="1") list(checked="checked") else list()),
        tags$label(class=paste0("btn btn-outline-success",if(val=="1")" active" else ""),
                   `for`=paste0(ns(id),"_1"), "Yes"),
        tags$input(type="radio", class="btn-check", name=ns(id),
                   id=paste0(ns(id),"_0"), value="0",
                   if (val=="0") list(checked="checked") else list()),
        tags$label(class=paste0("btn btn-outline-secondary",if(val=="0")" active" else ""),
                   `for`=paste0(ns(id),"_0"), "No")))

  div(
    div(class="alert alert-info mb-3 py-2 px-3",
        style="font-size:0.83rem; border-left:4px solid #0d6efd;",
        tags$i(class="bi bi-mortarboard-fill me-1"),
        tags$strong("Graduating Year"), " — Your final self-evaluation."),
    chk,

    .sec_card(title="Self-Reflection", icon="person-lines-fill",
      .eval_reference_panel(ev),
      div(class="mt-2",
        .ta(ns("s_e_plus"),"What went well over your training?",
            .fv(sr,"s_e_plus"),rows=3)),
      .ta(ns("s_e_delta"),"What would you do differently?",
          .fv(sr,"s_e_delta"),rows=3),
      .save_btn(ns,"save_reflection")),

    # Board prep — full fields per item 8
    .sec_card(title="Board Preparation", icon="clipboard2-data-fill",
      if (!is.na(ite_pct)) {
        risk_col   <- if (ite_pct>=70) "#2e7d32" else if (ite_pct>=60) "#e65100" else "#c62828"
        risk_label <- if (ite_pct>=70) "On track" else if (ite_pct>=60) "Monitor" else "At risk"
        div(class="alert mb-3 py-2 px-3",
            style=paste0("background:#f8fafc; border-left:4px solid ",risk_col,"; font-size:0.83rem;"),
          div(class="d-flex align-items-center gap-3",
            div(style="text-align:center; min-width:60px;",
              tags$span(style=paste0("font-size:1.4rem; font-weight:700; color:",risk_col),
                        paste0(round(ite_pct,1),"%")),
              tags$p(style="font-size:0.68rem; color:#6c757d; margin:0;", "PGY3 ITE")),
            tags$p(style="margin:0; font-size:0.82rem;",
                   tags$strong("Board readiness: "),
                   tags$span(style=paste0("color:",risk_col,"; font-weight:700;"), risk_label))))
      },
      yn_row("s_e_step3",         "Have you completed Step 3 / USMLE?",          .fv(sr,"s_e_step3")),
      yn_row("s_e_step3_contact", "Have you sent your Step 3 score to the program?", .fv(sr,"s_e_step3_contact")),
      div(class="mt-3 mb-1 pt-2", style="border-top:1px solid #f0f0f0;"),
      yn_row("s_e_board_concern", "Do you have concerns about failing boards?",   .fv(sr,"s_e_board_concern")),
      yn_row("s_e_board_help",    "Have you previously discussed this with the program?", .fv(sr,"s_e_board_help")),
      .ta(ns("s_e_board_discu"),  "Who have you spoken with / what steps are being taken?",
          .fv(sr,"s_e_board_discu"), rows=2),
      if (!is.null(mksap_ch)) div(class="mt-2",
        tags$label("MKSAP completion", style="font-size:0.83rem; color:#2c3e50; font-weight:600;"),
        tags$select(id=ns("s_e_mksap_comp"), class="form-select form-select-sm",
                    style="max-width:340px;",
          tags$option(value="", if(.fv(sr,"s_e_mksap_comp")=="") "-- select --"),
          lapply(names(mksap_ch), function(v)
            tags$option(value=v, selected=if(.fv(sr,"s_e_mksap_comp")==v) NA else NULL,
                        mksap_ch[[v]])))),
      .ta(ns("s_e_board_plan"), "Study plan / resources being used",
          .fv(sr,"s_e_board_plan"), rows=2),
      .save_btn(ns,"save_board")),

    # Graduation / Alumni info → saved to resident_data
    .sec_card(title="Graduation Info", icon="mortarboard-fill",
      tags$p(class="text-muted mb-3", style="font-size:0.82rem;",
             "This information helps us track alumni outcomes. All fields update your resident record."),
      div(class="row g-3",
        div(class="col-md-6",
          if (!is.null(grad_spec_ch)) {
            sv <- .fv(res,"grad_spec")
            tagList(
              tags$label("Post-residency specialty", class="form-label fw-semibold",
                         style="font-size:0.85rem;"),
              tags$select(id=ns("grad_spec"), class="form-select form-select-sm",
                tags$option(value="", if(sv=="") "-- select --"),
                lapply(names(grad_spec_ch), function(v)
                  tags$option(value=v, selected=if(sv==v) NA else NULL, grad_spec_ch[[v]]))))
          },
          div(class="mt-3",
            tags$label("Current / future position title", class="form-label fw-semibold",
                       style="font-size:0.85rem;"),
            tags$input(type="text", id=ns("res_alumni_position"),
                       class="form-control form-control-sm",
                       placeholder="e.g., Hospitalist at SSM Health",
                       value=.fv(res,"res_alumni_position")))),
        div(class="col-md-6",
          yn_row("res_alumni_academic", "Academic medicine position?",      .fv(res,"res_alumni_academic")),
          div(class="mt-2",
          yn_row("chief",               "Chief resident?",                   .fv(res,"chief"))),
          div(class="mt-2",
          yn_row("ssm",                 "Staying within SSM Health System?", .fv(res,"ssm"))),
          div(class="mt-2",
          yn_row("mo_prac",             "Practicing in Missouri?",           .fv(res,"mo_prac"))),
          div(class="mt-2",
          yn_row("rural",               "Rural practice setting?",           .fv(res,"rural"))),
          div(class="mt-2",
          yn_row("und_urban",           "Underserved / urban setting?",      .fv(res,"und_urban"))))),
      div(class="row g-3 mt-1",
        div(class="col-md-6",
          tags$label("Best future email address", class="form-label fw-semibold",
                     style="font-size:0.85rem;"),
          tags$input(type="email", id=ns("grad_email"), class="form-control form-control-sm",
                     placeholder="you@example.com", value=.fv(res,"grad_email"))),
        div(class="col-md-6",
          tags$label("Best future phone number", class="form-label fw-semibold",
                     style="font-size:0.85rem;"),
          tags$input(type="tel", id=ns("grad_phone"), class="form-control form-control-sm",
                     placeholder="314-555-0100", value=.fv(res,"grad_phone")))),
      .save_btn(ns,"save_alumni","Save Graduation Info")),

    .form_topics_section(ns, sr, dd, tc, sc, sel_t, sel_s, psr),

    .sec_card(title="Program Feedback", icon="chat-dots-fill",
      .ta(ns("s_e_prog_plus"),"What did the program do well?",
          .fv(sr,"s_e_prog_plus"),rows=2),
      .ta(ns("s_e_prog_delta"),"What should the program improve?",
          .fv(sr,"s_e_prog_delta"),rows=2),
      .ta(ns("s_e_progfeed"),"Final comments / advice for future residents",
          .fv(sr,"s_e_progfeed"),rows=3),
      .save_btn(ns,"save_feedback")),

    .sec_card(title="Milestone Self-Assessment", icon="graph-up-arrow",
      uiOutput(ns("milestone_ui")),
      uiOutput(ns("milestone_save_status")))
  )
}

# ── Shared: Topics & Learning Styles (#7 — shows previous selections) ─────────

.form_topics_section <- function(ns, sr, dd, tc, sc, sel_t, sel_s, psr) {
  prev_t <- if (!is.null(psr)&&nrow(psr)>0) .checked_codes(psr,"s_e_topic_sel") else character(0)
  prev_s <- if (!is.null(psr)&&nrow(psr)>0) .checked_codes(psr,"s_e_learn_style") else character(0)

  # Build label maps from choices for chip display
  t_labels <- if (!is.null(tc)) unname(tc[prev_t]) else prev_t
  s_labels <- if (!is.null(sc)) unname(sc[prev_s]) else prev_s
  t_labels <- t_labels[!is.na(t_labels) & nzchar(t_labels)]
  s_labels <- s_labels[!is.na(s_labels) & nzchar(s_labels)]

  .chips <- function(labels, bg)
    if (length(labels)>0)
      div(class="d-flex flex-wrap gap-1 mb-2",
          lapply(labels, function(l)
            tags$span(style=paste0("background:",bg,"; color:#fff; border-radius:20px;",
                                   " padding:2px 10px; font-size:0.75rem;"), l)))
    else tags$p(class="text-muted fst-italic", style="font-size:0.78rem;", "None previously selected")

  .sec_card(title="Topics & Learning Styles", icon="lightbulb-fill",
    div(class="row g-3",
      div(class="col-md-6",
        tags$label("Topics you feel least comfortable with",
                   class="form-label fw-semibold", style="font-size:0.85rem;"),
        if (length(prev_t)>0) div(class="mb-2",
          tags$p(style="font-size:0.75rem; color:#6c757d; margin-bottom:4px;",
                 tags$i(class="bi bi-clock-history me-1"), "Previously selected:"),
          .chips(t_labels, "#a87b3e")),
        if (!is.null(tc))
          checkboxGroupInput(ns("s_e_topic_sel"),NULL,
            choices=setNames(names(tc),unname(tc)), selected=sel_t)
        else tags$p(class="text-muted small", "Loading..."),
        textInput(ns("s_e_topic_oth"),"Other (free text)",.fv(sr,"s_e_topic_oth"),
                  placeholder="Specify other topic...")),
      div(class="col-md-6",
        tags$label("Preferred learning experiences",
                   class="form-label fw-semibold", style="font-size:0.85rem;"),
        if (length(prev_s)>0) div(class="mb-2",
          tags$p(style="font-size:0.75rem; color:#6c757d; margin-bottom:4px;",
                 tags$i(class="bi bi-clock-history me-1"), "Previously selected:"),
          .chips(s_labels, "#0066a1")),
        if (!is.null(sc))
          checkboxGroupInput(ns("s_e_learn_style"),NULL,
            choices=setNames(names(sc),unname(sc)), selected=sel_s)
        else tags$p(class="text-muted small", "Loading..."),
        textInput(ns("s_e_learn_oth"),"Other (free text)",.fv(sr,"s_e_learn_oth"),
                  placeholder="Specify other style..."))),
    .save_btn(ns,"save_topics"))
}
