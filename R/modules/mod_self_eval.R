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

# ILP goal subcompetency choices — values are REDCap numeric codes for each dropdown
.SUBCOMP_CHOICES <- list(
  pcmk = c(
    "PC1 — History-taking / interviewing"        = "1",
    "PC2 — Physical examination"                 = "2",
    "PC3 — Clinical reasoning / synthesis"       = "3",
    "PC4 — Diagnosis and management"             = "4",
    "PC5 — Procedures"                           = "5",
    "PC6 — Transitions of care / discharge"      = "6",
    "MK1 — Core medical knowledge"               = "7",
    "MK2 — Evidence-based medicine"              = "8",
    "MK3 — Basic science application"            = "9"
  ),
  sbppbl = c(
    "SBP1 — Patient safety / error prevention"   = "1",
    "SBP2 — Quality improvement"                 = "2",
    "SBP3 — System navigation / advocacy"        = "3",
    "PBLI1 — Self-directed learning"             = "4",
    "PBLI2 — Teaching and education"             = "5"
  ),
  profics = c(
    "PROF1 — Professional conduct / reliability" = "1",
    "PROF2 — Ethics and accountability"          = "2",
    "PROF3 — Self-awareness / well-being"        = "3",
    "PROF4 — Diversity, equity, inclusion"       = "4",
    "ICS1 — Communication with patients/families"= "5",
    "ICS2 — Communication with the care team"    = "6",
    "ICS3 — Documentation"                       = "7"
  )
)

# Milestone self-rating REDCap fields → ILP subcompetency code, by domain
# Used to surface low self-ratings as context in the ILP section
.MS_SELF_FIELDS <- list(
  pcmk    = c(rep_pc1_self="1", rep_pc2_self="2", rep_pc3_self="3",
               rep_pc4_self="4", rep_pc5_self="5", rep_pc6_self="6",
               rep_mk1_self="7", rep_mk2_self="8", rep_mk3_self="9"),
  sbppbl  = c(rep_sbp1_self="1", rep_sbp2_self="2", rep_sbp3_self="3",
               rep_pbl1_self="4", rep_pbl2_self="5"),
  profics = c(rep_prof1_self="1", rep_prof2_self="2", rep_prof3_self="3",
               rep_prof4_self="4", rep_ics1_self="5", rep_ics2_self="6",
               rep_ics3_self="7")
)

# Map numeric subcompetency code → comp code string (e.g., "1" for pcmk → "PC1")
.get_comp_code <- function(domain, value) {
  v <- suppressWarnings(as.integer(value))
  if (is.na(v)) return(NULL)
  switch(domain,
    "pcmk"    = if (v <= 6) paste0("PC", v) else paste0("MK", v - 6),
    "sbppbl"  = if (v <= 3) paste0("SBP", v) else paste0("PBLI", v - 3),
    "profics" = if (v <= 4) paste0("PROF", v) else paste0("ICS", v - 4),
    NULL)
}

# Extract milestone row table for a comp code from the data dictionary
# Returns data.frame with columns: Row (row index), Level_1..Level_5 (descriptions)
.get_milestone_table <- function(comp_code, dd) {
  if (is.null(dd) || is.null(comp_code)) return(NULL)
  dom  <- gsub("\\d+$", "", comp_code)
  num  <- gsub("^[A-Z]+", "", comp_code)
  pfx  <- tolower(dom)
  if (dom == "PBLI") pfx <- "pbl"
  pattern <- paste0("^", pfx, num, "_r\\d+$")
  flds <- dd[grepl(pattern, dd$field_name), , drop=FALSE]
  if (nrow(flds) == 0) return(NULL)
  flds <- flds[order(flds$field_name), ]
  result <- data.frame(
    Row     = integer(nrow(flds)),
    Level_1 = character(nrow(flds)), Level_2 = character(nrow(flds)),
    Level_3 = character(nrow(flds)), Level_4 = character(nrow(flds)),
    Level_5 = character(nrow(flds)),
    stringsAsFactors = FALSE)
  for (i in seq_len(nrow(flds))) {
    result$Row[i] <- as.integer(gsub(paste0("^", pfx, num, "_r"), "", flds$field_name[i]))
    ct <- flds$select_choices_or_calculations[i]
    if (is.na(ct) || !nzchar(ct)) next
    for (ch in strsplit(ct, "\\s*\\|\\s*")[[1]]) {
      parts <- strsplit(trimws(ch), ",\\s*", perl=TRUE)[[1]]
      if (length(parts) < 2) next
      lv <- suppressWarnings(as.integer(trimws(parts[1])))
      if (is.na(lv) || lv < 1 || lv > 5) next
      result[i, paste0("Level_", lv)] <- trimws(paste(parts[-1], collapse=", "))
    }
  }
  result
}

# Look up the actual behavioral anchor text for a saved ILP goal
# domain:     "pcmk", "sbppbl", or "profics"
# goal_code:  numeric subcompetency code stored in REDCap (e.g. "7")
# row:        milestone row number (e.g. "2")
# level:      milestone level 1-5 (e.g. "4")
# Returns the text string or NULL if not found
.get_goal_text <- function(domain, goal_code, row, level, dd) {
  if (is.null(goal_code) || !nzchar(goal_code)) return(NULL)
  if (is.null(row)       || !nzchar(row))       return(NULL)
  if (is.null(level)     || !nzchar(level))     return(NULL)
  comp <- .get_comp_code(domain, goal_code)
  if (is.null(comp)) return(NULL)
  tbl  <- .get_milestone_table(comp, dd)
  if (is.null(tbl) || nrow(tbl) == 0) return(NULL)
  r   <- suppressWarnings(as.integer(row))
  col <- paste0("Level_", level)
  if (is.na(r) || !col %in% names(tbl)) return(NULL)
  idx <- which(tbl$Row == r)
  if (!length(idx)) return(NULL)
  txt <- tbl[[col]][idx[1]]
  if (!is.null(txt) && !is.na(txt) && nzchar(trimws(txt))) trimws(txt) else NULL
}

# Sections per period (for checklist)
.PERIOD_SECTIONS <- list(
  # Period 7: concerns shown but NOT tracked (optional); milestones + background added
  "7" = c(goals="Learning Goals", background="Background Questions",
          career="Career Planning",
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
  # Period 6: graduation info + reflection + feedback
  "6" = c(reflection="Self-Reflection", alumni="Graduation Info",
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
    status <- httr::status_code(resp)
    body   <- httr::content(resp, "text", encoding = "UTF-8")
    body_trim <- trimws(body)
    has_err <- grepl("\"error\"", body, fixed = TRUE) ||
               grepl("^ERROR", body, ignore.case = TRUE)
    # REDCap returns "[]" (or similar empty body) when the POST was accepted
    # (HTTP 200) but zero records were actually written — typically a schema
    # mismatch (instrument not set up as repeating, field doesn't exist, etc).
    empty_body <- body_trim %in% c("", "[]", "{}") ||
                  grepl("\"count\"\\s*:\\s*0", body, perl = TRUE)
    message("[rc_save] record=", record_id, " instrument=", instrument,
            " instance=", instance, " n_fields=", length(fields),
            " status=", status, " body=", substr(body_trim, 1, 120))
    initial_ok <- status == 200 && !has_err && !empty_body
    if (!initial_ok) {
      return(list(success = FALSE,
                  message = paste0("REDCap (HTTP ", status, "): ",
                                   if (empty_body) "empty response \u2014 0 records written. "
                                   else "",
                                   substr(body, 1, 500))))
    }

    # Verification read-back: REDCap can return the record id (so the body
    # check passes) even when it silently rejected every field \u2014 most
    # commonly when the API token has read but not write access to the
    # form. Pick a field whose VALUE we just wrote (skipping key/period
    # fields whose value is preserved from any pre-existing record) and
    # confirm REDCap returns the exact value we sent. If the round-trip
    # disagrees, surface a real save failure.
    skip_keys <- c("record_id", "redcap_repeat_instrument",
                   "redcap_repeat_instance", "year_resident", "s_e_period",
                   "prog_mile_period_self")
    chk_field <- NULL; written <- ""
    for (fn in names(fields)) {
      if (fn %in% skip_keys) next
      v <- as.character(fields[[fn]])
      if (length(v) >= 1 && !is.na(v[1]) && nzchar(trimws(v[1]))) {
        chk_field <- fn; written <- v[1]; break
      }
    }
    if (!is.null(chk_field)) {
      verify <- tryCatch({
        v_resp <- httr::POST(
          url  = app_config$redcap_url,
          body = list(token = app_config$rdm_token, content = "record",
                      format = "json", type = "flat",
                      `records[0]` = as.character(record_id),
                      `fields[0]`  = "record_id",
                      `fields[1]`  = chk_field,
                      `forms[0]`   = instrument,
                      returnFormat = "json"),
          encode = "form", httr::timeout(30))
        if (httr::status_code(v_resp) != 200) NULL
        else jsonlite::fromJSON(
          httr::content(v_resp, "text", encoding = "UTF-8"),
          simplifyVector = TRUE)
      }, error = function(e) NULL)
      verify_ok <- !is.null(verify) && is.data.frame(verify) && nrow(verify) > 0 &&
                   chk_field %in% names(verify) &&
                   "redcap_repeat_instance" %in% names(verify)
      if (verify_ok) {
        match_row <- verify[as.character(verify$redcap_repeat_instance) ==
                              as.character(instance), , drop = FALSE]
        actual <- if (nrow(match_row) > 0)
                    trimws(as.character(match_row[[chk_field]][1])) else ""
        if (!identical(actual, trimws(written))) {
          message("[rc_save] VERIFY FAILED instrument=", instrument,
                  " instance=", instance, " field=", chk_field,
                  " expected='", substr(written, 1, 60),
                  "' actual='", substr(actual, 1, 60), "'")
          return(list(success = FALSE,
                      message = paste0(
                        "REDCap returned success but the round-trip read of '",
                        chk_field, "' did not match what was sent. ",
                        "The API token likely lacks write access to the '",
                        instrument, "' form (Read-Only or No Access). ",
                        "Update User Rights in REDCap and try again.")))
        }
      } else {
        # Verify call returned a shape we can't compare against. Log so we
        # can tell when REDCap is silently dropping writes vs. actually
        # persisting them.
        v_class <- if (is.null(verify)) "NULL" else paste(class(verify), collapse=",")
        v_n     <- if (is.data.frame(verify)) nrow(verify) else NA_integer_
        v_names <- if (!is.null(verify) && !is.null(names(verify)))
                     paste(head(names(verify), 8), collapse=",") else ""
        message("[rc_save] VERIFY SKIPPED instrument=", instrument,
                " instance=", instance, " field=", chk_field,
                " verify_class=", v_class, " nrow=", v_n,
                " names=", v_names)
      }
    }
    list(success = TRUE,
         ts      = format(Sys.time(), "%b %d %I:%M %p"),
         body    = substr(body, 1, 200))
  }, error = function(e) list(success = FALSE,
                              message = paste("Error:", e$message)))
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
    status <- httr::status_code(resp)
    body   <- httr::content(resp, "text", encoding="UTF-8")
    body_trim <- trimws(body)
    has_err <- grepl("\"error\"", body, fixed=TRUE) ||
               grepl("^ERROR", body, ignore.case=TRUE)
    empty_body <- body_trim %in% c("", "[]", "{}") ||
                  grepl("\"count\"\\s*:\\s*0", body, perl=TRUE)
    message("[rc_save_resident] record=", record_id,
            " n_fields=", length(fields),
            " status=", status, " body=", substr(body_trim, 1, 120))
    if (status == 200 && !has_err && !empty_body)
      list(success=TRUE, ts=format(Sys.time(),"%b %d %I:%M %p"))
    else
      list(success=FALSE,
           message=paste0("REDCap (HTTP ", status, "): ",
                          if (empty_body) "empty response \u2014 0 records written. " else "",
                          substr(body, 1, 500)))
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

# parse_redcap_choices returns c(code = "label") — names are codes, values are labels.
# selectInput / radioButtons need c("label" = code) — names displayed, values returned.
# Use this wrapper wherever choices feed into a Shiny UI widget.
.dd_select <- function(dd, fld, fallback = NULL) {
  ch <- .dd_choices(dd, fld)
  if (is.null(ch)) return(fallback)
  setNames(names(ch), unname(ch))   # flip: c(code="label") → c("label"=code)
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
    boards     = has(sr, "s_e_step3"),          # periods 1-5
    ilp_review = has(ir, "prior_goal_pcmk") && has(ir, "prior_goal_sbppbl") &&
                 has(ir, "prior_goal_profics"),
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

.sec_card <- function(..., title, icon, id = NULL, collapsed = FALSE,
                      saved = FALSE) {
  body_id <- paste0("sc_", gsub("[^a-zA-Z0-9]", "_", tolower(title)))
  saved_badge <- if (isTRUE(saved))
    tags$span(class = "badge",
              style = paste0("background:#d1fae5; color:#065f46;",
                             " font-size:0.7rem; font-weight:600;",
                             " padding:3px 8px; border-radius:20px;",
                             " margin-left:8px;"),
      tags$i(class = "bi bi-check-circle-fill me-1"),
      "Previously saved — review and edit") else NULL
  div(class = "card border-0 shadow-sm mb-3", style = "border-radius:8px;", id = id,
    div(class = "card-header border-0 d-flex align-items-center justify-content-between",
        style = "background:#f8fafc; border-radius:8px 8px 0 0; padding:12px 18px; cursor:pointer;",
        `data-bs-toggle` = "collapse",
        `data-bs-target` = paste0("#", body_id),
        `aria-expanded` = tolower(as.character(!collapsed)),
      div(class = "d-flex align-items-center gap-2 flex-wrap",
        tags$i(class = paste0("bi bi-", icon), style = "color:#003d5c; font-size:1rem;"),
        tags$span(style = "font-weight:700; color:#003d5c; font-size:0.95rem;", title),
        saved_badge),
      tags$i(class = paste0("bi bi-chevron-", if (collapsed) "down" else "up"),
             style = "color:#adb5bd; font-size:0.8rem; transition:transform .2s;")),
    div(id = body_id,
        class = paste0("collapse", if (!collapsed) " show" else ""),
      div(class = "card-body", ...)))
}

# Format a phone number string for display — keeps digits only and renders as
# "(123) 456-7890". Partial entries fall back gracefully.
.format_phone <- function(x) {
  if (is.null(x)) return("")
  d <- gsub("\\D", "", as.character(x))
  if (!nzchar(d)) return("")
  if (nchar(d) > 10) d <- substr(d, 1, 10)
  if (nchar(d) >= 7)
    paste0("(", substr(d,1,3), ") ", substr(d,4,6), "-", substr(d,7,nchar(d)))
  else if (nchar(d) >= 4)
    paste0("(", substr(d,1,3), ") ", substr(d,4,nchar(d)))
  else
    paste0("(", d)
}

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

# Preparedness rating matrix — one row per skill with a Bootstrap segmented
# btn-group (1–5). Label onclick fires Shiny.setInputValue so clicks actually
# register server-side (bare radios with only `checked` attribute don't).
.prep_matrix_ui <- function(ns, sr) {
  # Each option is number + explicit word — no guessing, no tooltips needed.
  scale_opts <- list(
    list(v="1", lbl="Not at all",  cls="btn-outline-danger"),
    list(v="2", lbl="Slightly",    cls="btn-outline-warning"),
    list(v="3", lbl="Moderately",  cls="btn-outline-secondary"),
    list(v="4", lbl="Very",        cls="btn-outline-info"),
    list(v="5", lbl="Extremely",   cls="btn-outline-success")
  )

  rows <- lapply(names(.PREP_LABELS), function(n) {
    input_id <- paste0("s_e_prep_", n)
    cur <- .fv(sr, input_id)
    btns <- lapply(scale_opts, function(o) {
      active <- isTRUE(cur == o$v)
      tagList(
        tags$input(type="radio", class="btn-check", name=ns(input_id),
                   id=paste0(ns(input_id),"_",o$v), value=o$v,
                   checked = if (active) NA else NULL),
        tags$label(class=paste0("btn btn-sm ", o$cls,
                                if (active) " active" else ""),
                   `for`=paste0(ns(input_id),"_",o$v),
                   style="flex:1; font-weight:600; padding:6px 4px; font-size:0.78rem; line-height:1.15;",
                   onclick=paste0("Shiny.setInputValue('",ns(input_id),"','",o$v,"',{priority:'event'})"),
                   tags$div(style="font-size:0.95rem;", o$v),
                   tags$div(style="font-size:0.68rem; opacity:0.85;", o$lbl))
      )
    })
    seed <- if (nzchar(cur)) tags$script(HTML(sprintf(
      "Shiny.setInputValue('%s','%s',{priority:'event'});", ns(input_id), cur))) else NULL
    # Row: number circle + question text + full-width 5-button group
    div(class="py-2 px-3",
        style=paste0("border-bottom:1px solid #eef1f4; ",
                     "background:", if (as.integer(n)%%2==0) "#f8fafc" else "white", ";"),
      div(class="d-flex align-items-center gap-2 mb-2",
        tags$span(style=paste0("flex-shrink:0; width:22px; height:22px; border-radius:50%; ",
                               "background:#e9ecef; color:#495057; font-size:0.72rem; font-weight:700; ",
                               "display:inline-flex; align-items:center; justify-content:center;"), n),
        tags$span(style="font-size:0.88rem; color:#2c3e50; font-weight:500;", .PREP_LABELS[[n]])),
      div(class="btn-group w-100", role="group",
          `aria-label`=paste("Rating for", .PREP_LABELS[[n]]),
          btns),
      seed)
  })
  div(
    div(class="alert alert-light py-2 px-3 mb-2",
        style="font-size:0.78rem; color:#495057; border-left:3px solid #0d6efd;",
      tags$i(class="bi bi-info-circle me-1"),
      tags$strong("Rate each item"), " from 1 (Not at all prepared) to 5 (Extremely prepared)."),
    div(style="border:1px solid #e6e9ec; border-radius:6px; overflow:hidden;", rows))
}

# ITE score panel + board self-report questions (periods 1-5)
.ite_board_section_ui <- function(ns, sr, dd, ite_data, pgy) {
  # ITE percent correct for current PGY year
  pgy_fld <- paste0("pgy", pgy, "_tot_correct")
  ite_pct  <- if (!is.null(ite_data) && nrow(ite_data)>0 && pgy_fld %in% names(ite_data)) {
    v <- ite_data[[pgy_fld]][1]; if (!is.na(v) && nzchar(v)) as.numeric(v) else NA
  } else NA_real_

  ite_panel <- if (!is.na(ite_pct)) {
    prob <- .pass_prob(ite_pct, pgy)
    risk <- .risk_from_prob(prob)
    div(class="alert mb-3 py-2 px-3",
        style=paste0("background:#f8fafc; border-left:4px solid ", risk$color, "; font-size:0.83rem;"),
      div(class="d-flex align-items-center gap-3",
        div(style="text-align:center; min-width:70px;",
          tags$span(style=paste0("font-size:1.5rem; font-weight:700; color:", risk$color),
                    paste0(round(ite_pct,1),"%")),
          tags$p(style="font-size:0.68rem; color:#6c757d; margin:0;", "ITE % correct")),
        div(style="flex:1;",
          tags$p(style="margin:0; font-weight:600; font-size:0.85rem; color:#003d5c;",
                 paste0("PGY", pgy, " ACP ITE Score")),
          if (!is.na(prob))
            tags$p(style="margin:0; font-size:0.78rem; color:#6c757d;",
                   paste0(round(prob * 100, 0), "% predicted ABIM pass probability \u2014 "),
                   tags$span(style=paste0("font-weight:700; color:", risk$color), risk$level))
          else
            tags$p(style="margin:0; font-size:0.78rem; color:#6c757d;", risk$level)))
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
                   checked = if (val=="1") NA else NULL),
        tags$label(class=paste0("btn btn-outline-success",
                                if (val=="1") " active" else ""),
                   `for`=paste0(ns(id),"_1"), "Yes"),
        tags$input(type="radio", class="btn-check", name=ns(id),
                   id=paste0(ns(id),"_0"), value="0",
                   checked = if (val=="0") NA else NULL),
        tags$label(class=paste0("btn btn-outline-secondary",
                                if (val=="0") " active" else ""),
                   `for`=paste0(ns(id),"_0"), "No")))
  }

  tagList(
    ite_panel,
    # Step 3 yes/no — conditional detail rendered server-side
    yn_row("s_e_step3", "Have you completed Step 3 (USMLE or COMLEX)?", .fv(sr,"s_e_step3")),
    uiOutput(ns("boards_step3_cond")),
    div(class="mt-3 pt-2", style="border-top:1px solid #f0f0f0;",
      .ta(ns("s_e_board_discu"), "Board study plan / discussion notes",
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
    # Scoped font bump for the self-eval module only — multiplies every
    # font-size declared with rem/em inside this scope. 1.10 = ~10% bigger.
    # Module-scoped font bump (~25% larger than previous).
    # Uses !important + broad selectors to override inline font-size styles
    # set throughout the module (many elements have style="font-size:0.8rem").
    tags$style(HTML(sprintf("
      #%s, #%s * { font-size: 1.1rem; }
      #%s p, #%s label, #%s .form-label, #%s .alert,
      #%s .btn, #%s .form-control, #%s .form-select,
      #%s textarea, #%s input, #%s select {
        font-size: 1.1rem !important;
      }
      #%s .text-muted, #%s .small, #%s small {
        font-size: 1rem !important;
      }
      #%s h1 { font-size: 1.6rem !important; }
      #%s h2 { font-size: 1.45rem !important; }
      #%s h3, #%s h4 { font-size: 1.3rem !important; }
      #%s h5, #%s h6 { font-size: 1.2rem !important; }
      #%s .card-header, #%s .sec-card-title,
      #%s .card-title, #%s strong { font-size: 1.15rem !important; }
      #%s .btn-sm { font-size: 1rem !important; padding: 6px 12px !important; }
    ",
      ns("wrap"), ns("wrap"),
      ns("wrap"), ns("wrap"), ns("wrap"), ns("wrap"),
      ns("wrap"), ns("wrap"), ns("wrap"),
      ns("wrap"), ns("wrap"), ns("wrap"),
      ns("wrap"), ns("wrap"), ns("wrap"),
      ns("wrap"),
      ns("wrap"),
      ns("wrap"), ns("wrap"),
      ns("wrap"), ns("wrap"),
      ns("wrap"), ns("wrap"), ns("wrap"), ns("wrap"),
      ns("wrap")))),
    div(id = ns("wrap"),
      uiOutput(ns("status_table")),
      uiOutput(ns("fac_eval_gate")),
      div(class="mt-3", uiOutput(ns("period_form"))))
  )
}

# ── Server ────────────────────────────────────────────────────────────────────

mod_self_eval_server <- function(id, rdm_data, resident_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── data reactives ────────────────────────────────────────────────────────
    # Local patches to resident_data written since the app loaded. rdm_data()
    # is a cached load and isn't refreshed after .rc_save_resident() posts to
    # REDCap — without this, saved grad_email / chief / ssm / etc. disappear
    # from the UI on rerender because the cached row has no knowledge of them.
    res_patch <- reactiveValues()
    .merge_res <- function(fields) {
      for (fld in names(fields)) {
        v <- fields[[fld]]
        if (!is.null(v)) res_patch[[fld]] <- as.character(v)
      }
    }

    resident_r <- reactive({
      req(rdm_data(), resident_id())
      df <- rdm_data()$residents
      if (is.null(df)) return(NULL)
      row <- df[df$record_id == resident_id(), , drop = FALSE]
      if (nrow(row) == 0) return(row)
      # Layer in any fields written during this session
      patch <- reactiveValuesToList(res_patch)
      for (fld in names(patch)) {
        if (!(fld %in% names(row))) row[[fld]] <- NA_character_
        row[[fld]][1] <- patch[[fld]]
      }
      row
    })

    period_info_r <- reactive({
      res <- resident_r(); req(res, nrow(res)>0)
      gy  <- suppressWarnings(as.numeric(res$grad_yr[1]))
      tp  <- suppressWarnings(as.numeric(res$type[1]))
      # grad_yr is stored as a raw numeric code (e.g. 7) when raw_or_label="raw",
      # not the actual year (e.g. 2029).  Treat any value < 2000 as a code and
      # look up the real year from the data dictionary label.
      if (is.na(gy) || gy < 2000) {
        dd <- rdm_data()$data_dict
        ch <- .dd_choices(dd, "grad_yr")          # c("code" = "year_label")
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

    # ── Faculty-eval completion gate ─────────────────────────────────────────
    # Resident expectation: 25 faculty evaluations per academic year
    # (July 1 \u2013 June 30).  Linear pace: 13 by mid-year, 25 by year-end.
    # Bands (ratio = done / expected-by-today):
    #   >= 0.9  \u2192 on track (congratulate)
    #   0.5\u20130.9 \u2192 behind (encourage)
    #   < 0.5   \u2192 well behind (warn)
    #   < 0.10 of annual goal (i.e., < 3 evals)   \u2192 hard block
    fac_eval_status_r <- reactive({
      req(rdm_data(), resident_id())
      fe <- rdm_data()$all_forms$faculty_evaluation
      mine <- if (!is.null(fe) && nrow(fe) > 0)
                fe[fe$record_id == resident_id(), , drop = FALSE]
              else data.frame()
      today     <- Sys.Date()
      yr        <- as.integer(format(today, "%Y"))
      ay_start  <- if (as.integer(format(today, "%m")) >= 7L)
                     as.Date(sprintf("%d-07-01", yr))
                   else
                     as.Date(sprintf("%d-07-01", yr - 1L))
      days_in   <- as.integer(today - ay_start)
      goal      <- 25L
      expected  <- max(1L, round(goal * days_in / 365))
      done <- if (nrow(mine) > 0 && "fac_eval_date" %in% names(mine)) {
        d <- suppressWarnings(as.Date(mine$fac_eval_date))
        sum(!is.na(d) & d >= ay_start)
      } else 0L
      ratio   <- if (expected > 0) done / expected else 0
      blocked <- done < ceiling(goal * 0.10)  # < 3 evals \u2192 hard block
      band    <- if (blocked)            "block"
                 else if (ratio >= 0.9)  "ontrack"
                 else if (ratio >= 0.5)  "behind"
                 else                    "warn"
      list(done = done, expected = expected, goal = goal,
           ay_start = ay_start, days_in = days_in,
           ratio = ratio, band = band, blocked = blocked)
    })

    output$fac_eval_gate <- renderUI({
      req(local$ready)
      # Skip the faculty-eval gate for residents eligible for the intern
      # intro (period 7, Entering Residency) — they haven't had a chance
      # to complete faculty evaluations yet.
      pi <- tryCatch(period_info_r(), error = function(e) NULL)
      if (!is.null(pi) && !is.na(pi$period_number) &&
          as.character(pi$period_number) == "7") return(NULL)
      s <- tryCatch(fac_eval_status_r(), error = function(e) NULL)
      if (is.null(s)) return(NULL)

      # Band -> colors + headline
      spec <- switch(s$band,
        ontrack = list(bg="#e8f5e9", border="#2e7d32", icon="emoji-smile-fill",
                       ic_col="#2e7d32",
                       head="Great work — you're on track with faculty evaluations!",
                       body=paste0("You've completed ", s$done, " of ", s$goal,
                                   " evaluations expected this academic year. Keep going.")),
        behind  = list(bg="#fff8e1", border="#f9a825", icon="exclamation-circle-fill",
                       ic_col="#f9a825",
                       head="A bit behind on faculty evaluations.",
                       body=paste0("You've done ", s$done, "; by this point in the year we'd expect about ",
                                   s$expected, " (goal: ", s$goal, " per year). ",
                                   "Consider completing a few before finishing this self-evaluation.")),
        warn    = list(bg="#fff3e0", border="#e65100", icon="exclamation-triangle-fill",
                       ic_col="#e65100",
                       head="Faculty evaluations are well behind pace.",
                       body=paste0("You've done ", s$done, " out of ", s$expected,
                                   " expected by now (annual goal ", s$goal, "). ",
                                   "Please complete several before moving on.")),
        block   = list(bg="#ffebee", border="#c62828", icon="lock-fill",
                       ic_col="#c62828",
                       head="Faculty evaluations required before starting.",
                       body=paste0("You've done ", s$done, " faculty evaluation",
                                   if (s$done == 1) "" else "s",
                                   " this academic year. The self-assessment is locked until at least ",
                                   ceiling(s$goal * 0.10),
                                   " are submitted. Please go to the Faculty Evaluations page to get started."))
      )

      # Progress bar
      pct <- min(100L, as.integer(round(100 * s$done / s$goal)))
      exp_pct <- min(100L, as.integer(round(100 * s$expected / s$goal)))
      bar <- div(style="position:relative; height:14px; background:#eceff1; border-radius:7px; overflow:hidden; margin:8px 0 6px;",
        # target marker
        div(style=sprintf("position:absolute; left:%d%%; top:-3px; bottom:-3px; width:2px; background:#455a64;", exp_pct)),
        # fill
        div(style=sprintf("height:100%%; width:%d%%; background:%s; transition:width .3s;",
                          pct, spec$ic_col)))

      div(class="mt-3 mb-3 px-3 py-3",
          style=sprintf("background:%s; border-left:5px solid %s; border-radius:8px;",
                        spec$bg, spec$border),
        div(class="d-flex align-items-start gap-3",
          tags$i(class=paste0("bi bi-", spec$icon),
                 style=sprintf("font-size:1.6rem; color:%s; flex-shrink:0;", spec$ic_col)),
          div(style="flex:1;",
            tags$div(style="font-weight:700; font-size:1.1rem; color:#1a2e42;", spec$head),
            tags$div(style="font-size:0.98rem; color:#2c3e50; margin-top:4px;", spec$body),
            bar,
            tags$div(style="display:flex; justify-content:space-between; font-size:0.85rem; color:#546e7a;",
              tags$span(sprintf("Completed: %d", s$done)),
              tags$span(sprintf("Expected by today: %d", s$expected)),
              tags$span(sprintf("Annual goal: %d", s$goal))),
            tags$div(class="mt-2", style="font-size:0.92rem; color:#37474f;",
              tags$i(class="bi bi-lightbulb-fill me-1", style="color:#f9a825;"),
              tags$strong("Tip: "),
              "Evaluate every attending or fellow you worked with this year \u2014 ",
              "especially your ", tags$b("continuity clinic attendings"),
              ". Honest, timely feedback helps the program and your peers."),
            if (s$band == "block")
              div(class="mt-3",
                actionButton(ns("goto_fac_eval"),
                  tagList(tags$i(class="bi bi-arrow-right-circle-fill me-1"),
                          "Go to Faculty Evaluations"),
                  class="btn btn-danger btn-sm"))
          )
        )
      )
    })

    # Navigate to faculty eval page when the block button is clicked.
    # The parent server.R listens on input$nav_block (top-level, unnamespaced);
    # setInputValue with a plain id targets the root session's input.
    observeEvent(input$goto_fac_eval, {
      shinyjs::runjs("Shiny.setInputValue('nav_block','faculty_eval',{priority:'event'});")
    })
    ite_data_r <- reactive({
      req(rdm_data(), resident_id())
      # pgy*_tot_correct may be in the base record (residents), not a separate instrument
      df <- rdm_data()$all_forms$test_data %||%
            rdm_data()$test_data           %||%
            rdm_data()$residents
      if (is.null(df) || nrow(df) == 0) return(NULL)
      td <- df[df$record_id == resident_id(), , drop = FALSE]
      if (nrow(td) == 0) return(td)
      # If repeating, take most recent instance; otherwise return as-is
      if ("redcap_repeat_instance" %in% names(td) && nrow(td) > 1) {
        td <- td %>%
          dplyr::mutate(.inst = suppressWarnings(as.integer(.data$redcap_repeat_instance))) %>%
          dplyr::arrange(dplyr::desc(.inst)) %>%
          dplyr::slice(1) %>%
          dplyr::select(-.inst)
      } else {
        td <- td[1, , drop = FALSE]
      }
      td
    })

    # Local live cache
    local <- reactiveValues(seva=NULL, ilp=NULL, ms=NULL, ready=FALSE, sel_period=NULL)

    # Session progression: strict sequential unlock (start at step 1 whenever
    # the period changes; each successful save bumps it). Pre-existing data
    # from prior sessions does NOT advance it automatically — the resident
    # always starts at Self-Reflection when opening a period.
    #   1=reflection 2=career 3=topics 4=feedback 5=boards
    #   6=ilp_review 7=scholarship_ack 8=milestones 9=ilp
    progress <- reactiveValues(step = 1L)

    # Scholarship completeness (schol_ps==1 AND schol_rca==1 in any row)
    .schol_done <- function() {
      sd_all <- tryCatch(rdm_data()$all_forms$scholarship, error = function(e) NULL)
      rid    <- tryCatch(resident_id(),                    error = function(e) NULL)
      if (is.null(sd_all) || is.null(rid) || nrow(sd_all) == 0) return(FALSE)
      if (!"record_id" %in% names(sd_all)) return(FALSE)
      sd <- sd_all[as.character(sd_all$record_id) == as.character(rid), , drop = FALSE]
      if (nrow(sd) == 0) return(FALSE)
      has_ps  <- "schol_ps"  %in% names(sd) &&
                 any(as.character(sd$schol_ps)  == "1", na.rm = TRUE)
      has_rca <- "schol_rca" %in% names(sd) &&
                 any(as.character(sd$schol_rca) == "1", na.rm = TRUE)
      has_ps && has_rca
    }

    # Compute starting step for a period from persisted data. Walks the section
    # sequence in order and stops at the first incomplete section — so the user
    # always lands on the next thing to do. Periods 1-5 share the same order;
    # periods 6 (graduating) and 7 (entering) have their own sequences.
    .initial_step_for <- function(p) {
      if (is.null(p)) return(1L)
      sr <- if (!is.null(local$seva) && nrow(local$seva) > 0)
              local$seva[as.character(local$seva$s_e_period) == p, , drop = FALSE] else NULL
      ir <- if (!is.null(local$ilp) && nrow(local$ilp) > 0)
              local$ilp[as.character(local$ilp$year_resident) == p, , drop = FALSE] else NULL
      md <- if (!is.null(local$ms) && nrow(local$ms) > 0)
              local$ms[as.character(local$ms$redcap_repeat_instance) == p, , drop = FALSE] else NULL
      res <- tryCatch({
        r <- rdm_data()$residents; r[r$record_id == resident_id(), , drop = FALSE]
      }, error = function(e) NULL)

      seq_for <- switch(as.character(p),
        "1" = c("reflection","career","topics","feedback",
                "boards","scholarship","milestones","ilp"),
        "2"=, "3"=, "4"=, "5" = c("reflection","career","topics","feedback",
                                  "boards","ilp_review","scholarship",
                                  "milestones","ilp"),
        "6" = c("reflection","alumni","feedback","ilp_review","scholarship","milestones"),
        "7" = c("goals","background","career","prep","topics","milestones"),
        character(0))
      if (length(seq_for) == 0) return(1L)

      pir_empty <- {
        pir <- tryCatch(prev_ilp_r(), error = function(e) NULL)
        is.null(pir) || nrow(pir) == 0
      }
      step <- 1L
      for (s in seq_for) {
        done <- if (s == "scholarship") .schol_done()
                else if (s == "ilp_review" && pir_empty) TRUE
                else .section_complete(s, sr, ir, md, res)
        if (isTRUE(done)) step <- step + 1L else break
      }
      as.integer(step)
    }

    # On period change: compute the starting step from saved data so a returning
    # resident lands on the next unfinished section (and past sections render
    # collapsed but accessible, with their data shown).
    observeEvent(local$sel_period, {
      progress$step <- .initial_step_for(local$sel_period)
    }, ignoreInit = FALSE)

    # Also re-seed once data finishes loading (seva/ilp/ms may arrive after
    # sel_period is first set, particularly on a cold app start).
    observeEvent(list(local$seva, local$ilp, local$ms), {
      req(local$sel_period)
      seeded <- .initial_step_for(local$sel_period)
      if (seeded > isolate(progress$step)) progress$step <- seeded
    }, ignoreInit = TRUE)

    .advance <- function(to) {
      cur <- isolate(progress$step)
      if (is.null(cur) || to > cur) progress$step <- as.integer(to)
    }

    # Period-aware sequence lookup — returns the ordered list of sections that
    # make up the form for the given period.
    .seq_for <- function(p) {
      switch(as.character(p),
        "1" = c("reflection","career","topics","feedback",
                "boards","scholarship","milestones","ilp"),
        "2"=, "3"=, "4"=, "5" = c("reflection","career","topics","feedback",
                                  "boards","ilp_review","scholarship",
                                  "milestones","ilp"),
        "6" = c("reflection","alumni","feedback","ilp_review","scholarship","milestones"),
        "7" = c("goals","background","career","prep","topics","milestones"),
        character(0))
    }

    # Bump progress$step to the section AFTER `section` (i.e. unlock the next
    # section in this period's sequence). No-op if `section` isn't in the
    # sequence for the current period.
    .advance_after <- function(section) {
      p   <- isolate(local$sel_period)
      seq <- .seq_for(p)
      idx <- match(section, seq)
      if (is.na(idx)) return(invisible())
      # Walk forward through any already-complete sections so the user lands
      # on the next thing that actually needs their attention.
      sr <- if (!is.null(local$seva) && nrow(local$seva) > 0)
              local$seva[as.character(local$seva$s_e_period) == p, , drop = FALSE] else NULL
      ir <- if (!is.null(local$ilp) && nrow(local$ilp) > 0)
              local$ilp[as.character(local$ilp$year_resident) == p, , drop = FALSE] else NULL
      md <- if (!is.null(local$ms) && nrow(local$ms) > 0)
              local$ms[as.character(local$ms$redcap_repeat_instance) == p, , drop = FALSE] else NULL
      res <- tryCatch({
        r <- rdm_data()$residents; r[r$record_id == resident_id(), , drop = FALSE]
      }, error = function(e) NULL)
      # ilp_review renderUI returns NULL when there's no prior ILP data —
      # skip it in that case so progression doesn't dead-end on an invisible
      # section.
      pir_empty <- {
        pir <- tryCatch(prev_ilp_r(), error = function(e) NULL)
        is.null(pir) || nrow(pir) == 0
      }
      next_step <- idx + 1L
      while (next_step <= length(seq)) {
        s <- seq[[next_step]]
        done <- if (s == "scholarship") .schol_done()
                else if (s == "ilp_review" && pir_empty) TRUE
                else .section_complete(s, sr, ir, md, res)
        if (!isTRUE(done)) break
        next_step <- next_step + 1L
      }
      .advance(next_step)
    }

    # Coalesce duplicate REDCap columns (e.g. `s_e_period...2`, `...4`) back to
    # a single canonical column. Some REDCap projects export duplicate field
    # names which readr/tibble renames with numeric suffixes — if that happens
    # `df$s_e_period` becomes NULL and every downstream lookup breaks silently.
    .coalesce_dup_cols <- function(df, canon) {
      if (is.null(df) || ncol(df) == 0) return(df)
      pat  <- paste0("^", canon, "(\\.\\.\\.[0-9]+)?$")
      hits <- grep(pat, names(df), value = TRUE)
      if (length(hits) == 0) return(df)
      if (length(hits) == 1 && hits == canon) return(df)
      # Row-wise first non-empty value across all duplicates
      mat <- as.data.frame(df[, hits, drop = FALSE], stringsAsFactors = FALSE)
      merged <- apply(mat, 1, function(r) {
        r <- as.character(r); r <- r[!is.na(r) & nzchar(r)]
        if (length(r)) r[1] else NA_character_
      })
      df[[canon]] <- merged
      drop_these <- setdiff(hits, canon)
      df <- df[, setdiff(names(df), drop_these), drop = FALSE]
      df
    }

    # Auto-coalesce any duplicated REDCap fields the tibble renamed with
    # `...N` suffixes. We find all suffixed columns, strip the suffix to get
    # the canonical name, and coalesce into one column. Fixes s_e_plus, s_e_delta,
    # etc. being hidden because the fake/test REDCap project exports duplicates.
    .coalesce_all_dups <- function(df) {
      if (is.null(df) || ncol(df) == 0) return(df)
      suffixed <- grep("\\.\\.\\.[0-9]+$", names(df), value = TRUE)
      if (length(suffixed) == 0) return(df)
      canons <- unique(sub("\\.\\.\\.[0-9]+$", "", suffixed))
      for (canon in canons) df <- .coalesce_dup_cols(df, canon)
      df
    }

    observe({
      req(rdm_data(), resident_id(), !local$ready)
      rid <- resident_id()
      df_s <- rdm_data()$all_forms$s_eval
      df_i <- rdm_data()$all_forms$ilp
      df_m <- rdm_data()$all_forms$milestone_selfevaluation_c33c %||%
              rdm_data()$milestone_selfevaluation_c33c
      # Repair any duplicate column names before filtering. Coalesce the
      # period keys explicitly first (so filtering works), then catch any
      # other duplicated fields (s_e_plus, s_e_delta, etc.).
      if (!is.null(df_s)) { df_s <- .coalesce_dup_cols(df_s, "s_e_period");    df_s <- .coalesce_all_dups(df_s) }
      if (!is.null(df_i)) { df_i <- .coalesce_dup_cols(df_i, "year_resident"); df_i <- .coalesce_all_dups(df_i) }
      if (!is.null(df_m)) df_m <- .coalesce_all_dups(df_m)
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

    # ── Period access mode ────────────────────────────────────────────────────
    # "active"  = current period (fully editable)
    # "past"    = chronologically before active (read-only — data preserved)
    # "future"  = not yet reached (blocked — form hidden)
    # "unknown" = can't determine active period (fail open — allow editing)
    .PERIOD_NAMES <- c("7"="Entering Residency","1"="Mid-Intern","2"="End Intern Year",
                       "3"="Mid-PGY2","4"="End PGY2","5"="Mid-PGY3","6"="Graduating")

    period_mode_r <- reactive({
      p  <- local$sel_period
      pi <- period_info_r()
      if (is.null(p) || is.null(pi) || is.na(pi$period_number)) return("unknown")
      active_chron <- .PERIOD_CHRON[as.character(pi$period_number)]
      sel_chron    <- .PERIOD_CHRON[p]
      if (is.na(active_chron) || is.na(sel_chron)) return("unknown")
      if      (sel_chron < active_chron)  "past"
      else if (sel_chron == active_chron) "active"
      else                                "future"
    })

    prev_seva_r <- reactive({
      p <- local$sel_period; if (is.null(p)) return(data.frame())
      df <- local$seva; if (is.null(df)||nrow(df)==0) return(data.frame())
      df <- .coalesce_all_dups(.coalesce_dup_cols(df, "s_e_period"))
      if (!"s_e_period" %in% names(df)) return(data.frame())
      cur_chron <- .PERIOD_CHRON[p] %||% -1L
      df$.chron <- unname(.PERIOD_CHRON[as.character(df$s_e_period)])
      df <- df[!is.na(df$.chron) & df$.chron < cur_chron, , drop = FALSE]
      if (nrow(df) == 0) return(data.frame())
      df <- df[order(-df$.chron), , drop = FALSE][1, , drop = FALSE]
      df$.chron <- NULL
      df
    })

    # Previous period ILP row (for context in ILP goal section)
    prev_ilp_r <- reactive({
      p <- local$sel_period; if (is.null(p)) return(data.frame())
      df <- local$ilp; if (is.null(df) || nrow(df) == 0) return(data.frame())
      df <- .coalesce_all_dups(.coalesce_dup_cols(df, "year_resident"))
      if (!"year_resident" %in% names(df)) return(data.frame())
      cur_chron <- .PERIOD_CHRON[p] %||% -1L
      df$.chron <- unname(.PERIOD_CHRON[as.character(df$year_resident)])
      df <- df[!is.na(df$.chron) & df$.chron < cur_chron, , drop = FALSE]
      if (nrow(df) == 0) return(data.frame())
      df <- df[order(-df$.chron), , drop = FALSE][1, , drop = FALSE]
      df$.chron <- NULL
      df
    })

    # ── Previous milestone self-eval scores (for context display) ─────────────
    # Returns a named list of item-key → integer score from the most recent
    # prior period (by redcap_repeat_instance ordering). Keys match the
    # milestone module's internal IDs (PC_1, MK_2, …).
    .MS_FIELD_MAP <- get_milestone_field_mapping_rdm2("milestone_selfevaluation_c33c")
    prev_ms_scores_r <- reactive({
      p <- local$sel_period
      if (is.null(p) || is.null(local$ms) || nrow(local$ms) == 0) return(NULL)
      cur_chron <- .PERIOD_CHRON[p] %||% -1L
      df <- local$ms
      if (!"redcap_repeat_instance" %in% names(df)) return(NULL)
      df$.chron <- unname(.PERIOD_CHRON[as.character(df$redcap_repeat_instance)])
      df <- df[!is.na(df$.chron) & df$.chron < cur_chron, , drop = FALSE]
      if (nrow(df) == 0) return(NULL)
      df <- df[order(-df$.chron), , drop = FALSE][1, , drop = FALSE]
      out <- list()
      for (k in names(.MS_FIELD_MAP)) {
        fld <- .MS_FIELD_MAP[[k]]
        if (fld %in% names(df)) {
          v <- df[[fld]][1]
          if (!is.na(v) && nzchar(as.character(v))) {
            suppressWarnings(iv <- as.integer(v))
            if (!is.na(iv)) out[[k]] <- iv
          }
        }
      }
      if (length(out) == 0) NULL else out
    })

    # ── Current-period saved milestone ratings (for prefill) ──────────────────
    # Lets the resident return to a partially-completed milestone section and
    # see their previously entered scores / descriptions instead of an empty
    # grid. Keys are PC_1, MK_2, … matching the milestone module's internal IDs.
    cur_ms_scores_r <- reactive({
      p <- local$sel_period
      if (is.null(p) || is.null(local$ms) || nrow(local$ms) == 0) return(NULL)
      df <- local$ms
      if (!"redcap_repeat_instance" %in% names(df)) return(NULL)
      df <- df[as.character(df$redcap_repeat_instance) == p, , drop = FALSE]
      if (nrow(df) == 0) return(NULL)
      out <- list()
      for (k in names(.MS_FIELD_MAP)) {
        fld <- .MS_FIELD_MAP[[k]]
        if (fld %in% names(df)) {
          v <- df[[fld]][1]
          if (!is.na(v) && nzchar(as.character(v))) {
            suppressWarnings(iv <- as.integer(v))
            if (!is.na(iv)) out[[k]] <- iv
          }
        }
      }
      if (length(out) == 0) NULL else out
    })
    cur_ms_descs_r <- reactive({
      p <- local$sel_period
      if (is.null(p) || is.null(local$ms) || nrow(local$ms) == 0) return(NULL)
      df <- local$ms
      if (!"redcap_repeat_instance" %in% names(df)) return(NULL)
      df <- df[as.character(df$redcap_repeat_instance) == p, , drop = FALSE]
      if (nrow(df) == 0) return(NULL)
      out <- list()
      for (k in names(.MS_FIELD_MAP)) {
        fld <- paste0(.MS_FIELD_MAP[[k]], "_desc")
        if (fld %in% names(df)) {
          v <- df[[fld]][1]
          if (!is.na(v) && nzchar(as.character(v))) out[[k]] <- as.character(v)
        }
      }
      if (length(out) == 0) NULL else out
    })

    # ── milestone module (initialised once, period reactive) ──────────────────
    milestone_result <- mod_miles_rating_server("milestone_rating",
                                                period         = period_name_r,
                                                prev_scores    = prev_ms_scores_r,
                                                initial_scores = cur_ms_scores_r,
                                                initial_descs  = cur_ms_descs_r)

    # Shared save helper — called both from debounced auto-save and on done()
    # All 22 milestone self-eval fields have a matching <fld>_desc column in
    # REDCap (verified against RDM20 data dict 2026-04-10), so descriptions
    # save unconditionally for every rated subcompetency.
    .save_milestone_scores <- function(scores, descs, p) {
      if (length(scores) == 0 || is.null(p)) return(invisible(NULL))
      field_map <- get_milestone_field_mapping_rdm2("milestone_selfevaluation_c33c")
      fields    <- list(prog_mile_date_self   = format(Sys.Date(), "%Y-%m-%d"),
                        prog_mile_period_self = as.character(p))
      for (key in names(scores)) {
        rdm_fld <- field_map[key]
        if (!is.na(rdm_fld)) {
          fields[[rdm_fld]] <- as.character(scores[[key]])
          if (!is.null(descs[[key]]) && nzchar(trimws(descs[[key]])))
            fields[[paste0(rdm_fld, "_desc")]] <- descs[[key]]
        }
      }
      message("[milestone save] record=", resident_id(),
              " instance=", p, " n_fields=", length(fields),
              " keys=", paste(head(names(fields), 6), collapse=","),
              if (length(fields) > 6) "..." else "")
      res <- .rc_save(resident_id(), "milestone_selfevaluation_c33c", as.integer(p), fields)
      if (!isTRUE(res$success))
        message("[milestone save] FAILED: ", res$message %||% "(no message)")
      else
        message("[milestone save] OK ", res$ts %||% "", " body=", res$body %||% "")
      if (res$success) {
        existing <- local$ms %||% data.frame()
        row_idx  <- which(as.character(existing$redcap_repeat_instance) == as.character(p))
        new_row  <- as.data.frame(c(list(record_id              = resident_id(),
                                         redcap_repeat_instance = as.character(p)),
                                    lapply(fields, as.character)),
                                  stringsAsFactors = FALSE, check.names = FALSE)
        if (length(row_idx) > 0)
          existing[row_idx[1], intersect(names(new_row), names(existing))] <-
            new_row[1, intersect(names(new_row), names(existing))]
        else existing <- dplyr::bind_rows(existing, new_row)
        local$ms <- existing
      }
      res
    }

    # Auto-save: fires 2.5 s after the last rating / description change
    # (debounced). This is the ONLY save path — partial progress is written
    # throughout the session, and each save is an overwrite of the same
    # instance (period number), so changing a value replaces the prior one.
    # A signature hash prevents re-POSTing identical data (e.g. when "done"
    # flips TRUE but scores haven't actually changed).
    save_signal <- reactive({
      list(scores = milestone_result$scores(),
           descs  = milestone_result$desc())
    })
    save_debounced <- debounce(save_signal, 2500)
    last_sig <- reactiveVal(NULL)

    # Mirrors thresholds in gmed::mod_miles_rating_server — ratings >= the
    # period threshold require a written justification before the score is
    # saved, otherwise the auto-save would persist a high rating with no
    # description and prematurely advance the section.
    .ms_thresholds <- list(
      "Entering Residency" = 3, "Mid Intern" = 4, "End Intern" = 5,
      "Mid PGY2" = 6, "End PGY2" = 7, "Mid PGY3" = 8, "Graduating" = 9)

    observe({
      sig <- save_debounced()
      req(length(sig$scores) > 0,
          !is.null(local$sel_period),
          local$sel_period %in% as.character(c(1:6, 7)),
          period_mode_r() %in% c("active", "unknown"))
      # Hold the save if any rated milestone needs a justification but doesn't
      # have one yet — prevents losing focus / advancing the section while the
      # resident is mid-typing.
      thr <- .ms_thresholds[[period_name_r() %||% ""]]
      if (!is.null(thr)) {
        pending <- vapply(names(sig$scores), function(k) {
          v <- sig$scores[[k]]
          if (is.null(v) || is.na(v) || v < thr) return(FALSE)
          d <- sig$descs[[k]]
          is.null(d) || !nzchar(trimws(as.character(d)))
        }, logical(1))
        if (any(pending)) return()
      }
      # Skip if this exact payload was just saved
      sig_str <- rlang::hash(list(sig, local$sel_period))
      if (isTRUE(identical(sig_str, isolate(last_sig())))) return()
      res <- .save_milestone_scores(sig$scores, sig$descs, local$sel_period)
      if (!is.null(res)) {
        ss$milestones <- res
        if (isTRUE(res$success)) { last_sig(sig_str); .advance_after("milestones") }
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
      # Drop any pre-existing s_e_period from fields so we never construct a row
      # with two s_e_period columns (which would force bind_rows to rename them
      # and corrupt subsequent filtering on s_e_period).
      flds <- fields[setdiff(names(fields), "s_e_period")]
      idx <- if (!is.null(df)&&nrow(df)>0) which(as.character(df$s_e_period)==p) else integer(0)
      if (length(idx)>0) {
        df[idx[1], "s_e_period"] <- p
        for (fld in names(flds)) df[idx[1], fld] <- as.character(flds[[fld]])
      } else {
        df <- dplyr::bind_rows(df, as.data.frame(c(
          list(record_id=resident_id(), s_e_period=p, redcap_repeat_instance=p),
          lapply(flds, as.character)), stringsAsFactors=FALSE, check.names=FALSE))
      }
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
    output$milestone_save_status <- renderUI({
      r <- ss$milestones
      if (is.null(r)) return(NULL)
      if (isTRUE(r$success))
        tags$span(class = "badge",
                  style = "background:#d1fae5; color:#065f46; font-size:0.72rem;
                           font-weight:500; padding:3px 8px; border-radius:20px;",
          tags$i(class = "bi bi-cloud-check-fill me-1"), "Auto-saved")
      else {
        msg <- r$message %||% "(no detail)"
        tags$span(class = "badge",
                  style = "background:#fee2e2; color:#991b1b; font-size:0.72rem;
                           font-weight:500; padding:3px 8px; border-radius:20px;
                           max-width:560px; white-space:normal; text-align:left;",
                  title = msg,
          tags$i(class = "bi bi-exclamation-triangle-fill me-1"),
          "Save failed \u2014 ", msg)
      }
    })

    # ── Boards & ITE — whole-section renderUI (Fix 1) ─────────────────────────
    output$boards_full_ui <- renderUI({
      req(local$sel_period %in% as.character(1:5))
      sr  <- sel_seva()
      res <- resident_r()
      dd  <- data_dict_r()
      pi  <- period_info_r()
      ite <- ite_data_r()
      pgy <- if (!is.null(pi)) as.integer(pi$pgy_year) else 1L

      # Declare reactive dependencies so the renderUI re-fires on every click
      .dep_step3      <- input$s_e_step3
      .dep_step3_cs   <- input$s_e_step3_date_set
      .dep_step3_cont <- input$s_e_step3_contact
      .dep_exam_type  <- input$step3_exam_type

      # ITE percent correct for current PGY year
      pgy_fld <- paste0("pgy", pgy, "_tot_correct")
      ite_pct <- if (!is.null(ite) && nrow(ite)>0 && pgy_fld %in% names(ite)) {
        v <- ite[[pgy_fld]][1]; if (!is.na(v) && nzchar(v)) as.numeric(v) else NA
      } else NA_real_

      ite_panel <- if (!is.na(ite_pct)) {
        prob       <- .pass_prob(ite_pct, pgy)
        risk       <- .risk_from_prob(prob)
        div(class="alert mb-3 py-2 px-3",
            style=paste0("background:#f8fafc; border-left:4px solid ", risk$color, "; font-size:0.83rem;"),
          div(class="d-flex align-items-center gap-3",
            div(style="text-align:center; min-width:70px;",
              tags$span(style=paste0("font-size:1.5rem; font-weight:700; color:", risk$color),
                        paste0(round(ite_pct,1),"%")),
              tags$p(style="font-size:0.68rem; color:#6c757d; margin:0;", "ITE % correct")),
            div(style="flex:1;",
              tags$p(style="margin:0; font-weight:600; font-size:0.85rem; color:#003d5c;",
                     paste0("PGY", pgy, " ACP ITE Score")),
              if (!is.na(prob))
                tags$p(style="margin:0; font-size:0.78rem; color:#6c757d;",
                       paste0(round(prob * 100, 0), "% predicted ABIM pass probability \u2014 "),
                       tags$span(style=paste0("font-weight:700; color:", risk$color),
                                 risk$level))
              else
                tags$p(style="margin:0; font-size:0.78rem; color:#6c757d;",
                       risk$level))))
      } else {
        div(class="alert alert-light mb-3 py-2 px-3",
            style="font-size:0.82rem; border-left:4px solid #adb5bd;",
            tags$i(class="bi bi-info-circle me-1"),
            "ITE score not yet available. Complete your ACP ITE to see your score here.")
      }

      # yn_row — btn-group toggle visual; onclick on LABEL (the visible click target)
      # Bootstrap btn-check hides the input visually; label click is what the user sees.
      yn_row <- function(id, label, val)
        div(class="mb-2 d-flex align-items-center gap-3",
          tags$label(label, style="font-size:0.83rem; color:#2c3e50; min-width:280px; margin:0;"),
          div(class="btn-group btn-group-sm",
            tags$input(type="radio", class="btn-check", name=ns(id),
                       id=paste0(ns(id),"_1"), value="1",
                       checked=if(!is.null(val)&&val=="1") NA else NULL),
            tags$label(class=paste0("btn btn-outline-success",
                                    if(!is.null(val)&&val=="1")" active" else ""),
                       `for`=paste0(ns(id),"_1"),
                       onclick=paste0("Shiny.setInputValue('",ns(id),"','1',{priority:'event'})"),
                       "Yes"),
            tags$input(type="radio", class="btn-check", name=ns(id),
                       id=paste0(ns(id),"_0"), value="0",
                       checked=if(!is.null(val)&&val=="0") NA else NULL),
            tags$label(class=paste0("btn btn-outline-secondary",
                                    if(!is.null(val)&&val=="0")" active" else ""),
                       `for`=paste0(ns(id),"_0"),
                       onclick=paste0("Shiny.setInputValue('",ns(id),"','0',{priority:'event'})"),
                       "No")))

      # If a Step 3 score is already on file in resident_data, skip the
      # yes/no flow entirely — just show a "complete" badge and let the user
      # move on to board prep notes.
      usmle_on_file  <- if (!is.null(res)&&nrow(res)>0) .fv(res,"usmle_step3_score") else ""
      comlex_on_file <- if (!is.null(res)&&nrow(res)>0) .fv(res,"comlex_step3_score") else ""
      step3_on_file  <- nzchar(usmle_on_file) || nzchar(comlex_on_file)

      step3_row <- if (step3_on_file) {
        label <- if (nzchar(usmle_on_file))
                   paste0("USMLE Step 3: ", usmle_on_file)
                 else
                   paste0("COMLEX Level 3: ", comlex_on_file)
        div(class="mb-3 py-2 px-3",
            style="background:#e8f5e9; border-left:4px solid #2e7d32;
                   border-radius:6px; font-size:0.85rem;",
          tags$i(class="bi bi-check-circle-fill me-2", style="color:#2e7d32;"),
          tags$span(style="font-weight:600; color:#1b5e20;", "Step 3 complete"),
          tags$span(style="color:#33691e; margin-left:8px;", label))
      } else {
        step3_val <- { v <- input$s_e_step3
          if (!is.null(v) && nzchar(v)) v else .fv(sr, "s_e_step3") }
        yn_row("s_e_step3",
               "Have you completed Step 3 (USMLE or COMLEX)?",
               step3_val)
      }

      step3_cond <- if (step3_on_file) {
        NULL
      } else {
        step3_val <- { v <- input$s_e_step3
          if (!is.null(v) && nzchar(v)) v else .fv(sr, "s_e_step3") }
        if (!nzchar(step3_val)) NULL
        else if (step3_val == "1") {
          tagList(
            div(class="mt-2 mb-2",
              tags$label("USMLE Step 3 or COMLEX Level 3?",
                         style="font-size:0.83rem; font-weight:600; color:#2c3e50;"),
              div(class="btn-group btn-group-sm",
                tags$input(type="radio", class="btn-check", name=ns("step3_exam_type"),
                           id=paste0(ns("step3_exam_type"),"_usmle"), value="usmle",
                           checked=if(!is.null(.dep_exam_type)&&.dep_exam_type=="usmle") NA else NULL),
                tags$label(class=paste0("btn btn-outline-primary",
                                        if(!is.null(.dep_exam_type)&&.dep_exam_type=="usmle")" active" else ""),
                           `for`=paste0(ns("step3_exam_type"),"_usmle"),
                           onclick=paste0("Shiny.setInputValue('",ns("step3_exam_type"),"','usmle',{priority:'event'})"),
                           "USMLE"),
                tags$input(type="radio", class="btn-check", name=ns("step3_exam_type"),
                           id=paste0(ns("step3_exam_type"),"_comlex"), value="comlex",
                           checked=if(!is.null(.dep_exam_type)&&.dep_exam_type=="comlex") NA else NULL),
                tags$label(class=paste0("btn btn-outline-warning",
                                        if(!is.null(.dep_exam_type)&&.dep_exam_type=="comlex")" active" else ""),
                           `for`=paste0(ns("step3_exam_type"),"_comlex"),
                           onclick=paste0("Shiny.setInputValue('",ns("step3_exam_type"),"','comlex',{priority:'event'})"),
                           "COMLEX"))),
            uiOutput(ns("step3_score_ui")))
        } else {
          date_set_val <- { v <- input$s_e_step3_date_set
            if (!is.null(v)&&nzchar(v)) v else .fv(sr,"s_e_step3_date_set") }
          tagList(
            yn_row("s_e_step3_date_set", "Have you set a Step 3 exam date?", date_set_val),
            if (isTRUE(date_set_val=="1"))
              div(class="mt-2",
                tags$label("Scheduled exam date", style="font-size:0.83rem; color:#2c3e50;"),
                tags$input(type="date", id=ns("s_e_step3_date"),
                           class="form-control form-control-sm", style="max-width:200px;",
                           value=.fv(sr,"s_e_step3_date"))))
        }
      }

      mksap_ch <- .dd_select(dd, "s_e_mksap_comp")   # flipped: label=code

      # Collapse only when the boards section has been saved (step3 answered).
      # Avoid deriving collapse from feedback completion: the renderUI fires on
      # every input$s_e_step3 change, which would fight any manual user expansion.
      boards_collapsed <- .section_complete("boards", sr, NULL, NULL, NULL)

      .sec_card(title="Boards & ITE", icon="clipboard2-pulse-fill",
                collapsed=isTRUE(boards_collapsed),
        tags$p(class="text-muted mb-3", style="font-size:0.82rem;",
               "Review your ITE score and update your board preparation status."),
        ite_panel,
        step3_row,
        step3_cond,
        div(class="mt-3 pt-2", style="border-top:1px solid #f0f0f0;",
          .ta(ns("s_e_board_discu"), "Board study plan / discussion notes",
              .fv(sr,"s_e_board_discu"), rows=2)),
        if (!is.null(mksap_ch))
          div(class="mb-2",
            tags$label("MKSAP completion",
                       style="font-size:0.83rem; color:#2c3e50; font-weight:600;"),
            selectInput(ns("s_e_mksap_comp"), label=NULL,
              choices=c("-- select --"="", mksap_ch),
              selected=.fv(sr,"s_e_mksap_comp"),
              selectize=FALSE, width="340px")),
        .save_btn(ns, "save_boards", "Save Board Info"))
    })

    # ── Step 3 score input (rendered after exam type chosen) ───────────────────
    output$step3_score_ui <- renderUI({
      exam_type <- input$step3_exam_type
      if (is.null(exam_type) || !nzchar(exam_type)) return(NULL)
      res <- resident_r()
      usmle_s  <- if (!is.null(res)&&nrow(res)>0) .fv(res,"usmle_step3_score") else ""
      comlex_s <- if (!is.null(res)&&nrow(res)>0) .fv(res,"comlex_step3_score") else ""
      score_val <- if (exam_type=="usmle") usmle_s else if (exam_type=="comlex") comlex_s else ""
      div(class="mt-2",
        tags$label(paste0(if(exam_type=="usmle")"USMLE Step 3" else "COMLEX Level 3", " Score"),
                   style="font-size:0.83rem; color:#2c3e50;"),
        tags$input(type="number", id=ns("step3_score_val"),
                   class="form-control form-control-sm", style="max-width:140px;",
                   value=score_val, placeholder="e.g., 215", min="1", max="999"))
    })

    # ── Scholarship readiness check (PS + RCA) ────────────────────────────────
    # Checks any existing scholarship rows for schol_ps=="1" / schol_rca=="1".
    # If both already yes, shows a completed badge. Otherwise shows yes/no
    # inputs + a save button that writes a new scholarship instance marking
    # the acknowledged activity. Also renders the "Go to Scholarship" link.
    output$scholarship_check_ui <- renderUI({
      req(local$sel_period %in% as.character(1:6))
      # Pull this resident's scholarship rows (if any)
      sd_all <- tryCatch(rdm_data()$all_forms$scholarship, error=function(e) NULL)
      rid    <- resident_id()
      sd     <- if (!is.null(sd_all) && nrow(sd_all)>0 && "record_id" %in% names(sd_all))
                  sd_all[as.character(sd_all$record_id)==as.character(rid), , drop=FALSE]
                else data.frame()
      has_ps  <- if (nrow(sd)>0 && "schol_ps"  %in% names(sd))
                   any(as.character(sd$schol_ps)  == "1", na.rm=TRUE) else FALSE
      has_rca <- if (nrow(sd)>0 && "schol_rca" %in% names(sd))
                   any(as.character(sd$schol_rca) == "1", na.rm=TRUE) else FALSE

      done_badge <- function(label)
        tags$span(style="background:#198754; color:#fff; border-radius:20px;
                         padding:2px 10px; font-size:0.75rem; font-weight:600;",
                  tags$i(class="bi bi-check-circle-fill me-1"), label)

      yn <- function(id, label, current=NULL) {
        div(class="d-flex align-items-center gap-3 mb-2",
          tags$label(label, style="font-size:0.83rem; color:#2c3e50; min-width:280px; margin:0;"),
          div(class="btn-group btn-group-sm",
            tags$input(type="radio", class="btn-check", name=ns(id),
                       id=paste0(ns(id),"_1"), value="1",
                       checked=if (identical(current,"1")) NA else NULL),
            tags$label(class=paste0("btn btn-outline-success",
                                    if (identical(current,"1")) " active" else ""),
                       `for`=paste0(ns(id),"_1"),
                       onclick=paste0("Shiny.setInputValue('",ns(id),"','1',{priority:'event'})"),
                       "Yes"),
            tags$input(type="radio", class="btn-check", name=ns(id),
                       id=paste0(ns(id),"_0"), value="0",
                       checked=if (identical(current,"0")) NA else NULL),
            tags$label(class=paste0("btn btn-outline-secondary",
                                    if (identical(current,"0")) " active" else ""),
                       `for`=paste0(ns(id),"_0"),
                       onclick=paste0("Shiny.setInputValue('",ns(id),"','0',{priority:'event'})"),
                       "No")))
      }

      both_done <- has_ps && has_rca
      body <- if (both_done) {
        div(class="d-flex flex-wrap gap-2 align-items-center mb-2",
          done_badge("Patient safety review \u2014 on file"),
          done_badge("Root cause analysis \u2014 on file"))
      } else {
        tagList(
          tags$p(class="text-muted mb-2", style="font-size:0.82rem;",
                 "Confirm whether you've completed a real or simulated patient safety review ",
                 "and/or root cause analysis (RCA). Already-recorded \"Yes\" answers are shown as complete."),
          div(class="d-flex flex-wrap gap-2 mb-2",
            if (has_ps)  done_badge("Patient safety review \u2014 on file") else NULL,
            if (has_rca) done_badge("RCA \u2014 on file") else NULL),
          if (!has_ps)
            yn("schol_ps_ack",  "Real or simulated patient safety review completed?",
               input$schol_ps_ack),
          if (!has_rca)
            yn("schol_rca_ack", "Real or simulated root cause analysis (RCA) completed?",
               input$schol_rca_ack),
          div(class="mt-2",
            actionButton(ns("save_schol_ack"), "Save",
              class="btn btn-sm btn-primary",
              style="padding:4px 14px; font-size:0.82rem;")),
          uiOutput(ns("save_schol_ack_status")))
      }

      # Wrapped card styled like the old scholarship prompt. The "Go to
      # Scholarship" button sits inline at the bottom of the flow (not floated
      # to the side) so it reads as the natural next step after the PS / RCA
      # acknowledgment.
      div(class="card border-0 mb-3",
          style="background:linear-gradient(135deg,#f8f4ff 0%,#eef2ff 100%);
                 border-left:4px solid #6f42c1 !important; border-radius:8px;",
        div(class="card-body py-3 px-4",
          # Header — title + helper text, stacked
          div(class="mb-2",
            tags$p(style="font-weight:700; color:#4a1d96; font-size:0.9rem; margin:0;",
                   tags$i(class="bi bi-award-fill me-2"), "Scholarship & Teaching"),
            tags$p(style="font-size:0.82rem; color:#6c757d; margin:4px 0 0;",
                   "Log research, presentations, teaching, and academic activities in the Scholarship tab.")),
          # Body — Q&A / completion badges
          div(class="mt-2 pt-2", style="border-top:1px dashed #cfc4e5;", body),
          # Inline call-to-action at the bottom of the flow
          div(class="mt-3 pt-2 d-flex align-items-center gap-2",
              style="border-top:1px dashed #cfc4e5;",
            tags$span(style="font-size:0.82rem; color:#4a1d96;",
                      tags$i(class="bi bi-arrow-right-circle me-1"),
                      "Next:"),
            tags$button(
              class="btn btn-sm",
              style="background:#6f42c1; color:#fff; border:none;
                     padding:6px 18px; font-size:0.85rem; font-weight:600;",
              onclick="Shiny.setInputValue('nav_block','scholarship',{priority:'event'})",
              tags$i(class="bi bi-box-arrow-up-right me-1"),
              "Enter details in the Scholarship section"))))
    })

    # Save ack — creates a new scholarship instance with ps/rca flags set.
    # Reuses an existing instance if the row has no data yet; otherwise adds.
    observeEvent(input$save_schol_ack, {
      ps_ack  <- input$schol_ps_ack  %||% ""
      rca_ack <- input$schol_rca_ack %||% ""
      if (!(identical(ps_ack,"1") || identical(rca_ack,"1"))) {
        output$save_schol_ack_status <- renderUI(
          tags$span(class="text-muted small",
                    "Nothing to save — select Yes for at least one item."))
        return()
      }
      sd_all <- tryCatch(rdm_data()$all_forms$scholarship, error=function(e) NULL)
      rid    <- resident_id()
      existing <- if (!is.null(sd_all) && nrow(sd_all)>0)
                    sd_all[as.character(sd_all$record_id)==as.character(rid), , drop=FALSE]
                  else data.frame()
      inst <- if (nrow(existing)==0) 1L
              else as.integer(max(as.integer(existing$redcap_repeat_instance), na.rm=TRUE)) + 1L
      fields <- list()
      if (identical(ps_ack, "1"))  fields$schol_ps  <- "1"
      if (identical(rca_ack,"1"))  fields$schol_rca <- "1"
      res <- .rc_save(rid, "scholarship", inst, fields)
      if (isTRUE(res$success)) {
        output$save_schol_ack_status <- renderUI(
          tags$span(class="text-success small",
            tags$i(class="bi bi-check-circle-fill me-1"),
            paste0("Saved \u2014 scholarship entry #", inst, " (", res$ts, ")")))
        .advance_after("scholarship")
      } else {
        output$save_schol_ack_status <- renderUI(
          tags$span(class="text-danger small",
            tags$i(class="bi bi-exclamation-triangle-fill me-1"),
            paste0("Save failed: ", res$message %||% "unknown error")))
      }
    })

    # ── ILP Goal Review (periods 1-5, shown above Milestones) ─────────────────
    # Uses radioButtons() so Shiny registers live input — conditional text areas
    # react immediately without a save round-trip.
    output$ilp_review_ui <- renderUI({
      req(local$sel_period %in% as.character(2:6))
      pir_data <- prev_ilp_r()
      if (is.null(pir_data) || nrow(pir_data) == 0) {
        return(.sec_card(title="ILP Goal Review", icon="clipboard-check-fill",
                         collapsed = FALSE,
          div(class="alert alert-light mb-0 py-3 px-3",
              style="font-size:0.85rem; border-left:4px solid #adb5bd;",
            tags$i(class="bi bi-info-circle me-2"),
            tags$strong("No prior ILP goals on record. "),
            "Once you set goals in the Individual Learning Plan section below, ",
            "they'll appear here for review next period.",
            div(class="mt-3",
              actionButton(ns("ilp_review_skip"), "Continue",
                           class="btn btn-primary btn-sm")))))
      }
      ir <- sel_ilp()
      dd <- data_dict_r()

      # Declare reactive dependency on the three radio inputs so the
      # conditional text areas re-appear as soon as a button is clicked
      live_pcmk    <- input$prior_goal_pcmk
      live_sbppbl  <- input$prior_goal_sbppbl
      live_profics <- input$prior_goal_profics

      # DD-derived choices, flipped to label=code format for selectInput/lookup
      pcmk_ch    <- .dd_select(dd, "goal_pcmk",            .SUBCOMP_CHOICES$pcmk)
      sbppbl_ch  <- .dd_select(dd, "goal_sbppbl",          .SUBCOMP_CHOICES$sbppbl)
      profics_ch <- .dd_select(dd, "goal_subcomp_profics", .SUBCOMP_CHOICES$profics)

      rev_collapsed <- .section_complete("ilp_review", NULL, ir, NULL, NULL)

      # Helper: one domain review block
      # cur_val: live radio input value (takes precedence over saved DB value)
      make_domain_review <- function(domain_label, color, icon_cls,
                                     domain, goal_fld, level_fld, row_fld,
                                     prior_fld, q_fld, q2_fld,
                                     choices, cur_val) {
        prev_code  <- .fv(pir_data, goal_fld)
        prev_level <- .fv(pir_data, level_fld)
        prev_name  <- if (nzchar(prev_code)) {
          n <- names(choices)[choices == prev_code]
          if (length(n)) n[1] else paste0("Code ", prev_code)
        } else NULL

        if (is.null(prev_name)) return(NULL)   # domain had no previous goal

        # Prefer the descriptive label from .SUBCOMP_CHOICES (e.g. "PC1 \u2014
        # History-taking / interviewing") when the data dictionary only has
        # the bare label ("Patient Care 1"). Falls back to whatever was found.
        sc <- .SUBCOMP_CHOICES[[domain]]
        rich_name <- if (!is.null(sc)) {
          rn <- names(sc)[sc == prev_code]
          if (length(rn)) rn[1] else prev_name[1]
        } else prev_name[1]

        # Prefer live input; fall back to saved value
        saved_val  <- .fv(ir, prior_fld)
        disp_val   <- if (!is.null(cur_val) && nzchar(cur_val)) cur_val
                      else if (nzchar(saved_val)) saved_val
                      else NULL

        prev_row   <- .fv(pir_data, row_fld)
        lv_txt     <- if (nzchar(prev_level)) paste0(" \u2014 Level ", prev_level) else ""
        row_txt    <- if (nzchar(prev_row))   paste0("Row ", prev_row, ": ")    else ""
        goal_text  <- .get_goal_text(domain, prev_code, prev_row, prev_level, dd)

        div(class = "mb-4 pb-3", style = "border-bottom:1px solid #f0f0f0;",
          # Previous goal display
          div(class = "mb-2 p-2",
              style = paste0("background:#f8fafc; border-left:3px solid ", color,
                             "; border-radius:0 6px 6px 0;"),
            tags$p(style = paste0("font-size:0.68rem; font-weight:700; color:#6c757d;",
                                  " text-transform:uppercase; letter-spacing:.05em; margin:0 0 2px;"),
                   tags$i(class = paste0("bi bi-", icon_cls, " me-1")), domain_label,
                   " \u2014 Previous Goal"),
            tags$p(style = "font-size:0.78rem; color:#5b6e84; margin:0 0 2px; font-weight:600;",
                   paste0(rich_name, lv_txt)),
            if (!is.null(goal_text))
              tags$p(style = "font-size:0.82rem; color:#2c3e50; margin:0; line-height:1.4;
                              font-style:italic;",
                     tags$i(class = "bi bi-quote me-1", style = "opacity:0.4;"),
                     paste0(row_txt, goal_text))
            else if (nzchar(prev_row) || nzchar(prev_level))
              tags$p(style = "font-size:0.78rem; color:#9ca3af; margin:0; font-style:italic;",
                     "(milestone anchor text not recorded for this row/level)")),

          # Achievement yes/no — onclick on LABEL (visible click target in btn-check)
          div(class = "mb-2 d-flex align-items-center gap-3",
            tags$label("Did you reach this goal?",
                       style = "font-size:0.83rem; color:#2c3e50; min-width:200px; margin:0;"),
            div(class = "btn-group btn-group-sm",
              tags$input(type="radio", class="btn-check", name=ns(prior_fld),
                         id=paste0(ns(prior_fld),"_1"), value="1",
                         checked=if(identical(disp_val,"1")) NA else NULL),
              tags$label(class=paste0("btn btn-outline-success",
                                      if(identical(disp_val,"1"))" active" else ""),
                         `for`=paste0(ns(prior_fld),"_1"),
                         onclick=paste0("Shiny.setInputValue('",ns(prior_fld),"','1',{priority:'event'})"),
                         "Yes"),
              tags$input(type="radio", class="btn-check", name=ns(prior_fld),
                         id=paste0(ns(prior_fld),"_0"), value="0",
                         checked=if(identical(disp_val,"0")) NA else NULL),
              tags$label(class=paste0("btn btn-outline-secondary",
                                      if(identical(disp_val,"0"))" active" else ""),
                         `for`=paste0(ns(prior_fld),"_0"),
                         onclick=paste0("Shiny.setInputValue('",ns(prior_fld),"','0',{priority:'event'})"),
                         "No"))),

          # Conditional reflection text area — shown immediately on click
          if (identical(disp_val, "1"))
            .ta(ns(q2_fld),
                "What helped you succeed / what did you learn?",
                .fv(ir, q2_fld), rows = 2,
                placeholder = "Reflect on what supported your progress...")
          else if (identical(disp_val, "0"))
            .ta(ns(q_fld),
                "What got in the way? What will you do differently?",
                .fv(ir, q_fld), rows = 2,
                placeholder = "Barriers, lessons learned, or next steps...")
        )
      }

      pcmk_blk    <- make_domain_review(
        "Patient Care / Med Knowledge", "#0d6efd", "heart-pulse-fill",
        "pcmk", "goal_pcmk", "goal_level_pcmk", "goal_level_r_pcmk",
        "prior_goal_pcmk", "review_q_pcmk", "review_q2_pcmk",
        pcmk_ch, live_pcmk)
      sbppbl_blk  <- make_domain_review(
        "Systems / Practice-Based Learning", "#198754", "diagram-3-fill",
        "sbppbl", "goal_sbppbl", "goal_level_sbppbl", "goal_r_sbppbl",
        "prior_goal_sbppbl", "review_q_sbppbl", "review_q2_sbppbl",
        sbppbl_ch, live_sbppbl)
      profics_blk <- make_domain_review(
        "Professionalism / Interpersonal", "#6f42c1", "people-fill",
        "profics", "goal_subcomp_profics", "goal_level_profics", "goal_r_profics",
        "prior_goal_profics", "review_q_profics", "review_q2_profics",
        profics_ch, live_profics)

      # All three domains returned NULL (each domain's prior goal is blank) —
      # render the empty-state card so the section is always visible.
      if (is.null(pcmk_blk) && is.null(sbppbl_blk) && is.null(profics_blk))
        return(.sec_card(title="ILP Goal Review", icon="clipboard-check-fill",
                         collapsed = FALSE,
          div(class="alert alert-light mb-0 py-3 px-3",
              style="font-size:0.85rem; border-left:4px solid #adb5bd;",
            tags$i(class="bi bi-info-circle me-2"),
            tags$strong("No prior ILP goals on record. "),
            "Once you set goals in the Individual Learning Plan section below, ",
            "they'll appear here for review next period.",
            div(class="mt-3",
              actionButton(ns("ilp_review_skip"), "Continue",
                           class="btn btn-primary btn-sm")))))

      .sec_card(title = "ILP Goal Review", icon = "clipboard-check-fill",
                collapsed = isTRUE(rev_collapsed),
        info_note(
          summary = "What is ILP Goal Review?",
          tags$p("At each evaluation period, you review whether you achieved the goals set in your ",
                 tags$strong("Individual Learning Plan (ILP)"), " from the previous period."),
          tags$p("For each domain, indicate whether you reached your goal and briefly reflect on what helped ",
                 "or what barriers you encountered. This reflection informs the goals you'll set below.")
        ),
        tags$p(class = "text-muted mb-3", style = "font-size:0.82rem;",
               "Reflect on the goals you set last period before rating your milestones."),
        pcmk_blk,
        sbppbl_blk,
        profics_blk,
        .save_btn(ns, "save_ilp_review", "Save Goal Review"))
    })

    # Empty-state "Continue" button — no prior ILP goals to review, just advance.
    observeEvent(input$ilp_review_skip, { .advance_after("ilp_review") })

    # ── ILP Review save handler ────────────────────────────────────────────────
    observeEvent(input$save_ilp_review, {
      req(local$sel_period %in% as.character(2:6), period_mode_r() %in% c("active","unknown"))
      p    <- local$sel_period
      fv_i <- function(id) { v <- input[[id]]; if (is.null(v)) "" else as.character(v) }
      f <- list(
        year_resident      = p,
        prior_goal_pcmk    = fv_i("prior_goal_pcmk"),
        review_q_pcmk      = fv_i("review_q_pcmk"),
        review_q2_pcmk     = fv_i("review_q2_pcmk"),
        prior_goal_sbppbl  = fv_i("prior_goal_sbppbl"),
        review_q_sbppbl    = fv_i("review_q_sbppbl"),
        review_q2_sbppbl   = fv_i("review_q2_sbppbl"),
        prior_goal_profics = fv_i("prior_goal_profics"),
        review_q_profics   = fv_i("review_q_profics"),
        review_q2_profics  = fv_i("review_q2_profics")
      )
      res <- .rc_save(resident_id(), "ilp", as.integer(p), f)
      ss$ilp_review <- res
      if (isTRUE(res$success)) { .merge_ilp(p, f); .advance_after("ilp_review") }
    })

    output$save_ilp_review_status <- renderUI({ .save_status_ui(ss$ilp_review) })

    # ── Graduation info cascading UI (period 6) ────────────────────────────────
    output$grad_info_ui <- renderUI({
      req(local$sel_period == "6")
      res <- resident_r()
      sr  <- sel_seva()

      # Yes/No toggle — uses onclick → Shiny.setInputValue so clicks actually
      # register (Bootstrap btn-check alone doesn't feed Shiny inputs reliably
      # across rerenders in a namespaced module).
      .yn_g <- function(id, label, val) {
        push <- function(v) paste0("Shiny.setInputValue('", ns(id), "','", v,
                                   "',{priority:'event'})")
        div(class="mb-2 d-flex align-items-center gap-3",
          tags$label(label, style="font-size:0.83rem; color:#2c3e50; min-width:280px; margin:0;"),
          div(class="btn-group btn-group-sm",
            tags$input(type="radio", class="btn-check", name=ns(id),
                       id=paste0(ns(id),"_1"), value="1",
                       checked=if(identical(val,"1")) NA else NULL),
            tags$label(class=paste0("btn btn-outline-success",
                                    if(identical(val,"1"))" active" else ""),
                       `for`=paste0(ns(id),"_1"),
                       onclick=push("1"), "Yes"),
            tags$input(type="radio", class="btn-check", name=ns(id),
                       id=paste0(ns(id),"_0"), value="0",
                       checked=if(identical(val,"0")) NA else NULL),
            tags$label(class=paste0("btn btn-outline-secondary",
                                    if(identical(val,"0"))" active" else ""),
                       `for`=paste0(ns(id),"_0"),
                       onclick=push("0"), "No")))
      }

      get_res <- function(fld) if (!is.null(res)&&nrow(res)>0) .fv(res, fld) else ""

      # Always-collected contact info — every graduating resident fills these.
      email_phone <- div(class="row g-2 mt-3",
        div(class="col-md-6",
          tags$label("Best future email", class="form-label",
                     style="font-size:0.83rem; font-weight:600;"),
          tags$input(type="email", id=ns("grad_email"), class="form-control form-control-sm",
                     placeholder="you@example.com", value=get_res("grad_email"))),
        div(class="col-md-6",
          tags$label("Best future phone", class="form-label",
                     style="font-size:0.83rem; font-weight:600;"),
          tags$input(type="tel", id=ns("grad_phone"), class="form-control form-control-sm",
                     placeholder="(314) 555-0100",
                     value=.format_phone(get_res("grad_phone")),
                     oninput=paste0(
                       "(function(el){",
                       "  var d=(el.value||'').replace(/\\D/g,'').slice(0,10);",
                       "  var out=d;",
                       "  if (d.length>6) out='('+d.slice(0,3)+') '+d.slice(3,6)+'-'+d.slice(6);",
                       "  else if (d.length>3) out='('+d.slice(0,3)+') '+d.slice(3);",
                       "  else if (d.length>0) out='('+d;",
                       "  el.value=out;",
                       "  if (window.Shiny) Shiny.setInputValue('", ns("grad_phone"),
                       "', out, {priority:'event'});",
                       "})(this)"))))

      chief_val <- { v <- input$chief
        if (!is.null(v)&&nzchar(v)) v else get_res("chief") }

      chief_ui <- .yn_g("chief", "Are you staying on as Chief Resident?", chief_val)

      dd <- data_dict_r()
      grad_spec_ch <- .dd_choices(dd,"grad_spec")
      sv <- { v <- input$grad_spec
        if (!is.null(v)&&nzchar(v)) v else get_res("grad_spec") }

      grad_spec_label <- if (identical(chief_val,"1")) "Future plans" else "Specialty"
      grad_spec_ui <- div(class="mt-3",
        tags$label(grad_spec_label,
                   style="font-size:0.83rem; font-weight:600;"),
        if (!is.null(grad_spec_ch))
          tags$select(id=ns("grad_spec"), class="form-select form-select-sm mt-1",
                      style="max-width:300px;",
            tags$option(value="", selected=if(!nzchar(sv)) NA else NULL,
                        "-- select specialty --"),
            lapply(names(grad_spec_ch), function(v)
              tags$option(value=v, selected=if(identical(sv,v)) NA else NULL,
                          grad_spec_ch[[v]])))
        else tags$input(type="text", id=ns("grad_spec"),
                        class="form-control form-control-sm mt-1",
                        value=sv, placeholder="Specialty"))

      path_ui <- if (!nzchar(chief_val)) {
        NULL
      } else if (identical(chief_val,"1")) {
        # Chief: only ask grad_spec ("Future plans"); ssm/mo_prac/und_urban
        # auto-set to yes on save.
        grad_spec_ui
      } else {
        # Non-chief: grad_spec, practice details, then position.
        tagList(
          grad_spec_ui,
          div(class="mt-3",
            tags$p(style="font-size:0.78rem; font-weight:700; text-transform:uppercase;
                          letter-spacing:.06em; color:#6c757d; margin-bottom:8px;",
                   "Practice details"),
            .yn_g("res_alumni_academic","Academic medicine?",         get_res("res_alumni_academic")),
            .yn_g("ssm",                "Staying within SSM Health?", get_res("ssm")),
            .yn_g("mo_prac",            "Practicing in Missouri?",    get_res("mo_prac")),
            .yn_g("rural",              "Rural practice setting?",    get_res("rural")),
            .yn_g("und_urban",          "Underserved / urban setting?", get_res("und_urban"))),
          div(class="mt-3",
            tags$label("Current / planned position title",
                       style="font-size:0.83rem; font-weight:600;"),
            tags$input(type="text", id=ns("res_alumni_position"),
                       class="form-control form-control-sm mt-1",
                       placeholder="e.g., Hospitalist at SSM Health",
                       value=get_res("res_alumni_position"))))
      }

      tagList(email_phone, chief_ui, path_ui)
    })

    # ── ILP milestone tables (period 1-5) ────────────────────────────────────────
    # Clicking any level cell sets BOTH the row AND the level in one action.
    # No separate row radio or level dropdown needed — the table IS the selector.
    .ilp_table_ui <- function(domain, goal_input_name, row_field, level_field) {
      req(local$sel_period %in% as.character(1:5))
      ir  <- sel_ilp()
      dd  <- data_dict_r()
      val <- input[[goal_input_name]] %||% .fv(ir, goal_input_name)
      if (is.null(val) || !nzchar(val)) return(NULL)

      # Reactive deps so table re-renders on click
      live_row   <- input[[row_field]]
      live_level <- input[[level_field]]

      comp <- .get_comp_code(domain, val)
      tbl  <- .get_milestone_table(comp, dd)
      if (is.null(tbl) || nrow(tbl) == 0) return(NULL)

      # Prefer live input over saved DB value, but only fall back to the saved
      # row/level when the dropdown still points at the same subcompetency
      # that was saved — picking a different goal invalidates the prior anchor.
      saved_goal   <- .fv(ir, goal_input_name)
      goal_changed <- nzchar(saved_goal) && !identical(as.character(val),
                                                       as.character(saved_goal))
      cur_row   <- if (!is.null(live_row)   && nzchar(live_row))   live_row
                   else if (goal_changed) ""
                   else .fv(ir, row_field)
      cur_level <- if (!is.null(live_level) && nzchar(live_level)) live_level
                   else if (goal_changed) ""
                   else .fv(ir, level_field)

      lv_styles <- c(
        "1" = "background:#e8f5e9; color:#1b5e20;",
        "2" = "background:#e3f2fd; color:#0d47a1;",
        "3" = "background:#fff3e0; color:#e65100;",
        "4" = "background:#fce4ec; color:#880e4f;",
        "5" = "background:#ede7f6; color:#4a148c;")
      lv_labels <- c("1"="Novice","2"="Adv Beginner","3"="Competent",
                     "4"="Proficient","5"="Expert")

      # JS helper — clicking a cell sets row + level in one shot
      cell_js <- function(rn, l)
        paste0("Shiny.setInputValue('", ns(row_field),   "','", rn, "',{priority:'event'});",
               "Shiny.setInputValue('", ns(level_field), "','", l,  "',{priority:'event'});")

      tagList(
        div(class="mt-2 mb-1",
          tags$span(style="font-size:0.7rem; font-weight:700; text-transform:uppercase;
                           letter-spacing:.06em; color:#6c757d;",
                    comp, " \u2014 click a cell to select a row and target level")),
        div(class="table-responsive",
          tags$table(class="table table-bordered mb-2",
                     style="font-size:0.72rem; min-width:600px;",
            tags$thead(
              tags$tr(
                tags$th(style="width:28px; text-align:center; background:#f8fafc; color:#6c757d;", "#"),
                lapply(as.character(1:5), function(l)
                  tags$th(style=paste0(lv_styles[[l]],
                                       " text-align:center; font-size:0.7rem; cursor:default;"),
                          lv_labels[[l]])))),
            tags$tbody(
              lapply(seq_len(nrow(tbl)), function(i) {
                rn      <- as.character(tbl$Row[i])
                row_sel <- isTRUE(cur_row == rn)
                tags$tr(
                  tags$td(style=paste0(
                    "text-align:center; vertical-align:middle; font-weight:700;",
                    " font-size:0.75rem; padding:3px 4px;",
                    if (row_sel) " background:#e8eaf6; color:#3949ab;" else " color:#9e9e9e;"),
                    rn),
                  lapply(as.character(1:5), function(l) {
                    txt       <- tbl[[paste0("Level_", l)]][i]
                    cell_sel  <- row_sel && isTRUE(cur_level == l)
                    base_bg   <- if (row_sel && !cell_sel) "background:#f3f4f6;" else ""
                    high_bg   <- if (cell_sel) lv_styles[[l]] else base_bg
                    tags$td(
                      style = paste0(high_bg,
                                     " vertical-align:top; padding:5px 6px;",
                                     " line-height:1.35; cursor:pointer;",
                                     " transition:background .1s;",
                                     if (cell_sel) " font-weight:600; border:2px solid #666 !important;"
                                     else          " color:#444;"),
                      onclick = cell_js(rn, l),
                      title   = paste0("Select row ", rn, ", Level ", l, ": ", lv_labels[[l]]),
                      txt)
                  }))
              })))),
        # Current selection summary
        if (nzchar(cur_row) && nzchar(cur_level))
          div(class="d-flex align-items-center gap-2 mb-2",
            tags$i(class="bi bi-check-circle-fill", style="color:#198754; font-size:0.9rem;"),
            tags$span(style="font-size:0.78rem; color:#198754; font-weight:600;",
              paste0("Selected: Row ", cur_row, ", Level ", cur_level,
                     " (", lv_labels[[cur_level]], ")")))
        else
          div(class="text-muted mb-2",
            style="font-size:0.75rem; font-style:italic;",
            tags$i(class="bi bi-hand-index me-1"),
            "Click any cell above to select a target row and level")
      )
    }

    output$ilp_level_pcmk <- renderUI({
      .ilp_table_ui("pcmk",    "goal_pcmk",           "goal_level_r_pcmk", "goal_level_pcmk")
    })
    output$ilp_level_sbppbl <- renderUI({
      .ilp_table_ui("sbppbl",  "goal_sbppbl",          "goal_r_sbppbl",     "goal_level_sbppbl")
    })
    output$ilp_level_profics <- renderUI({
      .ilp_table_ui("profics", "goal_subcomp_profics", "goal_r_profics",    "goal_level_profics")
    })

    # Live preview of the three goals about to be submitted — gives the user a
    # clear summary before they click Save. Reads directly from current inputs
    # so unsaved changes are visible.
    output$ilp_pending_preview <- renderUI({
      req(local$sel_period %in% as.character(1:5))
      dd <- data_dict_r()
      ir <- sel_ilp()
      lv_labels <- c("1"="Novice","2"="Adv Beginner","3"="Competent",
                     "4"="Proficient","5"="Expert")
      # Live-or-saved: use the input value if set, otherwise fall back to the
      # saved REDCap value. Mirrors .ilp_table_ui's fallback so the preview
      # reflects what's currently displayed in the table (saved selections
      # remain visible until the user actively replaces them).
      ls <- function(input_name, fld) {
        v <- input[[input_name]]
        if (!is.null(v) && nzchar(trimws(as.character(v)))) as.character(v)
        else .fv(ir, fld)
      }
      domains <- list(
        list(key="pcmk",    title="Patient Care / Med Knowledge",
             color="#0d6efd", icon="heart-pulse-fill",
             dd_fld="goal_pcmk",
             goal=ls("goal_pcmk", "goal_pcmk"),
             level=ls("goal_level_pcmk", "goal_level_pcmk"),
             row=ls("goal_level_r_pcmk", "goal_level_r_pcmk"),
             how=ls("how_pcmk", "how_pcmk")),
        list(key="sbppbl",  title="Systems / Practice-Based Learning",
             color="#198754", icon="diagram-3-fill",
             dd_fld="goal_sbppbl",
             goal=ls("goal_sbppbl", "goal_sbppbl"),
             level=ls("goal_level_sbppbl", "goal_level_sbppbl"),
             row=ls("goal_r_sbppbl", "goal_r_sbppbl"),
             how=ls("how_sbppbl", "how_sbppbl")),
        list(key="profics", title="Professionalism / Interpersonal",
             color="#6f42c1", icon="people-fill",
             dd_fld="goal_subcomp_profics",
             goal=ls("goal_subcomp_profics", "goal_subcomp_profics"),
             level=ls("goal_level_profics", "goal_level_profics"),
             row=ls("goal_r_profics", "goal_r_profics"),
             how=ls("how_profics", "how_profics")))

      cards <- lapply(domains, function(d) {
        ch     <- .dd_select(dd, d$dd_fld, .SUBCOMP_CHOICES[[d$key]])
        sc     <- .SUBCOMP_CHOICES[[d$key]]
        goal_v <- d$goal %||% ""
        complete <- nzchar(trimws(goal_v))
        if (complete) {
          # Prefer .SUBCOMP_CHOICES descriptive label over bare DD label.
          goal_lbl <- if (!is.null(sc)) {
                        rn <- names(sc)[sc == goal_v]
                        if (length(rn)) rn[1]
                        else if (!is.null(ch)) {
                          nm <- names(ch)[ch == goal_v]
                          if (length(nm)) nm[1] else goal_v
                        } else goal_v
                      } else if (!is.null(ch)) {
                        nm <- names(ch)[ch == goal_v]
                        if (length(nm)) nm[1] else goal_v
                      } else goal_v
          lv_v   <- d$level %||% ""
          lv_txt <- if (nzchar(lv_v) && lv_v %in% names(lv_labels))
                      paste0("Level ", lv_v, " — ", lv_labels[[lv_v]]) else "Level not set"
          row_v  <- d$row %||% ""
          row_txt <- if (nzchar(row_v)) paste0("Row ", row_v) else "Row not set"
          # Pull milestone anchor text for the selected row/level when both
          # are set, so the user sees exactly which behavior they're targeting.
          anchor_text <- if (nzchar(row_v) && nzchar(lv_v))
            tryCatch(.get_goal_text(d$key, goal_v, row_v, lv_v, dd),
                     error = function(e) NULL) else NULL
          how_v  <- trimws(d$how %||% "")
          status_icon <- if (nzchar(lv_v) && nzchar(row_v) && nzchar(how_v))
                           tags$i(class="bi bi-check-circle-fill me-1",
                                  style=paste0("color:", d$color, ";"))
                         else
                           tags$i(class="bi bi-exclamation-circle-fill me-1",
                                  style="color:#f59e0b;")
          div(class="col-md-4",
            div(style=paste0("border-left:3px solid ", d$color,
                             "; padding:8px 10px; background:#fff; border-radius:4px;",
                             " height:100%;"),
              tags$p(style=paste0("font-size:0.7rem; font-weight:700; color:", d$color,
                                  "; text-transform:uppercase; letter-spacing:.05em; margin:0 0 4px;"),
                     status_icon, d$title),
              tags$p(style="font-size:0.82rem; font-weight:600; margin:0 0 2px; color:#212529;",
                     goal_lbl),
              tags$p(style="font-size:0.74rem; color:#495057; margin:0;",
                     paste0(row_txt, " · ", lv_txt)),
              if (!is.null(anchor_text) && nzchar(anchor_text))
                tags$p(style="font-size:0.74rem; color:#2c3e50; margin:4px 0 0;
                              line-height:1.35; font-style:italic;",
                       tags$i(class="bi bi-quote me-1", style="opacity:0.4;"),
                       anchor_text),
              if (nzchar(how_v))
                tags$p(style="font-size:0.74rem; color:#6c757d; margin:4px 0 0; font-style:italic;
                              white-space:pre-wrap;", how_v)
              else
                tags$p(style="font-size:0.72rem; color:#dc3545; margin:4px 0 0;",
                       tags$i(class="bi bi-exclamation-circle me-1"),
                       "How / Plan still empty")))
        } else {
          div(class="col-md-4",
            div(style=paste0("border-left:3px dashed #adb5bd; padding:8px 10px;",
                             " background:#f8f9fa; border-radius:4px; height:100%;"),
              tags$p(style="font-size:0.7rem; font-weight:700; color:#6c757d;
                            text-transform:uppercase; letter-spacing:.05em; margin:0 0 4px;",
                     tags$i(class="bi bi-circle me-1"), d$title),
              tags$p(style="font-size:0.78rem; color:#adb5bd; margin:0; font-style:italic;",
                     "No goal selected yet")))
        }
      })

      div(class="mt-4 mb-3 p-3",
          style="background:#f1f3f5; border:1px solid #dee2e6; border-radius:6px;",
        tags$p(style="font-size:0.75rem; font-weight:700; text-transform:uppercase;
                      letter-spacing:.05em; color:#495057; margin:0 0 8px;",
               tags$i(class="bi bi-clipboard-check me-1"),
               "Goals to be submitted"),
        div(class="row g-2", cards))
    })

    observeEvent(input$save_goals, {
      req(local$sel_period=="7", period_mode_r() %in% c("active","unknown"))
      f <- list(s_e_period="7", s_e_ume_goal1=input$s_e_ume_goal1%||%"",
                s_e_ume_goal2=input$s_e_ume_goal2%||%"",
                s_e_ume_goal3=input$s_e_ume_goal3%||%"")
      message("[goals input] rid=", resident_id(),
              " g1_nchar=", nchar(f$s_e_ume_goal1),
              " g2_nchar=", nchar(f$s_e_ume_goal2),
              " g3_nchar=", nchar(f$s_e_ume_goal3),
              " g1_preview='", substr(f$s_e_ume_goal1, 1, 40), "'")
      res <- .rc_save(resident_id(),"s_eval",7,f); ss$goals <- res
      if (res$success) { .merge_seva(7,f); .advance_after("goals") }
    })
    observeEvent(input$save_prep, {
      req(local$sel_period=="7", period_mode_r() %in% c("active","unknown"))
      f <- c(list(s_e_period="7"), setNames(
        lapply(names(.PREP_LABELS), function(n) input[[paste0("s_e_prep_",n)]]%||%""),
        paste0("s_e_prep_",names(.PREP_LABELS))))
      res <- .rc_save(resident_id(),"s_eval",7,f); ss$prep <- res
      if (res$success) { .merge_seva(7,f); .advance_after("prep") }
    })
    observeEvent(input$save_topics, {
      p <- local$sel_period; req(p, period_mode_r() %in% c("active","unknown"))
      dd <- data_dict_r()
      tc <- .dd_choices(dd,"s_e_topic_sel"); sc <- .dd_choices(dd,"s_e_learn_style")
      f <- c(list(s_e_period=p,
                  s_e_topic_oth=input$s_e_topic_oth%||%"",
                  s_e_learn_oth=input$s_e_learn_oth%||%""),
             if(!is.null(tc)) .checkbox_fields("s_e_topic_sel",tc,input$s_e_topic_sel%||%character(0)),
             if(!is.null(sc)) .checkbox_fields("s_e_learn_style",sc,input$s_e_learn_style%||%character(0)))
      res <- .rc_save(resident_id(),"s_eval",as.integer(p),f); ss$topics <- res
      if (res$success) { .merge_seva(p,f); .advance_after("topics") }
    })
    observeEvent(input$save_concerns, {
      req(local$sel_period=="7", period_mode_r() %in% c("active","unknown"))
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
      if (isTRUE(res$success)) { .merge_res(f); .advance_after("background") }
    })
    # Boards section → s_eval (periods 1-5) + optionally resident_data for score
    observeEvent(input$save_boards, {
      p <- local$sel_period; req(p %in% as.character(1:5), period_mode_r() %in% c("active","unknown"))
      # If a Step 3 score is already on file in resident_data, the yes/no
      # question is bypassed — auto-mark s_e_step3 as "1" so this section
      # counts as complete.
      res_now <- tryCatch(resident_r(), error = function(e) NULL)
      score_on_file <- (!is.null(res_now) && nrow(res_now) > 0) &&
                       (nzchar(.fv(res_now, "usmle_step3_score")) ||
                        nzchar(.fv(res_now, "comlex_step3_score")))
      step3_val <- if (score_on_file) "1" else (input$s_e_step3 %||% "")
      f <- list(s_e_period=p,
                s_e_step3         = step3_val,
                s_e_step3_date_set= input$s_e_step3_date_set  %||% "",
                s_e_step3_date    = input$s_e_step3_date       %||% "",
                s_e_board_discu   = input$s_e_board_discu      %||% "",
                s_e_mksap_comp    = input$s_e_mksap_comp       %||% "")
      res <- .rc_save(resident_id(),"s_eval",as.integer(p),f); ss$boards <- res
      if (res$success) {
        .merge_seva(p,f)
        # If step 3 is taken, save score to resident_data
        if (isTRUE(input$s_e_step3 == "1")) {
          exam_type <- input$step3_exam_type %||% ""
          score_val <- input$step3_score_val  %||% ""
          rd <- list(step3 = "1")
          if (exam_type == "usmle" && nzchar(score_val))
            rd$usmle_step3_score <- score_val
          else if (exam_type == "comlex" && nzchar(score_val))
            rd$comlex_step3_score <- score_val
          .rc_save_resident(resident_id(), rd)
          .merge_res(rd)
        }
        .advance_after("boards")   # boards done → ILP review unlocks next
      }
    })
    observeEvent(input$save_reflection, {
      p <- local$sel_period; req(p %in% as.character(1:6), period_mode_r() %in% c("active","unknown"))
      f <- list(s_e_period=p, s_e_plus=input$s_e_plus%||%"", s_e_delta=input$s_e_delta%||%"",
                s_e_well=input$s_e_well%||%"",
                s_e_discussion = if (isTRUE(input$discuss_with_mentor))
                                   input$s_e_discussion %||% "" else "",
                s_e_prog_assist = if (isTRUE(input$prog_assist_toggle))
                                    input$s_e_prog_assist %||% "" else "")
      res <- .rc_save(resident_id(),"s_eval",as.integer(p),f); ss$reflection <- res
      if (res$success) { .merge_seva(p,f); .advance_after("reflection") }
    })
    observeEvent(input$save_career, {
      p <- local$sel_period; req(p %in% as.character(c(1:5, 7)), period_mode_r() %in% c("active","unknown"))
      dd <- data_dict_r()
      cp <- .dd_choices(dd,"s_e_career_path"); fl <- .dd_choices(dd,"s_e_fellow")
      tr <- .dd_choices(dd,"s_e_track_type")
      f <- c(list(s_e_period=p),
             if(!is.null(cp)) .checkbox_fields("s_e_career_path",cp,input$s_e_career_path%||%character(0)),
             if(!is.null(fl)) .checkbox_fields("s_e_fellow",fl,input$s_e_fellow%||%character(0)),
             if(!is.null(tr)) .checkbox_fields("s_e_track_type",tr,input$s_e_track_type%||%character(0)))
      res <- .rc_save(resident_id(),"s_eval",as.integer(p),f); ss$career <- res
      if (res$success) { .merge_seva(p,f); .advance_after("career") }
    })
    observeEvent(input$save_feedback, {
      p <- local$sel_period; req(p %in% as.character(1:6), period_mode_r() %in% c("active","unknown"))
      f <- list(s_e_period       = p,
                s_e_prog_plus    = input$s_e_prog_plus    %||% "",
                s_e_prog_delta   = input$s_e_prog_delta   %||% "",
                s_e_progconf     = input$s_e_progconf     %||% "",
                s_e_progfeed     = input$s_e_progfeed     %||% "")
      res <- .rc_save(resident_id(),"s_eval",as.integer(p),f); ss$feedback <- res
      if (res$success) { .merge_seva(p,f); .advance_after("feedback") }
    })
    # Grey out the ILP save button until all three domain goals are selected.
    observe({
      all_set <- nzchar(trimws(input$goal_pcmk            %||% "")) &&
                 nzchar(trimws(input$goal_sbppbl          %||% "")) &&
                 nzchar(trimws(input$goal_subcomp_profics %||% ""))
      shinyjs::toggleState("save_ilp", condition = all_set)
    })

    # NOTE: changing a goal dropdown does NOT clear the row/level/how inputs.
    # Those values stay as pending submission state until the user actively
    # replaces them by clicking a new anchor cell or editing the How/Plan
    # textarea. The table-render logic in .ilp_table_ui still suppresses the
    # highlighted-cell visual when the dropdown points at a different
    # subcompetency than was last saved, so the user isn't misled into
    # thinking a stale row applies to the new milestone.

    observeEvent(input$save_ilp, {
      p <- local$sel_period; req(p %in% as.character(1:5), period_mode_r() %in% c("active","unknown"))
      # Require all three domain goals before saving — partial saves used to
      # advance the section and collapse it, which made in-progress entries
      # look lost.
      goal_vals <- list(pcmk    = input$goal_pcmk            %||% "",
                        sbppbl  = input$goal_sbppbl          %||% "",
                        profics = input$goal_subcomp_profics %||% "")
      missing <- names(goal_vals)[!nzchar(trimws(unlist(goal_vals)))]
      if (length(missing) > 0) {
        showNotification(
          paste0("Please select a goal for all three domains before saving",
                 " (still missing: ",
                 paste(c(pcmk="Patient Care / Med Knowledge",
                         sbppbl="Systems / Practice-Based Learning",
                         profics="Professionalism / Interpersonal")[missing],
                       collapse = ", "),
                 ")."),
          type = "warning", duration = 6)
        return()
      }
      f <- list(year_resident        = p,
                goal_pcmk            = input$goal_pcmk            %||% "",
                goal_level_pcmk      = input$goal_level_pcmk      %||% "",
                goal_level_r_pcmk    = input$goal_level_r_pcmk    %||% "",
                how_pcmk             = input$how_pcmk             %||% "",
                goal_sbppbl          = input$goal_sbppbl          %||% "",
                goal_level_sbppbl    = input$goal_level_sbppbl    %||% "",
                goal_r_sbppbl        = input$goal_r_sbppbl        %||% "",
                how_sbppbl           = input$how_sbppbl           %||% "",
                goal_subcomp_profics = input$goal_subcomp_profics %||% "",
                goal_level_profics   = input$goal_level_profics   %||% "",
                goal_r_profics       = input$goal_r_profics       %||% "",
                how_profics          = input$how_profics          %||% "")
      # Mirror each goal's row+level into the canonical milestone-anchor
      # field on the ilp form (e.g. PC4 row 1 level 4 → pc4_r1 = "4"). These
      # dropdown fields are how the ACGME-style milestone level is recorded
      # per-row in REDCap; without them the resident's targeted level isn't
      # visible on the milestone field itself.
      .anchor_field <- function(domain, code, row) {
        comp <- .get_comp_code(domain, code)
        if (is.null(comp) || !nzchar(row)) return(NULL)
        dom <- gsub("\\d+$", "", comp)
        num <- gsub("^[A-Z]+", "", comp)
        pfx <- if (dom == "PBLI") "pbl" else tolower(dom)
        paste0(pfx, num, "_r", row)
      }
      anchors <- list(
        list(domain="pcmk",    code=input$goal_pcmk,            row=input$goal_level_r_pcmk, level=input$goal_level_pcmk),
        list(domain="sbppbl",  code=input$goal_sbppbl,          row=input$goal_r_sbppbl,     level=input$goal_level_sbppbl),
        list(domain="profics", code=input$goal_subcomp_profics, row=input$goal_r_profics,    level=input$goal_level_profics))
      for (a in anchors) {
        if (is.null(a$code) || !nzchar(a$code)) next
        if (is.null(a$row)  || !nzchar(a$row))  next
        if (is.null(a$level) || !nzchar(a$level)) next
        fld <- .anchor_field(a$domain, a$code, a$row)
        if (!is.null(fld)) f[[fld]] <- as.character(a$level)
      }
      res <- .rc_save(resident_id(),"ilp",as.integer(p),f); ss$ilp <- res
      if (res$success) { .merge_ilp(p,f); .advance_after("ilp") }
    })
    # save_board kept for legacy compatibility but period 6 no longer has a board section
    # Alumni / graduation data → resident_data (non-repeating)
    observeEvent(input$save_alumni, {
      req(local$sel_period == "6")
      chief_val <- input$chief %||% ""

      rd <- list(chief      = chief_val,
                 grad_email = input$grad_email %||% "",
                 grad_phone = input$grad_phone %||% "",
                 grad_spec  = input$grad_spec  %||% "")

      if (identical(chief_val, "1")) {
        # Chief: auto-set ssm / mo_prac / und_urban to yes.
        rd$ssm       <- "1"
        rd$mo_prac   <- "1"
        rd$und_urban <- "1"
      } else {
        rd$res_alumni_academic <- input$res_alumni_academic %||% ""
        rd$ssm                 <- input$ssm                 %||% ""
        rd$mo_prac             <- input$mo_prac             %||% ""
        rd$rural               <- input$rural               %||% ""
        rd$und_urban           <- input$und_urban           %||% ""
        rd$res_alumni_position <- input$res_alumni_position %||% ""
      }

      res <- .rc_save_resident(resident_id(), rd); ss$alumni <- res
      if (isTRUE(res$success)) { .merge_res(rd); .advance_after("alumni") }
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

      # Count how many periods are fully complete (for any badge use)
      n_complete <- sum(sapply(periods, function(p) {
        ps <- .period_status(local$seva, local$ilp, local$ms, p, res_row)
        ps$status == "complete"
      }))
      # Aggregate completion across periods, weighted by sections — so
      # partial progress on one period still moves the overall %.
      agg <- lapply(periods, function(p) .period_status(local$seva, local$ilp, local$ms, p, res_row))
      total_sections <- sum(sapply(agg, function(x) x$total %||% 0L))
      done_sections  <- sum(sapply(agg, function(x) x$done  %||% 0L))

      rows <- lapply(periods, function(p) {
        p_str  <- as.character(p)
        ps     <- .period_status(local$seva, local$ilp, local$ms, p, res_row)
        is_sel <- isTRUE(local$sel_period == p_str)
        is_act <- isTRUE(active_p == p_str)

        # Determine whether this period is past, active, or future
        active_chron  <- if (!is.null(active_p)) .PERIOD_CHRON[active_p]  else NA_integer_
        this_chron    <- .PERIOD_CHRON[p_str]
        is_future     <- !is.na(active_chron) && !is.na(this_chron) && this_chron > active_chron

        # Colors per status (future always muted)
        status_col <- if (is_future)
          list(border="#cfd8dc", bg="#fafafa", badge_bg="#eceff1", text="#90a4ae")
        else switch(ps$status,
          complete    = list(border="#2e7d32", bg="#f0faf4", badge_bg="#c8e6c9", text="#1a6b3a"),
          in_progress = list(border="#bf360c", bg="#fff8f5", badge_bg="#ffe0b2", text="#bf360c"),
                        list(border="#9e9e9e", bg="#fafafa",  badge_bg="#eeeeee", text="#757575"))

        icon_el <- if (is_future)
          tags$i(class="bi bi-lock", style="color:#b0bec5; font-size:1rem;")
        else switch(ps$status,
          complete    = tags$i(class="bi bi-check-circle-fill",
                               style=paste0("color:",status_col$border,"; font-size:1rem;")),
          in_progress = tags$i(class="bi bi-circle-half",
                               style=paste0("color:",status_col$border,"; font-size:1rem;")),
          tags$i(class="bi bi-circle", style="color:#bdbdbd; font-size:1rem;"))

        badge_txt <- if (is_future) "" else if (ps$total>0) paste0(ps$done,"/",ps$total) else ""

        row_bg      <- if (is_sel) "#e3eef8" else status_col$bg
        row_border  <- if (is_sel) "#0066a1" else status_col$border
        row_outline <- if (is_sel) "2px solid #0066a1" else "1px solid #e8e8e8"
        row_cursor  <- if (is_future) "not-allowed" else "pointer"

        div(style=paste0("cursor:", row_cursor, "; display:flex; align-items:center; gap:10px;",
                         " padding:9px 14px; border-radius:7px; margin-bottom:4px;",
                         " background:", row_bg, ";",
                         " outline:", row_outline, ";",
                         " border-left: 4px solid ", row_border, ";",
                         if (is_future) " opacity:0.55;" else ""),
            # Only fire selection for non-future periods
            onclick = if (!is_future)
              sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})", ns("select_period"), p_str)
            else NULL,
          div(style="width:20px; text-align:center; flex-shrink:0;", icon_el),
          div(style="flex:1; min-width:0;",
            tags$span(style=paste0("font-size:0.87rem; font-weight:",
                                   if(is_sel||is_act)"700" else "500",
                                   "; color:", if(is_sel)"#003d5c" else if(is_future)"#90a4ae" else "#2c3e50"),
              period_names[[p_str]]),
            if (is_act) tags$span(style="font-size:0.68rem; background:#0066a1; color:#fff;
                                        border-radius:10px; padding:1px 7px; margin-left:6px;",
                                  "Current"),
            if (is_future) tags$span(style="font-size:0.68rem; background:#eceff1; color:#78909c;
                                           border-radius:10px; padding:1px 7px; margin-left:6px;",
                                     "Not yet"),
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

      # Overall progress summary bar — section-weighted (not just fully-done
      # periods), so partial progress on the active period shows up.
      pct <- if (total_sections > 0) round(100 * done_sections / total_sections) else 0L
      prog_col <- if (pct==100) "#2e7d32" else if (pct>0) "#bf360c" else "#9e9e9e"

      div(
        info_note(
          summary = "About Self-Evaluations",
          tags$p("The Self-Evaluation is a structured reflection completed at up to seven points during residency: ",
                 tags$strong("Entering Residency"), ", ",
                 tags$strong("Mid-Intern"), ", ",
                 tags$strong("End Intern"), ", ",
                 tags$strong("Mid-PGY2"), ", ",
                 tags$strong("End PGY2"), ", ",
                 tags$strong("Mid-PGY3"), ", and ",
                 tags$strong("Graduating"), "."),
          tags$p("Each evaluation captures reflection on your growth, learning goals, career direction, and board preparation. ",
                 "Completed evaluations are reviewed with your coach and inform your Individual Learning Plan (ILP)."),
          tags$p("Click any period below to open that evaluation. Green = fully submitted, orange = started but incomplete, gray = not yet started.")
        ),

        # ── Completion overview panel ──────────────────────────────────────────
        div(style=paste0("background:#1a2e42; border-radius:10px 10px 0 0;",
                         " padding:14px 18px 10px;"),
          div(
            tags$p(style="margin:0; font-size:0.72rem; font-weight:700; letter-spacing:.1em;
                         color:#7fb3d3; text-transform:uppercase;",
                   "Self-Evaluation"),
            tags$p(style="margin:0; font-size:1.05rem; font-weight:700; color:#ffffff;",
                   "Completion Overview"))),

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
      mode <- period_mode_r()
      pname <- .PERIOD_NAMES[[p]] %||% paste0("Period ", p)

      # Hard block when faculty-eval count is critically low on an active
      # period. Past periods remain viewable (read-only) so residents can
      # still review prior entries; future periods already show their own
      # lock panel. Hidden behind isolate() so data merges don't retrigger.
      fstat <- tryCatch(isolate(fac_eval_status_r()), error = function(e) NULL)
      # Intern-intro residents (period 7) are exempt from the faculty-eval
      # hard-block — they haven't had a chance to complete any yet.
      if (!is.null(fstat) && isTRUE(fstat$blocked) && mode == "active" &&
          as.character(p) != "7") {
        return(div(class = "text-center py-4 px-4",
          style = paste0("background:#fff5f5; border:1px solid #ffcdd2;",
                         " border-left:5px solid #c62828; border-radius:10px;",
                         " margin-top:8px;"),
          tags$i(class = "bi bi-lock-fill",
                 style = "font-size:2rem; color:#c62828; display:block; margin-bottom:6px;"),
          tags$p(style = "font-weight:700; color:#b71c1c; font-size:1.05rem; margin:0 0 4px;",
                 "Self-evaluation locked"),
          tags$p(style = "color:#6d4c41; font-size:0.95rem; max-width:520px; margin:0 auto;",
                 "See the message above. Please complete at least ",
                 tags$strong(paste0(ceiling(fstat$goal * 0.10), " faculty evaluations")),
                 " this academic year before starting your self-evaluation.")))
      }

      # ── Future period: blocked ──────────────────────────────────────────────
      if (mode == "future") {
        return(div(
          class = "text-center py-5 px-4",
          style = paste0("background:#fafafa; border:1px solid #e0e0e0;",
                         " border-radius:10px; margin-top:8px;"),
          tags$i(class = "bi bi-lock-fill",
                 style = "font-size:2.2rem; color:#b0bec5; display:block; margin-bottom:10px;"),
          tags$p(style = "font-weight:700; color:#546e7a; font-size:1rem; margin:0 0 4px;",
                 paste0(pname, " \u2014 Not Yet Available")),
          tags$p(style = "color:#9e9e9e; font-size:0.83rem; max-width:420px; margin:0 auto;",
                 "This evaluation period hasn\u2019t opened yet. ",
                 "It will become available when you reach that checkpoint in your training.")
        ))
      }

      # Isolate data reads so that in-session merges (.merge_seva / .merge_res /
      # .merge_ms) don't re-invalidate the whole form — otherwise every save
      # causes a full re-render of the period_form, which visibly flickers
      # every accordion section closed/open. The renderUI still re-runs on
      # period change, mode change, progress$step advance, or local$ready —
      # which is what we actually want. Data is still fresh because isolate()
      # reads current values at each render.
      sr   <- isolate(sel_seva()); ir <- isolate(sel_ilp()); md <- isolate(sel_ms())
      dd   <- isolate(data_dict_r())
      psr  <- isolate(prev_seva_r())
      pir  <- isolate(prev_ilp_r())
      ev   <- isolate(evals_r())
      res  <- isolate(resident_r())
      ite  <- isolate(ite_data_r())
      pi   <- isolate(period_info_r())
      pgy  <- if (!is.null(pi)) as.integer(pi$pgy_year) else 1L
      secs <- .PERIOD_SECTIONS[[p]]
      chk  <- .section_checklist_card(secs, sr, ir, md, res)

      # Peer median for current period (from historical self-eval medians)
      peer_med <- tryCatch(isolate({
        hm <- rdm_data()$historical_medians$milestone_selfevaluation_c33c$medians
        if (!is.null(hm) && nrow(hm) > 0) {
          pm <- hm[as.character(hm$prog_mile_period) == p, , drop = FALSE]
          if (nrow(pm) > 0) pm[1, , drop = FALSE] else NULL
        } else NULL
      }), error = function(e) NULL)

      # In past mode, force every section to render as "past" (step past the
      # end of the sequence) so each section appears — collapsed but openable —
      # even if that section was never completed. This lets residents review
      # whatever partial data they entered at the time. Editing is prevented
      # below via <fieldset disabled>.
      active_step_for_render <- if (mode == "past") 999L else progress$step

      form_ui <- if (p=="7")      .form_p7(ns, p, sr, dd, psr, res, chk, md=md,
                                           active_step = active_step_for_render)
                 else if (p=="6") .form_p6(ns, p, sr, dd, psr, ev, res, chk,
                                           active_step = active_step_for_render)
                 else             .form_std(ns, p, sr, ir, dd, psr, ev, chk, ite, pgy,
                                            pir = pir, md = md, peer_med = peer_med,
                                            active_step = active_step_for_render)

      # Completion banner — shown when every section in the active period is
      # done. Reassures the resident that the work is in and they can revisit
      # later. Recomputes from the freshly-merged local state so it appears
      # immediately after the final save.
      done_ps <- isolate(.period_status(local$seva, local$ilp, local$ms, p, res))
      complete_banner <- if (mode != "past" &&
                             isTRUE(done_ps$status == "complete")) {
        div(class = "d-flex align-items-start gap-3 mb-3 py-3 px-3",
            style = paste0("background:#e8f5e9; border:1px solid #a5d6a7;",
                           " border-left:5px solid #2e7d32 !important;",
                           " border-radius:8px; font-size:0.9rem;"),
          tags$i(class = "bi bi-check-circle-fill",
                 style = "color:#2e7d32; font-size:1.4rem; flex-shrink:0; margin-top:2px;"),
          div(
            tags$div(style = "font-weight:700; color:#1b5e20; font-size:1rem; margin-bottom:2px;",
                     paste0(pname, " — All sections complete!")),
            tags$div(style = "color:#2e7d32; line-height:1.4;",
                     "Your self-evaluation has been submitted. ",
                     tags$strong("You can come back any time before this period closes "),
                     "to revise your answers — just open a section, edit, and click save again.")))
      } else NULL

      # ── Past period: read-only wrapper ──────────────────────────────────────
      # <fieldset disabled> disables every input / textarea / select / button
      # inside, but leaves non-form elements (the collapsible card headers)
      # clickable so users can expand each section to read what was saved.
      if (mode == "past") {
        tagList(
          div(class = "d-flex align-items-center gap-2 mb-3 py-2 px-3",
              style = paste0("background:#fff8e1; border:1px solid #ffe082;",
                             " border-left:4px solid #f9a825 !important;",
                             " border-radius:6px; font-size:0.83rem;"),
            tags$i(class = "bi bi-lock-fill", style = "color:#f57f17; font-size:1rem; flex-shrink:0;"),
            div(
              tags$span(style = "font-weight:700; color:#5d4037;",
                        paste0(pname, " \u2014 Read-Only")),
              tags$span(style = "color:#795548; margin-left:6px;",
                        "This evaluation is from a previous period. ",
                        "Expand any section to review what was saved \u2014 editing is disabled.")
            )
          ),
          tags$fieldset(disabled = NA,
            style = "border:0; padding:0; margin:0; min-width:0;",
            form_ui)
        )
      } else {
        tagList(complete_banner, form_ui)
      }
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
    selectInput(ns(field), label=NULL,
      choices=c("-- select --"="", choices),
      selected=if(matched) existing_val else "",
      selectize=FALSE, width="100%"),
    if (!matched && nzchar(existing_val))
      tags$p(class="text-muted", style="font-size:0.75rem; margin:0 0 8px;",
             tags$i(class="bi bi-info-circle me-1"),
             "Previously entered: ", tags$em(existing_val)))
}

# Read-only reference box showing a previous period's text value above a
# blank current-period entry field. Pass label (e.g. "Previous period")
# and the text value; renders nothing if value is empty.
.prev_ref_text <- function(value, label = "Previous period") {
  if (is.null(value) || !nzchar(trimws(as.character(value)))) return(NULL)
  div(class="mb-2 py-2 px-3",
      style="background:#f6f7fb; border-left:3px solid #adb5bd;
             border-radius:4px; font-size:0.82rem; color:#495057;",
    tags$span(style="font-size:0.7rem; text-transform:uppercase; letter-spacing:.06em;
                     font-weight:700; color:#6c757d; display:block; margin-bottom:4px;",
              tags$i(class="bi bi-clock-history me-1"), label),
    tags$div(style="white-space:pre-wrap;", as.character(value)))
}

# Read-only reference chips for previously-selected checkbox codes.
.prev_ref_chips <- function(codes, choices, label = "Previous period", bg = "#6c757d") {
  codes <- codes[!is.na(codes) & nzchar(codes)]
  if (length(codes) == 0) return(NULL)
  labels <- if (!is.null(choices)) unname(choices[codes]) else codes
  labels <- labels[!is.na(labels) & nzchar(labels)]
  if (length(labels) == 0) return(NULL)
  div(class="mb-2 py-2 px-3",
      style="background:#f6f7fb; border-left:3px solid #adb5bd;
             border-radius:4px; font-size:0.82rem;",
    tags$span(style="font-size:0.7rem; text-transform:uppercase; letter-spacing:.06em;
                     font-weight:700; color:#6c757d; display:block; margin-bottom:6px;",
              tags$i(class="bi bi-clock-history me-1"), label),
    div(class="d-flex flex-wrap gap-1",
        lapply(labels, function(l)
          tags$span(style=paste0("background:",bg,"; color:#fff; border-radius:20px;",
                                 " padding:2px 10px; font-size:0.75rem;"), l))))
}

# Locked placeholder for sections not yet unlocked in the accordion
.locked_section <- function(title)
  div(class="card border-0 mb-3",
      style="border-radius:8px; background:#f9f9f9; border:1px dashed #ced4da !important; opacity:0.65;",
    div(class="card-body py-2 px-3 d-flex align-items-center gap-2",
      tags$i(class="bi bi-lock-fill", style="color:#adb5bd;"),
      tags$span(style="font-size:0.83rem; color:#9e9e9e; font-style:italic;",
                paste0(title, " — complete the section above to unlock"))))

# ── ILP context panel (previous goal + low milestone scores) ──────────────────
# domain:  "pcmk" | "sbppbl" | "profics"
# pir:     previous period ILP row (data.frame, may be empty)
# md:      current period milestone self-eval row (data.frame, may be NULL/empty)
# dd:      data dictionary (used to derive human-readable subcompetency labels)
# ns:      module namespace function (used for setInputValue onclick)
# goal_input: current input ID for the goal select (for click-chip-to-set)
.ilp_context_panel <- function(domain, pir, md, dd = NULL, ns = NULL, goal_input = NULL,
                                peer_med = NULL) {
  dd_fld  <- switch(domain, pcmk = "goal_pcmk", sbppbl = "goal_sbppbl",
                             profics = "goal_subcomp_profics")
  choices <- .dd_select(dd, dd_fld, .SUBCOMP_CHOICES[[domain]])  # label=code
  goal_fld   <- dd_fld
  level_fld  <- switch(domain, pcmk="goal_level_pcmk", sbppbl="goal_level_sbppbl",
                                profics="goal_level_profics")
  panels <- list()

  # ── 1. Previous ILP goal ─────────────────────────────────────────────────────
  row_fld_map <- c(pcmk = "goal_level_r_pcmk", sbppbl = "goal_r_sbppbl",
                   profics = "goal_r_profics")
  if (!is.null(pir) && nrow(pir) > 0) {
    prev_code  <- .fv(pir, goal_fld)
    prev_level <- .fv(pir, level_fld)
    prev_row   <- .fv(pir, row_fld_map[[domain]])
    if (nzchar(prev_code)) {
      # Prefer the descriptive .SUBCOMP_CHOICES label (e.g.
      # "PC1 — History-taking / interviewing") over the bare data-dictionary
      # label ("Patient Care 1") — falls back to whichever lookup yields a
      # match.
      sc_choices <- .SUBCOMP_CHOICES[[domain]]
      rich_name <- if (!is.null(sc_choices)) {
        rn <- names(sc_choices)[sc_choices == prev_code]
        if (length(rn) > 0 && nzchar(rn[1])) rn[1] else NULL
      } else NULL
      bare_name <- names(choices)[choices == prev_code]
      prev_name <- if (!is.null(rich_name)) rich_name
                   else if (length(bare_name) > 0 && nzchar(bare_name[1])) bare_name[1]
                   else NULL
      if (!is.null(prev_name) && nzchar(prev_name)) {
        lv_txt    <- if (nzchar(prev_level)) paste0(" — Level ", prev_level) else ""
        row_pfx   <- if (nzchar(prev_row))   paste0("Row ", prev_row, ": ")  else ""
        goal_text <- .get_goal_text(domain, prev_code, prev_row, prev_level, dd)
        panels <- c(panels, list(
          div(class = "mb-2 p-2",
              style = "background:#eef4f8; border-radius:6px; border-left:3px solid #7fb3d3;",
            tags$p(style = "font-size:0.68rem; font-weight:700; color:#5b8ba5;
                            text-transform:uppercase; letter-spacing:.05em; margin:0 0 2px;",
                   tags$i(class = "bi bi-arrow-counterclockwise me-1"),
                   "Previous goal"),
            tags$p(style = "font-size:0.74rem; color:#5b8ba5; margin:0 0 2px; font-weight:600;",
                   paste0(prev_name, lv_txt)),
            if (!is.null(goal_text))
              tags$p(style = "font-size:0.76rem; color:#2c3e50; margin:0; line-height:1.35;
                              font-style:italic;",
                     tags$i(class = "bi bi-quote me-1", style = "opacity:0.4;"),
                     paste0(row_pfx, goal_text))
            else if (nzchar(prev_row) || nzchar(prev_level))
              tags$p(style = "font-size:0.72rem; color:#9ca3af; margin:0; font-style:italic;",
                     "(milestone anchor text not recorded)"))))
      }
    }
  }

  # ── 2. Low milestone self-ratings (clickable to set goal) ───────────────────
  ms_map <- .MS_SELF_FIELDS[[domain]]
  if (!is.null(md) && nrow(md) > 0 && length(ms_map) > 0) {
    # Build onclick JS: sets both the select value and fires Shiny input event
    click_js <- if (!is.null(ns) && !is.null(goal_input)) {
      function(code) paste0(
        "var s=document.getElementById('", ns(goal_input), "');",
        "if(s){s.value='", code, "';}",
        "Shiny.setInputValue('", ns(goal_input), "','", code, "',{priority:'event'});")
    } else function(code) NULL

    # First pass: collect (field, value, peer, gap) for every below-average item.
    # We show ONLY below-average milestones, and highlight the 1-2 with the
    # largest gap below peer median.
    gaps <- lapply(names(ms_map), function(fld) {
      if (!(fld %in% names(md))) return(NULL)
      v <- suppressWarnings(as.integer(md[[fld]][1]))
      peer_v <- if (!is.null(peer_med) && fld %in% names(peer_med))
                  suppressWarnings(as.numeric(peer_med[[fld]][1])) else NA_real_
      if (is.na(v) || is.na(peer_v) || v >= peer_v - 0.4) return(NULL)
      list(fld = fld, v = v, peer_v = peer_v, gap = peer_v - v)
    })
    gaps <- Filter(Negate(is.null), gaps)

    # Rank by gap; the top 2 (largest gaps) get a stronger highlight.
    gap_order <- order(sapply(gaps, `[[`, "gap"), decreasing = TRUE)
    top_n     <- min(2L, length(gap_order))
    top_idx   <- if (top_n > 0) gap_order[seq_len(top_n)] else integer(0)

    low_chips <- lapply(seq_along(gaps), function(i) {
      g    <- gaps[[i]]
      code <- ms_map[[g$fld]]
      lbl  <- names(choices)[choices == code]
      if (!length(lbl)) return(NULL)
      short     <- sub(" \u2014.*| .*", "", lbl[1])
      desc_full <- trimws(sub(paste0("^", short, ".*?\u2014\\s*"), "", lbl[1]))
      desc_trim <- if (nchar(desc_full) > 22) paste0(substr(desc_full, 1, 20), "\u2026") else desc_full
      is_top    <- i %in% top_idx
      bg   <- if (is_top) "#fce8e8" else "#f0e6ff"
      bord <- if (is_top) "#dc3545" else "#7c3aed"
      col  <- if (is_top) "#b02a37" else "#5b21b6"
      js   <- click_js(code)
      tags$span(
        style = paste0("display:inline-flex; align-items:center; gap:3px;",
                       " background:", bg, "; border:1px solid ", bord, ";",
                       " border-radius:4px; padding:2px 8px;",
                       " font-size:0.72rem; color:", col, "; margin:2px;",
                       if (is_top) " font-weight:600;" else "",
                       if (!is.null(js)) " cursor:pointer;" else ""),
        onclick = js,
        title = if (!is.null(js)) paste0("Click to select ", lbl[1], " as your goal") else NULL,
        tags$b(short),
        if (nzchar(desc_trim)) tags$span(
          style = "opacity:0.75; font-size:0.68rem;",
          paste0("\u00a0", desc_trim)),
        if (!is.null(js)) tags$i(class = "bi bi-cursor-fill",
                                  style = "font-size:0.6rem; opacity:0.6;"))
    })
    low_chips <- Filter(Negate(is.null), low_chips)
    if (length(low_chips) > 0) {
      panels <- c(panels, list(
        div(class = "mb-2",
          tags$p(style = "font-size:0.68rem; font-weight:700; color:#856404;
                          text-transform:uppercase; letter-spacing:.05em; margin:0 0 3px;",
                 tags$i(class = "bi bi-exclamation-triangle-fill me-1"),
                 if (!is.null(goal_input))
                   "Areas to develop — click to set as goal"
                 else
                   "Areas to develop — consider for your goal"),
          div(style = "display:flex; flex-wrap:wrap; gap:2px;", low_chips))))
    }
  }

  if (length(panels) > 0)
    div(class = "mb-3", style = "font-size:0.82rem;", tagList(panels))
  else NULL
}

# ── Period 7 form ─────────────────────────────────────────────────────────────

.form_p7 <- function(ns, p, sr, dd, psr, res, chk, md=NULL, active_step = 1L) {
  tc <- .dd_choices(dd,"s_e_topic_sel"); sc <- .dd_choices(dd,"s_e_learn_style")
  sel_t <- .checked_codes(sr,"s_e_topic_sel"); sel_s <- .checked_codes(sr,"s_e_learn_style")
  cp <- .dd_choices(dd,"s_e_career_path"); fl <- .dd_choices(dd,"s_e_fellow")
  tr <- .dd_choices(dd,"s_e_track_type")
  sel_cp <- .checked_codes(sr,"s_e_career_path")
  sel_fl <- .checked_codes(sr,"s_e_fellow")
  sel_tr <- .checked_codes(sr,"s_e_track_type")

  # Yes/No helper for background questions.
  # Click handler on the LABEL is required — Bootstrap btn-check hides the
  # native radio, so the label is the actual click target. Without it the
  # Shiny input value is never set (input$hs_mo stays "" even after clicking).
  yn_q <- function(id, label, val) {
    cur <- as.character(val %||% "")
    div(class="mb-2 d-flex align-items-center gap-3 flex-wrap",
      tags$label(label, style="font-size:0.84rem; color:#2c3e50; min-width:320px; margin:0;"),
      div(class="btn-group btn-group-sm",
        tags$input(type="radio", class="btn-check", name=ns(id),
                   id=paste0(ns(id),"_1"), value="1",
                   checked = if (cur=="1") NA else NULL),
        tags$label(class=paste0("btn btn-outline-success",
                                if (cur=="1") " active" else ""),
                   `for`=paste0(ns(id),"_1"),
                   onclick=paste0("Shiny.setInputValue('",ns(id),"','1',{priority:'event'})"),
                   "Yes"),
        tags$input(type="radio", class="btn-check", name=ns(id),
                   id=paste0(ns(id),"_0"), value="0",
                   checked = if (cur=="0") NA else NULL),
        tags$label(class=paste0("btn btn-outline-secondary",
                                if (cur=="0") " active" else ""),
                   `for`=paste0(ns(id),"_0"),
                   onclick=paste0("Shiny.setInputValue('",ns(id),"','0',{priority:'event'})"),
                   "No")),
      # Seed the Shiny input with any pre-existing saved value so the server
      # observer sees the loaded value immediately (not just clicks).
      if (nzchar(cur)) tags$script(HTML(sprintf(
        "Shiny.setInputValue('%s','%s',{priority:'event'});", ns(id), cur))))
  }

  # Strict sequential gating driven by session state (active_step).
  # Step numbers for P7: 1 goals  2 background  3 career  4 prep  5 topics  6 milestones
  STEP <- list(goals=1L, background=2L, career=3L, prep=4L, topics=5L, milestones=6L)
  as_step   <- as.integer(active_step %||% 1L)
  is_past   <- function(n) as_step >  STEP[[n]]
  is_active <- function(n) as_step == STEP[[n]]
  is_locked <- function(n) as_step <  STEP[[n]]
  col_for   <- function(n) is_past(n)

  div(
    div(class="alert alert-info mb-3 py-2 px-3",
        style="font-size:0.83rem; border-left:4px solid #0d6efd;",
        tags$i(class="bi bi-info-circle me-1"),
        tags$strong("Entering Residency"),
        " — Complete each section below. You can save and return at any time."),
    chk,

    if (is_locked("goals")) .locked_section("Learning Goals")
    else .sec_card(title="Learning Goals", icon="bullseye", collapsed=col_for("goals"),
                   saved = .section_complete("goals", sr, ir, ms_data, res),
      tags$p(class="text-muted", style="font-size:0.82rem;",
             "What are your 3 main learning goals entering residency? These will be revisited at each evaluation."),
      .ta(ns("s_e_ume_goal1"),"Goal 1",.fv(sr,"s_e_ume_goal1"),rows=2,
          placeholder="e.g., Become comfortable leading a code"),
      .ta(ns("s_e_ume_goal2"),"Goal 2",.fv(sr,"s_e_ume_goal2"),rows=2),
      .ta(ns("s_e_ume_goal3"),"Goal 3",.fv(sr,"s_e_ume_goal3"),rows=2),
      .save_btn(ns,"save_goals")),

    # Background questions → saved to resident_data
    if (is_locked("background")) .locked_section("Background Questions")
    else {
      bg_summary <- local({
        yn <- function(v) switch(as.character(v %||% ""), "1"="Yes", "0"="No", "—")
        div(class="alert alert-success mb-0 py-2 px-3",
            style="font-size:0.84rem; border-left:4px solid #198754;",
          tags$i(class="bi bi-check-circle-fill me-2", style="color:#198754;"),
          tags$strong("Saved. "),
          tags$span(style="color:#2c3e50;",
            "HS in MO: ",       tags$b(yn(.fv(res,"hs_mo"))),      " \u2022 ",
            "College in MO: ",  tags$b(yn(.fv(res,"college_mo"))), " \u2022 ",
            "Med school in MO: ",tags$b(yn(.fv(res,"med_mo"))),     " \u2022 ",
            "SLUSOM grad: ",    tags$b(yn(.fv(res,"slusom")))))
      })
      .sec_card(title="Background Questions", icon="geo-alt-fill",
              collapsed=col_for("background"),
              saved = .section_complete("background", sr, ir, ms_data, res),
        tags$p(class="text-muted mb-3", style="font-size:0.82rem;",
               "Help us track our program's regional reach — answered once."),
        yn_q("hs_mo",     "Did any of your high school education take place in Missouri?",     .fv(res,"hs_mo")),
        yn_q("college_mo","Did any of your college / undergraduate education take place in Missouri?", .fv(res,"college_mo")),
        yn_q("med_mo",    "Did you attend medical school in Missouri?",                        .fv(res,"med_mo")),
        yn_q("slusom",    "Are you a Saint Louis University School of Medicine (SLUSOM) graduate?", .fv(res,"slusom")),
        .save_btn(ns,"save_background"),
        if (is_past("background")) div(class="mt-3", bg_summary))
    },

    # Career Planning — mirrors the one in .form_std.
    # Fellowship specialty checkboxes only appear when the "Sub-specialty care
    # (fellowship)" option (code 2) is checked in career path.
    if (!is.null(cp) || !is.null(fl)) {
      if (is_locked("career")) .locked_section("Career Planning")
      else .sec_card(title="Career Planning", icon="briefcase-medical",
                     collapsed=col_for("career"),
                     saved = .section_complete("career", sr, ir, ms_data, res),
        tags$p(class="text-muted mb-3", style="font-size:0.82rem;",
               "Track your career interests entering residency."),
        if (!is.null(cp)) div(class="mb-3",
          tags$label("Career path interests", class="form-label fw-semibold",
                     style="font-size:0.85rem;"),
          checkboxGroupInput(ns("s_e_career_path"), NULL,
            choices=setNames(names(cp),unname(cp)), selected=sel_cp, inline=TRUE)),
        if (!is.null(fl))
          conditionalPanel(
            condition = sprintf("input['%s'] && input['%s'].indexOf('2') > -1",
                                ns("s_e_career_path"), ns("s_e_career_path")),
            div(class="mb-3",
              tags$label("Fellowship interests", class="form-label fw-semibold",
                         style="font-size:0.85rem;"),
              checkboxGroupInput(ns("s_e_fellow"), NULL,
                choices=setNames(names(fl),unname(fl)), selected=sel_fl, inline=TRUE))),
        if (!is.null(tr)) div(class="mb-3",
          tags$label("Track pursuit", class="form-label fw-semibold",
                     style="font-size:0.85rem;"),
          checkboxGroupInput(ns("s_e_track_type"), NULL,
            choices=setNames(names(tr),unname(tr)), selected=sel_tr, inline=TRUE)),
        .save_btn(ns,"save_career"))
    },

    # Preparedness ratings — compact matrix instead of 17 dropdowns
    if (is_locked("prep")) .locked_section("Preparedness Ratings")
    else .sec_card(title="Preparedness Ratings", icon="clipboard-check",
              collapsed=col_for("prep"),
              saved = .section_complete("prep", sr, ir, ms_data, res),
      tags$p(class="text-muted mb-3", style="font-size:0.82rem;",
             "Rate yourself honestly on each clinical skill. This is for your benefit."),
      .prep_matrix_ui(ns, sr),
      .save_btn(ns,"save_prep")),

    if (is_locked("topics")) .locked_section("Topics & Learning Styles")
    else .form_topics_section(ns, sr, dd, tc, sc, sel_t, sel_s, psr,
                              collapsed=col_for("topics"),
                              saved = .section_complete("topics", sr, ir, ms_data, res)),

    # Milestone self-assessment
    if (is_locked("milestones")) .locked_section("Milestone Self-Assessment")
    else .sec_card(title="Milestone Self-Assessment", icon="graph-up-arrow",
              collapsed=FALSE,
              saved = .section_complete("milestones", sr, ir, ms_data, res),
      div(class="d-flex align-items-center justify-content-between mb-3",
        tags$p(class="text-muted mb-0", style="font-size:0.82rem;",
               "Rate yourself on each ACGME milestone. Ratings of 4+ require a brief description. Your previously saved ratings will load automatically — review and edit as needed."),
        uiOutput(ns("milestone_save_status"))),
      uiOutput(ns("milestone_ui"))),

    # Concerns — optional (not tracked for completion)
    .sec_card(title="Concerns / Additional Notes", icon="chat-text",
              saved = nzchar(.fv(sr, "s_e_ume_concern")),
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

.form_std <- function(ns, p, sr, ir, dd, psr, ev, chk, ite_data = NULL, pgy = 1L,
                      pir = NULL, md = NULL, peer_med = NULL, active_step = 1L) {
  tc <- .dd_choices(dd,"s_e_topic_sel"); sc <- .dd_choices(dd,"s_e_learn_style")
  cp <- .dd_choices(dd,"s_e_career_path"); fl <- .dd_choices(dd,"s_e_fellow")
  tr <- .dd_choices(dd,"s_e_track_type")

  # Current-period-only: do NOT prefill from previous periods. Each period
  # starts clean so the resident's current thinking is captured, not last
  # period's data reused unintentionally.
  sel_t  <- .checked_codes(sr,"s_e_topic_sel")
  sel_s  <- .checked_codes(sr,"s_e_learn_style")
  sel_cp <- .checked_codes(sr,"s_e_career_path")
  sel_fl <- .checked_codes(sr,"s_e_fellow")
  sel_tr <- .checked_codes(sr,"s_e_track_type")

  # Strict sequential flow driven by session state (progress$step).
  # Step numbers:
  #   1 reflection   2 career      3 topics      4 feedback
  #   5 boards       6 ilp_review  7 scholarship 8 milestones   9 ilp
  # Pre-existing data does NOT skip ahead; the resident works through every
  # section in order. Prior steps render collapsed, active expanded, later
  # steps as locked placeholders.
  STEP <- list(reflection=1L, career=2L, topics=3L, feedback=4L,
               boards=5L, ilp_review=6L, scholarship=7L,
               milestones=8L, ilp=9L)
  as_step <- as.integer(active_step %||% 1L)
  is_past   <- function(n) as_step >  STEP[[n]]
  is_active <- function(n) as_step == STEP[[n]]
  is_locked <- function(n) as_step <  STEP[[n]]
  # Shortcut for sec_card collapsed argument — collapsed when past (done),
  # expanded when active.
  col_for <- function(n) is_past(n)

  div(
    chk,

    # 1. Self-Reflection
    .sec_card(title="Self-Reflection", icon="person-lines-fill",
              collapsed=col_for("reflection"),
      tags$p(class="text-muted mb-3", style="font-size:0.82rem;",
             "Review your recent evaluations and reflect on your growth this period."),
      .eval_reference_panel(ev),
      div(class="mt-2",
        .section_hdr("plus-circle-fill","What went well this period?"),
        .prev_ref_text(.fv(psr,"s_e_plus"), label="Previous period \u2014 what went well"),
        .ta(ns("s_e_plus"),NULL,.fv(sr,"s_e_plus"),rows=3,
            placeholder="Strengths, successes, moments you're proud of...")),
      .section_hdr("arrow-up-circle-fill","Areas to develop?"),
      .prev_ref_text(.fv(psr,"s_e_delta"), label="Previous period \u2014 areas to develop"),
      .ta(ns("s_e_delta"),NULL,.fv(sr,"s_e_delta"),rows=3,
          placeholder="What do you want to work on next period?"),
      .section_hdr("heart-pulse-fill","How are you doing mentally, emotionally, and physically?"),
      .ta(ns("s_e_well"),NULL,.fv(sr,"s_e_well"),rows=3,
          placeholder="Share anything about your wellness \u2014 work-life balance, stress, support needs..."),
      # Mentor discussion gate
      div(class="mt-3 mb-1 pt-2", style="border-top:1px dashed #dee2e6;"),
      checkboxInput(ns("discuss_with_mentor"),
                    "I have specific topics I'd like to discuss at my next mentor meeting",
                    value=nzchar(.fv(sr,"s_e_discussion"))),
      conditionalPanel(
        condition=paste0("input['", ns("discuss_with_mentor"), "'] === true"),
        .ta(ns("s_e_discussion"), NULL, .fv(sr,"s_e_discussion"), rows=3,
            placeholder="Topics / agenda items for your next mentor meeting...")),
      # Program-assistance gate
      div(class="mt-3 mb-1 pt-2", style="border-top:1px dashed #dee2e6;"),
      checkboxInput(ns("prog_assist_toggle"),
                    "Is there anything the program can do to assist you?",
                    value=nzchar(.fv(sr,"s_e_prog_assist"))),
      conditionalPanel(
        condition=paste0("input['", ns("prog_assist_toggle"), "'] === true"),
        .ta(ns("s_e_prog_assist"), NULL, .fv(sr,"s_e_prog_assist"), rows=3,
            placeholder="Tell us what would help \u2014 scheduling, mentorship, academic support, wellness resources, etc.")),
      .save_btn(ns,"save_reflection")),

    # 2. Career Planning
    if (!is.null(cp)||!is.null(fl)) {
      if (is_locked("career")) .locked_section("Career Planning")
      else .sec_card(title="Career Planning", icon="briefcase-medical",
                     collapsed=col_for("career"),
        tags$p(class="text-muted mb-3", style="font-size:0.82rem;",
               "Track your career interests for this period."),
        if (!is.null(cp)) div(class="mb-3",
          tags$label("Career path interests", class="form-label fw-semibold",
                     style="font-size:0.85rem;"),
          checkboxGroupInput(ns("s_e_career_path"),NULL,
            choices=setNames(names(cp),unname(cp)), selected=sel_cp, inline=TRUE)),
        if (!is.null(fl))
          conditionalPanel(
            condition = sprintf("input['%s'] && input['%s'].indexOf('2') > -1",
                                ns("s_e_career_path"), ns("s_e_career_path")),
            div(class="mb-3",
              tags$label("Fellowship interests", class="form-label fw-semibold",
                         style="font-size:0.85rem;"),
              checkboxGroupInput(ns("s_e_fellow"),NULL,
                choices=setNames(names(fl),unname(fl)), selected=sel_fl, inline=TRUE))),
        if (!is.null(tr)) div(class="mb-3",
          tags$label("Track pursuit", class="form-label fw-semibold",
                     style="font-size:0.85rem;"),
          checkboxGroupInput(ns("s_e_track_type"),NULL,
            choices=setNames(names(tr),unname(tr)), selected=sel_tr, inline=TRUE)),
        .save_btn(ns,"save_career"))
    },

    # 3. Topics & Learning Styles
    if (is_locked("topics")) .locked_section("Topics & Learning Styles")
    else .form_topics_section(ns, sr, dd, tc, sc, sel_t, sel_s, psr,
                              collapsed=col_for("topics")),

    # 4. Program Feedback
    if (is_locked("feedback")) .locked_section("Program Feedback")
    else .form_feedback_section(ns, sr, collapsed=col_for("feedback")),

    # 5. Boards & ITE — fully server-rendered to handle reactive Step 3 conditional
    if (is_locked("boards")) .locked_section("Boards & ITE")
    else uiOutput(ns("boards_full_ui")),

    # 6. ILP Goal Review — only shows if previous ILP goals exist; otherwise
    #    the server renderUI returns NULL and progression skips this step.
    if (is_locked("ilp_review")) NULL
    else uiOutput(ns("ilp_review_ui")),

    # 7. Scholarship readiness check (PS review + RCA) + link to Scholarship tab
    if (is_locked("scholarship")) NULL
    else uiOutput(ns("scholarship_check_ui")),

    # 8. Milestone Self-Assessment — never auto-collapse (milestone auto-saves
    #    keep retriggering .initial_step_for, which would otherwise mark the
    #    section as "past" and fold it closed mid-entry).
    if (is_locked("milestones")) .locked_section("Milestone Self-Assessment")
    else .sec_card(title="Milestone Self-Assessment", icon="graph-up-arrow",
                   collapsed=FALSE,
      div(class="d-flex align-items-center justify-content-between mb-3",
        tags$p(class="text-muted mb-0", style="font-size:0.82rem;",
               "Rate yourself on each ACGME milestone. Ratings of 4+ require a brief description."),
        uiOutput(ns("milestone_save_status"))),
      uiOutput(ns("milestone_ui"))),

    # 9. ILP Goals — locked until current-period milestones are entered
    if (is_locked("ilp")) .locked_section("Individual Learning Plan Goals")
    else .sec_card(title="Individual Learning Plan Goals", icon="map-fill",
              collapsed=col_for("ilp"),
      info_note(
        summary = "About the ILP and how to use this section",
        tags$p("Your ", tags$strong("Individual Learning Plan (ILP)"), " is a structured set of learning goals ",
               "organized by ACGME competency domain. Goals are reviewed each period with your coach."),
        tags$p("For each of the three domains, choose a ", tags$strong("subcompetency"), " (e.g., PC1 — History-taking), ",
               "a ", tags$strong("target milestone level"), " (1–9 scale from the behavioral anchor table), and ",
               "a specific ", tags$strong("anchor row"), " to work toward."),
        tags$p("The anchor table below your selection shows the behavioral descriptions for each level — ",
               "use these to pick a realistic but challenging target for the coming period.")
      ),
      .prev_ref_text(.fv(pir, "coach_ilp_final"),
                     label = "Coach comment \u2014 last period"),
      tags$p(class="text-muted mb-3", style="font-size:0.82rem;",
             "Set one focused goal per competency domain. Select the milestone row and target level."),
      div(class="alert alert-light border mb-3 py-1 px-2",
          style="font-size:0.8rem; border-left:4px solid #6f42c1 !important;",
          tags$i(class="bi bi-info-circle me-1", style="color:#6f42c1;"),
          "Select the specific subcompetency, milestone level, and row you want to target for each domain."),
      {
        # Pull choices from data dictionary (flipped: label=code for selectInput)
        pcmk_ch    <- .dd_select(dd, "goal_pcmk",            .SUBCOMP_CHOICES$pcmk)
        sbppbl_ch  <- .dd_select(dd, "goal_sbppbl",          .SUBCOMP_CHOICES$sbppbl)
        profics_ch <- .dd_select(dd, "goal_subcomp_profics", .SUBCOMP_CHOICES$profics)
        div(class="row g-3",
          div(class="col-md-4",
            div(style="border-left:4px solid #0d6efd; padding-left:10px;",
              tags$p(style="font-weight:700; font-size:0.8rem; color:#0d6efd; margin-bottom:8px;",
                     tags$i(class="bi bi-heart-pulse-fill me-1"), "Patient Care / Med Knowledge"),
              .ilp_context_panel("pcmk", pir, md, dd=dd, ns=ns, goal_input="goal_pcmk",
                                  peer_med=peer_med),
              .ilp_goal_select(ns,"goal_pcmk","Subcompetency goal",
                               pcmk_ch, .fv(ir,"goal_pcmk")),
              uiOutput(ns("ilp_level_pcmk")),
              .ta(ns("how_pcmk"),"How / Plan",.fv(ir,"how_pcmk"),rows=2))),
          div(class="col-md-4",
            div(style="border-left:4px solid #198754; padding-left:10px;",
              tags$p(style="font-weight:700; font-size:0.8rem; color:#198754; margin-bottom:8px;",
                     tags$i(class="bi bi-diagram-3-fill me-1"), "Systems / Practice-Based Learning"),
              .ilp_context_panel("sbppbl", pir, md, dd=dd, ns=ns, goal_input="goal_sbppbl",
                                  peer_med=peer_med),
              .ilp_goal_select(ns,"goal_sbppbl","Subcompetency goal",
                               sbppbl_ch, .fv(ir,"goal_sbppbl")),
              uiOutput(ns("ilp_level_sbppbl")),
              .ta(ns("how_sbppbl"),"How / Plan",.fv(ir,"how_sbppbl"),rows=2))),
          div(class="col-md-4",
            div(style="border-left:4px solid #6f42c1; padding-left:10px;",
              tags$p(style="font-weight:700; font-size:0.8rem; color:#6f42c1; margin-bottom:8px;",
                     tags$i(class="bi bi-people-fill me-1"), "Professionalism / Interpersonal"),
              .ilp_context_panel("profics", pir, md, dd=dd, ns=ns, goal_input="goal_subcomp_profics",
                                  peer_med=peer_med),
              .ilp_goal_select(ns,"goal_subcomp_profics","Subcompetency goal",
                               profics_ch, .fv(ir,"goal_subcomp_profics")),
              uiOutput(ns("ilp_level_profics")),
              .ta(ns("how_profics"),"How / Plan",.fv(ir,"how_profics"),rows=2))))
      },
      uiOutput(ns("ilp_pending_preview")),
      .save_btn(ns,"save_ilp","Save ILP Goals"))
  )
}

# ── Period 6 (Graduating) ────────────────────────────────────────────────────

.form_p6 <- function(ns, p, sr, dd, psr, ev, res, chk, active_step = 1L) {

  # Strict sequential gating driven by session state (active_step).
  # Step numbers for P6:
  #   1 reflection  2 alumni  3 feedback  4 ilp_review  5 scholarship  6 milestones
  STEP <- list(reflection=1L, alumni=2L, feedback=3L,
               ilp_review=4L, scholarship=5L, milestones=6L)
  as_step   <- as.integer(active_step %||% 1L)
  is_past   <- function(n) as_step >  STEP[[n]]
  is_active <- function(n) as_step == STEP[[n]]
  is_locked <- function(n) as_step <  STEP[[n]]
  col_for   <- function(n) is_past(n)

  div(
    div(class="alert alert-info mb-3 py-2 px-3",
        style="font-size:0.83rem; border-left:4px solid #0d6efd;",
        tags$i(class="bi bi-mortarboard-fill me-1"),
        tags$strong("Graduating Year"), " — Your final self-evaluation."),
    chk,

    # 1. Self-Reflection
    if (is_locked("reflection")) .locked_section("Self-Reflection")
    else .sec_card(title="Self-Reflection", icon="person-lines-fill",
              collapsed=col_for("reflection"),
      tags$p(class="text-muted mb-3", style="font-size:0.82rem;",
             "Review your recent evaluations and reflect on your growth this period."),
      .eval_reference_panel(ev),
      div(class="mt-2",
        .ta(ns("s_e_plus"),"What went well over your training?",
            .fv(sr,"s_e_plus"),rows=3)),
      .ta(ns("s_e_delta"),"What would you do differently?",
          .fv(sr,"s_e_delta"),rows=3),
      .ta(ns("s_e_well"),"How are you doing mentally, emotionally, and physically?",
          .fv(sr,"s_e_well"),rows=3,
          placeholder="Share anything about your wellness — work-life balance, stress, support needs..."),
      # Mentor discussion gate
      div(class="mt-3 mb-1 pt-2", style="border-top:1px dashed #dee2e6;"),
      checkboxInput(ns("discuss_with_mentor"),
                    "I have specific topics I'd like to discuss at my next mentor meeting",
                    value=nzchar(.fv(sr,"s_e_discussion"))),
      conditionalPanel(
        condition=paste0("input['", ns("discuss_with_mentor"), "'] === true"),
        .ta(ns("s_e_discussion"), NULL, .fv(sr,"s_e_discussion"), rows=3,
            placeholder="Topics / agenda items for your next mentor meeting...")),
      # Program-assistance gate
      div(class="mt-3 mb-1 pt-2", style="border-top:1px dashed #dee2e6;"),
      checkboxInput(ns("prog_assist_toggle"),
                    "Is there anything the program can do to assist you?",
                    value=nzchar(.fv(sr,"s_e_prog_assist"))),
      conditionalPanel(
        condition=paste0("input['", ns("prog_assist_toggle"), "'] === true"),
        .ta(ns("s_e_prog_assist"), NULL, .fv(sr,"s_e_prog_assist"), rows=3,
            placeholder="Tell us what would help — scheduling, mentorship, academic support, wellness resources, etc.")),
      .save_btn(ns,"save_reflection")),

    # 2. Graduation Info — cascading logic rendered server-side
    if (is_locked("alumni")) .locked_section("Graduation Info")
    else .sec_card(title="Graduation Info", icon="mortarboard-fill",
              collapsed=col_for("alumni"),
      tags$p(class="text-muted mb-3", style="font-size:0.82rem;",
             "Helps us track alumni outcomes and maintain connections. Under 2 minutes."),
      uiOutput(ns("grad_info_ui")),
      .save_btn(ns,"save_alumni","Save Graduation Info")),

    # 3. Program Feedback
    if (is_locked("feedback")) .locked_section("Program Feedback")
    else .form_feedback_section(ns, sr, is_grad=TRUE, collapsed=col_for("feedback")),

    # 4. ILP Goal Review
    if (is_locked("ilp_review")) NULL
    else uiOutput(ns("ilp_review_ui")),

    # 5. Scholarship readiness check (PS review + RCA) + link to Scholarship tab
    if (is_locked("scholarship")) NULL
    else uiOutput(ns("scholarship_check_ui")),

    # 6. Milestone Self-Assessment — never auto-collapse (see .form_std note)
    if (is_locked("milestones")) .locked_section("Milestone Self-Assessment")
    else .sec_card(title="Milestone Self-Assessment", icon="graph-up-arrow",
              collapsed=FALSE,
      div(class="d-flex align-items-center justify-content-between mb-3",
        tags$p(class="text-muted mb-0", style="font-size:0.82rem;",
               "Rate yourself on each ACGME milestone. Ratings of 4+ require a brief description."),
        uiOutput(ns("milestone_save_status"))),
      uiOutput(ns("milestone_ui")))
  )
}

# ── Shared: Program Feedback section ─────────────────────────────────────────
# is_grad=TRUE adds "advice for future residents" framing to the last textarea

.form_feedback_section <- function(ns, sr, is_grad = FALSE, collapsed = FALSE) {
  .sec_card(title="Program Feedback", icon="chat-dots-fill", collapsed=collapsed,
    tags$p(class="text-muted mb-3", style="font-size:0.82rem;",
           "Share candid feedback — this goes to program leadership, not your evaluating faculty."),
    div(class="alert alert-light border mb-3 py-1 px-2",
        style="font-size:0.78rem; border-left:3px solid #0d6efd !important;",
        tags$i(class="bi bi-shield-check me-1"),
        "Shared with program leadership — not your evaluating faculty."),
    # Core feedback quartet
    .ta(ns("s_e_prog_plus"),  "What is the program doing well?",
        .fv(sr,"s_e_prog_plus"), rows=2),
    .ta(ns("s_e_prog_delta"), "What should the program improve?",
        .fv(sr,"s_e_prog_delta"), rows=2),
    .ta(ns("s_e_progconf"),   "Conference / didactics feedback",
        .fv(sr,"s_e_progconf"), rows=2),
    .ta(ns("s_e_progfeed"),
        if (is_grad) "Final comments / advice for future residents" else "Other program comments",
        .fv(sr,"s_e_progfeed"), rows=2),
    .save_btn(ns,"save_feedback"))
}

# ── Shared: Topics & Learning Styles (#7 — shows previous selections) ─────────

.form_topics_section <- function(ns, sr, dd, tc, sc, sel_t, sel_s, psr,
                                 collapsed = FALSE, saved = FALSE) {
  .sec_card(title="Topics & Learning Styles", icon="lightbulb-fill",
            collapsed=collapsed, saved=saved,
    div(class="row g-3",
      div(class="col-md-6",
        tags$label("Topics you feel least comfortable with",
                   class="form-label fw-semibold", style="font-size:0.85rem;"),
        tags$p(class="text-muted small mb-2", style="font-size:0.75rem;",
               tags$i(class="bi bi-info-circle me-1"), "Choose up to 3"),
        if (!is.null(tc))
          tagList(
            checkboxGroupInput(ns("s_e_topic_sel"),NULL,
              choices=setNames(names(tc),unname(tc)), selected=sel_t),
            tags$script(HTML(sprintf(
              "(function(){
                var groupId = '%s';
                var maxN = 3;
                function enforce() {
                  var boxes = document.querySelectorAll(
                    \"#\" + groupId + \" input[type=checkbox]\");
                  var checked = Array.prototype.filter.call(boxes, function(b){return b.checked;});
                  var atMax = checked.length >= maxN;
                  Array.prototype.forEach.call(boxes, function(b){
                    if (!b.checked) b.disabled = atMax;
                    var lbl = b.closest('label');
                    if (lbl) lbl.style.opacity = (atMax && !b.checked) ? 0.45 : 1;
                  });
                }
                document.addEventListener('change', function(e){
                  if (e.target && e.target.closest('#' + groupId)) enforce();
                });
                // Apply on initial render
                setTimeout(enforce, 50);
              })();",
              ns("s_e_topic_sel")))))
        else tags$p(class="text-muted small", "Loading..."),
        textInput(ns("s_e_topic_oth"),"Other (free text)",.fv(sr,"s_e_topic_oth"),
                  placeholder="Specify other topic...")),
      div(class="col-md-6",
        tags$label("Preferred learning experiences",
                   class="form-label fw-semibold", style="font-size:0.85rem;"),
        if (!is.null(sc))
          checkboxGroupInput(ns("s_e_learn_style"),NULL,
            choices=setNames(names(sc),unname(sc)), selected=sel_s)
        else tags$p(class="text-muted small", "Loading..."),
        textInput(ns("s_e_learn_oth"),"Other (free text)",.fv(sr,"s_e_learn_oth"),
                  placeholder="Specify other style..."))),
    .save_btn(ns,"save_topics"))
}
