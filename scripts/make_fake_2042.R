#!/usr/bin/env Rscript
# Generate a fake PGY2 resident (record_id=2042) REDCap import CSV
# ready to demo the "End PGY2" self-evaluation on the IMSLU dashboard.

suppressPackageStartupMessages(library(data.table))

dict_path     <- "/Users/fredbuckhold/Downloads/RDM20_DataDictionary_2026-04-10.csv"
template_path <- "/Users/fredbuckhold/Downloads/TESTRDM20_ImportTemplate_2026-04-24.csv"
out_path      <- "/Users/fredbuckhold/Downloads/fake_resident_2042_reformatted.csv"

# Read the REDCap import template header — this is the authoritative column
# order & naming. All output must conform to this header exactly so the file
# can be imported into REDCap without column-mismatch errors.
template_cols <- strsplit(readLines(template_path, n = 1, warn = FALSE), ",")[[1]]

d <- fread(dict_path)
setnames(d, make.names(names(d)))

# Parse choice strings "1, A | 2, B" -> codes
parse_codes <- function(x) {
  if (is.na(x) || !nzchar(x)) return(character(0))
  parts <- strsplit(x, "\\s*\\|\\s*")[[1]]
  codes <- sub("^\\s*([^,]+),.*$", "\\1", parts)
  trimws(codes)
}

# Build ordered list of output columns
# For checkbox fields, expand into fieldname___code columns
build_cols <- function(d) {
  cols <- character(0)
  for (i in seq_len(nrow(d))) {
    fn <- d$Variable...Field.Name[i]
    ft <- d$Field.Type[i]
    if (ft == "checkbox") {
      codes <- parse_codes(d$Choices..Calculations..OR.Slider.Labels[i])
      if (length(codes) == 0) {
        cols <- c(cols, fn)
      } else {
        cols <- c(cols, paste0(fn, "___", codes))
      }
    } else {
      cols <- c(cols, fn)
    }
  }
  cols
}

field_cols_raw <- build_cols(d)
# record_id appears in the dict as the first field; don't duplicate it in the meta block
field_cols <- field_cols_raw[field_cols_raw != "record_id"]
all_cols   <- c("record_id", "redcap_repeat_instrument", "redcap_repeat_instance", field_cols)

# Helper: new empty row (named list of "")
blank <- function() {
  l <- as.list(rep("", length(all_cols)))
  names(l) <- all_cols
  l
}

rows <- list()

# ---------------------------------------------------------------------------
# 1. Main resident_data row (non-repeating)
# ---------------------------------------------------------------------------
r <- blank()
r$record_id                 <- "2042"
r$redcap_repeat_instrument  <- ""
r$redcap_repeat_instance    <- ""
r$first_name                <- "Alex"
r$last_name                 <- "Morgan"
r$name                      <- "Alex Morgan"
r$type                      <- "2"     # Categorical
r$grad_yr                   <- "5"     # 2027 -> PGY2 in AY 2025-26
r$dob                       <- "1996-05-14"
r$gender                    <- "2"     # Female
r$race_ethn___6             <- "1"     # White
r$email                     <- "amorgan@example.edu"
r$phone                     <- "3145550142"
r$deg                       <- "1"     # US MD
r$usmle_step1_failure       <- "0"
r$usmle_step2_score         <- "238"
r$step3                     <- "0"
r$coach                     <- "18"    # Wheeler
r$access_code               <- "2042"
r$hs_mo                     <- "1"
r$college_mo                <- "0"
r$med_mo                    <- "1"
r$slusom                    <- "1"
r$track                     <- "1"     # Primary Care Track
r$chief                     <- "0"
rows[[length(rows)+1]] <- r

# ---------------------------------------------------------------------------
# 2. Four s_eval repeating instances (7, 1, 2, 3)
# ---------------------------------------------------------------------------
s_eval_dates <- list("7"="2024-06-15","1"="2024-12-10","2"="2025-05-20","3"="2025-12-15")

for (per in c("7","1","2","3")) {
  r <- blank()
  r$record_id                <- "2042"
  r$redcap_repeat_instrument <- "s_eval"
  r$redcap_repeat_instance   <- per
  r$s_e_period               <- per
  r$s_e_date                 <- s_eval_dates[[per]]
  r$s_e_career_path___1      <- "1"      # Primary Care
  r$s_e_track_type___1       <- "1"      # PC track
  r$s_e_track                <- "1"

  if (per == "7") {
    # Entering Residency: UME prep
    r$s_e_ume_goal1 <- "Build strong foundation in general internal medicine."
    r$s_e_ume_goal2 <- "Develop efficient and compassionate communication with patients."
    r$s_e_ume_goal3 <- "Grow toward a career in primary care."
    prep_vals <- c(3,3,2,3,4,3,2,3,3,3,2,3,4,3,3,2,3)   # 17 values, 2-4 mix
    for (k in seq_along(prep_vals)) r[[paste0("s_e_prep_", k)]] <- as.character(prep_vals[k])
    r$s_e_topic_sel___1  <- "1"  # Abdominal pain
    r$s_e_topic_sel___9  <- "1"  # Diabetes
    r$s_e_topic_sel___17 <- "1"  # Pneumonia
    r$s_e_learn_style___1 <- "1" # Case discussion
    r$s_e_learn_style___7 <- "1" # Mentoring
  } else {
    # Periodic check-ins
    pluses  <- c("1"="Improved efficiency on wards; better presentations.",
                 "2"="Solidified end-of-intern differential reasoning and note quality.",
                 "3"="Taking clear ownership of patients as PGY2; teaching interns effectively.")
    deltas  <- c("1"="Need to read more consistently around cases each night.",
                 "2"="Work on time management in clinic; deeper chronic disease learning.",
                 "3"="Want to refine transitions of care and outpatient follow-up discipline.")
    r$s_e_plus       <- pluses[per]
    r$s_e_delta      <- deltas[per]
    r$s_e_prog_plus  <- "Program provides strong, responsive coaching and rich case volume."
    r$s_e_prog_delta <- "Would value more structured outpatient feedback sessions."
    r$s_e_step3      <- "0"
    r$s_e_board_discu<- "Plan to use MKSAP and board-review questions daily going into PGY3."
    r$s_e_mksap_comp <- switch(per, "1"="1", "2"="2", "3"="3")  # progressing
    # rotate topic selections per period
    sel <- switch(per,
                  "1" = c(3, 8, 10),    # ACS, CHF, Dyspnea
                  "2" = c(4, 12, 18),   # AKI, GI bleed, Shock
                  "3" = c(6, 7, 21))    # Anticoag, Cirrhosis, Syncope
    for (s in sel) r[[paste0("s_e_topic_sel___", s)]] <- "1"
    # fellow: none
    for (k in 1:12) r[[paste0("s_e_fellow___", k)]] <- "0"
  }
  rows[[length(rows)+1]] <- r
}

# ---------------------------------------------------------------------------
# 3. Four milestone_selfevaluation_c33c instances (7, 1, 2, 3)
# ---------------------------------------------------------------------------
mile_dates <- s_eval_dates
# Derive the list of rep_*_self slider fields and matching *_desc fields from the dict
mfields <- d[Form.Name == "milestone_selfevaluation_c33c"]
slider_fields <- mfields[Field.Type == "slider", Variable...Field.Name]
desc_fields   <- mfields[Field.Type == "notes", Variable...Field.Name]

# Desc text rotation
descs <- c(
  "Developing consistency in history-taking across varied presentations.",
  "Comfortable with common inpatient management; building outpatient depth.",
  "Role-modeling communication and teaching junior learners."
)

period_vals <- list(
  "7" = c(2, 3),
  "1" = c(3, 3),
  "2" = c(3, 4),
  "3" = c(4, 5)
)

set.seed(2042)
for (per in c("7","1","2","3")) {
  r <- blank()
  r$record_id                <- "2042"
  r$redcap_repeat_instrument <- "milestone_selfevaluation_c33c"
  r$redcap_repeat_instance   <- per
  r$prog_mile_period_self    <- per
  r$prog_mile_date_self      <- mile_dates[[per]]

  vv <- period_vals[[per]]
  for (i in seq_along(slider_fields)) {
    r[[slider_fields[i]]] <- as.character(sample(vv, 1))
  }
  for (i in seq_along(desc_fields)) {
    r[[desc_fields[i]]] <- descs[((i - 1) %% length(descs)) + 1]
  }
  rows[[length(rows)+1]] <- r
}

# ---------------------------------------------------------------------------
# 3b. ACGME milestone rows (acgme_miles) - 3 instances (faculty-reported).
# Mirrors self-eval periods 1/2/3 (ACGME period codes 1=Mid Intern, 2=End
# Intern, 3=Mid PGY2). Faculty trend lags self-rating: 2 -> 2.5 -> 3.
# ---------------------------------------------------------------------------
acgme_fields <- d[Form.Name == "acgme_miles" & Field.Type == "slider",
                  Variable...Field.Name]

acgme_period_vals <- list(
  "1" = c(2, 2, 2, 2, 3),      # Mid Intern: mostly 2
  "2" = c(2, 2, 3, 3, 3),      # End Intern: 2-3 mix
  "3" = c(3, 3, 3, 3, 2, 4)    # Mid PGY2: mostly 3
)

set.seed(20420)
for (per in c("1","2","3")) {
  r <- blank()
  r$record_id                <- "2042"
  r$redcap_repeat_instrument <- "acgme_miles"
  r$redcap_repeat_instance   <- per
  r$acgme_mile_period        <- per
  vv <- acgme_period_vals[[per]]
  for (f in acgme_fields) {
    r[[f]] <- as.character(sample(vv, 1))
  }
  rows[[length(rows)+1]] <- r
}

# ---------------------------------------------------------------------------
# 4. Faculty evaluations (faculty_evaluation) - 15 entries
# ---------------------------------------------------------------------------
fac_names <- c("Smith","Johnson","Patel","Nguyen","Garcia","Williams","Brown","Lee")
att_rots  <- c("1","2","3","5","8","9","10","11") # valid codes
# 15 evenly-spaced dates 2025-07-15 .. 2026-04-15
start <- as.Date("2025-07-15"); end <- as.Date("2026-04-15")
fac_dates <- format(seq(start, end, length.out = 15), "%Y-%m-%d")

for (i in 1:15) {
  r <- blank()
  r$record_id                <- "2042"
  r$redcap_repeat_instrument <- "faculty_evaluation"
  r$redcap_repeat_instance   <- as.character(i)
  r$fac_eval_date   <- fac_dates[i]
  r$fac_eval_level  <- "2"   # PGY2
  r$att_or_fell     <- "1"   # Attending
  r$fac_fell_name   <- fac_names[((i-1) %% length(fac_names)) + 1]
  r$att_rot         <- att_rots[((i-1) %% length(att_rots)) + 1]
  # rating scales 4-5
  rating_fields <- c("approachability","respect","bedside_manner","time_teaching",
                     "ques_clin_des","autonomy","feedback","organ","att_overall")
  for (f in rating_fields) r[[f]] <- as.character(sample(4:5, 1))
  r$att_ext_tea   <- "2"   # Agree
  r$att_give_feed <- "3"   # Both oral and written
  r$eval_done     <- "1"   # Yes
  r$plus          <- "Clear teaching on rounds; pushed me to refine differentials."
  r$delta         <- "Could involve learners more explicitly in plan-of-the-day framing."
  rows[[length(rows)+1]] <- r
}

# ---------------------------------------------------------------------------
# 5. Assessment rows (resident-received evaluations) - 6 entries
# ---------------------------------------------------------------------------
ass_start <- as.Date("2025-07-20"); ass_end <- as.Date("2026-04-10")
ass_dates <- format(seq(ass_start, ass_end, length.out = 6), "%Y-%m-%d")
ass_plusses <- c(
  "Strong clinical reasoning and thorough patient presentations.",
  "Excellent rapport with patients and families.",
  "Reliable, efficient, and genuinely invested in team learning.",
  "Smart, curious, and quick to apply feedback.",
  "Teaches well and supports interns on busy call nights.",
  "Handles complex multi-morbid patients with poise."
)
ass_deltas <- c(
  "Continue building confidence leading family meetings.",
  "Work on succinct verbal handoffs under time pressure.",
  "Refine outpatient chronic-disease follow-up plans.",
  "Push into owning more of the plan before staffing.",
  "Read more around less-common consult questions.",
  "Polish written documentation of assessment/plan."
)
ass_faculty <- c("Smith","Johnson","Patel","Nguyen","Garcia","Williams")
ass_rot     <- c("SLUH Floors","VA Floors","MICU","Continuity Clinic","Consult Rotations","Acute Care")

for (i in 1:6) {
  r <- blank()
  r$record_id                <- "2042"
  r$redcap_repeat_instrument <- "assessment"
  r$redcap_repeat_instance   <- as.character(i)
  r$ass_date     <- ass_dates[i]
  r$ass_level    <- "2"  # PGY2
  r$ass_plus     <- ass_plusses[i]
  r$ass_delta    <- ass_deltas[i]
  r$ass_faculty  <- ass_faculty[i]
  r$ass_rotator  <- ass_rot[i]
  rows[[length(rows)+1]] <- r
}

# ---------------------------------------------------------------------------
# Combine and write
# ---------------------------------------------------------------------------
DT <- rbindlist(lapply(rows, as.data.table), use.names = TRUE, fill = TRUE)

# Conform columns to the REDCap import template: add any template cols we
# didn't populate as blanks, drop any extras, then order exactly as template.
missing_cols <- setdiff(template_cols, names(DT))
for (m in missing_cols) DT[, (m) := ""]
dropped_cols <- setdiff(names(DT), template_cols)
if (length(dropped_cols)) DT[, (dropped_cols) := NULL]
setcolorder(DT, template_cols)

# Replace any NA with empty string
for (j in names(DT)) set(DT, which(is.na(DT[[j]])), j, "")

# Write the template's raw header line byte-identically, then append data.
# This preserves the template's exact header bytes (quoting style + trailing
# comma for the unnamed 636th column) so the file imports cleanly to REDCap.
template_header_raw <- readLines(template_path, n = 1, warn = FALSE)
writeLines(template_header_raw, out_path, useBytes = TRUE)
fwrite(DT, out_path, quote = TRUE, na = "", col.names = FALSE, append = TRUE)

# ---------------------------------------------------------------------------
# Verification report
# ---------------------------------------------------------------------------
cat("\n==== VERIFICATION ====\n")
cat("Output file:        ", out_path, "\n")
cat("Template columns:   ", length(template_cols), "\n")
cat("Total columns:      ", ncol(DT), "\n")
cat("Total rows written: ", nrow(DT), "\n")
cat("Blank template cols:", length(missing_cols), "\n")
cat("Dropped extra cols: ", length(dropped_cols), "\n\n")
cat("Instrument row counts:\n")
print(DT[, .N, by = redcap_repeat_instrument])

cat("\nFirst 5 fields of resident_data row (record_id=2042):\n")
main <- DT[redcap_repeat_instrument == ""]
first5 <- c("record_id", field_cols[1:4])
for (f in first5) cat(sprintf("  %-20s = %s\n", f, main[[f]][1]))

cat("\nLevel check:   type =", main$type[1], ", grad_yr =", main$grad_yr[1],
    "  (expect type=2, grad_yr=5 -> PGY2)\n")
