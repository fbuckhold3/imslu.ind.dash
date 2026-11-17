# mod_scholarship_cv.R ─ Scholarship CV (Word) export
#
# Builds a CV-style .docx of a resident's scholarly work, grouped by the 5 ERAS
# types plus ongoing projects (type 6, listed last), from the scholarship rows. Only categorized rows (schol_work_type set)
# are included; legacy "needs review" rows are skipped.
#
# `.schol_cv_citation()` is a pure function (no officer dependency) so it can be
# unit-tested; `.build_scholarship_cv()` renders the document via officer.

# Format one scholarship row as a CV citation string.
.schol_cv_citation <- function(row) {
  gv <- function(f) { v <- if (f %in% names(row)) row[[f]][1] else NA
                      if (is.na(v)) "" else trimws(as.character(v)) }
  yr <- function(f) { d <- gv(f); if (nchar(d) >= 4) substr(d, 1, 4) else "" }
  nz <- function(x) x[nzchar(x)]

  type    <- gv("schol_work_type")
  parts   <- c(gv("schol_authors"), gv("schol_title"))

  if (type %in% c("1", "3")) {                     # Journal Article / Abstract
    vip <- paste0(gv("schol_volume"),
                  if (nzchar(gv("schol_issue"))) paste0("(", gv("schol_issue"), ")"),
                  if (nzchar(gv("schol_pages"))) paste0(":", gv("schol_pages")))
    src <- paste0(gv("schol_source_name"),
                  if (nzchar(yr("schol_pub_date"))) paste0(". ", yr("schol_pub_date")),
                  if (nzchar(vip)) paste0(";", vip))
    parts <- c(parts, src)
    st <- gv("schol_pub_status")
    if (st %in% c("1", "2")) parts <- c(parts, c("1" = "Submitted", "2" = "Accepted")[[st]])
    if (nzchar(gv("schol_pmid"))) parts <- c(parts, paste0("PMID: ", gv("schol_pmid")))
  } else if (type == "2") {                        # Book Chapter
    if (nzchar(gv("schol_source_name"))) parts <- c(parts, paste0("In: ", gv("schol_source_name")))
  } else if (type == "6") {                        # Ongoing project
    kind <- c("1" = "Research", "2" = "Quality improvement", "3" = "Patient safety",
              "4" = "Case report", "5" = "Medical education", "6" = "Other")[gv("schol_proj_kind")]
    stat <- c("1" = "Planning", "2" = "Data collection", "3" = "Analysis",
              "4" = "Writing up", "5" = "On hold", "6" = "Stopped")[gv("schol_proj_status")]
    parts <- c(gv("schol_title"),
               if (!is.na(kind)) kind,
               if (nzchar(gv("schol_proj_mentor"))) paste0("Mentor: ", gv("schol_proj_mentor")),
               if (!is.na(stat)) paste0("Status: ", stat))
  } else if (type %in% c("4", "5")) {              # Oral / Poster
    loc <- paste(nz(c(gv("schol_event_city"), gv("schol_event_state"), gv("schol_event_country"))), collapse = ", ")
    ev  <- paste0(gv("schol_event_name"),
                  if (nzchar(loc)) paste0(", ", loc),
                  if (nzchar(yr("schol_event_date"))) paste0("; ", yr("schol_event_date")))
    parts <- c(parts, ev, if (type == "4") "Oral presentation" else "Poster presentation")
  }

  parts <- nz(parts)
  if (length(parts) == 0) return("")
  cite <- sub("\\.$", "", paste(parts, collapse = ". "))  # avoid a double period
  paste0(cite, ".")
}

# Render the CV document to `file`. Requires officer.
.build_scholarship_cv <- function(rows, display_name, file) {
  if (!requireNamespace("officer", quietly = TRUE))
    stop("The 'officer' package is required for CV export.")

  TYPE_LABELS <- c("1" = "Journal Articles (peer reviewed)",
                   "2" = "Book Chapters (peer reviewed)",
                   "3" = "Journal Abstracts (peer reviewed)",
                   "4" = "Oral Presentations",
                   "5" = "Poster Presentations",
                   "6" = "Ongoing Projects")

  doc <- officer::read_docx()
  doc <- officer::body_add_par(doc, display_name, style = "heading 1")
  doc <- officer::body_add_par(doc, "Scholarly Work", style = "heading 2")

  cat_rows <- if (is.null(rows) || nrow(rows) == 0 || !"schol_work_type" %in% names(rows))
                data.frame()
              else rows[!is.na(rows$schol_work_type) & rows$schol_work_type != "", , drop = FALSE]

  if (nrow(cat_rows) == 0) {
    doc <- officer::body_add_par(doc, "No scholarly work recorded yet.", style = "Normal")
  } else {
    for (tp in names(TYPE_LABELS)) {
      trows <- cat_rows[as.character(cat_rows$schol_work_type) == tp, , drop = FALSE]
      if (nrow(trows) == 0) next
      doc <- officer::body_add_par(doc, TYPE_LABELS[[tp]], style = "heading 3")
      for (i in seq_len(nrow(trows))) {
        cite <- .schol_cv_citation(trows[i, , drop = FALSE])
        if (identical(as.character(trows[i, "schol_meaningful"]), "1"))
          cite <- paste0(cite, "  [most meaningful]")
        # officer's default template has no list style ("List Paragraph"
        # errored), so entries are indented Normal paragraphs.
        doc <- officer::body_add_fpar(doc, officer::fpar(cite,
                 fp_p = officer::fp_par(padding.left = 18, padding.bottom = 6)), style = "Normal")
      }
    }
  }

  print(doc, target = file)
  invisible(file)
}
