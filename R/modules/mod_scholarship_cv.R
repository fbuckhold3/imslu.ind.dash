# mod_scholarship_cv.R ─ Scholarship CV (Word) export
#
# Builds a CV-style .docx of a resident's scholarly work, grouped by the 5 ERAS
# types, from the scholarship rows. Only categorized rows (schol_work_type set)
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
                   "5" = "Poster Presentations")

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
        doc <- officer::body_add_par(doc, cite, style = "List Paragraph")
      }
    }
  }

  print(doc, target = file)
  invisible(file)
}
