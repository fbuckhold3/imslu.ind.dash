# imslu.ind.dash

Individual resident dashboard for IMSLU Internal Medicine, deployed on
Posit Connect Cloud. Covers architecture and gotchas not obvious from the
code — no README in this repo currently.

**This repo has NO renv** (no `renv.lock`/`renv/`/`.Rprofile`), unlike the
global convention. Posit Connect deploys off `manifest.json`
(`rsconnect::writeManifest()`). There is one libPath — the R system
library — so packages are shared across all of Fred's local apps/scripts,
not project-isolated. Don't `install.packages()` or locally build a
package (e.g. `gmed`) here; it silently drifts the shared system library
and breaks `writeManifest()` for every app that depends on it (hit once
with `gmed` — fixed via `renv::install("fbuckhold3/gmed")` to pin a
GitHub-sourced build; see gmed section of the root CLAUDE.md).

## Scholarship module — ERAS-aligned rebuild

The scholarship data model has two homes, not one:
- **`scholarship` instrument** — discrete OUTPUTS only, mirroring MyERAS's
  5 scholarly-work types (Journal Article, Book Chapter, Journal Abstract,
  Oral Presentation, Poster Presentation). Type field is `schol_work_type`
  (deliberately not `schol_output_type`, to avoid colliding with the old
  field's different meaning in migrated data).
- **`s_eval` form** (per period) — QI / Patient Safety / Committee /
  current-work questions (`s_e_did_qi`, `s_e_did_ps`, `s_e_did_committee`,
  `s_e_current_work`, …). These used to live under scholarship and were
  moved here so the self-eval gate isn't blocked by scholarship entry.

Migration was **non-destructive and additive**: old scholarship fields
were never deleted, ERAS fields were added alongside them, and
`schol_work_type` is NULL on legacy rows ("needs review" in the UI). Both
TEST and PROD REDCap have the ERAS fields applied.

Module split (all under `R/`):
- `mod_scholarship_view.R` — read-only, reusable, takes a data reactive.
- `mod_scholarship_entry.R` — form + save; supports new/edit/recategorize
  via a `form_mode` param. Recategorize best-guess-maps legacy fields
  (output_type→work_type, status→pub_status, venue→source/event, date)
  and shows the old citation for reference.
- `mod_scholarship.R` — orchestrator; owns the `schol_store`, upserts on
  edit vs. appends on new, deletes via a scoped `.rc_delete()` (must pass
  both `instrument` and `repeat_instance` — omitting either wipes the
  whole record, not just one instance).
- `mod_scholarship_cv.R` — Word CV export via `officer`, grouped by the 5
  ERAS types, most-meaningful-flagged. `officer` calls are
  requireNamespace-guarded so the app still runs if it isn't installed.

## Gotchas

- Named-vector labels (e.g. `.TYPE_LABELS[tp]`) render as HTML attributes
  in Shiny UI, not visible text — always wrap in `unname()`.
- `gmed::pull_all_redcap_data()` exports all fields unrestricted, so new
  REDCap fields auto-appear in `all_forms$scholarship` /
  `all_forms$s_eval` without any gmed change. Checkboxes export as
  `___N` columns.
- REDCap's "Import a Data Dictionary" **replaces the whole project
  dictionary** — a partial import wipes other instruments. Use
  field-level edits or a fully merged dictionary, never a partial one.
- Test against `RDM_TOKEN_TEST` first:
  `Sys.setenv(RDM_TOKEN=Sys.getenv("RDM_TOKEN_TEST")); shiny::runApp(...)`
  — startup probe should report "TEST RDM 2.0 [id=684]".

## Known pending work

- Date pickers in the scholarship entry form still default to today
  (minor, not fixed).
- `mod_attendance.R` work belongs to a separate workstream and should be
  reviewed/committed independently rather than folded into scholarship
  changes.
