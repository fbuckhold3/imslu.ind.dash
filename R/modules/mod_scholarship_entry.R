# mod_scholarship_entry.R ─ Scholarship & Teaching Portfolio (entry form)
#
# Resident-facing data entry for scholarly OUTPUTS, mirroring the MyERAS
# scholarly-work structure. One reusable form serves three modes:
#   - "new"          : blank form, saves a new repeat instance (additive)
#   - "edit"         : prefilled from an existing ERAS row, overwrites that instance
#   - "recategorize" : prefilled with best-guess mappings from a legacy row's old
#                      fields (+ the original citation shown for reference),
#                      writes ERAS fields onto that same instance
#
# Saves to the REDCap `scholarship` repeating instrument. Returns a reactive that
# emits each saved row (with its instance) so a parent can refresh a view live.
#
# ERAS schema: schol_work_type (1 Journal Article | 2 Book Chapter |
#   3 Journal Abstract | 4 Oral Presentation | 5 Poster Presentation),
#   schol_title, schol_authors, schol_first_author, schol_source_name (1,2,3),
#   schol_pub_status/pub_date (1,3), schol_volume/issue/pages (1,3 + Published),
#   schol_url/pmid (1,3), schol_event_name/date/city/state/country/url/abstract_url
#   (4,5), schol_collection, schol_meaningful.

mod_scholarship_entry_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("add_button_panel")),
    uiOutput(ns("add_form_panel"))
  )
}

# resident_id   : reactive() -> record_id to save under
# existing_data : reactive() -> this resident's current scholarship rows
#                 (next repeat instance + most-meaningful cap)
# edit_req      : reactive() -> NULL, or list(mode, instance, row) to open the
#                 form in edit / recategorize mode prefilled from `row`
# returns       : reactive() -> saved row (data.frame incl. redcap_repeat_instance)
mod_scholarship_entry_server <- function(id, resident_id, existing_data,
                                         edit_req = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ss        <- reactiveValues(save = NULL)
    saved_row <- reactiveVal(NULL)
    # form_mode: NULL (closed) or list(mode, instance, initial=named list, ref=text)
    form_mode <- reactiveVal(NULL)

    gv <- function(row, f) if (!is.null(row) && f %in% names(row)) {
      v <- row[[f]][1]; if (is.na(v)) "" else as.character(v) } else ""

    # ── legacy -> ERAS best-guess prefill (recategorize) ───────────────────────
    .prefill <- function(row, mode) {
      if (mode == "edit") {
        eras <- c("schol_work_type","schol_title","schol_authors","schol_first_author",
                  "schol_source_name","schol_pub_status","schol_pub_date","schol_volume",
                  "schol_issue","schol_pages","schol_url","schol_pmid","schol_event_name",
                  "schol_event_date","schol_event_city","schol_event_state",
                  "schol_event_country","schol_event_url","schol_abstract_url",
                  "schol_collection","schol_meaningful")
        list(initial = setNames(lapply(eras, function(f) gv(row, f)), eras), ref = "")
      } else {
        # recategorize: pull every legacy field we can into the ERAS form so the
        # resident edits rather than retypes. The free-text citation (which held
        # most of the old content) is dropped into Authors for them to split up.
        map_type   <- c("1"="3","2"="5","3"="4","4"="1")  # Phase-1 output_type -> work_type
        map_status <- c("2"="1","3"="2","5"="3")          # Phase-1 status -> pub_status
        wt <- unname(map_type[gv(row, "schol_output_type")]); if (is.na(wt)) wt <- ""
        if (!nzchar(wt) && gv(row, "schol_type") == "5") wt <- "1"   # old "Publication" -> Journal Article
        ps <- unname(map_status[gv(row, "schol_status")]); if (is.na(ps)) ps <- ""
        if (!nzchar(ps) && gv(row, "schol_pub") == "1") ps <- "3"    # legacy "published" flag
        venue <- gv(row, "schol_venue")
        cit   <- gv(row, "schol_citation"); if (!nzchar(cit)) cit <- gv(row, "schol_cit")
        init <- list(
          schol_work_type   = wt,
          schol_title       = gv(row, "schol_title"),
          schol_authors     = cit,                        # editable — trim to authors
          schol_source_name = venue,
          schol_pub_status  = ps,
          schol_pub_date    = gv(row, "schol_date"),
          schol_event_name  = if (nzchar(gv(row,"schol_pres_conf"))) gv(row,"schol_pres_conf") else venue,
          schol_event_date  = gv(row, "schol_date"),
          schol_meaningful  = "0")
        list(initial = init, ref = cit)
      }
    }

    open_for <- function(mode, instance = NULL, row = NULL) {
      p <- if (mode == "new") list(initial = list(), ref = "") else .prefill(row, mode)
      form_mode(list(mode = mode, instance = instance, initial = p$initial, ref = p$ref))
      ss$save <- NULL
    }

    observeEvent(input$btn_add,    open_for("new"))
    observeEvent(input$btn_cancel, { form_mode(NULL); ss$save <- NULL })
    observeEvent(edit_req(), {
      r <- edit_req(); req(r)
      open_for(r$mode, instance = r$instance, row = r$row)
    }, ignoreNULL = TRUE)

    # ── add button (hidden while a form is open) ───────────────────────────────
    output$add_button_panel <- renderUI({
      if (!is.null(form_mode())) return(NULL)
      div(class = "mb-3",
        actionButton(ns("btn_add"), "+ Add Entry", class = "btn btn-sm",
          style = "background:#003d5c; color:#fff; border:none; padding:4px 14px; font-size:0.85rem;"))
    })

    # ── entry form ─────────────────────────────────────────────────────────────
    output$add_form_panel <- renderUI({
      fm <- form_mode(); if (is.null(fm)) return(NULL)
      init <- fm$initial %||% list()
      iv <- function(f) { v <- init[[f]]; if (is.null(v)) "" else as.character(v) }
      idate <- function(f) { d <- suppressWarnings(as.Date(iv(f))); if (length(d) == 1 && !is.na(d)) d else Sys.Date() }

      # Existing collection/project labels this resident has already used, so a
      # new work can be attached to one without retyping it.
      existing_colls <- {
        df <- tryCatch(existing_data(), error = function(e) NULL)
        if (!is.null(df) && "schol_collection" %in% names(df))
          sort(unique(df$schol_collection[nzchar(df$schol_collection %||% "") & !is.na(df$schol_collection)]))
        else character(0)
      }

      lbl <- function(text)
        tags$label(text, class = "form-label fw-semibold", style = "font-size:0.92rem; color:#2c3e50;")
      txt <- function(fid, ph = "")
        tags$input(type = "text", id = ns(fid), class = "form-control",
                   style = "font-size:0.92rem;", placeholder = ph, value = iv(fid))

      c_source <- "input.schol_work_type=='1'||input.schol_work_type=='2'||input.schol_work_type=='3'"
      c_pub    <- "input.schol_work_type=='1'||input.schol_work_type=='3'"
      c_pubd   <- "(input.schol_work_type=='1'||input.schol_work_type=='3')&&input.schol_pub_status=='3'"
      c_event  <- "input.schol_work_type=='4'||input.schol_work_type=='5'"

      hdr <- switch(fm$mode, edit = "Edit Scholarly Work",
                    recategorize = "Recategorize Older Entry", "Add Scholarly Work")

      div(class = "card border-0 shadow-sm mb-3", style = "border-radius:8px;",
        div(class = "card-header border-0 d-flex align-items-center gap-2",
            style = "background:#f8fafc; border-radius:8px 8px 0 0; padding:12px 18px;",
          tags$i(class = "bi bi-plus-circle-fill", style = "color:#003d5c; font-size:1rem;"),
          tags$span(style = "font-weight:700; color:#003d5c; font-size:0.95rem;", hdr)),
        div(class = "card-body",
          if (fm$mode == "recategorize" && nzchar(fm$ref %||% ""))
            div(class = "alert alert-secondary py-2 px-3 mb-3", style = "font-size:0.85rem;",
              div(tags$span(style = "font-weight:600;", "Original entry: "), fm$ref),
              tags$div(class = "text-muted mt-1", style = "font-size:0.8rem;",
                tags$i(class = "bi bi-arrow-down me-1"),
                "This text is pre-filled into Authors below — split it into Title / Authors / Journal and set the type.")),
          tags$p(class = "text-muted mb-3", style = "font-size:0.88rem;",
                 "Fields mirror the ERAS scholarly-work categories, so entries copy straight into your application."),

          div(class = "mb-3", lbl("Type of scholarly work"),
            selectInput(ns("schol_work_type"), label = NULL,
              choices = c("-- select type --"                = "",
                          "Journal Article (peer reviewed)"  = "1",
                          "Book Chapter (peer reviewed)"     = "2",
                          "Journal Abstract (peer reviewed)" = "3",
                          "Oral Presentation"                = "4",
                          "Poster Presentation"              = "5"),
              selected = iv("schol_work_type"), selectize = FALSE, width = "100%")),

          div(class = "mb-3", lbl("Title of work"),
            txt("schol_title", "Title only — not the full citation")),

          div(class = "mb-3", lbl("Authors"),
            tags$textarea(id = ns("schol_authors"), class = "form-control",
              rows = 2, style = "font-size:0.92rem; resize:vertical;",
              placeholder = "All authors, in publication order (as they should appear on a CV)",
              iv("schol_authors"))),

          div(class = "mb-3", lbl("Are you the first author?"),
            radioButtons(ns("schol_first_author"), label = NULL,
              choices = c("Yes" = "1", "No" = "0"),
              selected = if (nzchar(iv("schol_first_author"))) iv("schol_first_author") else character(0),
              inline = TRUE)),

          conditionalPanel(c_source, ns = ns,
            div(class = "mb-3", lbl("Journal or book / publication name"),
              txt("schol_source_name", "e.g., N Engl J Med — or the book title for a chapter"))),

          conditionalPanel(c_pub, ns = ns,
            div(class = "mb-3", lbl("Publication status"),
              selectInput(ns("schol_pub_status"), label = NULL,
                choices = c("-- select --" = "", "Submitted" = "1", "Accepted" = "2", "Published" = "3"),
                selected = iv("schol_pub_status"), selectize = FALSE, width = "100%")),
            div(class = "mb-3", lbl("Publication / status date"),
              dateInput(ns("schol_pub_date"), label = NULL, value = idate("schol_pub_date"), width = "180px")),
            conditionalPanel(c_pubd, ns = ns,
              div(class = "row g-2 mb-3",
                div(class = "col-4", lbl("Volume"), txt("schol_volume")),
                div(class = "col-4", lbl("Issue"),  txt("schol_issue")),
                div(class = "col-4", lbl("Pages"),  txt("schol_pages")))),
            div(class = "row g-2 mb-3",
              div(class = "col-8", lbl("Article URL"), txt("schol_url", "https://…")),
              div(class = "col-4", lbl("PMID"),        txt("schol_pmid")))),

          conditionalPanel(c_event, ns = ns,
            div(class = "alert alert-light py-2 px-3 mb-3",
                style = "font-size:0.85rem; border-left:3px solid #6f42c1;",
              tags$i(class = "bi bi-info-circle me-1"),
              "Presentations ", tags$strong("within the program count too"),
              " — e.g., noon conference, grand rounds, morning report, or research day."),
            div(class = "mb-3", lbl("Event / meeting name"),
              txt("schol_event_name", "e.g., IMSLU Noon Conference, or ACP Annual Meeting")),
            div(class = "mb-3", lbl("Presentation date"),
              dateInput(ns("schol_event_date"), label = NULL, value = idate("schol_event_date"), width = "180px")),
            div(class = "row g-2 mb-3",
              div(class = "col-5", lbl("City"),          txt("schol_event_city")),
              div(class = "col-4", lbl("State / Prov."), txt("schol_event_state")),
              div(class = "col-3", lbl("Country"),       txt("schol_event_country"))),
            div(class = "row g-2 mb-3",
              div(class = "col-6", lbl("Event URL"),    txt("schol_event_url", "https://…")),
              div(class = "col-6", lbl("Abstract URL"), txt("schol_abstract_url", "https://…")))),

          div(class = "mb-3", lbl("Scholarly collection / project (optional)"),
            selectizeInput(ns("schol_collection"), label = NULL,
              choices  = union(existing_colls, iv("schol_collection")),
              selected = iv("schol_collection"),
              options  = list(create = TRUE,
                              placeholder = "Select an existing project, or type a new one"),
              width = "100%")),
          div(class = "mb-3 form-check",
            tags$input(type = "checkbox", class = "form-check-input", id = ns("schol_meaningful"),
                       checked = if (identical(iv("schol_meaningful"), "1")) NA else NULL),
            tags$label(class = "form-check-label", `for` = ns("schol_meaningful"),
                       style = "font-size:0.9rem; color:#2c3e50;",
                       "Mark as one of my ", tags$strong("most meaningful"), " works (max 3)")),

          div(class = "d-flex align-items-center gap-2 mt-3",
            actionButton(ns("btn_save"), if (fm$mode == "new") "Save Entry" else "Save Changes",
              class = "btn btn-sm",
              style = "background:#003d5c; color:#fff; border:none; padding:6px 18px;"),
            actionButton(ns("btn_cancel"), "Cancel",
              class = "btn btn-sm btn-outline-secondary", style = "padding:6px 14px;"),
            uiOutput(ns("save_status")))
        )
      )
    })

    output$save_status <- renderUI({
      r <- ss$save
      if (is.null(r)) return(NULL)
      if (isTRUE(r$success))
        tags$span(class = "text-success", style = "font-size:0.82rem;",
          tags$i(class = "bi bi-check-circle-fill me-1"), paste("Saved", r$ts))
      else
        tags$span(class = "text-danger", style = "font-size:0.82rem;",
          tags$i(class = "bi bi-exclamation-triangle-fill me-1"), r$message)
    })

    # ── save handler ───────────────────────────────────────────────────────────
    observeEvent(input$btn_save, {
      req(resident_id())
      fm <- form_mode(); req(fm)
      fv <- function(fid) { v <- input[[fid]]; if (is.null(v)) "" else trimws(as.character(v)) }
      date_iso <- function(fid) {
        d <- input[[fid]]
        if (inherits(d, "Date") && length(d) == 1 && !is.na(d)) format(d, "%Y-%m-%d") else ""
      }

      type  <- fv("schol_work_type")
      title <- fv("schol_title")
      if (!nzchar(type))  { ss$save <- list(success = FALSE, message = "Please select a type of work."); return() }
      if (!nzchar(title)) { ss$save <- list(success = FALSE, message = "Please enter a title."); return() }

      is_pub     <- type %in% c("1", "3")
      is_pres    <- type %in% c("4", "5")
      has_source <- type %in% c("1", "2", "3")
      status     <- fv("schol_pub_status")
      meaningful <- if (isTRUE(input$schol_meaningful)) "1" else "0"

      # Most-meaningful cap of 3 — exclude the instance being edited so a starred
      # entry can be re-saved without counting itself.
      if (meaningful == "1") {
        df <- tryCatch(existing_data(), error = function(e) NULL)
        if (!is.null(df) && "schol_meaningful" %in% names(df)) {
          others <- df[as.character(df$redcap_repeat_instance) != as.character(fm$instance %||% ""), , drop = FALSE]
          if (sum(as.character(others$schol_meaningful) == "1", na.rm = TRUE) >= 3) {
            ss$save <- list(success = FALSE,
              message = "You already have 3 'most meaningful' works — unstar one first.")
            return()
          }
        }
      }

      fields <- list(
        schol_work_type     = type,
        schol_title         = title,
        schol_authors       = fv("schol_authors"),
        schol_first_author  = fv("schol_first_author"),
        schol_source_name   = if (has_source) fv("schol_source_name") else "",
        schol_pub_status    = if (is_pub) status else "",
        schol_pub_date      = if (is_pub) date_iso("schol_pub_date") else "",
        schol_volume        = if (is_pub && status == "3") fv("schol_volume") else "",
        schol_issue         = if (is_pub && status == "3") fv("schol_issue")  else "",
        schol_pages         = if (is_pub && status == "3") fv("schol_pages")  else "",
        schol_url           = if (is_pub) fv("schol_url")  else "",
        schol_pmid          = if (is_pub) fv("schol_pmid") else "",
        schol_event_name    = if (is_pres) fv("schol_event_name") else "",
        schol_event_date    = if (is_pres) date_iso("schol_event_date") else "",
        schol_event_city    = if (is_pres) fv("schol_event_city")    else "",
        schol_event_state   = if (is_pres) fv("schol_event_state")   else "",
        schol_event_country = if (is_pres) fv("schol_event_country") else "",
        schol_event_url     = if (is_pres) fv("schol_event_url")     else "",
        schol_abstract_url  = if (is_pres) fv("schol_abstract_url")  else "",
        schol_collection    = fv("schol_collection"),
        schol_meaningful    = meaningful
      )

      # Target instance: overwrite for edit/recategorize, next for new.
      inst <- if (identical(fm$mode, "new")) {
        df <- tryCatch(existing_data(), error = function(e) NULL)
        if (!is.null(df) && nrow(df) > 0)
          max(suppressWarnings(as.integer(df$redcap_repeat_instance)), na.rm = TRUE) + 1L else 1L
      } else as.integer(fm$instance)

      result <- .rc_save(resident_id(), "scholarship", inst, fields)
      ss$save <- result

      if (isTRUE(result$success)) {
        new_row <- as.data.frame(
          c(list(record_id                = resident_id(),
                 redcap_repeat_instrument = "scholarship",
                 redcap_repeat_instance   = as.character(inst)),
            lapply(fields, as.character)),
          stringsAsFactors = FALSE, check.names = FALSE)
        saved_row(new_row)
        form_mode(NULL)
      }
    })

    reactive(saved_row())
  }) # end moduleServer
}
