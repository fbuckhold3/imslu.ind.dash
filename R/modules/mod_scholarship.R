# mod_scholarship.R ─ Scholarship & Teaching Portfolio (orchestrator)
#
# Thin composer that wires together two reusable sub-modules:
#   - mod_scholarship_view  : display of a resident's outputs + per-row actions
#   - mod_scholarship_entry : the add / edit / recategorize form + save
#
# Keeps the original public interface (mod_scholarship_ui / mod_scholarship_server
# with rdm_data + resident_id) so server.R and ui.R need no changes.
#
# Owns the single source of truth — `schol_store` — seeded from REDCap at startup,
# upserted on save (edit replaces the row, new appends), and pruned on delete, so
# the view refreshes live without an app restart.
#
# Scope note (2026 redesign): scholarship records OUTPUTS only, mirroring the
# ERAS scholarly-work categories. QI / patient-safety / committee / ongoing-project
# work lives in the per-period `s_eval` form, not here.

# Delete a single repeating instance from REDCap. The `instrument` +
# `repeat_instance` params scope the delete to that one instance — WITHOUT them
# REDCap would delete the entire record, so both are always sent.
.rc_delete <- function(record_id, instrument, instance) {
  tryCatch({
    resp <- httr::POST(
      url  = app_config$redcap_url,
      body = list(token = app_config$rdm_token, content = "record", action = "delete",
                  "records[0]"    = as.character(record_id),
                  instrument      = instrument,
                  repeat_instance = as.character(instance),
                  returnFormat    = "json"),
      encode = "form", httr::timeout(30))
    status <- httr::status_code(resp)
    body   <- httr::content(resp, "text", encoding = "UTF-8")
    if (status == 200 && !grepl("error", body, ignore.case = TRUE))
      list(success = TRUE, message = body)
    else
      list(success = FALSE, message = paste0("REDCap (HTTP ", status, "): ", substr(body, 1, 300)))
  }, error = function(e) list(success = FALSE, message = conditionMessage(e)))
}

mod_scholarship_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "d-flex justify-content-end mb-2",
      downloadButton(ns("dl_cv"), "Download CV (Word)",
        class = "btn btn-sm btn-outline-primary",
        style = "font-size:0.82rem;")),
    mod_scholarship_view_ui(ns("view")),
    mod_scholarship_entry_ui(ns("entry"))
  )
}

mod_scholarship_server <- function(id, rdm_data, resident_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── single source of truth — seeded once from loaded data ─────────────────
    schol_store <- reactiveVal(NULL)

    observe({
      req(rdm_data(), resident_id())
      if (!is.null(schol_store())) return()   # already seeded
      schol <- rdm_data()$all_forms$scholarship
      rows  <- if (!is.null(schol) && nrow(schol) > 0)
                 schol[schol$record_id == resident_id(), , drop = FALSE]
               else data.frame()
      schol_store(rows)
    })

    # Resident display name (residents$name, as server.R does) for the CV header.
    resident_name <- reactive({
      r   <- tryCatch(rdm_data()$residents, error = function(e) NULL)
      rid <- resident_id()
      if (is.null(r) || is.null(rid) || !"name" %in% names(r)) return("Resident")
      row <- r[as.character(r$record_id) == as.character(rid), , drop = FALSE]
      nm  <- if (nrow(row) > 0) row$name[1] else NA
      if (is.na(nm) || !nzchar(nm)) "Resident" else nm
    })

    # ── CV (Word) export ──────────────────────────────────────────────────────
    output$dl_cv <- downloadHandler(
      filename = function()
        paste0("Scholarship_CV_", gsub("[^A-Za-z0-9]+", "_", resident_name()), "_",
               Sys.Date(), ".docx"),
      content = function(file) {
        if (!requireNamespace("officer", quietly = TRUE)) {
          showNotification("CV export needs the 'officer' package installed.", type = "error", duration = 6)
          stop("officer not installed")
        }
        .build_scholarship_cv(schol_store() %||% data.frame(), resident_name(), file)
      }
    )

    row_by_instance <- function(inst) {
      st <- schol_store()
      if (is.null(st) || nrow(st) == 0 || !"redcap_repeat_instance" %in% names(st)) return(NULL)
      r <- st[as.character(st$redcap_repeat_instance) == as.character(inst), , drop = FALSE]
      if (nrow(r) == 0) NULL else r
    }

    # ── entry form (add / edit / recategorize) ────────────────────────────────
    edit_req_val <- reactiveVal(NULL)
    saved <- mod_scholarship_entry_server(
      "entry",
      resident_id   = resident_id,
      existing_data = reactive(schol_store()),
      edit_req      = reactive(edit_req_val())
    )

    # ── view (returns per-row actions) ────────────────────────────────────────
    action <- mod_scholarship_view_server("view", schol_data = reactive(schol_store()))

    # Route actions: edit / recategorize open the form prefilled; delete confirms.
    observeEvent(action(), {
      a <- action(); req(a, a$action)
      if (a$action %in% c("edit", "recategorize")) {
        row <- row_by_instance(a$instance); req(row)
        edit_req_val(list(mode = a$action, instance = a$instance, row = row, nonce = a$nonce))
      } else if (a$action == "delete") {
        row <- row_by_instance(a$instance)
        lbl <- if (!is.null(row)) {
          t <- row$schol_title[1]
          if (is.na(t) || !nzchar(t)) paste0("entry #", a$instance) else t
        } else paste0("entry #", a$instance)
        pending_del(a)
        showModal(modalDialog(
          title = "Delete this entry?",
          tags$p("This permanently removes ", tags$strong(lbl),
                 " from REDCap. This can't be undone."),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("confirm_delete"), "Delete", class = "btn btn-danger btn-sm")),
          easyClose = TRUE, size = "s"))
      }
    }, ignoreNULL = TRUE)

    # ── delete confirmation ───────────────────────────────────────────────────
    pending_del <- reactiveVal(NULL)
    observeEvent(input$confirm_delete, {
      a <- pending_del(); req(a)
      removeModal()
      res <- .rc_delete(a$record_id, "scholarship", a$instance)
      if (isTRUE(res$success)) {
        st <- schol_store()
        schol_store(st[as.character(st$redcap_repeat_instance) != as.character(a$instance), , drop = FALSE])
        showNotification("Entry deleted.", type = "message", duration = 3)
      } else {
        showNotification(paste0("Delete failed: ", res$message), type = "error", duration = 6)
      }
      pending_del(NULL)
    })

    # ── save upsert — edit/recategorize replace the row, new appends ──────────
    observeEvent(saved(), {
      nr <- saved(); req(nr)
      st <- schol_store() %||% data.frame()
      if (nrow(st) > 0 && "redcap_repeat_instance" %in% names(st)) {
        st <- st[as.character(st$redcap_repeat_instance) != as.character(nr$redcap_repeat_instance), , drop = FALSE]
      }
      schol_store(dplyr::bind_rows(st, nr))
    }, ignoreNULL = TRUE)

  }) # end moduleServer
}
