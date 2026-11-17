# mod_scholarship.R ─ Scholarship & Teaching Portfolio (orchestrator)
#
# Thin composer that wires together two reusable sub-modules:
#   - gmed::mod_scholarship_view : display of a resident's outputs + per-row actions
#                                  (moved to gmed 2026-11; coach.dash shows it read-only)
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

# embedded = TRUE: the version shown inside the self-evaluation's Scholarship
# section (no back link or CV button, no intro text).
mod_scholarship_ui <- function(id, embedded = FALSE) {
  ns <- NS(id)
  tagList(
    if (!embedded) uiOutput(ns("back_link")),
    if (!embedded) div(class = "d-flex justify-content-end mb-2",
      downloadButton(ns("dl_cv"), "Download CV (Word)",
        class = "btn btn-sm btn-outline-primary",
        style = "font-size:0.82rem;")),
    gmed::mod_scholarship_view_ui(ns("view")),
    # The entry form opens inline under the entry being edited (inside the
    # view); only a NEW entry's form shows down here, under "+ Add Entry".
    gmed::mod_scholarship_entry_button_ui(ns("entry")),
    uiOutput(ns("new_form_slot"))
  )
}

# store: optional reactiveVal shared with other instances of this module
# (server.R passes one to both the Scholarship page and the self-eval's
# embedded copy, so an edit in one shows in the other).
mod_scholarship_server <- function(id, rdm_data, resident_id, store = NULL, embedded = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── "Back to your self-evaluation" ─────────────────────────────────────────
    # The self-eval's Scholarship-page link sets input$return_to = "self_eval"
    # (as 'scholarship-return_to') before navigating here.
    output$back_link <- renderUI({
      if (!identical(input$return_to, "self_eval")) return(NULL)
      div(class = "mb-3 py-2 px-3 d-flex align-items-center justify-content-between gap-2",
          style = "background:var(--roundsui-surface-2, #f8fafc); border-left:4px solid var(--roundsui-accent, #3355D8); border-radius:4px; font-size:0.85rem;",
        tags$span(tags$i(class = "bi bi-info-circle me-1"),
                  "You came here from your self-evaluation. Your progress there is saved."),
        tags$button(class = "btn btn-sm btn-primary",
          onclick = paste0(
            "Shiny.setInputValue('", ns("return_to"), "','',{priority:'event'});",
            "Shiny.setInputValue('nav_block','self_eval',{priority:'event'});"),
          tags$i(class = "bi bi-arrow-left me-1"), "Back to your self-evaluation"))
    })

    # ── single source of truth — seeded once from loaded data ─────────────────
    schol_store <- store %||% reactiveVal(NULL)

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

    # Older-format rows still to review (no schol_work_type, but some content).
    n_pending <- reactive({
      st <- schol_store()
      if (is.null(st) || nrow(st) == 0) return(0L)
      wt <- if ("schol_work_type" %in% names(st)) as.character(st$schol_work_type) else rep(NA, nrow(st))
      txt <- intersect(c("schol_type", "schol_output_type", "schol_title", "schol_cit", "schol_citation",
                         "schol_res", "schol_qi", "schol_pres_conf", "schol_venue", "schol_comm",
                         "schol_comm_other"), names(st))
      has <- if (length(txt)) apply(st[, txt, drop = FALSE], 1, function(r) any(!is.na(r) & nzchar(trimws(r)))) else FALSE
      sum((is.na(wt) | !nzchar(wt)) & has)
    })

    # ── entry form (add / edit / recategorize) ────────────────────────────────
    edit_req_val <- reactiveVal(NULL)
    form_mode    <- reactiveVal(NULL)   # list(mode, instance) while the form is open
    form_del_req <- reactiveVal(NULL)   # "Delete this entry" from inside the form
    close_form   <- reactiveVal(NULL)
    saved <- gmed::mod_scholarship_entry_server(
      "entry",
      resident_id    = resident_id,
      existing_data  = reactive(schol_store()),
      edit_req       = reactive(edit_req_val()),
      redcap_url     = app_config$redcap_url,
      rdm_token      = app_config$rdm_token,
      mode_out       = form_mode,
      delete_req     = form_del_req,
      pending_review = n_pending,
      close_req      = reactive(close_form()),
      add_prompt     = "Anything new to add? Projects, presentations, posters, papers...",
      prominent      = TRUE
    )

    output$new_form_slot <- renderUI({
      fm <- form_mode()
      if (!is.null(fm) && identical(fm$mode, "new")) gmed::mod_scholarship_entry_form_ui(ns("entry"))
    })

    # ── view (returns per-row actions) ────────────────────────────────────────
    action <- gmed::mod_scholarship_view_server(
      "view", schol_data = reactive(schol_store()),
      title           = if (embedded) "Your current scholarship" else "Scholarship & Teaching Portfolio",
      show_intro      = !embedded,
      inline_form     = function() gmed::mod_scholarship_entry_form_ui(ns("entry")),
      active_instance = reactive({
        fm <- form_mode()
        if (is.null(fm) || identical(fm$mode, "new")) NULL else fm$instance
      }))


    # Route actions: edit / recategorize open the form prefilled; delete confirms.
    handle_action <- function(a) {
      req(a, a$action)
      if (a$action %in% c("edit", "recategorize")) {
        row <- row_by_instance(a$instance); req(row)
        edit_req_val(list(mode = a$action, instance = a$instance, row = row, nonce = a$nonce))
      } else if (a$action == "delete") {
        row <- row_by_instance(a$instance)
        lbl <- if (!is.null(row)) {
          # Older rows keep their text in citation / project fields, not the title.
          f <- intersect(c("schol_title", "schol_proj_desc", "schol_cit", "schol_citation",
                           "schol_res", "schol_qi", "schol_pres_conf", "schol_comm"), names(row))
          t <- vapply(f, function(x) { v <- row[[x]][1]; if (is.na(v)) "" else as.character(v) }, "")
          t <- t[nzchar(trimws(t))]
          if (length(t) == 0) paste0("entry #", a$instance)
          else if (nchar(t[1]) > 120) paste0(substr(t[1], 1, 117), "...") else t[1]
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
    }
    observeEvent(action(), handle_action(action()), ignoreNULL = TRUE)
    # "Delete this entry" inside the inline form -> same confirm flow.
    observeEvent(form_del_req(), {
      d <- form_del_req(); req(d)
      handle_action(list(action = "delete", instance = d$instance,
                         record_id = resident_id(), nonce = d$nonce))
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
        fm <- form_mode()
        if (!is.null(fm) && identical(as.character(fm$instance), as.character(a$instance)))
          close_form(Sys.time())
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
