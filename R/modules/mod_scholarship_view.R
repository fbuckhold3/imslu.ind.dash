# mod_scholarship_view.R ─ Scholarship & Teaching Portfolio (read-only view)
#
# Pure display of a resident's scholarly work, grouped by the 5 ERAS types.
# Takes a reactive returning one resident's already-filtered scholarship rows —
# no rdm_data coupling, so it is reusable across apps (ind.dash, CCC, coach).
#
# Rows on the new schema (have schol_work_type) render as ERAS entries grouped
# by type. Rows with only legacy fields (no schol_work_type) are surfaced in a
# "Needs review" section so they can be recategorized (Recategorize / Edit /
# Delete actions are added in the CRUD phase).

mod_scholarship_view_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("entries_panel"))
}

mod_scholarship_view_server <- function(id, schol_data,
                                        title = "Scholarship & Teaching Portfolio",
                                        show_intro = TRUE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    .TYPE_LABELS <- c("1" = "Journal Articles", "2" = "Book Chapters",
                      "3" = "Journal Abstracts", "4" = "Oral Presentations",
                      "5" = "Poster Presentations")
    .STATUS_LABELS <- c("1" = "Submitted", "2" = "Accepted", "3" = "Published")
    .STATUS_BADGE  <- c("1" = "background:#e2e3e5; color:#41464b;",
                        "2" = "background:#cfe2ff; color:#084298;",
                        "3" = "background:#198754; color:#ffffff;")

    gv <- function(row, f) if (f %in% names(row)) { v <- row[[f]][1]; if (is.na(v)) "" else as.character(v) } else ""
    trunc <- function(s, n) if (nchar(s) > n) paste0(substr(s, 1, n - 3), "...") else s

    # Per-row action buttons emit {action, instance, record_id, nonce} on the
    # ns'd `row_action` input; the orchestrator listens on the returned reactive.
    action_val <- reactiveVal(NULL)
    observeEvent(input$row_action, action_val(input$row_action))
    act_btn <- function(action, inst, rid, icon, tip, color)
      tags$button(class = "btn btn-sm p-1 border-0",
        style = paste0("color:", color, "; background:transparent; line-height:1;"), title = tip,
        onclick = sprintf(
          "Shiny.setInputValue('%s',{action:'%s',instance:'%s',record_id:'%s',nonce:Date.now()},{priority:'event'})",
          ns("row_action"), action, inst, rid),
        tags$i(class = paste0("bi ", icon)))

    # Render one categorized (ERAS) entry as a list item.
    render_entry <- function(row) {
      inst   <- gv(row, "redcap_repeat_instance"); rid <- gv(row, "record_id")
      type   <- gv(row, "schol_work_type")
      title_v<- gv(row, "schol_title"); if (!nzchar(title_v)) title_v <- "(untitled)"
      authors<- trunc(gv(row, "schol_authors"), 110)
      star   <- identical(gv(row, "schol_meaningful"), "1")
      first  <- identical(gv(row, "schol_first_author"), "1")
      coll   <- gv(row, "schol_collection")

      # Type-specific meta line
      meta <- if (type %in% c("1", "3")) {
        src <- gv(row, "schol_source_name")
        vol <- gv(row, "schol_volume"); iss <- gv(row, "schol_issue"); pg <- gv(row, "schol_pages")
        vip <- paste0(vol, if (nzchar(iss)) paste0("(", iss, ")"), if (nzchar(pg)) paste0(":", pg))
        dt  <- gv(row, "schol_pub_date")
        paste(c(src, if (nzchar(vip)) vip, dt)[nzchar(c(src, if (nzchar(vip)) vip, dt))], collapse = " · ")
      } else if (type == "2") {
        gv(row, "schol_source_name")
      } else {
        ev  <- gv(row, "schol_event_name")
        loc <- paste(c(gv(row,"schol_event_city"), gv(row,"schol_event_state"),
                       gv(row,"schol_event_country"))[nzchar(c(gv(row,"schol_event_city"),
                       gv(row,"schol_event_state"), gv(row,"schol_event_country")))], collapse = ", ")
        dt  <- gv(row, "schol_event_date")
        paste(c(ev, loc, dt)[nzchar(c(ev, loc, dt))], collapse = " · ")
      }

      status_badge <- if (type %in% c("1","3")) {
        sv <- gv(row, "schol_pub_status")
        if (nzchar(sv)) {
          sty <- unname(.STATUS_BADGE[sv]);  if (is.na(sty)) sty <- "background:#e9ecef;color:#495057;"
          slb <- unname(.STATUS_LABELS[sv]); if (is.na(slb)) slb <- sv
          tags$span(class = "badge",
            style = paste0(sty, " font-size:0.72rem; font-weight:600; margin-left:6px;"), slb)
        } else NULL
      } else NULL

      div(class = "py-2", style = "border-bottom:1px solid #f0f2f5;",
        div(class = "d-flex align-items-start gap-2",
          if (star) tags$i(class = "bi bi-star-fill", style = "color:#f0ad4e; font-size:0.85rem; margin-top:2px;"),
          div(style = "flex:1;",
            div(tags$span(style = "font-weight:600; color:#2c3e50; font-size:0.96rem;", title_v),
                status_badge,
                if (first) tags$span(class = "badge",
                  style = "background:#eef2ff; color:#3730a3; font-size:0.72rem; font-weight:600; margin-left:6px;",
                  "First author")),
            if (nzchar(authors)) div(style = "font-size:0.87rem; color:#6c757d;", authors),
            if (nzchar(meta)) div(style = "font-size:0.87rem; color:#495057; font-style:italic;", meta),
            if (nzchar(coll)) tags$span(class = "badge",
              style = "background:#f8f4ff; color:#4a1d96; font-size:0.72rem; margin-top:3px;",
              tags$i(class = "bi bi-collection me-1"), coll)),
          div(class = "d-flex align-items-start gap-1",
            act_btn("edit",   inst, rid, "bi-pencil", "Edit",   "#0d6efd"),
            act_btn("delete", inst, rid, "bi-trash",  "Delete", "var(--gmed-error-red)"))))
    }

    output$entries_panel <- renderUI({
      df <- schol_data()
      gb <- input$group_by %||% "type"
      has_rows <- !(is.null(df) || nrow(df) == 0)

      is_cat <- if (has_rows) nzchar(vapply(seq_len(nrow(df)), function(i) gv(df[i, ], "schol_work_type"), character(1))) else logical(0)
      legacy_content <- if (has_rows) vapply(seq_len(nrow(df)), function(i) {
        r <- df[i, ]
        any(nzchar(vapply(c("schol_type","schol_output_type","schol_cit","schol_citation",
                            "schol_res","schol_pres_conf","schol_venue","schol_qi","schol_comm"),
                          function(f) gv(r, f), character(1))))
      }, logical(1)) else logical(0)
      cat_df    <- if (has_rows) df[is_cat, , drop = FALSE] else data.frame()
      legacy_df <- if (has_rows) df[!is_cat & legacy_content, , drop = FALSE] else data.frame()

      # Most-meaningful entries float to the top of any group.
      sort_starred <- function(rows) {
        if (!"schol_meaningful" %in% names(rows) || nrow(rows) == 0) return(rows)
        rows[order(as.character(rows$schol_meaningful) != "1"), , drop = FALSE]
      }
      render_group <- function(rows, heading, upper = TRUE) {
        rows <- sort_starred(rows)
        div(class = "mb-3",
          div(style = paste0("font-weight:700; color:var(--gmed-primary); font-size:0.86rem; ",
                      if (upper) "text-transform:uppercase; letter-spacing:0.03em; " else "", "margin-bottom:4px;"),
              heading,
              tags$span(style = "color:#adb5bd; font-weight:500;", paste0(" (", nrow(rows), ")"))),
          lapply(seq_len(nrow(rows)), function(i) render_entry(rows[i, ])))
      }

      cat_sections <- if (nrow(cat_df) == 0) NULL
        else if (gb == "collection") {
          cvec <- if ("schol_collection" %in% names(cat_df)) as.character(cat_df$schol_collection) else rep("", nrow(cat_df))
          cvec[is.na(cvec)] <- ""
          colls <- sort(unique(cvec[nzchar(cvec)]))
          secs <- lapply(colls, function(cl)
            render_group(cat_df[cvec == cl, , drop = FALSE],
                         tagList(tags$i(class = "bi bi-collection me-1"), cl), upper = FALSE))
          ung <- cat_df[!nzchar(cvec), , drop = FALSE]
          if (nrow(ung) > 0) secs <- c(secs, list(render_group(ung, "Not in a collection", upper = FALSE)))
          secs
        } else {
          lapply(names(.TYPE_LABELS), function(tp) {
            rows <- cat_df[as.character(cat_df$schol_work_type) == tp, , drop = FALSE]
            if (nrow(rows) == 0) return(NULL)
            render_group(rows, unname(.TYPE_LABELS[tp]))
          })
        }

      legacy_section <- if (nrow(legacy_df) > 0) {
        items <- lapply(seq_len(nrow(legacy_df)), function(i) {
          r <- legacy_df[i, ]
          li <- gv(r, "redcap_repeat_instance"); lrid <- gv(r, "record_id")
          desc <- gv(r, "schol_title"); if (!nzchar(desc)) desc <- gv(r, "schol_cit")
          if (!nzchar(desc)) desc <- gv(r, "schol_citation"); if (!nzchar(desc)) desc <- gv(r, "schol_res")
          if (!nzchar(desc)) desc <- "(legacy entry)"
          div(class = "py-2 d-flex align-items-center justify-content-between gap-2",
              style = "border-bottom:1px solid #f0f2f5;",
            div(style = "font-size:0.88rem; color:#495057;", trunc(desc, 90)),
            div(class = "d-flex align-items-center gap-2",
              tags$span(class = "badge",
                style = "background:#fff3cd; color:#664d03; font-size:0.72rem; font-weight:600;",
                tags$i(class = "bi bi-exclamation-triangle me-1"), "Needs review"),
              act_btn("recategorize", li, lrid, "bi-arrow-repeat", "Recategorize", "#6f42c1"),
              act_btn("delete",       li, lrid, "bi-trash",        "Delete",       "var(--gmed-error-red)")))
        })
        div(class = "mt-3 pt-2", style = "border-top:1px dashed #dee2e6;",
          div(style = "font-weight:700; color:#8a6d3b; font-size:0.85rem; margin-bottom:4px;",
              tags$i(class = "bi bi-clock-history me-1"),
              paste0("Older entries to recategorize (", nrow(legacy_df), ")")),
          tags$p(class = "text-muted", style = "font-size:0.8rem; margin-bottom:6px;",
                 "These predate the ERAS format. Use ", tags$i(class = "bi bi-arrow-repeat"),
                 " to recategorize each into the new format."),
          items)
      } else NULL

      body <- if (is.null(cat_sections) && is.null(legacy_section))
          div(class = "text-muted fst-italic", style = "font-size:0.85rem; padding:16px 0;",
              tags$i(class = "bi bi-inbox me-2"),
              "No scholarly work yet — click below to add your first entry.")
        else tagList(cat_sections, legacy_section)

      # Group-by toggle — shown once there is more than one categorized entry.
      seg_btn <- function(val, label) tags$button(type = "button",
        class = paste0("btn ", if (identical(gb, val)) "btn-primary" else "btn-outline-secondary"),
        style = "font-size:0.75rem; padding:2px 10px;",
        onclick = sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})", ns("group_by"), val), label)
      group_toggle <- if (nrow(cat_df) > 1)
        div(class = "d-flex justify-content-end align-items-center gap-2 mb-2",
          tags$span(class = "text-muted", style = "font-size:0.8rem;", "Group by:"),
          div(class = "btn-group btn-group-sm", role = "group",
            seg_btn("type", "Type"), seg_btn("collection", "Collection"))) else NULL

      div(class = "card border-0 shadow-sm mb-3", style = "border-radius:8px;",
        div(class = "card-header border-0 d-flex align-items-center gap-2",
            style = "background:#f8fafc; border-radius:8px 8px 0 0; padding:12px 18px;",
          tags$i(class = "bi bi-award-fill", style = "color:var(--gmed-primary); font-size:1rem;"),
          tags$span(style = "font-weight:700; color:var(--gmed-primary); font-size:0.95rem;", title)),
        div(class = "card-body",
          if (isTRUE(show_intro))
            div(class = "mb-3 px-3 py-2",
                style = "background:#f8fafc; border-left:3px solid var(--gmed-primary); border-radius:4px;",
              tags$p(class = "mb-2", style = "font-size:0.85rem; color:#495057;",
                "Log your scholarly ", tags$strong("outputs"), " here — grouped by the ERAS ",
                "categories so entries copy straight into your application. Use ",
                tags$strong("Download CV (Word)"), " for a formatted copy. QI, patient-safety, ",
                "and committee work live in your Self Evaluation, not here."),
              tags$ul(class = "mb-0 ps-3", style = "font-size:0.83rem; color:#495057;",
                tags$li(class = "mb-1",
                  tags$i(class = "bi bi-star-fill", style = "color:#f0ad4e;"),
                  tags$strong(" Most meaningful"),
                  " — star up to 3 works you'd most want highlighted (as ERAS allows). ",
                  "They pin to the top of each group and are flagged in your CV."),
                tags$li(class = "mb-1",
                  tags$i(class = "bi bi-collection", style = "color:#4a1d96;"),
                  tags$strong(" Collections"),
                  " — tag related works with a project name, then switch ",
                  tags$em("Group by"), " to ", tags$em("Collection"), " to see them together. ",
                  "When adding, pick an existing project from the list rather than retyping it."),
                tags$li(
                  tags$i(class = "bi bi-pencil", style = "color:#0d6efd;"), tags$strong(" Edit"),
                  " and ",
                  tags$i(class = "bi bi-trash", style = "color:var(--gmed-error-red);"), tags$strong(" Delete"),
                  " sit on each entry — deleting is permanent. ",
                  tags$i(class = "bi bi-arrow-repeat", style = "color:#6f42c1;"),
                  tags$strong(" Recategorize"),
                  " converts an older entry into the new format, pre-filled from what you'd entered before."))),
          group_toggle,
          body))
    })

    # Expose per-row actions to the orchestrator.
    reactive(action_val())

  }) # end moduleServer
}
