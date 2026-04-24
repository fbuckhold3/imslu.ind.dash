# server.R ─ IMSLU Resident Dashboard
# Handles: data loading, auth, nav routing, module wiring.

server <- function(input, output, session) {

  # ── App state ──────────────────────────────────────────────────────────────
  values <- reactiveValues(
    authenticated = FALSE,
    resident      = NULL,   # list of resident fields from REDCap
    nav           = "login" # "login" | "home" | <block id>
  )

  # ── Two-phase data loading ─────────────────────────────────────────────────
  # Phase 1 (~4 s): fetch residents only — unblocks login UI
  # Phase 2 (~25 s): fetch full data — triggered after Phase 1 completes
  residents_ready <- reactiveVal(FALSE)
  data_ready      <- reactiveVal(FALSE)

  # Phase 1 — blocking but fast; dismisses loading overlay when done
  observe({
    tryCatch(load_app_residents(), error = function(e) {
      message("Phase 1 error: ", e$message)
    })
    residents_ready(TRUE)
  })

  # Phase 2 — background R worker populates globals; poller detects completion
  observeEvent(residents_ready(), once = TRUE, {
    if (!is.null(app_data_store)) {
      data_ready(TRUE)
      return()
    }

    token     <- app_config$rdm_token
    url       <- app_config$redcap_url
    fac_token <- app_config$fac_token

    message("Phase 2: loading full RDM data (background)...")

    # Only assign to globals in the callback — no reactive state touched here.
    # data_ready() is set by the poller below, which runs in the proper domain.
    future_promise({
      list(
        rdm = gmed::load_rdm_complete(rdm_token = token, redcap_url = url,
                                      verbose = FALSE, raw_or_label = "raw"),
        fac = gmed::load_faculty_roster(fac_token = fac_token, redcap_url = url)
      )
    }, globals  = list(token = token, url = url, fac_token = fac_token),
       packages = c("gmed", "dplyr", "httr", "jsonlite"),
       seed     = NULL) %...>% (function(result) {
      app_data_store       <<- result$rdm
      faculty_roster_store <<- result$fac
    }) %...!% (function(err) {
      message("Phase 2 error: ", conditionMessage(err))
      # Leave app_data_store NULL — poller will detect and unblock anyway
    })

    NULL
  })

  # Poller: runs inside the reactive domain; sets data_ready once globals land
  observe({
    req(residents_ready())
    if (isolate(data_ready())) return()
    invalidateLater(500)
    if (!is.null(app_data_store)) {
      message("Phase 2 done — ", nrow(app_data_store$residents), " residents, ",
              nrow(app_data_store$all_forms$assessment), " assessments.")
      data_ready(TRUE)
    }
  })

  # Hide loading overlay as soon as Phase 1 completes (residents available)
  output$overlay_hide <- renderUI({
    if (!residents_ready()) return(NULL)
    tags$style("#loading_overlay { display: none !important; }")
  })

  # ── Auto-fill access code from URL query param ?code=XXX ──────────────────
  # Allows deep-linking from CCC dashboard: open this app with resident pre-authed
  observeEvent(residents_ready(), once = TRUE, {
    query <- parseQueryString(session$clientData$url_search)
    code  <- query[["code"]]
    if (!is.null(code) && nchar(trimws(code)) > 0) {
      safe_code <- gsub("'", "\\'", trimws(code), fixed = TRUE)
      shinyjs::runjs(sprintf(
        "setTimeout(function(){
           var el = document.getElementById('auth-access_code');
           if (el && el.value === '') {
             el.value = '%s';
             Shiny.setInputValue('auth-access_code_btn', Math.random(), {priority:'event'});
           }
         }, 400);",
        safe_code
      ))
    }
  })

  # ── Authentication ─────────────────────────────────────────────────────────
  # Phase 1 residents are enough for auth; Phase 2 data used by modules.
  auth_result <- mod_auth_server(
    id          = "auth",
    residents_r = reactive({
      req(residents_ready())
      # Prefer Phase 2 residents (already merged + leveled via full load)
      # but fall back to Phase 1 store while Phase 2 is still running.
      if (data_ready()) app_data_store$residents else residents_store
    })
  )

  observeEvent(auth_result(), {
    res <- auth_result()
    req(isTRUE(res$success))
    values$authenticated <- TRUE
    values$resident      <- res$resident_info
    values$nav           <- "home"
  })

  # ── Phase 3: per-resident data load (triggered on login, ~3–5 s) ──────────
  # Fetches only this resident's records across all forms.  Much faster than
  # waiting for Phase 2 (full cohort).  Modules become usable immediately.
  resident_rv <- reactiveValues(store = NULL, ready = FALSE)

  observeEvent(values$resident, {
    req(values$resident)
    rec_id    <- values$resident$record_id
    token     <- app_config$rdm_token
    url       <- app_config$redcap_url

    message("Phase 3: loading data for resident ", rec_id, "...")

    future_promise({
      gmed::load_rdm_for_resident(
        rdm_token   = token,
        redcap_url  = url,
        record_id   = rec_id,
        raw_or_label = "raw"
      )
    }, globals  = list(token = token, url = url, rec_id = rec_id),
       packages = c("gmed", "httr", "jsonlite"),
       seed     = NULL) %...>% (function(result) {
      resident_rv$store <- result
      resident_rv$ready <- TRUE
      message("Phase 3 done — resident ", rec_id)
    }) %...!% (function(err) {
      message("Phase 3 error for ", rec_id, ": ", conditionMessage(err))
      # Fall through to Phase 2 data via rdm_data()
    })

    NULL
  })

  # ── Navigation ─────────────────────────────────────────────────────────────
  observeEvent(input$nav_block, {
    req(values$authenticated)
    values$nav <- input$nav_block
  })

  observeEvent(input$nav_back, {
    values$nav <- "home"
  })

  observeEvent(input$sign_out, {
    values$authenticated <- FALSE
    values$resident      <- NULL
    values$nav           <- "login"
  })

  # ── Shared reactives passed to every module ────────────────────────────────
  # Priority: Phase 3 (per-resident, fast) > Phase 2 (full cohort, slow).
  # When Phase 2 finishes, enriches Phase 3 data with cohort stats in-place.
  rdm_data <- reactive({
    req(values$authenticated)

    if (resident_rv$ready) {
      rd <- resident_rv$store
      # Splice in cohort stats from Phase 2 if available
      if (data_ready() && !is.null(app_data_store)) {
        rd$milestone_workflow <- app_data_store$milestone_workflow
        rd$historical_medians <- app_data_store$historical_medians
      }
      rd
    } else {
      # Phase 3 not yet done — wait for Phase 2 as fallback
      req(data_ready())
      app_data_store
    }
  })

  resident_id    <- reactive({ req(values$resident); values$resident$record_id })
  faculty_roster <- reactive({ req(values$authenticated, data_ready()); faculty_roster_store })

  # ── Module servers (initialized once; guard internally with req()) ─────────
  mod_evaluations_server( "evaluations",  rdm_data = rdm_data, resident_id = resident_id)
  mod_learning_server(    "learning",     rdm_data = rdm_data, resident_id = resident_id)
  mod_milestones_server(  "milestones",   rdm_data = rdm_data, resident_id = resident_id)
  mod_scholarship_server( "scholarship",  rdm_data = rdm_data, resident_id = resident_id)
  mod_faculty_eval_server("faculty_eval", rdm_data = rdm_data, resident_id = resident_id,
                          faculty_roster_r = faculty_roster)
  mod_self_eval_server(   "self_eval",    rdm_data = rdm_data, resident_id = resident_id)
  mod_schedule_server(    "schedule")
  mod_resources_server(   "resources")

  # ── Main view ──────────────────────────────────────────────────────────────
  output$main_view <- renderUI({

    # ── LOGIN ────────────────────────────────────────────────────────────────
    if (!values$authenticated) {
      return(mod_auth_page_ui())
    }

    state <- values$nav

    # ── Shared resident metadata ──────────────────────────────────────────────
    res        <- values$resident
    res_name   <- if (!is.null(res[["name"]])) res[["name"]] else "Resident"
    res_level  <- if (!is.null(res[["Level"]])) res[["Level"]] else ""
    coach_code <- res[["coach"]]
    coach_name <- if (!is.null(coach_code) && !is.na(coach_code) && nzchar(as.character(coach_code)))
                    get_coach_name_from_code(coach_code) else NULL

    sign_out_btn <- tags$button(
      class   = "btn btn-sm btn-outline-danger ms-2",
      onclick = "Shiny.setInputValue('sign_out', Math.random(), {priority:'event'})",
      tags$i(class = "bi bi-box-arrow-right me-1"), "Sign Out"
    )

    # ── HOME ─────────────────────────────────────────────────────────────────
    if (state == "home") {
      level_str <- if (nzchar(res_level)) paste0(" \u00b7 ", res_level) else ""
      coach_str <- if (!is.null(coach_name)) paste0(" \u00b7 Coach: ", coach_name) else ""

      return(tagList(
        div(class = "d-flex justify-content-end mb-n2", sign_out_btn),
        gmed_nav_blocks(
          blocks   = resident_nav_blocks,
          title    = paste0("Welcome, ", res_name),
          subtitle = paste0("Internal Medicine \u00b7 Saint Louis University", level_str, coach_str),
          input_id = "nav_block"
        )
      ))
    }

    # ── SECTION ──────────────────────────────────────────────────────────────
    block_info    <- Filter(function(b) b$id == state, resident_nav_blocks)
    section_label <- if (length(block_info)) block_info[[1]]$label else state
    section_icon  <- if (length(block_info)) block_info[[1]]$icon  else "grid"

    tagList(

      # Back / breadcrumb bar
      div(
        class = "d-flex align-items-center mb-4",
        style = paste(
          "background: white;",
          "border-radius: 10px;",
          "padding: 10px 16px;",
          "box-shadow: 0 2px 8px rgba(0,61,92,0.07);",
          "border-left: 4px solid var(--ssm-secondary-blue);"
        ),
        tags$button(
          class   = "btn btn-sm btn-outline-secondary me-3",
          onclick = "Shiny.setInputValue('nav_back', Math.random(), {priority: 'event'})",
          tags$i(class = "bi bi-arrow-left me-1"), "Home"
        ),
        tags$i(
          class = paste0("bi bi-", section_icon, " me-2"),
          style = "color: var(--ssm-secondary-blue); font-size: 1.15rem;"
        ),
        tags$span(
          section_label,
          style = "font-weight: 700; font-size: 1.1rem; color: var(--ssm-primary-blue);"
        ),
        # Right side: resident name + coach + sign out
        div(
          class = "ms-auto d-flex align-items-center gap-2",
          div(
            style = "text-align:right; font-size:0.875rem; line-height:1.4;",
            tags$div(style = "font-weight:600; color:var(--ssm-primary-blue);", res_name),
            if (!is.null(coach_name))
              tags$div(style = "font-size:0.8rem; color:#546e7a;", paste0("Coach: ", coach_name))
          ),
          sign_out_btn
        )
      ),

      # Module content
      switch(state,
        evaluations  = mod_evaluations_ui("evaluations"),
        learning     = mod_learning_ui("learning"),
        milestones   = mod_milestones_ui("milestones"),
        scholarship  = mod_scholarship_ui("scholarship"),
        faculty_eval = mod_faculty_eval_ui("faculty_eval"),
        self_eval    = mod_self_eval_ui("self_eval"),
        schedule     = mod_schedule_ui("schedule"),
        resources    = mod_resources_ui("resources"),
        div("Unknown section")
      )
    )
  })
}
