# server.R ─ IMSLU Resident Dashboard
# Handles: data loading, auth, nav routing, module wiring.

server <- function(input, output, session) {

  # ── App state ──────────────────────────────────────────────────────────────
  values <- reactiveValues(
    authenticated = FALSE,
    resident      = NULL,   # list of resident fields from REDCap
    nav           = "login" # "login" | "home" | <block id>
  )

  # ── Data loading (fires once on startup) ───────────────────────────────────
  data_ready <- reactiveVal(FALSE)

  observe({
    load_app_data()
    data_ready(TRUE)
  })

  # ── Auto-fill access code from URL query param ?code=XXX ──────────────────
  # Allows deep-linking from CCC dashboard: open this app with resident pre-authed
  observeEvent(data_ready(), {
    req(data_ready())
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
  }, once = TRUE)

  # ── Authentication ─────────────────────────────────────────────────────────
  auth_result <- mod_auth_server(
    id          = "auth",
    residents_r = reactive({
      req(data_ready())
      app_data_store$residents
    })
  )

  observeEvent(auth_result(), {
    res <- auth_result()
    req(isTRUE(res$success))
    values$authenticated <- TRUE
    values$resident      <- res$resident_info
    values$nav           <- "home"
  })

  # ── Navigation ─────────────────────────────────────────────────────────────
  observeEvent(input$nav_block, {
    req(values$authenticated)
    values$nav <- input$nav_block
  })

  observeEvent(input$nav_back, {
    values$nav <- "home"
  })

  # ── Shared reactives passed to every module ────────────────────────────────
  rdm_data    <- reactive({ req(values$authenticated); app_data_store })
  resident_id <- reactive({ req(values$resident); values$resident$record_id })

  # ── Module servers (initialized once; guard internally with req()) ─────────
  mod_evaluations_server( "evaluations",  rdm_data = rdm_data, resident_id = resident_id)
  mod_learning_server(    "learning",     rdm_data = rdm_data, resident_id = resident_id)
  mod_milestones_server(  "milestones",   rdm_data = rdm_data, resident_id = resident_id)
  mod_scholarship_server( "scholarship",  rdm_data = rdm_data, resident_id = resident_id)
  mod_faculty_eval_server("faculty_eval", rdm_data = rdm_data, resident_id = resident_id)
  mod_self_eval_server(   "self_eval",    rdm_data = rdm_data, resident_id = resident_id)
  mod_schedule_server(    "schedule")
  mod_resources_server(   "resources")

  # ── Main view ──────────────────────────────────────────────────────────────
  output$main_view <- renderUI({

    # ── LOGIN ────────────────────────────────────────────────────────────────
    if (!values$authenticated) {
      return(tagList(

        # Full-width brand header
        div(
          style = paste(
            "background: #003d5c; color: white;",
            "padding: 18px 32px;",
            "display: flex; align-items: center; gap: 16px;",
            "margin: -24px -24px 40px -24px;"   # bleed past container padding
          ),
          div(
            style = "background: rgba(255,255,255,0.15); font-size:0.68rem;
                     font-weight:700; letter-spacing:0.12em; padding: 4px 10px;
                     border-radius: 3px; white-space: nowrap;",
            "SSM HEALTH \u00b7 SLUCARE"
          ),
          tags$h1(
            "IMSLU Resident Dashboard",
            style = "margin:0; font-size:1.2rem; font-weight:700; letter-spacing:0.01em;"
          ),
          div(
            style = "margin-left:auto; font-size:0.8rem; opacity:0.65;",
            "Internal Medicine \u00b7 Saint Louis University"
          )
        ),

        # Centered login card — col-lg-8 like the self-assessment
        div(
          class = "row justify-content-center",
          div(
            class = "col-lg-8 col-md-10 col-12",
            div(
              class = "card shadow-lg",
              div(
                class = "card-body p-5",

                # Welcome header
                div(
                  class = "text-center mb-4",
                  tags$h2(
                    class = "mb-2",
                    style = "color: #003d5c; font-weight: 700;",
                    tags$i(class = "bi bi-person-circle me-2",
                           style = "color: #0066a1;"),
                    "Welcome"
                  ),
                  tags$p(
                    class = "lead text-muted",
                    "Access your evaluations, milestones, learning plan, and more."
                  ),
                  tags$hr()
                ),

                # Disclaimer
                div(
                  style = paste(
                    "background: #f8fafc;",
                    "border: 1px solid #dde5ed;",
                    "border-left: 4px solid #0066a1;",
                    "border-radius: 4px;",
                    "padding: 16px 18px;",
                    "font-size: 0.82rem;",
                    "color: #4a5568;",
                    "line-height: 1.7;",
                    "margin-bottom: 28px;"
                  ),
                  paste(
                    "This dashboard is for residents in the IMSLU Internal Medicine",
                    "Residency Program to access their evaluations, competency progress,",
                    "and learning plans. By entering your access code you acknowledge that",
                    "this data is intended solely for the named resident and authorized",
                    "program leadership. Unauthorized access or distribution is prohibited.",
                    "All activity within this system may be monitored and logged."
                  )
                ),

                # Access code form
                # Input IDs match what mod_auth_server("auth") expects
                div(
                  tags$label(
                    "Access Code",
                    style = "font-size:0.82rem; font-weight:600; color:#2d3748; margin-bottom:6px; display:block;"
                  ),
                  tags$input(
                    id          = "auth-access_code",
                    type        = "password",
                    placeholder = "Enter your access code",
                    autocomplete = "off",
                    style = paste(
                      "width:100%; padding:10px 14px;",
                      "border:1.5px solid #cdd5df; border-radius:5px;",
                      "font-size:1rem; letter-spacing:0.08em;",
                      "font-family:inherit; outline:none; box-sizing:border-box;"
                    )
                  ),
                  tags$button(
                    "Sign In",
                    style = paste(
                      "width:100%; margin-top:14px; padding:12px;",
                      "background:#003d5c; color:white; border:none;",
                      "border-radius:5px; font-size:0.95rem; font-weight:600;",
                      "font-family:inherit; cursor:pointer;",
                      "transition:background 0.2s;"
                    ),
                    onmouseover = "this.style.background='#0066a1'",
                    onmouseout  = "this.style.background='#003d5c'",
                    onclick = "Shiny.setInputValue('auth-access_code_btn', Math.random(), {priority:'event'})"
                  ),
                  # Enter key support
                  tags$script(HTML(
                    "document.getElementById('auth-access_code').addEventListener('keypress', function(e){
                       if(e.key==='Enter') Shiny.setInputValue('auth-access_code_btn', Math.random(), {priority:'event'});
                    });"
                  )),
                  uiOutput("auth-access_code_error")
                )
              )
            )
          )
        )
      ))
    }

    state <- values$nav

    # ── HOME ─────────────────────────────────────────────────────────────────
    if (state == "home") {
      res   <- values$resident
      name  <- if (!is.null(res[["name"]])) res[["name"]] else "Resident"
      level <- if (!is.null(res[["Level"]])) paste0(" \u00b7 ", res[["Level"]]) else ""

      return(
        gmed_nav_blocks(
          blocks   = resident_nav_blocks,
          title    = paste0("Welcome, ", name),
          subtitle = paste0("Internal Medicine \u00b7 Saint Louis University", level),
          input_id = "nav_block"
        )
      )
    }

    # ── SECTION ──────────────────────────────────────────────────────────────
    block_info    <- Filter(function(b) b$id == state, resident_nav_blocks)
    section_label <- if (length(block_info)) block_info[[1]]$label else state
    section_icon  <- if (length(block_info)) block_info[[1]]$icon  else "grid"

    tagList(

      # Back / breadcrumb bar
      div(
        class = "d-flex align-items-center mb-4 pb-3",
        style = "border-bottom: 1px solid var(--ssm-border);",
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
