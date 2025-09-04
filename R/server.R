# ============================================================================
# SIMPLIFIED SERVER.R - Individual Resident Dashboard
# Uses gmed package for all heavy lifting
# ============================================================================

server <- function(input, output, session) {
  
  # ============================================================================
  # DATA LOADING (Simple!)
  # ============================================================================
  
  # Load data once using gmed package
  app_data <- reactive({
    load_app_data()  # Defined in global.R, calls gmed::load_rdm_simple()
  })
  
  # ============================================================================
  # ACCESS CODE LOGIC
  # ============================================================================
  
  # Get access code from URL or input
  access_code <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    code <- query$code %||% input$access_code_input %||% ""
    trimws(code)
  })
  
  # Get resident info
  resident_info <- reactive({
    req(access_code())
    get_resident_by_code(access_code())  # Simple helper from global.R
  })
  
  # ============================================================================
  # UI DISPLAY LOGIC
  # ============================================================================
  
  # Show/hide sections based on valid access code
  observe({
    valid_resident <- !is.null(resident_info())
    
    shinyjs::toggle("resident_info_banner", condition = valid_resident)
    shinyjs::toggle("assessment_section", condition = valid_resident) 
    shinyjs::toggle("module_cards_section", condition = valid_resident)
    shinyjs::toggle("access_code_error", condition = !valid_resident && nzchar(access_code()))
  })
  
  # Resident name display
  output$resident_name <- renderText({
    if (!is.null(resident_info())) {
      resident_info()$name
    } else if (nzchar(access_code())) {
      "Invalid Access Code"
    } else {
      "Enter Access Code"
    }
  })
  
  # Coach name display  
  output$coach_name <- renderText({
    if (!is.null(resident_info())) {
      coach <- resident_info()$coach %||% resident_info()$coach_name %||% "Coach TBD"
      if (coach != "Coach TBD") paste("Coach:", coach) else coach
    } else {
      ""
    }
  })
  
  # ============================================================================
  # GMED MODULE INTEGRATION (Clean!)
  # ============================================================================
  


  assessment_viz_server(
    "main_assessment",
    data = reactive({
      if (!is.null(resident_info())) {
        # Combine all forms with source_form column
        all_data <- bind_rows(app_data()$all_forms, .id = "source_form")
        
        # ONLY ensure columns exist - DON'T override them!
        if (!"ass_level" %in% names(all_data)) {
          all_data$ass_level <- NA_integer_
        }
        if (!"fac_eval_level" %in% names(all_data)) {
          all_data$fac_eval_level <- NA_integer_
        }
        
        # REMOVE the mutate() block that was overriding the levels!
        # The ass_level and fac_eval_level are already correctly calculated
        
        all_data
      } else {
        data.frame()
      }
    }),
    record_id = reactive({
      if (!is.null(resident_info())) resident_info()$record_id else NULL
    }),
    resident_name = reactive({
      if (!is.null(resident_info())) resident_info()$name else NULL
    })
  )
  
  # Assessment module container - always render the UI, let the module handle empty states
  output$assessment_module_container <- renderUI({
    assessment_viz_ui("main_assessment", "Assessment Dashboard")
  })
  
  # Plus/delta module - this one is fine as-is
  mod_plus_delta_table_server(
    "resident_plus_delta",
    rdm_data = reactive(app_data()$assessment_data),
    record_id = reactive(if(!is.null(resident_info())) resident_info()$record_id else NULL)
  )
  

  
  # ============================================================================
  # MODAL HANDLING (Simplified)
  # ============================================================================
  
  # Modal title
  output$modal_title <- renderText({
    switch(input$module_selected %||% "",
           "plus_delta" = "Plus / Delta Feedback",
           "evaluation_data" = "Evaluation Data", 
           "milestone_plots" = "Milestone Plots",
           "learning_plan" = "Learning Plan",
           "scholarship" = "Scholarship",
           "schedule_data" = "Schedule Data",
           "peer_evaluations" = "Peer Evaluations",
           "Unknown Module"
    )
  })

  
  # Modal content - replace the milestone_plots case
  output$selected_module_ui <- renderUI({
    req(input$module_selected)
    
    if (is.null(resident_info())) {
      return(div(class = "alert alert-warning",
                 "Please enter a valid access code to view module data."))
    }
    
    switch(input$module_selected,
           "plus_delta" = {
             # Just the gmed module - no extra wrapper needed
             mod_plus_delta_table_ui("resident_plus_delta")
           },
           
           "evaluation_data" = {
             # When you build this module, it will be similarly simple
             div(
               h4("Evaluation Data", class = "text-primary mb-3"),
               div(class = "alert alert-secondary", "Coming Soon"),
               p("Will use: gmed::evaluation_data_ui() when implemented")
             )
           },
           
           "milestone_plots" = {
             div(
               h4("Milestone Assessment", class = "text-warning mb-3"),
               # Period display
               div(class = "alert alert-info mb-3",
                   icon("calendar"), " Current Period: ", textOutput("current_period_display", inline = TRUE)),
               p("Complete your milestone self-assessment and compare with ACGME format.", 
                 class = "text-muted mb-4"),
               
               # Side-by-side milestone modules - FIXED SIZING
               fluidRow(
                 column(
                   width = 6,
                   div(
                     class = "milestone-module-container",
                     style = "min-height: 600px; height: 600px;", # ADDED: Fixed height
                     h5("Self-Assessment Milestones", class = "text-primary mb-3"),
                     milestone_dashboard_ui("modal_self_milestone", 
                                            milestone_type = "self", 
                                            height = "550px") # INCREASED height
                   )
                 ),
                 column(
                   width = 6,
                   div(
                     class = "milestone-module-container",
                     style = "min-height: 600px; height: 600px;", # ADDED: Fixed height
                     h5("ACGME Milestones", class = "text-success mb-3"),
                     milestone_dashboard_ui("modal_acgme_milestone", 
                                            milestone_type = "acgme", 
                                            height = "550px") # INCREASED height
                   )
                 )
               )
             )
           },
          
           
           "learning_plan" = {
             div(
               h4("Learning Plan", class = "text-success mb-3"), 
               div(class = "alert alert-secondary", "Coming Soon"),
               p("Will use: gmed::learning_plan_ui() when implemented")
             )
           },
           
           "scholarship" = {
             div(
               h4("Scholarship", class = "text-warning mb-3"),
               div(class = "alert alert-secondary", "Coming Soon"), 
               p("Will use: gmed::scholarship_ui() when implemented")
             )
           },
           
           "schedule_data" = {
             div(
               h4("Schedule Data", class = "text-info mb-3"),
               div(class = "alert alert-secondary", "Coming Soon"),
               p("Will use: gmed::schedule_ui() when implemented")
             )
           },
           
           "peer_evaluations" = {
             div(
               h4("Peer Evaluations", class = "text-secondary mb-3"),
               div(class = "alert alert-secondary", "Coming Soon"),
               p("Will use: gmed::peer_eval_ui() when implemented")
             )
           },
           
           # Default case
           {
             div(class = "alert alert-warning", "Module not yet implemented.")
           }
    )
  })
  
  # ============================================================================
  # MILESTONE MODULES FOR MODAL (FIXED)
  # ============================================================================
  
  # Create milestone workflow results using clean data
  milestone_workflow_results <- reactive({
    req(app_data())
    
    tryCatch({
      message("Creating milestone workflow with clean data structure...")
      
      # Use the clean load_rdm_complete data structure
      workflow_results <- create_milestone_workflow_from_dict(
        all_forms = app_data()$all_forms,      
        data_dict = app_data()$data_dict,      
        resident_data = app_data()$residents,  
        verbose = TRUE  # Enable verbose for debugging
      )
      
      message("Milestone workflow created successfully")
      return(workflow_results)
      
    }, error = function(e) {
      message("Error creating milestone workflow: ", e$message)
      return(NULL)
    })
  })
  
  # FIXED: Better period selection and module initialization
  observe({
    req(resident_info(), milestone_workflow_results())
    
    message("=== MILESTONE MODULE INITIALIZATION ===")
    
    # Get resident info
    resident_id <- resident_info()$record_id
    resident_level <- resident_info()$Level %||% "Unknown"
    
    message("Initializing for resident: ", resident_id, " (", resident_level, ")")
    
    # FIXED: Better period selection with more debugging
    tryCatch({
      # Use the gmed function but with error handling
      most_recent_period <- gmed::get_most_recent_period_for_resident(
        resident_info(), 
        app_data(),
        verbose = TRUE
      )
      
      message("Selected period: ", most_recent_period)
      
    }, error = function(e) {
      message("Error getting most recent period: ", e$message)
      # Better fallback period selection
      most_recent_period <- switch(resident_level,
                                   "Intern" = "End Intern",
                                   "PGY2" = "End PGY2", 
                                   "PGY3" = "End PGY2",  # Use End PGY2 as fallback
                                   "End Intern")  # Default fallback
      
      message("Using fallback period: ", most_recent_period)
    })
    
    # FIXED: Initialize modules with better reactive handling
    tryCatch({
      message("Initializing self milestone module...")
      gmed::milestone_dashboard_server(
        "modal_self_milestone", 
        milestone_results = milestone_workflow_results,
        record_id = reactive({
          message("Self module record_id: ", resident_id)
          resident_id
        }),
        period = reactive({
          message("Self module period: ", most_recent_period)
          most_recent_period
        }),
        milestone_type = "self",
        resident_data = reactive({
          residents_data <- app_data()$residents
          message("Self module residents data: ", nrow(residents_data), " residents")
          residents_data
        })
      )
      message("Self milestone module initialized")
      
    }, error = function(e) {
      message("Error initializing self milestone module: ", e$message)
      message("Traceback: ", paste(traceback(), collapse = "\n"))
    })
    
    tryCatch({
      message("Initializing ACGME milestone module...")
      gmed::milestone_dashboard_server(
        "modal_acgme_milestone",
        milestone_results = milestone_workflow_results,
        record_id = reactive({
          message("ACGME module record_id: ", resident_id)
          resident_id
        }),
        period = reactive({
          message("ACGME module period: ", most_recent_period)
          most_recent_period
        }),
        milestone_type = "acgme", 
        resident_data = reactive({
          residents_data <- app_data()$residents
          message("ACGME module residents data: ", nrow(residents_data), " residents")
          residents_data
        })
      )
      message("ACGME milestone module initialized")
      
    }, error = function(e) {
      message("Error initializing ACGME milestone module: ", e$message)
      message("Traceback: ", paste(traceback(), collapse = "\n"))
    })
    
    # Display which period is being shown
    output$current_period_display <- renderText({
      paste("Displaying milestone data for:", most_recent_period)
    })
    
    message("=== MILESTONE MODULE INITIALIZATION COMPLETE ===")
  })
  
}
