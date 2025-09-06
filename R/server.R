# ============================================================================
# IMSLU INDIVIDUAL DASHBOARD - SERVER.R (COMPLETE FIXED VERSION)
# Fixed data loading conflicts, proper gmed integration, and debug panel
# ============================================================================

server <- function(input, output, session) {
  
  # ============================================================================
  # REACTIVE VALUES AND INITIALIZATION
  # ============================================================================
  
  values <- reactiveValues(
    authenticated = FALSE,
    current_resident = NULL,
    current_period = NULL,
    milestone_results = NULL,
    app_data = NULL,
    debug_mode = Sys.getenv("DEBUG_MODE", "true") == "true"  # Read from environment
  )
  
  # Enable shinyjs
  shinyjs::useShinyjs()
  
  # ============================================================================
  # AUTHENTICATION SYSTEM - FIXED
  # ============================================================================
  
  # Handle access code authentication
  observeEvent(input$access_code_input, {
    if (!is.null(input$access_code_input) && nzchar(input$access_code_input)) {
      
      if (values$debug_mode) {
        message("=== AUTHENTICATION ATTEMPT ===")
        message("Access code: ", input$access_code_input)
      }
      
      # Validate against actual resident data
      tryCatch({
        global_data <- load_app_data()  # Get global data
        
        # Check if this is a valid access code
        valid_resident <- global_data$residents %>%
          filter(access_code == input$access_code_input | record_id == input$access_code_input)
        
        if (nrow(valid_resident) > 0) {
          values$authenticated <- TRUE
          values$current_resident <- as.character(valid_resident$record_id[1])
          shinyjs::hide("access_code_error")
          
          if (values$debug_mode) {
            message("Authentication successful for resident: ", values$current_resident)
            message("Resident name: ", valid_resident$name[1])
          }
          
          showNotification("Access granted! Loading your data...", 
                           type = "message", duration = 3)
          
          # Show authenticated sections
          shinyjs::show("resident_info_banner")
          shinyjs::show("assessment_section") 
          shinyjs::show("module_cards_section")
          
          # Initialize data loading
          load_resident_data()
          
        } else {
          # Try demo codes for testing
          if (input$access_code_input %in% c("demo", "test", "88")) {
            values$authenticated <- TRUE
            values$current_resident <- "88"  # Default test resident
            shinyjs::hide("access_code_error")
            
            showNotification("Demo access granted!", type = "message", duration = 3)
            
            # Show authenticated sections
            shinyjs::show("resident_info_banner")
            shinyjs::show("assessment_section")
            shinyjs::show("module_cards_section")
            
            load_resident_data()
            
          } else {
            # Authentication failed
            shinyjs::show("access_code_error")
            updateTextInput(session, "access_code_input", value = "")
            
            if (values$debug_mode) {
              message("Authentication failed for code: ", input$access_code_input)
            }
          }
        }
        
      }, error = function(e) {
        message("Error during authentication: ", e$message)
        shinyjs::show("access_code_error")
        updateTextInput(session, "access_code_input", value = "")
      })
    }
  })
  
  # ============================================================================
  # DATA LOADING FUNCTIONS - FIXED (No duplicate loading)
  # ============================================================================
  
  # Load resident data and initialize milestone system
  load_resident_data <- function() {
    if (values$debug_mode) {
      message("=== LOADING RESIDENT DATA ===")
      message("Resident ID: ", values$current_resident)
    }
    
    # FIXED: Use the global data, don't reload!
    tryCatch({
      showNotification("Initializing data...", id = "loading", duration = NULL)
      
      # Get the already-loaded data from global.R
      rdm_data <- load_app_data()  # This uses your global function
      
      if (values$debug_mode) {
        message("Using global RDM data")
        message("Residents: ", nrow(rdm_data$residents))
        message("Assessment records: ", nrow(rdm_data$assessment_data))
        message("All forms available: ", paste(names(rdm_data$all_forms), collapse = ", "))
        message("Data dict rows: ", nrow(rdm_data$data_dict))
      }
      
      # Store reference to the global data
      values$app_data <- rdm_data
      
      # Initialize milestone workflow
      initialize_milestone_workflow()
      
      removeNotification("loading")
      showNotification("Data initialized successfully!", type = "message", duration = 3)
      
    }, error = function(e) {
      removeNotification("loading")
      message("Error accessing RDM data: ", e$message)
      showNotification(paste("Error accessing data:", e$message), 
                       type = "error", duration = 10)
    })
  }
  
  # Initialize milestone workflow with enhanced debugging
  initialize_milestone_workflow <- function() {
    if (values$debug_mode) {
      message("=== INITIALIZING MILESTONE WORKFLOW ===")
    }
    
    tryCatch({
      # FIXED: Use correct function call with proper parameters
      milestone_workflow <- create_milestone_workflow_from_dict(
        all_forms = values$app_data$all_forms,
        data_dict = values$app_data$data_dict,
        resident_data = values$app_data$residents,
        verbose = values$debug_mode
      )
      
      if (values$debug_mode) {
        message("Milestone workflow created successfully")
        message("Available configurations: ", paste(names(milestone_workflow), collapse = ", "))
        
        # Debug each configuration
        for (config_name in names(milestone_workflow)) {
          config <- milestone_workflow[[config_name]]
          message("Config '", config_name, "': ", 
                  ifelse(is.null(config$data), "NO DATA", nrow(config$data)), " rows, ",
                  ifelse(is.null(config$medians), "NO MEDIANS", nrow(config$medians)), " median periods")
        }
      }
      
      values$milestone_results <- milestone_workflow
      
      # Set current period for the resident
      set_current_period()
      
    }, error = function(e) {
      message("Error initializing milestone workflow: ", e$message)
      message("Stack trace: ", paste(traceback(), collapse = "\n"))
      showNotification(paste("Error initializing milestones:", e$message), 
                       type = "warning", duration = 5)
    })
  }
  
  # Set the current period for milestone display
  set_current_period <- function() {
    if (is.null(values$app_data) || is.null(values$current_resident)) return()
    
    if (values$debug_mode) {
      message("=== SETTING CURRENT PERIOD ===")
      message("Looking for resident: ", values$current_resident)
    }
    
    # Find the most recent period for this resident
    resident_data <- values$app_data$residents %>%
      filter(record_id == values$current_resident)
    
    if (nrow(resident_data) > 0) {
      level <- resident_data$Level[1] %||% "Unknown"
      
      if (values$debug_mode) {
        message("Resident found - Level: ", level)
      }
      
      # Set period based on level - you can customize this logic
      period <- switch(level,
                       "Intern" = "End Intern",
                       "PGY2" = "End PGY2", 
                       "PGY3" = "Mid PGY3",
                       "Unknown" = "End PGY2",  # Default for testing
                       "End PGY2")  # Final fallback
      
      values$current_period <- period
      
      if (values$debug_mode) {
        message("Set current period: ", period, " for level: ", level)
      }
    } else {
      if (values$debug_mode) {
        message("Resident not found in data!")
        message("Available residents: ", paste(values$app_data$residents$record_id[1:5], collapse = ", "))
      }
      
      # Set default period anyway
      values$current_period <- "End PGY2"
    }
  }
  
  # ============================================================================
  # GMED INTEGRATION DEBUG HELPER
  # ============================================================================
  
  observe({
    req(values$app_data)
    
    if (values$debug_mode) {
      message("=== GMED INTEGRATION DEBUG ===")
      message("Available data structures:")
      message("- all_forms: ", length(values$app_data$all_forms))
      message("- residents: ", nrow(values$app_data$residents))
      message("- data_dict: ", nrow(values$app_data$data_dict))
      
      # Test gmed functions directly
      test_result <- tryCatch({
        create_milestone_workflow_from_dict(
          all_forms = values$app_data$all_forms,
          data_dict = values$app_data$data_dict,
          resident_data = values$app_data$residents,
          verbose = FALSE  # Reduce noise for this test
        )
      }, error = function(e) {
        message("GMED workflow error: ", e$message)
        return(NULL)
      })
      
      if (!is.null(test_result)) {
        message("GMED workflow SUCCESS: ", length(test_result), " configurations")
      } else {
        message("GMED workflow FAILED")
      }
    }
  })
  
  # ============================================================================
  # MILESTONE DASHBOARD MODULES - PROPERLY INTEGRATED
  # ============================================================================
  
  # Self-Assessment Milestone Module (with enhanced debugging)
  milestone_dashboard_server("self_milestone_dash", 
                             milestone_results = reactive({
                               req(values$milestone_results)
                               if (values$debug_mode) {
                                 message("=== SELF MILESTONE MODULE DATA REQUEST ===")
                                 message("Milestone results available: ", !is.null(values$milestone_results))
                                 message("Number of configurations: ", length(values$milestone_results))
                                 
                                 # Check for self-assessment data specifically
                                 self_configs <- names(values$milestone_results)[grepl("self", names(values$milestone_results))]
                                 message("Self configurations found: ", paste(self_configs, collapse = ", "))
                               }
                               return(values$milestone_results)
                             }),
                             record_id = reactive({
                               req(values$current_resident)
                               if (values$debug_mode) {
                                 message("Self module - Record ID: ", values$current_resident)
                               }
                               return(values$current_resident)
                             }),
                             period = reactive({
                               req(values$current_period)
                               if (values$debug_mode) {
                                 message("Self module - Period: ", values$current_period)
                               }
                               return(values$current_period)
                             }),
                             milestone_type = "self",
                             resident_data = reactive({
                               req(values$app_data)
                               if (values$debug_mode) {
                                 message("Self module - Residents data: ", nrow(values$app_data$residents), " residents")
                               }
                               return(values$app_data$residents)
                             })
  )
  
  # Program Assessment Milestone Module (with enhanced debugging)  
  milestone_dashboard_server("program_milestone_dash",
                             milestone_results = reactive({
                               req(values$milestone_results)
                               if (values$debug_mode) {
                                 message("=== PROGRAM MILESTONE MODULE DATA REQUEST ===")
                                 message("Milestone results available: ", !is.null(values$milestone_results))
                                 
                                 # Check for program data specifically
                                 program_configs <- names(values$milestone_results)[grepl("program", names(values$milestone_results))]
                                 message("Program configurations found: ", paste(program_configs, collapse = ", "))
                               }
                               return(values$milestone_results)
                             }),
                             record_id = reactive({
                               req(values$current_resident)
                               if (values$debug_mode) {
                                 message("Program module - Record ID: ", values$current_resident)
                               }
                               return(values$current_resident)
                             }),
                             period = reactive({
                               req(values$current_period)
                               if (values$debug_mode) {
                                 message("Program module - Period: ", values$current_period)
                               }
                               return(values$current_period)
                             }),
                             milestone_type = "program",
                             resident_data = reactive({
                               req(values$app_data)
                               if (values$debug_mode) {
                                 message("Program module - Residents data: ", nrow(values$app_data$residents), " residents")
                               }
                               return(values$app_data$residents)
                             })
  )
  
  # ============================================================================
  # ASSESSMENT VISUALIZATION MODULE
  # ============================================================================
  
  # Assessment visualization module (using gmed)
  assessment_viz_server(
    "main_assessment",
    data = reactive({  # CHANGED FROM: assessment_data
      req(values$app_data)
      
      # Use the same approach as the working version
      all_data <- bind_rows(values$app_data$all_forms, .id = "source_form")
      
      # Ensure required columns exist
      if (!"ass_level" %in% names(all_data)) {
        all_data$ass_level <- NA_integer_
      }
      if (!"fac_eval_level" %in% names(all_data)) {
        all_data$fac_eval_level <- NA_integer_
      }
      
      return(all_data)
    }),
    record_id = reactive({
      req(values$current_resident)
      return(values$current_resident)
    }),
    resident_name = reactive({  # ADDED: missing parameter
      req(values$app_data, values$current_resident)
      
      resident <- values$app_data$residents %>%
        filter(record_id == values$current_resident)
      
      if (nrow(resident) > 0) {
        return(resident$name[1] %||% paste("Resident", values$current_resident))
      } else {
        return(paste("Resident", values$current_resident))
      }
    })
  )
  
  # ============================================================================
  # MODAL CONTENT HANDLING
  # ============================================================================
  
  # Modal title based on selected module
  output$modal_title <- renderUI({
    switch(input$module_selected,
           "plus_delta" = "Plus / Delta Feedback",
           "evaluation_data" = "Evaluation Data",
           "milestone_plots" = "Milestone Assessment",
           "learning_plan" = "Learning Plan", 
           "scholarship" = "Scholarship",
           "schedule_data" = "Schedule Data",
           "peer_evaluations" = "Peer Evaluations",
           "Selected Module")
  })
  
  # Dynamic UI for selected module content
  output$selected_module_ui <- renderUI({
    req(input$module_selected)
    
    switch(input$module_selected,
           "plus_delta" = {
             # Use the gmed module instead of custom code
             mod_plus_delta_table_ui("resident_plus_delta")
           },
           
           "evaluation_data" = {
             div(
               h4("Evaluation Data", class = "text-primary mb-3"),
               div(class = "alert alert-secondary", "Coming Soon"),
               p("Will use: gmed::evaluation_data_ui() when implemented")
             )
           },
           
           "milestone_plots" = {
             div(
               h4("Milestone Assessment", class = "text-warning mb-3"),
               p("Complete your milestone self-assessment and compare with program data.", 
                 class = "text-muted mb-4"),
               
               # Side-by-side milestone modules - FIXED SIZING
               fluidRow(
                 column(
                   width = 6,
                   div(
                     class = "milestone-module-container",
                     style = "min-height: 600px; height: 600px;",
                     h5("Self-Assessment Milestones", class = "text-primary mb-3"),
                     milestone_dashboard_ui("self_milestone_dash", 
                                            milestone_type = "self", 
                                            height = "550px")
                   )
                 ),
                 column(
                   width = 6,
                   div(
                     class = "milestone-module-container", 
                     style = "min-height: 600px; height: 600px;",
                     h5("Program Assessment", class = "text-success mb-3"),
                     milestone_dashboard_ui("program_milestone_dash", 
                                            milestone_type = "program", 
                                            height = "550px")
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
  # PLUS/DELTA MODULE INTEGRATION
  # ============================================================================
  
  # Plus/Delta Module using gmed
  mod_plus_delta_table_server(
    "resident_plus_delta",
    rdm_data = reactive({
      req(values$app_data)
      # Use the assessment data - combine all forms if needed
      bind_rows(values$app_data$all_forms, .id = "source_form")
    }),
    record_id = reactive(values$current_resident)
  )
  
  # ============================================================================
  # RESIDENT INFO DISPLAY
  # ============================================================================
  
  output$resident_name <- renderText({
    req(values$authenticated, values$app_data, values$current_resident)
    
    resident <- values$app_data$residents %>%
      filter(record_id == values$current_resident)
    
    if (nrow(resident) > 0) {
      return(resident$name[1] %||% "Unknown Resident")
    } else {
      return("Unknown Resident")
    }
  })
  
  output$coach_name <- renderText({
    req(values$authenticated, values$app_data, values$current_resident)
    
    resident <- values$app_data$residents %>%
      filter(record_id == values$current_resident)
    
    if (nrow(resident) > 0) {
      coach <- resident$coach[1] %||% "No Coach Assigned"
      return(paste("Coach:", coach))
    } else {
      return("Coach: Unknown")
    }
  })
  
  # ============================================================================
  # FIX MILESTONE DROPDOWN USING GMED UNIVERSAL LABELS
  # Add this to your server.R to properly populate dropdowns
  # ============================================================================
  
  # Replace your milestone debugging section with this enhanced version:
  
  # ============================================================================
  # MILESTONE DROPDOWN POPULATION (USING GMED FUNCTIONS)
  # ============================================================================
  
  # Custom dropdown population using gmed's universal labeling system
  observe({
    req(values$milestone_results, values$authenticated)
    
    if (values$debug_mode) {
      message("=== GMED DROPDOWN POPULATION ===")
    }
    
    # Update self-assessment dropdown
    self_configs <- names(values$milestone_results)[grepl("self", names(values$milestone_results))]
    
    if (length(self_configs) > 0) {
      config <- values$milestone_results[[self_configs[1]]]
      
      if (!is.null(config$data)) {
        # Get self milestone columns using gmed pattern
        milestone_cols <- names(config$data)[grepl("^rep_(pc|mk|sbp|pbl|prof|ics)\\d+_self$", names(config$data))]
        
        if (length(milestone_cols) > 0) {
          # Use gmed's universal labeling system
          choices <- list()
          for (col in milestone_cols) {
            # Use gmed's get_milestone_label function for universal labels
            label <- gmed::get_milestone_label(col, "rep")
            choices[[label]] <- col
          }
          
          # Update dropdown
          updateSelectInput(session, "self_milestone_dash-selected_milestone", 
                            choices = choices,
                            selected = choices[[1]])
          
          if (values$debug_mode) {
            message("Self dropdown updated with ", length(choices), " gmed-labeled milestones")
            message("Sample labels: ", paste(head(names(choices), 3), collapse = ", "))
          }
        }
      }
    }
    
    # Update program assessment dropdown
    program_configs <- names(values$milestone_results)[grepl("program", names(values$milestone_results))]
    
    if (length(program_configs) > 0) {
      config <- values$milestone_results[[program_configs[1]]]
      
      if (!is.null(config$data)) {
        # Get program milestone columns using gmed pattern
        milestone_cols <- names(config$data)[grepl("^rep_(pc|mk|sbp|pbl|prof|ics)\\d+$", names(config$data))]
        
        if (length(milestone_cols) > 0) {
          # Use gmed's universal labeling system
          choices <- list()
          for (col in milestone_cols) {
            # Use gmed's get_milestone_label function for universal labels
            label <- gmed::get_milestone_label(col, "rep")
            choices[[label]] <- col
          }
          
          # Update dropdown
          updateSelectInput(session, "program_milestone_dash-selected_milestone", 
                            choices = choices,
                            selected = choices[[1]])
          
          if (values$debug_mode) {
            message("Program dropdown updated with ", length(choices), " gmed-labeled milestones")
            message("Sample labels: ", paste(head(names(choices), 3), collapse = ", "))
          }
        }
      }
    }
  })
  
  # ============================================================================
  # ALTERNATIVE: Test gmed functions directly
  # ============================================================================
  
  # Add this observe block to test if gmed functions are working:
  observe({
    if (values$debug_mode && values$authenticated) {
      message("=== TESTING GMED FUNCTIONS ===")
      
      # Test the universal labeling functions
      test_columns <- c("rep_pc1", "rep_pc1_self", "rep_mk2", "acgme_sbp1")
      
      for (col in test_columns) {
        # Test gmed's get_milestone_label function
        tryCatch({
          label <- gmed::get_milestone_label(col, "rep")
          message("gmed::get_milestone_label('", col, "', 'rep') = '", label, "'")
        }, error = function(e) {
          message("Error with gmed::get_milestone_label('", col, "'): ", e$message)
        })
      }
      
      # Test the definitions function
      tryCatch({
        definitions <- gmed::get_milestone_definitions("rep")
        message("gmed::get_milestone_definitions working: ", !is.null(definitions))
        if (!is.null(definitions)) {
          total_items <- sum(sapply(definitions, function(x) length(x$items)))
          message("Total milestone definitions: ", total_items)
        }
      }, error = function(e) {
        message("Error with gmed::get_milestone_definitions: ", e$message)
      })
    }
  })
  
  # ============================================================================
  # ENHANCED DEBUGGING FOR MILESTONE DATA
  # ============================================================================
  
  observe({
    req(values$milestone_results, values$current_resident, values$current_period)
    
    if (values$debug_mode) {
      message("=== ENHANCED MILESTONE DEBUG ===")
      message("Current resident: ", values$current_resident)
      message("Current period: ", values$current_period)
      
      # Check each configuration in detail
      for (config_name in names(values$milestone_results)) {
        config <- values$milestone_results[[config_name]]
        message("\n--- Config: ", config_name, " ---")
        
        if (!is.null(config$data)) {
          # Show data summary
          message("Data rows: ", nrow(config$data))
          message("Data columns: ", ncol(config$data))
          
          # Show milestone columns
          all_cols <- names(config$data)
          milestone_cols <- all_cols[grepl("^(rep_|acgme_)(pc|mk|sbp|pbl|prof|ics)", all_cols)]
          message("Milestone columns found: ", length(milestone_cols))
          message("Sample milestone columns: ", paste(head(milestone_cols, 5), collapse = ", "))
          
          # Check data for current resident
          resident_data <- config$data %>% filter(record_id == values$current_resident)
          message("Rows for current resident: ", nrow(resident_data))
          
          if (nrow(resident_data) > 0) {
            # Show available periods for this resident
            periods <- unique(resident_data$period_name)
            message("Available periods for resident: ", paste(periods, collapse = ", "))
            
            # Check if current period exists
            current_period_data <- resident_data %>% filter(period_name == values$current_period)
            message("Data for current period '", values$current_period, "': ", nrow(current_period_data), " rows")
          }
        }
        
        if (!is.null(config$medians)) {
          message("Median rows: ", nrow(config$medians))
          if (nrow(config$medians) > 0) {
            median_periods <- unique(config$medians$period_name)
            message("Median periods: ", paste(median_periods, collapse = ", "))
          }
        }
      }
    }
  })
  
  # ============================================================================
  # FALLBACK: Static dropdown with gmed labels
  # ============================================================================
  
  # If the dynamic approach doesn't work, use this static fallback:
  observe({
    
    # Create static choices using gmed definitions
    tryCatch({
      # Get milestone definitions from gmed
      rep_definitions <- gmed::get_milestone_definitions("rep")
      
      # Extract all milestone items into a flat list
      static_choices <- list()
      for (section in rep_definitions) {
        for (milestone_key in names(section$items)) {
          label <- section$items[[milestone_key]]
          static_choices[[label]] <- milestone_key
        }
      }
      
      # Update both dropdowns with gmed-derived choices
      updateSelectInput(session, "self_milestone_dash-selected_milestone", 
                        choices = static_choices,
                        selected = static_choices[[1]])
      
      updateSelectInput(session, "program_milestone_dash-selected_milestone", 
                        choices = static_choices,
                        selected = static_choices[[1]])
      
      if (values$debug_mode) {
        message("Applied gmed static fallback choices: ", length(static_choices), " milestones")
        message("Sample static choices: ", paste(head(names(static_choices), 3), collapse = ", "))
      }
      
    }, error = function(e) {
      if (values$debug_mode) {
        message("Error with gmed static fallback: ", e$message)
      }
    })
  })
  
  # ============================================================================
  # VERIFY GMED PACKAGE FUNCTIONS
  # ============================================================================
  
  # Add this to check which gmed functions are available:
  observe({
    if (values$debug_mode) {
      message("=== GMED PACKAGE VERIFICATION ===")
      
      # Check if gmed is loaded
      gmed_loaded <- "gmed" %in% loadedNamespaces()
      message("gmed package loaded: ", gmed_loaded)
      
      if (gmed_loaded) {
        # Check specific functions
        required_functions <- c(
          "get_milestone_label",
          "get_milestone_definitions", 
          "get_milestone_columns_simple",
          "create_milestone_label"
        )
        
        for (func in required_functions) {
          exists_check <- exists(func, where = "package:gmed")
          message("gmed::", func, " exists: ", exists_check)
        }
      }
    }
  })
  
  # ============================================================================
  # MILESTONE DROPDOWN POPULATION - TRIGGER ON MODAL OPEN
  # ============================================================================
  
  # Remove the immediate observeEvent(input$module_selected) for dropdowns
  
  # Instead, add this observer that waits for modal to be ready
  observe({
    # Only run when milestone modal is selected AND milestone results are available
    req(input$module_selected == "milestone_plots", 
        values$milestone_results, 
        values$authenticated)
    
    if (values$debug_mode) {
      message("=== WAITING FOR MODAL TO RENDER BEFORE DROPDOWN POPULATION ===")
    }
    
    # Use invalidateLater to delay the dropdown population
    invalidateLater(2000, session)  # Wait 2 seconds for modal to fully render
    
    # Now populate dropdowns
    isolate({
      if (values$debug_mode) {
        message("=== POPULATING DROPDOWNS AFTER DELAY ===")
      }
      
      # Self-assessment dropdown
      self_configs <- names(values$milestone_results)[grepl("self", names(values$milestone_results))]
      
      if (length(self_configs) > 0) {
        config <- values$milestone_results[[self_configs[1]]]
        
        if (!is.null(config$data)) {
          milestone_cols <- names(config$data)[grepl("^rep_(pc|mk|sbp|pbl|prof|ics)\\d+_self$", names(config$data))]
          
          if (length(milestone_cols) > 0) {
            choices <- list()
            for (col in milestone_cols) {
              label <- gmed::get_milestone_label(col, "rep")
              choices[[label]] <- col
            }
            
            updateSelectInput(session, "self_milestone_dash-selected_milestone", 
                              choices = choices, selected = choices[[1]])
            
            if (values$debug_mode) {
              message("Delayed: Self dropdown updated with ", length(choices), " choices")
            }
          }
        }
      }
      
      # Program assessment dropdown  
      program_configs <- names(values$milestone_results)[grepl("program", names(values$milestone_results))]
      
      if (length(program_configs) > 0) {
        config <- values$milestone_results[[program_configs[1]]]
        
        if (!is.null(config$data)) {
          milestone_cols <- names(config$data)[grepl("^rep_(pc|mk|sbp|pbl|prof|ics)\\d+$", names(config$data))]
          
          if (length(milestone_cols) > 0) {
            choices <- list()
            for (col in milestone_cols) {
              label <- gmed::get_milestone_label(col, "rep")
              choices[[label]] <- col
            }
            
            updateSelectInput(session, "program_milestone_dash-selected_milestone", 
                              choices = choices, selected = choices[[1]])
            
            if (values$debug_mode) {
              message("Delayed: Program dropdown updated with ", length(choices), " choices")
            }
          }
        }
      }
    })
  })
  
  # ============================================================================
  # ENHANCED DEBUG OUTPUTS
  # ============================================================================
  
  output$debug_info <- renderText({
    if (!values$debug_mode) return("")
    
    debug_lines <- c(
      "=== APPLICATION STATUS ===",
      paste("Authenticated:", values$authenticated),
      paste("Current Resident:", values$current_resident),
      paste("Current Period:", values$current_period),
      paste("App Data Available:", !is.null(values$app_data))
    )
    
    if (!is.null(values$app_data)) {
      debug_lines <- c(debug_lines,
                       "",
                       "=== DATA SUMMARY ===",
                       paste("Residents in data:", nrow(values$app_data$residents)),
                       paste("Data structures:", paste(names(values$app_data), collapse = ", "))
      )
      
      if ("all_forms" %in% names(values$app_data)) {
        debug_lines <- c(debug_lines,
                         paste("Available forms:", paste(names(values$app_data$all_forms), collapse = ", "))
        )
      }
    }
    
    if (!is.null(values$milestone_results)) {
      debug_lines <- c(debug_lines,
                       "",
                       "=== MILESTONE STATUS ===",
                       paste("Milestone Results Available: TRUE"),
                       paste("Milestone configurations:", paste(names(values$milestone_results), collapse = ", "))
      )
      
      # Show data availability for each config
      for (config_name in names(values$milestone_results)) {
        config <- values$milestone_results[[config_name]]
        data_rows <- ifelse(is.null(config$data), 0, nrow(config$data))
        median_rows <- ifelse(is.null(config$medians), 0, nrow(config$medians))
        debug_lines <- c(debug_lines,
                         paste("  ", config_name, ": ", data_rows, " data rows, ", median_rows, " median rows")
        )
      }
    } else {
      debug_lines <- c(debug_lines,
                       "",
                       "=== MILESTONE STATUS ===",
                       "Milestone Results Available: FALSE")
    }
    
    paste(debug_lines, collapse = "\n")
  })
  
  # Data structure debug output
  output$data_structure_debug <- renderText({
    if (!values$debug_mode) return("")
    
    req(values$app_data)
    
    debug_lines <- c("=== DATA STRUCTURE BREAKDOWN ===")
    
    # Show structure of each main component
    for (name in names(values$app_data)) {
      item <- values$app_data[[name]]
      if (is.data.frame(item)) {
        debug_lines <- c(debug_lines,
                         paste(name, ": data.frame with", nrow(item), "rows,", ncol(item), "columns"),
                         paste("  Sample columns:", paste(names(item)[1:min(5, ncol(item))], collapse = ", "))
        )
      } else if (is.list(item)) {
        debug_lines <- c(debug_lines,
                         paste(name, ": list with", length(item), "elements"),
                         paste("  Elements:", paste(names(item), collapse = ", "))
        )
      } else {
        debug_lines <- c(debug_lines,
                         paste(name, ": ", class(item), "with length", length(item))
        )
      }
    }
    
    # Add memory usage info
    debug_lines <- c(debug_lines,
                     "",
                     "=== MEMORY USAGE ===",
                     paste("App data object size:", format(object.size(values$app_data), units = "MB"))
    )
    
    paste(debug_lines, collapse = "\n")
  })
  
  # ============================================================================
  # CONDITIONAL UI DISPLAY LOGIC
  # ============================================================================
  
  observe({
    if (values$authenticated) {
      shinyjs::hide("access_code_error")
    }
  })
  
  # ============================================================================
  # SESSION MANAGEMENT AND ERROR HANDLING - COMPLETELY FIXED
  # ============================================================================
  
  # Session cleanup - capture debug mode value safely
  observe({
    # Capture debug mode state when app starts (only runs once)
    session$userData$debug_mode_captured <- values$debug_mode
  })
  
  session$onSessionEnded(function() {
    # Use the captured value instead of accessing reactive
    debug_mode_captured <- session$userData$debug_mode_captured %||% FALSE
    
    if (debug_mode_captured) {
      # Safely get current resident without reactive access
      current_resident <- tryCatch({
        # Don't access reactive values in session cleanup
        "session_ended"
      }, error = function(e) "unknown")
      
      message("Session ended for resident: ", current_resident)
    }
  })
  
  # Error handling without reactive access
  options(shiny.error = function() {
    message("Unhandled error in Shiny app")
    message("Stack trace: ", paste(traceback(), collapse = "\n"))
    
    # Show notification to user (this is safe in error handler)
    showNotification("An unexpected error occurred. Please refresh the page.", 
                     type = "error", duration = 10)
  })
}

