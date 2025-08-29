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
  

  # Assessment module - call server only when resident is valid
  # Assessment module - call server once at startup (always)
  # Assessment module - pass ALL forms data, not just assessment
  # Assessment module - pass combined data with all required columns
  assessment_viz_server(
    "main_assessment",
    data = reactive({
      if (!is.null(resident_info())) {
        # Combine all forms with source_form column
        all_data <- bind_rows(app_data()$all_forms, .id = "source_form")
        
        # Ensure all required level columns exist for the visualization functions
        if (!"ass_level" %in% names(all_data)) {
          all_data$ass_level <- NA
        }
        if (!"fac_eval_level" %in% names(all_data)) {
          all_data$fac_eval_level <- NA
        }
        
        # For assessment records, use the level column we created
        all_data <- all_data %>%
          mutate(
            ass_level = case_when(
              source_form == "assessment" & !is.na(level) ~ level,
              source_form == "assessment" & is.na(level) ~ "Unknown",
              TRUE ~ ass_level
            ),
            fac_eval_level = case_when(
              source_form == "faculty_evaluation" & !is.na(level) ~ level,
              source_form == "faculty_evaluation" & is.na(level) ~ "Unknown", 
              TRUE ~ fac_eval_level
            )
          )
        
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
  
  # Modal content - mostly just the gmed module calls
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
               h4("Milestone Plots", class = "text-warning mb-3"), 
               div(class = "alert alert-secondary", "Coming Soon"),
               p("Will use: gmed::milestone_viz_ui() when implemented")
             )
           },
           
           # Future modules will follow same simple pattern:
           # "module_name" = gmed::module_ui("namespace")
           
           # Default case
           {
             div(class = "alert alert-warning", "Module not yet implemented.")
           }
    )
  })
}
