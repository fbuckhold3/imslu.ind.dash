server <- function(input, output, session) {
  
  
  # 🔹 Determine Access Code:
  access_code <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query$code)) {
      return(trimws(query$code))  # Trim whitespace from URL parameter
    } else {
      return(trimws(input$access_code_input))  # Trim whitespace from input field
    }
  })
  
  # Debugging: print the access code to the console
  observe({
    cat("Current access code:", access_code(), "\n")
  })
  
  # 🔹 Extract Resident Name Using Access Code
  resident_info <- reactive({
    req(access_code())
    filtered_res <- resident_data %>% filter(access_code == access_code())
    
    # If no matching resident is found, return NULL.
    if (nrow(filtered_res) == 0) {
      return(NULL)
    }
    
    # Return the resident's name from the first matching row.
    filtered_res$name[1]
  })
  
  # 🔹 Show Resident Name
  output$resident_name <- renderText({
    req(resident_info())
    paste("Dashboard for:", resident_info())
  })
  
  coach_info <- reactive({
    req(access_code())
    filtered_res <- resident_data %>% filter(access_code == access_code())
    
    # If no matching resident is found, return NULL.
    if (nrow(filtered_res) == 0) {
      return(NULL)
    }
    
    # Return the coach name from the first matching row.
    filtered_res$coach[1]
  })
  
  # 🔹 Show Coach Name
  output$coach_name <- renderText({
    req(coach_info())
    paste("Coach is Dr:", coach_info())
  })
  
  #' Background data setup:
  obs_labels <- parse_ip_obs_labels(ass_dict)
  
  # Faculty Evaluation Link:
  observeEvent(input$faculty_eval_link, {
    req(resident_info())  # Ensure resident information is available
    # Generate the ID by filtering for the resident's name and taking the first record
    id <- resident_data %>%
      filter(name == resident_info()) %>%
      slice(1) %>%
      pull(record_id)
    
    # Generate the faculty evaluation URL using your function
    link <- generate_survey_link(id, 'faculty_evaluation', url, rdm_token)
    url_node <- xml_find_first(link, "//p")
    fac_eval_url <- xml_text(url_node)
    
    
    # Send the generated URL to the client so that JavaScript can open it in a new tab
    session$sendCustomMessage(type = 'openURL', message = list(url = fac_eval_url))
  })
  
  # 🔹 Render Resident Assessments Plot
  output$res_ass <- renderPlot({
    req(resident_info())
    generate_tot_eval_plot(resident_data, resident_info())
  })
  
  # 🔹 Render Resident Progress UI
  output$resident_progress <- renderUI({
    req(resident_info())
    # Fix: Pass count_res_assessments as the count_function parameter explicitly
    progress_data <- compute_eval_progress(
      data = resident_data, 
      resident_name = resident_info(), 
      count_function = count_res_assessments
    )
    display_progress(progress_data)
  })
  
  # 🔹 Render Faculty Evaluations Plot
  output$fac_eval <- renderPlot({
    req(resident_info())
    fac_eval_data <- count_fac_eval(resident_data, resident_info())
    
    ggplot(fac_eval_data, aes(x = reorder(rot, -Count), y = Count, fill = rot)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_brewer(palette = "Set3") +  # Set3 allows more than 8 colors
      theme_minimal() +
      labs(title = "Faculty Evaluations Completed", x = "", y = "Count") +
      theme(axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            plot.title = element_text(size = 16, face = "bold"),
            legend.position = "none")
  })
  
  # 🔹 Render Faculty Progress UI
  output$faculty_progress <- renderUI({
    req(resident_info())
    fac_eval_data <- count_fac_eval(resident_data, resident_info())
    if (is.null(fac_eval_data) || nrow(fac_eval_data) == 0) {
      return(div("No faculty evaluations found.", style = "text-align: center; font-size: 16px;"))
    }
    total_fac_evals <- sum(fac_eval_data$Count, na.rm = TRUE)
    div(
      style = "text-align: center; font-size: 16px; font-weight: bold;",
      paste("You have completed", total_fac_evals, "faculty evaluations")
    )
  })
  
  output$selected_module_ui <- renderUI({
    module <- input$module_selected
    
    title <- switch(module,
                    "plus_delta" = "Plus / Delta Feedback",
                    "continuity" = "Continuity Clinic Evaluations",
                    "observational" = "Observational Data",
                    "inpatient" = "Inpatient Data", 
                    "milestone" = "Milestones",
                    "assessment" = "Self-Assessment",
                    "peer" = "Peer Evaluations",
                    "other" = "Other Data",
                    "Select a module" # Default
    )
    
    h4(title, style = "margin: 0;")
  })
  
  output$selected_module_ui <- renderUI({
    # Initialize with nothing displayed
    if (is.null(input$module_selected)) {
      return(NULL)
    }
    # Show appropriate content based on which card was clicked
    switch(input$module_selected,
           "plus_delta" = {
             div(
               # Add a description section at the top
               div(
                 class = "module-description",
                 style = "background: #f8f9fa; padding: 15px; border-left: 4px solid #0072B2; margin-bottom: 20px; border-radius: 4px;",
                 h4("About Plus/Delta Feedback", style = "color: #0072B2; margin-top: 0;"),
                 p("This section displays all of the writtent feedback highlighting your strengths (Plus) and areas for improvement (Delta). This qualitative feedback helps you understand specific behaviors that are effective and identifies opportunities for growth.", 
                   style = "margin-bottom: 5px;"),
                 p("Use this feedback to recognize patterns and track improvements over time.",
                   style = "font-style: italic; color: #666;")
               ),
               # Original content
               DT::DTOutput("plus_delta_table")
             )
           },
           "continuity" = {
             div(
               # Add a description section at the top
               div(
                 class = "module-description",
                 style = "background: #f8f9fa; padding: 15px; border-left: 4px solid #0072B2; margin-bottom: 20px; border-radius: 4px;",
                 h4("About Continuity Clinic Evaluations", style = "color: #0072B2; margin-top: 0;"),
                 p("This section contains evaluations from your continuity clinic experiences, including performance metrics on patient care, documentation quality, and inbox management. You should be getting an evaluation every quarter; the table at tops provides a summary whether an evaluation was completed each quarter.", 
                   style = "margin-bottom: 5px;"),
                 p("Regular review of this data will help you track growth in your ambulatory care skills.",
                   style = "font-style: italic; color: #666;")
               ),
               # Original content
               div(
                 uiOutput("cc_missing_evals_warning"),
                 h4("Completion Status"),
                 reactableOutput("cc_completion_status"),
                 hr(),
                 h4("Plus/Delta Feedback"),
                 DT::DTOutput("cc_plusdelta_table"),
                 hr(),
                 h4("Inbasket Coverage"),
                 DT::DTOutput("cc_inbasket_table"),
                 hr(),
                 h4("Documentation"),
                 DT::DTOutput("cc_documentation_table"),
                 hr(),
                 h4("Summative Evaluations"),
                 DT::DTOutput("cc_summative_intern_table"),
                 DT::DTOutput("cc_summative_pgy2_table"),
                 DT::DTOutput("cc_summative_pgy3_table")
               )
             )
           },
           "observational" = {
             div(
               # Add a description section at the top
               div(
                 class = "module-description",
                 style = "background: #f8f9fa; padding: 15px; border-left: 4px solid #F57C00; margin-bottom: 20px; border-radius: 4px;",
                 h4("About Observational Data", style = "color: #F57C00; margin-top: 0;"),
                 p("This section presents data from direct observations of your clinical performance.  These observations capture specific clinical skills like history-taking, physical examination, clinical reasoning, and communication. Ideally, during your 3 year training, you will complete 5 of each evaluation - a progress tracker is on the left.", 
                   style = "margin-bottom: 5px;"),
                 p("Select different observation types to review detailed feedback on specific clinical competencies.",
                   style = "font-style: italic; color: #666;")
               ),
               # Original content
               fluidRow(
                 column(
                   width = 4,
                   h4("Observation Progress"),
                   uiOutput("obsProgressBars"),
                 ),
                 column(
                   width = 8,
                   h4("Observation Data"),
                   selectInput(
                     inputId  = "obs_label_choice", 
                     label    = "Select an Observation Type:",
                     choices  = obs_labels,
                     selected = NULL
                   ),
                   DT::DTOutput("obs_table")
                 )
               )
             )
           },
           "inpatient" = {
             div(
               # Add a description section at the top
               div(
                 class = "module-description",
                 style = "background: #f8f9fa; padding: 15px; border-left: 4px solid #2E7D32; margin-bottom: 20px; border-radius: 4px;",
                 h4("About Inpatient Data", style = "color: #2E7D32; margin-top: 0;"),
                 p("This section contains evaluations from your hospital rotations, including feedback on your performance as both an intern and a supervising resident. The data reflects your clinical decision-making, team leadership, and patient management in the inpatient setting.", 
                   style = "margin-bottom: 5px;"),
                 p("Compare your performance across different rotations and throughout your training to identify growth.",
                   style = "font-style: italic; color: #666;")
               ),
               # Original content
               fluidRow(
                 column(
                   width = 12,
                   h4("Inpatient Intern Evaluations"),
                   DT::dataTableOutput("inpatient_intern_table")
                 ),
                 column(
                   width = 12,
                   h4("Inpatient Resident Evaluations"),
                   DT::dataTableOutput("inpatient_resident_table")
                 )
               )
             )
           },
           "milestone" = {
             div(
               # Add a description section at the top
               div(
                 class = "module-description",
                 style = "background: #f8f9fa; padding: 15px; border-left: 4px solid #FBC02D; margin-bottom: 20px; border-radius: 4px;",
                 h4("About Milestones", style = "color: #FBC02D; margin-top: 0;"),
                 p("This section displays your progress toward achieving specialty-specific competency milestones. The radar plots provide a visual representation comparing your rating to the average of the program over time.", 
                   style = "margin-bottom: 5px;"),
                 p("Use the period selector to track your milestone progression over time.",
                   style = "font-style: italic; color: #666;")
               ),
               # Original content
               h4("Select Period"),
               mod_miles_select_ui("miles_period_select"),
               fluidRow(
                 column(
                   width = 6,
                   h4("Your own rating on self-reflection"),
                   plotOutput("s_miles_plot", height = "400px")
                 ),
                 column(
                   width = 6,
                   h4("Rating from the Program (Clinical Comptency Committee)"),
                   plotOutput("p_miles_plot", height = "400px")
                 )
               )
             )
           },
           "assessment" = {
             div(
               card(
                 card_header("Self-Assessment"),
                 card_body(
                   p("Self-assessment data and entry will be displayed here")
                   # Add your self-assessment outputs here
                 )
               )
             )
           },
           "peer" = {
             div(
               # Add a description section at the top
               div(
                 class = "module-description",
                 style = "background: #f8f9fa; padding: 15px; border-left: 4px solid #6200EA; margin-bottom: 20px; border-radius: 4px;",
                 h4("About Peer Evaluations", style = "color: #6200EA; margin-top: 0;"),
                 p("This section displays evaluations and feedback provided by your peers and co-residents. Peer evaluations offer unique insights into your teamwork, interpersonal skills, and collaborative abilities from the perspective of colleagues at your level of training.", 
                   style = "margin-bottom: 5px;"),
                 p("Coming soon",
                   style = "font-style: italic; color: #666;")
               ),
               # Content for peer evaluations
             )
 
           },
           "other" = {
             div(
               card(
                 card_header("Other Data"),
                 card_body(
                   p("Placeholder for future")
                   # Add your other data outputs here
                 )
               )
             )
           }
    )
  })
  
  # Card 1: Plus Delta Data
  
  output$plus_delta_table <- DT::renderDT({
    req(resident_info())
    plus_delta_data <- generate_p_d(resident_data, resident_info())
    create_styled_dt(plus_delta_data, caption = "Plus/Delta Feedback")
  })
  
  # Keep your existing p_d output for backward compatibility if needed
  output$p_d <- DT::renderDT({
    req(resident_info())
    plus_delta_data <- generate_p_d(resident_data, resident_info())
    create_styled_dt(plus_delta_data, caption = "Plus/Delta Feedback")
  })
  
  # Card 2: Continuity Clinic:
  # 
  # In your server function:
  output$cc_completion_status <- renderReactable({
    req(resident_info())
    create_cc_table(resident_data, resident_info())
  })
  
  output$cc_missing_evals_warning <- renderUI({
    req(resident_info())
    
    # Filter data for the selected resident
    resident_evals <- resident_data %>%
      filter(name == resident_info(), !is.na(cc_eval_type))
    
    # Count the unique evaluation types
    unique_evals <- unique(resident_evals$cc_eval_type)
    
    # If less than 4 unique evaluation types, show warning
    if (length(unique_evals) < 4) {
      missing_count <- 4 - length(unique_evals)
      div(
        class = "alert alert-warning",
        icon("exclamation-triangle"),
        paste("You need to get", missing_count, "more type(s) of evaluations!")
      )
    }
  })
  
  output$cc_plusdelta_table <- renderDT({
    req(resident_info())
    pd_data <- process_cc_pd_data(resident_data, resident_info())
    create_styled_dt(pd_data, caption = "Plus/Delta Feedback")
  })
  
  output$cc_inbasket_table <- renderDT({
    req(resident_info())
    inbasket_data <- process_cc_inbasket_data(resident_data, resident_info())
    create_styled_dt(inbasket_data, caption = "Inbasket Coverage Evaluations")
  })
  
  output$cc_documentation_table <- renderDT({
    req(resident_info())
    doc_data <- process_cc_document_data(resident_data, resident_info())
    create_styled_dt(doc_data, caption = "Documentation Evaluations")
  })
  
  output$cc_summative_intern_table <- renderDT({
    req(resident_info())
    cat("Calling process_summative_data with resident:", resident_info(), "and level: Intern\n")
    summative_intern_data <- process_summative_data(resident_data, resident_info(), "Intern")
    cat("Result from process_summative_data: class:", class(summative_intern_data), 
        "rows:", ifelse(is.null(summative_intern_data), "NULL", nrow(summative_intern_data)), "\n")
    create_styled_dt(summative_intern_data, caption = "Intern Summative Evaluations")
  })
  
  output$cc_summative_pgy2_table <- renderDT({
    req(resident_info())
    summative_pgy2_data <- process_summative_data(resident_data, resident_info(), "PGY2")
    create_styled_dt(summative_pgy2_data, caption = "PGY2 Summative Evaluations")
  })
  
  output$cc_summative_pgy3_table <- renderDT({
    req(resident_info())
    summative_pgy3_data <- process_summative_data(resident_data, resident_info(), "PGY3")
    create_styled_dt(summative_pgy3_data, caption = "PGY3 Summative Evaluations")
  })
  
  observe({
    req(resident_info())
    # Check for NAs in key fields
    print(sum(is.na(resident_data$name)))
    print(sum(is.na(resident_data$cc_eval_type)))
    print(sum(is.na(resident_data$Level)))
    
    # Check unique values
    print(unique(resident_data$cc_eval_type))
    print(unique(resident_data$Level))
  })
  
  #Card 3: Observational Data

  output$obsProgressBars <- renderUI({
 
    req(resident_info())
    
    summarized_df <- summarize_observations(ass_dat, resident_info(), obs_labels)
    progress_data <- prepare_progress_data(summarized_df, target = 5)
    display_progress(progress_data)
  })
  
  output$obs_table <- DT::renderDT({
    req(resident_info(), input$obs_label_choice)
    prep_obs_table(
      data          = ass_dat,
      dict          = ass_dict,
      resident      = resident_info(),
      selected_label= input$obs_label_choice
    )
  })
  
  #Card 4: Inpatient
  
  output$inpatient_intern_table <- DT::renderDataTable({
    ip_data <- pull_inpatient_eval_split(data          = ass_dat,
                                         dict          = ass_dict,
                                         resident      = resident_info())
    create_styled_dt(ip_data$intern_df, caption = "Inpatient Intern Evaluations")
  })
  
  output$inpatient_resident_table <- DT::renderDataTable({
    ip_data <- pull_inpatient_eval_split(data          = ass_dat,
                                         dict          = ass_dict,
                                         resident      = resident_info())
    create_styled_dt(ip_data$resident_df, caption = "Inpatient Resident Evaluations")
  })
  
  # Card 5: Milestones
  # 1) Call the milestone period selection module
  period_selection <- mod_miles_select_server("miles_period_select")
  # This returns a reactive that holds whatever the user chooses
  
  # 2) Build the two side-by-side plots
  output$s_miles_plot <- renderPlot({
  req(period_selection())
  miles_plot(s_miles, resident_info(), period_selection())
  })

  output$p_miles_plot <- renderPlot({
    req(period_selection())            # Ensure it’s not NULL
    miles_plot(p_miles, resident_info(), period_selection())
  })
  
 
 
  # Card 6: Self-assessment
  
  
  # Card 7: Other data
  # 
}
