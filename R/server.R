server <- function(input, output, session) {
  
  
  # 🔹 Determine Access Code:
  access_code <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query$code)) {
      return(query$code)
    } else {
      return(input$access_code_input)
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
  
  #' Background data setup:
  obs_labels <- parse_ip_obs_labels(ass_dict)
  
  # 🔹 Show Resident Name
  output$resident_name <- renderText({
    req(resident_info())
    paste("Resident:", resident_info())
  })
  
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
      paste("You have done", total_fac_evals, "faculty evaluations")
    )
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
               card(
                 card_header("Plus / Delta Feedback"),
                 card_body(
                   DT::DTOutput("plus_delta_table")
                 )
               )
             )
           },
           "continuity" = {
             div(
               card(
                 card_header("Continuity Clinic Evaluations"),
                 card_body(
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
               )
             )
           },
           "observational" = {
             div(
               card(
                 card_header("Observational Data"),
                 fluidRow(
                   h3("Observational Data"),
                   column(
                     width = 4,
                     # Left side: your progress bars, debug info, etc.
                     h4("Observation Progress"),
                     uiOutput("obsProgressBars"),
                   ),
                   column(
                     width = 8,
                     h4("Observation Data"),
                     # Right side: pick an observation type and show a data table
                     selectInput(
                       inputId  = "obs_label_choice", 
                       label    = "Select an Observation Type:",
                       choices  = obs_labels,   # or sort(labels) if you want alphabetical
                       selected = NULL
                     ),
                     
                     DT::DTOutput("obs_table")
                   
                 )
               )
             )
            )
           },
           "inpatient" = {
             div(
               card(
                 card_header("Inpatient Data"),
                 card_body(
                   # Use a fluidRow so columns can be arranged easily
                   fluidRow(
                     # First "block": Intern table
                     column(
                       width = 12,
                       h4("Inpatient Intern Evaluations"),
                       DT::dataTableOutput("inpatient_intern_table")
                     ),
                     # Second "block": Resident table
                     column(
                       width = 12,
                       h4("Inpatient Resident Evaluations"),
                       DT::dataTableOutput("inpatient_resident_table")
                     )
                   )
                 )
               )
             )
           },
           "milestone" = {
             div(
               card(
                 card_header("Milestones"),
                 card_body(
                   # 1) Period Selection (above the plots)
                   h4("Select Period"),
                   mod_miles_select_ui("miles_period_select"),
                   
                   # 2) A row with two side-by-side columns for the radar plots
                   fluidRow(
                     column(
                       width = 6,
                       h4("s_miles Radar Plot"),
                       plotOutput("s_miles_plot", height = "400px")
                     ),
                     column(
                       width = 6,
                       h4("p_miles Radar Plot"),
                       plotOutput("p_miles_plot", height = "400px")
                     )
                   )
                 )
               )
             )
           },
           "assessment" = {
             div(
               card(
                 card_header("Self-Assessment"),
                 card_body(
                   p("Self-assessment data will be displayed here")
                   # Add your self-assessment outputs here
                 )
               )
             )
           },
           "other" = {
             div(
               card(
                 card_header("Other Data"),
                 card_body(
                   p("Other metrics and data will be displayed here")
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
