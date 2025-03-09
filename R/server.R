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
    progress_data <- compute_eval_progress(resident_data, resident_info(), count_res_assessments)
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
  
  # Create a reactive for continuity clinic data
  cc_data <- reactive({
    req(resident_info())
    
    # In a real app, you would fetch this data from your database
    # For demonstration, we'll create sample data for the current resident
    set.seed(123)
    levels <- c("Level 1", "Level 2", "Level 3")
    eval_types <- c("Type A", "Type B", "Type C", "Type D")
    evaluators <- c("John", "Jane", "Mark", "Sarah", NA)
    
    # Create sample data for the current resident
    expand.grid(
      name = resident_info(),
      Level = levels,
      cc_eval_type = eval_types
    ) %>%
      mutate(
        # Randomly assign evaluators or NA for incomplete evaluations
        Evaluator = sample(evaluators, n(), replace = TRUE),
        is_complete = !is.na(Evaluator)
      )
  })
  
  # 🔹 Module Selection Handling - UPDATED for continuity clinic module
  output$selected_module_ui <- renderUI({
    req(input$module_selected)
    switch(input$module_selected,
           'plus_delta' = div(h3("Plus / Delta Feedback"), DT::DTOutput("p_d")),
           "continuity" = div(
             h3("Continuity Clinic Data", 
                style = "background: linear-gradient(135deg, #B71C1C 20%, #EF5350 80%); color: white; padding: 15px; border-radius: 8px; margin-bottom: 20px;"),
             fluidRow(
               column(width = 6,
                      # Original plot 
                      card(
                        card_header("Visit Data", style = "background-color: #f1f3f5; color: #212529; font-weight: 600;"),
                        card_body(
                          plotOutput("continuity_plot")
                        )
                      )
               ),
               column(width = 6,
                      # Add a new panel for CC evaluation summary
                      card(
                        card_header("Additional Metrics", style = "background-color: #f1f3f5; color: #212529; font-weight: 600;"),
                        card_body(
                          plotOutput("cc_summary_plot")
                        )
                      )
               )
             ),
             # Add the evaluation completion table
             card(
               card_header(
                 class = "bg-primary text-white",
                 h3(icon("check-square"), "Evaluation Completion Status", class = "mb-0")
               ),
               card_body(
                 reactableOutput("cc_completion_table")
               )
             )
           ),
           "observational" = div(h3("Observational Data"), plotOutput("observational_plot")),
           "inpatient" = div(h3("Inpatient Data"), plotOutput("inpatient_plot")),
           "other" = div(h3("Other Data"), plotOutput("other_plot")),
           "milestone" = div(
             h3("Milestone Data"),
             mod_miles_select_ui("period_select"),
             plotOutput("self_milestone_plot", height = "400px"),
             plotOutput("prog_milestone_plot", height = "400px")
           ),
           "assessment" = div(h3("Self-Assessment Data"), plotOutput("assessment_plot"))
    )
  })
  
  # Render the CC completion table
  output$cc_completion_table <- renderReactable({
    req(resident_info())
    
    # Get the CC data for this resident
    cc_data_for_table <- cc_data()
    
    # Use your helper function to generate the table
    create_cc_table(cc_data_for_table, resident_info())
  })
  
  # Add a placeholder summary plot for CC
  output$cc_summary_plot <- renderPlot({
    req(resident_info())
    # Sample plot - replace with actual data visualization
    barplot(table(cc_data()$is_complete), 
            main = "Completion Status", 
            col = c("#c5221f", "#137333"),
            names.arg = c("Incomplete", "Complete"))
  })
  
  # 🔹 Placeholder Plots for Modules
  output$p_d <- DT::renderDT({generate_p_d(resident_data, resident_info()) 
  })
  output$continuity_plot <- renderPlot({ 
    plot(1:10, 1:10, type = "l", main = "Continuity Clinic Visits") 
  })
  output$observational_plot <- renderPlot({ 
    plot(1:10, 10:1, type = "l", main = "Observational Data") 
  })
  output$inpatient_plot <- renderPlot({ 
    plot(1:10, sample(1:10), type = "l", main = "Inpatient Data") 
  })
  output$other_plot <- renderPlot({ 
    plot(1:10, cumsum(rnorm(10)), type = "l", main = "Other Data") 
  })
  output$assessment_plot <- renderPlot({ 
    plot(1:10, sin(1:10), type = "l", main = "Self-Assessment Data") 
  })
}

