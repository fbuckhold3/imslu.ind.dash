# ui.R - IMSLU Individual Resident Dashboard with GMED Theming
# Updated for RDM 2.0 and gmed package integration

# Try to load gmed, but provide fallback if not available
gmed_available <- tryCatch({
  library(gmed)
  TRUE
}, error = function(e) {
  message("gmed package not available, using fallback styling")
  FALSE
})

ui <- if (gmed_available) {
  gmed_page(
    title = "IMSLU Resident Dashboard",
    theme_variant = "slucare"
  )
} else {
  page_fluid(
    theme = bs_theme(version = 5),
    tags$head(tags$title("IMSLU Resident Dashboard")),
    useShinyjs()  # Enable shinyjs for show/hide functionality
  )
}

# Add the rest of the UI content to whatever container we have
ui <- tagAppendChildren(
  ui,
  
  # Custom CSS for module cards and hover effects
  tags$head(
    tags$style(HTML("
    .module-card {
      cursor: pointer;
      transition: transform 0.3s ease, box-shadow 0.3s ease;
    }
    .module-card:hover {
      transform: translateY(-5px);
      box-shadow: 0 10px 20px rgba(0,0,0,0.15);
    }
    .btn:hover {
      opacity: 0.9;
      transition: opacity 0.3s ease-in-out;
    }
    /* Modal styling with gmed colors */
    .modal-content {
      border-radius: 15px;
      box-shadow: 0 5px 20px rgba(0,61,92,0.2);
    }
    .modal-header {
      border-bottom: 1px solid #eee;
      background: linear-gradient(90deg, var(--ssm-primary-blue), var(--ssm-secondary-blue));
      color: white;
      border-radius: 15px 15px 0 0;
    }
    .modal-title {
      font-weight: bold;
    }
    .modal-footer {
      border-top: 1px solid #eee;
      background: var(--ssm-light-gray);
      border-radius: 0 0 15px 15px;
    }
    .assessment-viz-container {
  max-width: 100%;
  overflow: hidden;
}

.viz-card {
  max-height: 400px;
}

.plotly {
  max-height: 300px !important;
}")),
    tags$script(HTML("
    // JavaScript for card click handlers
    $(document).on('click', '#plus_delta_card', function() {
      Shiny.setInputValue('module_selected', 'plus_delta');
      $('#moduleModal').modal('show');
    });
    $(document).on('click', '#evaluation_data_card', function() {
      Shiny.setInputValue('module_selected', 'evaluation_data');
      $('#moduleModal').modal('show');
    });
    $(document).on('click', '#milestone_plots_card', function() {
      Shiny.setInputValue('module_selected', 'milestone_plots');
      $('#moduleModal').modal('show');
    });
    $(document).on('click', '#learning_plan_card', function() {
      Shiny.setInputValue('module_selected', 'learning_plan');
      $('#moduleModal').modal('show');
    });
    $(document).on('click', '#scholarship_card', function() {
      Shiny.setInputValue('module_selected', 'scholarship');
      $('#moduleModal').modal('show');
    });
    $(document).on('click', '#schedule_data_card', function() {
      Shiny.setInputValue('module_selected', 'schedule_data');
      $('#moduleModal').modal('show');
    });
    $(document).on('click', '#peer_evaluations_card', function() {
      Shiny.setInputValue('module_selected', 'peer_evaluations');
      $('#moduleModal').modal('show');
    });
    "))
  ),
  
  # Header with logo and title (always visible)
  div(
    style = "display: flex; align-items: center; padding: 1rem; background: white; margin-bottom: 20px;",
    img(src = "ssm_slucare.png", height = "60px", style = "margin-right: 15px; vertical-align: middle;"),
    h1("IMSLU Resident Dashboard", 
       style = "margin: 0; font-weight: bold; color: #003d5c;")
  ),
  
  # Main container (always visible)
  div(
    class = "container mt-4",
    
    # Access code input (always visible at top)
    div(
      class = "row justify-content-center mb-4",
      div(
        class = "col-md-6",
        div(
          class = "card",
          style = "border-radius: 12px; box-shadow: 0 4px 16px rgba(0,61,92,0.1);",
          div(
            class = "card-header text-center",
            style = "background: linear-gradient(135deg, #003d5c, #0066a1); color: white; border-radius: 12px 12px 0 0;",
            h4("Enter Access Code", style = "margin: 0;")
          ),
          div(
            class = "card-body text-center p-4",
            p("Please enter your access code to view your dashboard data:", 
              class = "mb-3 text-muted"),
            textInput("access_code_input", NULL, value = "", 
                      placeholder = "Enter your access code...",
                      width = "100%"),
            div(id = "access_code_error", 
                class = "alert alert-danger mt-2", 
                style = "display: none;",
                "Invalid access code. Please try again.")
          )
        )
      )
    ),
    
    # Resident info banner (shows when code is valid)
    div(
      id = "resident_info_banner",
      style = "display: none;",
      class = "mb-4",
      div(
        style = "background: linear-gradient(135deg, #003d5c 20%, #0066a1 80%); color: white; padding: 15px; border-radius: 12px;",
        div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          h3(textOutput("resident_name"), style = "margin: 0; font-weight: bold;"),
          h4(textOutput("coach_name"), style = "margin: 0; font-weight: normal;")
        )
      )
    ),
    
    # Assessment Section (shows when code is valid)
    # Assessment Section (always present, but conditionally visible)
    div(
      id = "assessment_section",
      style = "display: none;",  # Hidden by default, shown by shinyjs::toggle()
      class = "mb-4",
      div(
        class = "card",
        style = "border: none; border-radius: 16px; box-shadow: 0 8px 24px rgba(0,61,92,0.08);",
        div(
          class = "card-header",
          style = "background: linear-gradient(135deg, #003d5c, #0066a1); color: white; border-radius: 16px 16px 0 0; padding: 1.5rem;",
          h4("Assessment Overview", style = "margin: 0; font-weight: 600;")
        ),
        div(
          class = "card-body p-4",
          # Always render the assessment module UI
          assessment_viz_ui("main_assessment", "Assessment Dashboard")
        )
      )
    ),
    
    # Module cards section (shows when code is valid)
    div(
      id = "module_cards_section",
      style = "display: none;",
      
      # Module selection title
      div(
        class = "text-center mb-4 p-3",
        style = "background: linear-gradient(135deg, #0066a1, #4a90a4); border-radius: 12px; color: white;",
        h3("Explore Your Data", style = "margin-bottom: 10px; font-weight: 600;"),
        p("Click on a card to view detailed information", style = "margin: 0; opacity: 0.9;")
      ),
      
      # Updated clickable module cards with new names and consistent styling
      layout_column_wrap(
        width = "230px",
        gap = "12px",
        style = "margin-top: 20px;",
        
        # Plus/Delta Feedback - keep, use gmed module
        div(
          id = "plus_delta_card",
          class = "module-card card",
          style = "background: #E3F2FD; border-radius: 12px; box-shadow: 0 4px 12px rgba(0,0,0,0.1); height: 180px; cursor: pointer; border: none;",
          div(
            class = "card-header text-center",
            style = "color: #003d5c; font-weight: bold; background: transparent; border: none;",
            "Plus / Delta Feedback"
          ),
          div(
            class = "card-body text-center d-flex align-items-center justify-content-center",
            icon("comments", "fa-4x", style = "color: #0066a1;")
          )
        ),
        
        # Evaluation Data (was Continuity Clinic)
        div(
          id = "evaluation_data_card",
          class = "module-card card",
          style = "background: #E3F2FD; border-radius: 12px; box-shadow: 0 4px 12px rgba(0,0,0,0.1); height: 180px; cursor: pointer; border: none;",
          div(
            class = "card-header text-center",
            style = "color: #003d5c; font-weight: bold; background: transparent; border: none;",
            "Evaluation Data"
          ),
          div(
            class = "card-body text-center d-flex align-items-center justify-content-center",
            icon("stethoscope", "fa-4x", style = "color: #0066a1;")
          )
        ),
        
        # Milestone Plots (was Observational Data)
        div(
          id = "milestone_plots_card",
          class = "module-card card",
          style = "background: #FFF3E0; border-radius: 12px; box-shadow: 0 4px 12px rgba(0,0,0,0.1); height: 180px; cursor: pointer; border: none;",
          div(
            class = "card-header text-center",
            style = "color: #E65100; font-weight: bold; background: transparent; border: none;",
            "Milestone Plots"
          ),
          div(
            class = "card-body text-center d-flex align-items-center justify-content-center",
            icon("chart-line", "fa-4x", style = "color: #ff8c00;")
          )
        ),
        
        # Learning Plan (was Inpatient Data)
        div(
          id = "learning_plan_card",
          class = "module-card card",
          style = "background: #E8F5E9; border-radius: 12px; box-shadow: 0 4px 12px rgba(0,0,0,0.1); height: 180px; cursor: pointer; border: none;",
          div(
            class = "card-header text-center",
            style = "color: #1B5E20; font-weight: bold; background: transparent; border: none;",
            "Learning Plan"
          ),
          div(
            class = "card-body text-center d-flex align-items-center justify-content-center",
            icon("graduation-cap", "fa-4x", style = "color: #00a651;")
          )
        ),
        
        # Scholarship (was Milestones)
        div(
          id = "scholarship_card",
          class = "module-card card",
          style = "background: #FFFDE7; border-radius: 12px; box-shadow: 0 4px 12px rgba(0,0,0,0.1); height: 180px; cursor: pointer; border: none;",
          div(
            class = "card-header text-center",
            style = "color: #F9A825; font-weight: bold; background: transparent; border: none;",
            "Scholarship"
          ),
          div(
            class = "card-body text-center d-flex align-items-center justify-content-center",
            icon("book", "fa-4x", style = "color: #FBC02D;")
          )
        ),
        
        # Schedule Data (was Self-Assessment)
        div(
          id = "schedule_data_card",
          class = "module-card card",
          style = "background: #F3E5F5; border-radius: 12px; box-shadow: 0 4px 12px rgba(0,0,0,0.1); height: 180px; cursor: pointer; border: none;",
          div(
            class = "card-header text-center",
            style = "color: #6A1B9A; font-weight: bold; background: transparent; border: none;",
            "Schedule Data"
          ),
          div(
            class = "card-body text-center d-flex align-items-center justify-content-center",
            icon("calendar", "fa-4x", style = "color: #8E24AA;")
          )
        ),
        
        # Peer Evaluations - keep
        div(
          id = "peer_evaluations_card",
          class = "module-card card",
          style = "background: #EDE7F6; border-radius: 12px; box-shadow: 0 4px 12px rgba(0,0,0,0.1); height: 180px; cursor: pointer; border: none;",
          div(
            class = "card-header text-center",
            style = "color: #6200EA; font-weight: bold; background: transparent; border: none;",
            "Peer Evaluations"
          ),
          div(
            class = "card-body text-center d-flex align-items-center justify-content-center",
            icon("users", "fa-4x", style = "color: #6200EA;")
          )
        )
        # Note: "Other Data" card removed as requested
      )
    )
  ),
  
  # Bootstrap modal for module content
  tags$div(
    class = "modal fade", id = "moduleModal",
    tags$div(
      class = "modal-dialog", 
      style = "max-width: 90%; width: 90%;",
      tags$div(
        class = "modal-content",
        tags$div(
          class = "modal-header",
          tags$h4(class = "modal-title", uiOutput("modal_title")),
          tags$button(
            type = "button", 
            class = "close", 
            "data-dismiss" = "modal", 
            "aria-label" = "Close", 
            onclick = "$('#moduleModal').modal('hide');",
            tags$span("aria-hidden" = "true", HTML("&times;"))
          )
        ),
        tags$div(
          class = "modal-body",
          uiOutput("selected_module_ui")
        ),
        tags$div(
          class = "modal-footer",
          tags$button(
            type = "button", 
            class = "btn btn-primary", 
            "data-dismiss" = "modal", 
            onclick = "$('#moduleModal').modal('hide');",
            "Close"
          )
        )
      )
    )
  )
)