# ui.R - IMSLU Individual Resident Dashboard with Debug Integration
# Updated for RDM 2.0 and gmed package integration

# Try to load gmed, but provide fallback if not available
gmed_available <- tryCatch({
  library(gmed)
  TRUE
}, error = function(e) {
  message("gmed package not available, using fallback styling")
  FALSE
})

# Debug mode control - set to FALSE for production
DEBUG_MODE <- Sys.getenv("DEBUG_MODE", "true") == "true"

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
  
  tags$head(
    # Link to your custom CSS file (loaded AFTER gmed CSS)
    tags$link(rel = "stylesheet", type = "text/css", href = "milestone_dashboard.css"),
    
    # Keep only the essential CSS that doesn't conflict
    tags$style(HTML("
    /* Module Cards */
    .module-card {
      cursor: pointer;
      transition: transform 0.3s ease, box-shadow 0.3s ease;
    }
    .module-card:hover {
      transform: translateY(-5px);
      box-shadow: 0 10px 20px rgba(0,0,0,0.15);
    }
    
    /* Modal Styling */
    .modal-dialog {
      max-width: 95%;
      width: 95%;
    }
  ")), # <- Added closing parenthesis here
    
    tags$script(HTML("
// JavaScript for card click handlers and modal management
$(document).ready(function() {
  console.log('Modal JavaScript loaded');
  
  // Function to show modal with better error handling
  function showModal() {
    try {
      // Try Bootstrap 5 method first
      if (typeof bootstrap !== 'undefined') {
        var modalElement = document.getElementById('moduleModal');
        if (modalElement) {
          var modal = new bootstrap.Modal(modalElement);
          modal.show();
          console.log('Modal shown using Bootstrap 5');
          return;
        }
      }
      
      // Fallback to jQuery/Bootstrap 4 method
      if (typeof $ !== 'undefined' && $.fn.modal) {
        $('#moduleModal').modal('show');
        console.log('Modal shown using jQuery');
        return;
      }
      
      console.error('No modal method available');
    } catch (error) {
      console.error('Error showing modal:', error);
    }
  }
  
  // Card click handlers
  $(document).on('click', '#plus_delta_card', function() {
    console.log('Plus/Delta card clicked');
    Shiny.setInputValue('module_selected', 'plus_delta', {priority: 'event'});
    showModal();
  });
  
  $(document).on('click', '#evaluation_data_card', function() {
    console.log('Evaluation Data card clicked');
    Shiny.setInputValue('module_selected', 'evaluation_data', {priority: 'event'});
    showModal();
  });
  
  $(document).on('click', '#milestone_plots_card', function() {
    console.log('Milestone Plots card clicked');
    Shiny.setInputValue('module_selected', 'milestone_plots', {priority: 'event'});
    showModal();
  });
  
  $(document).on('click', '#learning_plan_card', function() {
    console.log('Learning Plan card clicked');
    Shiny.setInputValue('module_selected', 'learning_plan', {priority: 'event'});
    showModal();
  });
  
  $(document).on('click', '#scholarship_card', function() {
    console.log('Scholarship card clicked');
    Shiny.setInputValue('module_selected', 'scholarship', {priority: 'event'});
    showModal();
  });
  
  $(document).on('click', '#schedule_data_card', function() {
    console.log('Schedule Data card clicked');
    Shiny.setInputValue('module_selected', 'schedule_data', {priority: 'event'});
    showModal();
  });
  
  $(document).on('click', '#peer_evaluations_card', function() {
    console.log('Peer Evaluations card clicked');
    Shiny.setInputValue('module_selected', 'peer_evaluations', {priority: 'event'});
    showModal();
  });
  
  // Modal close handlers
  $(document).on('click', '[data-dismiss=modal]', function() {
    console.log('Modal close button clicked');
    try {
      if (typeof bootstrap !== 'undefined') {
        var modalElement = document.getElementById('moduleModal');
        if (modalElement) {
          var modal = bootstrap.Modal.getInstance(modalElement);
          if (modal) {
            modal.hide();
          }
        }
      } else if (typeof $ !== 'undefined' && $.fn.modal) {
        $('#moduleModal').modal('hide');
      }
    } catch (error) {
      console.error('Error hiding modal:', error);
    }
  });
  
}); // <- This closing was missing!
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
            # Add the descriptive text here
            div(
              class = "mb-4 p-3",
              style = "background: #f8f9fa; border-radius: 8px; border-left: 4px solid #003d5c;",
              p("The following is a dashboard for residents in the IMSLU Residency Program to access data on their evaluations, progress in their program, and learning plans. By entering your access code, you are acknowledging that this is data only intended for the individual resident and should only be accessed by that resident and / or designated members in the leadership of the Program.",
                style = "margin: 0; font-size: 0.95rem; line-height: 1.5; text-align: left; color: #495057;")
            ),
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
    
    # Quick Recording Links Section (shows when code is valid)
    div(
      id = "quick_links_section",
      style = "display: none;",
      class = "mb-4",
      div(
        class = "row mb-3",
        div(
          class = "col-12",
          div(
            class = "card",
            style = "border: none; border-radius: 12px; background: #f8f9fa;",
            div(
              class = "card-body p-3",
              h6("Quick Recording Links", style = "margin-bottom: 15px; color: #003d5c;"),
              div(
                class = "d-flex flex-wrap gap-2",
                a(href = "https://fbuckhold3-imslu-resident-assessment.share.connect.posit.cloud",
                  target = "_blank",
                  class = "btn btn-outline-primary btn-sm",
                  icon("clipboard-check"), " Record Assessment"),
                a(href = "https://fbuckhold3-imslu-facultyeval.share.connect.posit.cloud", 
                  target = "_blank",
                  class = "btn btn-outline-primary btn-sm",
                  icon("user-tie"), " Record Faculty Eval"),
                a(href = "https://fbuckhold3-imslu-at-noon.share.connect.posit.cloud",
                  target = "_blank", 
                  class = "btn btn-outline-primary btn-sm",
                  icon("users"), " Record Noon Conference")
              )
            )
          )
        )
      )
    ),
    
    # Assessment Section (shows when code is valid)
    div(
      id = "assessment_section",
      style = "display: none;",
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
        
        # Evaluation Data
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
        
        # Milestone Plots
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
        
        # Learning Plan
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
        
        # Scholarship
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
        
        # Schedule Data
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
        
        # Peer Evaluations
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
      )
    ),
    
    # ============================================================================
    # DEBUG PANEL - COLLAPSIBLE (only shows if DEBUG_MODE is TRUE)
    # ============================================================================
    
    conditionalPanel(
      condition = paste0("'", DEBUG_MODE, "' == 'true'"),
      
      div(
        class = "debug-panel",
        
        # Collapsible debug panel
        div(
          class = "card",
          
          div(
            class = "card-header",
            `data-bs-toggle` = "collapse",
            `data-bs-target` = "#debugCollapse",
            
            h6(
              icon("bug"), " Development Debug Panel ",
              tags$small("(click to toggle)", style = "color: rgba(255,255,255,0.8);"),
              style = "margin: 0; cursor: pointer;"
            )
          ),
          
          div(
            id = "debugCollapse",
            class = "collapse",  # Start collapsed
            
            div(
              class = "card-body",
              
              # Debug warning
              div(
                class = "debug-warning",
                icon("exclamation-triangle"), 
                strong(" Development Mode Active"), 
                " - This panel will be hidden in production. Set DEBUG_MODE=false to disable."
              ),
              
              # Debug tabs for better organization
              tabsetPanel(
                type = "pills",
                
                tabPanel(
                  "App Status",
                  br(),
                  p("Current application state and authentication status:"),
                  verbatimTextOutput("debug_info")
                ),
                
                tabPanel(
                  "Data Structure", 
                  br(),
                  p("Data structure and component breakdown:"),
                  verbatimTextOutput("data_structure_debug")
                ),
                
                tabPanel(
                  "Usage Guide",
                  br(),
                  div(
                    h6("Debug Panel Usage:"),
                    tags$ul(
                      tags$li("App Status: Shows authentication, resident info, and milestone data availability"),
                      tags$li("Data Structure: Shows loaded data components and their sizes"),
                      tags$li("Console output appears in your R console/server logs"),
                      tags$li("Set DEBUG_MODE=false environment variable for production")
                    ),
                    
                    h6("Common Issues:"),
                    tags$ul(
                      tags$li("If milestone data shows 0 rows, check data filtering"),
                      tags$li("If authentication fails, verify access codes in resident data"),
                      tags$li("If no configurations found, check form data availability")
                    )
                  )
                )
              )
            )
          )
        )
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