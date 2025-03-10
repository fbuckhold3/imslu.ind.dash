# UI with customized theme
ui <-  page_fluid(
  # Include custom CSS and JS for hover effects and custom message handling
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
  ")),
    tags$script(HTML("
    Shiny.addCustomMessageHandler('openURL', function(message) {
      window.open(message.url, '_blank');
    });
    
    $(document).on('click', '#plus_delta_card', function() {
      Shiny.setInputValue('module_selected', 'plus_delta');
    });
    $(document).on('click', '#continuity_card', function() {
      Shiny.setInputValue('module_selected', 'continuity');
    });
    $(document).on('click', '#observational_card', function() {
      Shiny.setInputValue('module_selected', 'observational');
    });
    $(document).on('click', '#inpatient_card', function() {
      Shiny.setInputValue('module_selected', 'inpatient');
    });
    $(document).on('click', '#other_card', function() {
      Shiny.setInputValue('module_selected', 'other');
    });
    $(document).on('click', '#milestone_card', function() {
      Shiny.setInputValue('module_selected', 'milestone');
    });
    $(document).on('click', '#assessment_card', function() {
      Shiny.setInputValue('module_selected', 'assessment');
    });
  "))
  ),
  
  # Title with logo and dashboard name
  div(
    style = "display: flex; align-items: center; margin-bottom: 20px;",
    img(src = "ssm_slucare.png", height = "60px", style = "margin-right: 15px; vertical-align: middle;"),
    span(style = "font-size: 24px; font-weight: bold; color: #004B87;", "IMSLU Resident Dashboard")
  ),
  
  # Link container for top navigation links
  div(
    class = "container mt-3",
    div(
      style = "display: flex; justify-content: space-around; align-items: center; padding: 10px; border: 1px solid #ddd; border-radius: 8px; background-color: #f8f9fa;",
      actionButton(
        "resident_assess_link", 
        "Resident Assessment System", 
        onclick = "window.open('https://redcapsurvey.slu.edu/surveys/?s=RT9NNXYYM7', '_blank')",
        class = "btn btn-primary btn-sm"
      ),
      actionButton(
        "faculty_eval_link", 
        "Faculty Evaluations", 
        class = "btn btn-primary btn-sm"
      )
    )
  ),
  
  # Container for resident intro info and access code input
  div(
    class = "container mt-4",
    # Resident name banner (will update after access code is provided)
    div(
      class = "resident-header mb-4",
      style = "background: linear-gradient(135deg, #0072B2 20%, #56B4E9 80%); color: white; padding: 15px; border-radius: 8px;",
      h3(textOutput("resident_name"), style = "margin: 0; font-weight: bold;")
    ),
    
    # Access Code input for manual testing
    div(
      style = "text-align: center; margin-bottom: 20px;",
      textInput("access_code_input", "Enter Access Code:", value = "WdyPEh")
    ),
    
    # Resident Assessments Section
    card(
      style = "background-color: #f8f9fa; border-radius: 10px; box-shadow: 2px 2px 10px rgba(0,0,0,0.1); margin-bottom: 20px;",
      card_header(
        h3("Resident Assessments", style = "margin: 0; color: #004B87;")
      ),
      card_body(
        # Row: Plot (3/4 width) and Progress (1/4 width)
        fluidRow(
          column(
            width = 9,
            plotOutput("res_ass", height = "250px")
          ),
          column(
            width = 3,
            uiOutput("resident_progress")
          )
        )
      )
    ),
    
    # Faculty Evaluations Section
    card(
      style = "background-color: #f8f9fa; border-radius: 10px; box-shadow: 2px 2px 10px rgba(0,0,0,0.1);",
      card_header(
        h3("Faculty Evaluations", style = "margin: 0; color: #004B87;")
      ),
      card_body(
        # Row: Plot (3/4 width) and Progress (1/4 width)
        fluidRow(
          column(
            width = 9,
            plotOutput("fac_eval", height = "250px")
          ),
          column(
            width = 3,
            uiOutput("faculty_progress")
          )
        )
      )
    )
  ),
  
  # Module selection title
  div(
    class = "mt-4 mb-3",
    style = "padding: 15px; background: linear-gradient(90deg, #0072B2, #56B4E9); border-radius: 8px; color: white; text-align: center;",
    h3("Explore Your Data", style = "margin-bottom: 10px;")
  ),
  
  # Clickable module cards with hover effects - simplified to just be navigation cards
  layout_column_wrap(
    width = "230px",
    gap = "12px",
    style = "margin-top: 20px;",
    
    # Plus/Delta card
    card(
      id = "plus_delta_card",
      card_header("Plus / Delta Feedback", style = "color: #004B87; font-weight: bold;"),
      card_body(
        style = "text-align: center;",
        icon("comments", "fa-4x", style = "color: #0072B2; margin-bottom: 10px;")
      ),
      style = "background: #E3F2FD; border-radius: 10px; box-shadow: 3px 3px 8px rgba(0,0,0,0.1); height: 180px; cursor: pointer;",
      class = "module-card"
    ),
    
    # Continuity Clinic card - simplified to just be a navigation card
    card(
      id = "continuity_card",
      card_header("Continuity Clinic Evaluations", style = "color: #004B87; font-weight: bold;"),
      card_body(
        style = "text-align: center;",
        icon("stethoscope", "fa-4x", style = "color: #0072B2; margin-bottom: 10px;")
      ),
      style = "background: #E3F2FD; border-radius: 10px; box-shadow: 3px 3px 8px rgba(0,0,0,0.1); height: 180px; cursor: pointer;",
      class = "module-card"
    ),
    
    # Observational Data card
    card(
      id = "observational_card",
      card_header("Observational Data", style = "color: #E65100; font-weight: bold;"),
      card_body(
        style = "text-align: center;",
        icon("eye", "fa-4x", style = "color: #F57C00; margin-bottom: 10px;")
      ),
      style = "background: #FFF3E0; border-radius: 10px; box-shadow: 3px 3px 8px rgba(0,0,0,0.1); height: 180px; cursor: pointer;",
      class = "module-card"
    ),
    
    # Inpatient Data card
    card(
      id = "inpatient_card",
      card_header("Inpatient Data", style = "color: #1B5E20; font-weight: bold;"),
      card_body(
        style = "text-align: center;",
        icon("hospital", "fa-4x", style = "color: #2E7D32; margin-bottom: 10px;")
      ),
      style = "background: #E8F5E9; border-radius: 10px; box-shadow: 3px 3px 8px rgba(0,0,0,0.1); height: 180px; cursor: pointer;",
      class = "module-card"
    ),
    
    # Milestones card
    card(
      id = "milestone_card",
      card_header("Milestones", style = "color: #F9A825; font-weight: bold;"),
      card_body(
        style = "text-align: center;",
        icon("chart-line", "fa-4x", style = "color: #FBC02D; margin-bottom: 10px;")
      ),
      style = "background: #FFFDE7; border-radius: 10px; box-shadow: 3px 3px 8px rgba(0,0,0,0.1); height: 180px; cursor: pointer;",
      class = "module-card"
    ),
    
    # Self-Assessment card
    card(
      id = "assessment_card",
      card_header("Self-Assessment", style = "color: #6A1B9A; font-weight: bold;"),
      card_body(
        style = "text-align: center;",
        icon("user-check", "fa-4x", style = "color: #8E24AA; margin-bottom: 10px;")
      ),
      style = "background: #F3E5F5; border-radius: 10px; box-shadow: 3px 3px 8px rgba(0,0,0,0.1); height: 180px; cursor: pointer;",
      class = "module-card"
    ),
    
    # Other Data card
    card(
      id = "other_card",
      card_header("Other Data", style = "color: #1565C0; font-weight: bold;"),
      card_body(
        style = "text-align: center;",
        icon("chart-bar", "fa-4x", style = "color: #1976D2; margin-bottom: 10px;")
      ),
      style = "background: #E3F2FD; border-radius: 10px; box-shadow: 3px 3px 8px rgba(0,0,0,0.1); height: 180px; cursor: pointer;",
      class = "module-card"
    )
  ),
  
  # Container for module content - This will display below the cards when a card is clicked
  div(
    id = "module_content",
    style = "margin-top: 30px;",
    uiOutput("selected_module_ui")
  )
)
