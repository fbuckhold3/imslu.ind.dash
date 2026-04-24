# mod_auth_ui.R — Login page HTML
# Kept separate from server.R so the login view is easy to maintain.
# Input IDs must match what mod_auth_server("auth") expects:
#   auth-access_code        — password input
#   auth-access_code_btn    — sign-in trigger (set via JS / button onclick)
#   auth-access_code_error  — error message uiOutput

mod_auth_page_ui <- function() {
  tagList(

    # Full-width brand header
    div(
      class = "login-brand-header",
      div(class = "login-brand-badge", "SSM HEALTH \u00b7 SLUCARE"),
      tags$h1("IMSLU Resident Dashboard", class = "login-brand-title"),
      div(class = "login-brand-sub",
          "Internal Medicine \u00b7 Saint Louis University")
    ),

    # Centered login card
    div(
      class = "row justify-content-center",
      div(
        class = "col-lg-6 col-md-8 col-12",
        div(
          class = "gmed-card",

          # Welcome header
          div(
            class = "text-center mb-4",
            tags$h2(
              class = "mb-2",
              style = "color: #003d5c; font-weight: 700;",
              tags$i(class = "bi bi-person-circle me-2", style = "color: #0066a1;"),
              "Welcome"
            ),
            tags$p(
              style = "font-size:1rem; color:#546e7a; margin-bottom:0;",
              "Access your evaluations, milestones, learning plan, and more."
            ),
            tags$hr(style = "margin-top:1rem;")
          ),

          # Disclaimer
          div(
            class = "login-disclaimer",
            paste(
              "This dashboard is for residents in the IMSLU Internal Medicine",
              "Residency Program to access their evaluations, competency progress,",
              "and learning plans. By entering your access code you acknowledge that",
              "this data is intended solely for the named resident and authorized",
              "program leadership. Unauthorized access or distribution is prohibited."
            )
          ),

          # Access code form
          div(
            tags$label(
              "Access Code",
              style = "font-size:0.9rem; font-weight:600; color:#2d3748; margin-bottom:6px; display:block;"
            ),
            tags$p(
              style = "font-size:0.82rem; color:#6c757d; margin-bottom:10px; margin-top:-2px;",
              "Your access code was provided by the program coordinator."
            ),
            tags$input(
              id           = "auth-access_code",
              type         = "password",
              placeholder  = "Enter your access code",
              autocomplete = "off",
              class        = "login-code-input"
            ),
            tags$button(
              class   = "btn-gmed-primary login-submit-btn",
              onclick = "Shiny.setInputValue('auth-access_code_btn', Math.random(), {priority:'event'})",
              tags$i(class = "bi bi-box-arrow-in-right me-2"), "Sign In"
            ),
            uiOutput("auth-access_code_error")
          )
        )
      )
    )
  )
}
