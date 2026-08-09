# mod_auth_ui.R — Login page HTML
# Kept separate from server.R so the login view is easy to maintain.
# Input IDs must match what mod_auth_server("auth") expects:
#   auth-access_code        — password input
#   auth-access_code_btn    — sign-in trigger (set via JS / button onclick)
#   auth-access_code_error  — error message uiOutput

mod_auth_page_ui <- function() {
  tagList(

    # Full-width brand header \u2014 includes a slow, looping "pulse trace" line,
    # on-brand with the vitals-monitor palette story (see gmed's Restrained
    # theme) rather than decoration for its own sake. Respects
    # prefers-reduced-motion (see milestone_dashboard.css).
    div(
      class = "login-brand-header",
      tags$svg(class = "pulse-trace", viewBox = "0 0 400 46", preserveAspectRatio = "none",
        tags$path(d = "M0,23 L120,23 L136,23 L146,4 L158,42 L170,14 L180,23 L400,23")
      ),
      div(class = "login-brand-badge", "GME TOOLS"),
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
          class = "gmed-card card-entrance",

          # Welcome header
          div(
            class = "text-center mb-4",
            tags$h2(
              class = "mb-2",
              style = "display:flex; align-items:center; justify-content:center; gap:10px;",
              div(class = "icon-glow",
                tags$i(class = "bi bi-person-circle", style = "color: var(--gmed-secondary); font-size:1.6rem;")
              ),
              tags$span(style = "color: var(--gmed-primary); font-weight: 700;", "Welcome")
            ),
            tags$p(
              style = "font-size:1rem; color:var(--gmed-text-secondary); margin-bottom:0;",
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
