# ui.R ─ IMSLU Resident Dashboard
# Thin shell — all content is driven by server-side nav state.
# roundsui-integration-test branch: gmed_page()/loading overlay swapped for
# roundsui equivalents to verify the port against this app's real code and
# real TEST-project data. Not merged to main.

ui <- roundsui::roundsui_page(
  title = "IMSLU Resident Dashboard",

  useShinyjs(),

  tags$head(
    # Bootstrap Icons
    tags$link(
      rel  = "stylesheet",
      href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/bootstrap-icons.min.css"
    ),
    tags$link(rel = "stylesheet", type = "text/css", href = "milestone_dashboard.css"),
    tags$script(src = "app.js"),

    # Font sizes for .gmed-nav-block-icon/-label/-desc now live in
    # gmed-themes.css (shared with imslu.ccc.dashboard, which used this same
    # tile layout). Only this app's own grid column count stays local.
    tags$style(HTML("
      .gmed-nav-grid {
        display: grid !important;
        grid-template-columns: repeat(4, 1fr) !important;
        gap: 20px !important;
      }
      .gmed-nav-block {
        flex-direction: column !important;
        align-items: center !important;
        justify-content: center !important;
        text-align: center !important;
        min-height: 240px !important;
        padding: 40px 28px !important;
        gap: 0 !important;
      }
      @media (max-width: 992px) {
        .gmed-nav-grid { grid-template-columns: repeat(2, 1fr) !important; }
      }
    "))
  ),

  # Injects a CSS rule to hide the overlay once data_ready() fires server-side
  uiOutput("overlay_hide"),

  # Startup loading overlay — hidden by server once data_ready() fires
  # (server.R's overlay_hide targets #loading_overlay by id; unchanged)
  roundsui::roundsui_loading_overlay(
    id = "loading_overlay",
    brand = "IMSLU Resident Dashboard",
    message = "Loading…"
  ),

  # Single content area — server renders login | home | section
  div(
    class = "container-fluid py-4 px-4",
    uiOutput("main_view")
  )
)
