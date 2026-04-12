# ui.R ─ IMSLU Resident Dashboard
# Thin shell — all content is driven by server-side nav state.

ui <- gmed_page(
  title         = "IMSLU Resident Dashboard",
  theme_variant = "slucare",

  useShinyjs(),

  tags$head(
    # Bootstrap Icons
    tags$link(
      rel  = "stylesheet",
      href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/bootstrap-icons.min.css"
    ),
    tags$link(rel = "stylesheet", type = "text/css", href = "milestone_dashboard.css"),

    # Inline overrides — guaranteed to load last and win
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
      .gmed-nav-block-icon {
        font-size: 3.6rem !important;
        padding-top: 0 !important;
        margin-bottom: 20px !important;
      }
      .gmed-nav-block-label {
        font-size: 1.15rem !important;
        margin-bottom: 8px !important;
      }
      .gmed-nav-block-desc {
        font-size: 0.84rem !important;
        line-height: 1.45 !important;
      }
      @media (max-width: 992px) {
        .gmed-nav-grid { grid-template-columns: repeat(2, 1fr) !important; }
      }
    "))
  ),

  # Single content area — server renders login | home | section
  div(
    class = "container-fluid py-4 px-4",
    uiOutput("main_view")
  )
)
