# mod_schedule.R ─ Schedule
# Placeholder — rotation schedule display.
# TODO: connect to scheduling system / data source (TBD).

mod_schedule_ui <- function(id) {
  ns <- NS(id)
  div(class = "gmed-card",
    div(
      class = "card-body d-flex flex-column align-items-center justify-content-center py-5",
      tags$i(class = "bi bi-calendar3-fill",
             style = "font-size:3rem; color:var(--ssm-secondary-blue); opacity:0.4; margin-bottom:16px;"),
      tags$p("Schedule", style = "font-weight:700; font-size:1.1rem; color:var(--ssm-primary-blue); margin-bottom:8px;"),
      tags$p("Rotation schedule integration coming soon.",
             style = "font-size:0.88rem; color:var(--ssm-text-muted); margin:0;")
    )
  )
}

mod_schedule_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # No data dependencies yet
  })
}
