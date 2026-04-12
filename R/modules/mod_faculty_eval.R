# mod_faculty_eval.R ─ Faculty Evaluations Done
# Shows a summary / graph of faculty evaluations the resident has submitted.
# Optionally embeds the faculty evaluation form for submission.
# TODO: add chart of evaluations completed over time, embed imslu.facultyeval link.

mod_faculty_eval_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "row g-3",
      # Summary chart
      div(class = "col-lg-7",
        div(class = "gmed-card h-100",
          div(class = "card-body", uiOutput(ns("chart")))
        )
      ),
      # Quick-submit link card
      div(class = "col-lg-5",
        div(class = "gmed-card h-100",
          div(
            class = "card-body d-flex flex-column justify-content-center align-items-center text-center",
            tags$i(class = "bi bi-box-arrow-up-right",
                   style = "font-size:2rem; color:var(--ssm-secondary-blue); margin-bottom:12px;"),
            tags$p("Submit a Faculty Evaluation",
                   style = "font-weight:600; color:var(--ssm-primary-blue); margin-bottom:12px;"),
            tags$a(
              href   = "https://fbuckhold3-imslu-facultyeval.share.connect.posit.cloud",
              target = "_blank",
              class  = "btn btn-sm",
              style  = "background:var(--ssm-primary-blue); color:white; border:none;",
              tags$i(class = "bi bi-pencil-square me-1"), "Open Faculty Eval Form"
            )
          )
        )
      )
    )
  )
}

mod_faculty_eval_server <- function(id, rdm_data, resident_id) {
  moduleServer(id, function(input, output, session) {

    output$chart <- renderUI({
      req(rdm_data(), resident_id())
      # TODO: query faculty eval form, count by period/date, render plotly bar
      section_placeholder("person-check-fill",
        "Faculty evaluations completed — chart coming soon")
    })
  })
}
