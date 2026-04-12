# mod_milestones.R ─ Milestones
# Shows spider / radar plots and individual milestone curves.
# Uses the same milestone visualization as the CCC dashboard.
# TODO: wire gmed milestone_module (spider) and individual curve plots.

mod_milestones_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "row g-3",
      # Spider plot card
      div(class = "col-lg-6",
        div(class = "gmed-card h-100",
          div(class = "card-body", uiOutput(ns("spider")))
        )
      ),
      # Individual curves card
      div(class = "col-lg-6",
        div(class = "gmed-card h-100",
          div(class = "card-body", uiOutput(ns("curves")))
        )
      )
    )
  )
}

mod_milestones_server <- function(id, rdm_data, resident_id) {
  moduleServer(id, function(input, output, session) {

    milestone_data <- reactive({
      req(rdm_data(), resident_id())
      # TODO: extract and shape milestone data for this resident
      # Similar pattern to imslu.ccc.dashboard milestone processing
      NULL
    })

    output$spider <- renderUI({
      req(rdm_data(), resident_id())
      # TODO: wire gmed::milestone_module spider plot
      #   milestone_ui(session$ns("spider_plot"))
      section_placeholder("graph-up-arrow", "Milestone spider — coming soon")
    })

    output$curves <- renderUI({
      req(rdm_data(), resident_id())
      # TODO: wire individual milestone curves
      #   (same code as CCC dashboard, adapted for single resident)
      section_placeholder("activity", "Individual milestone curves — coming soon")
    })

    # TODO: when wiring:
    # milestone_server("spider_plot",
    #   milestone_data = milestone_data,
    #   resident_id    = resident_id
    # )
  })
}
