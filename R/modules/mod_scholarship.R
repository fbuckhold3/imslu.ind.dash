# mod_scholarship.R ─ Scholarship & Teaching Portfolio
# Dynamic table of scholarly activities completed as a resident.
# Source data comes from the scholarship REDCap instrument.
# TODO: wire gmed scholarship_display / mod_scholarship_wrapper.

mod_scholarship_ui <- function(id) {
  ns <- NS(id)
  div(class = "gmed-card",
    div(class = "card-body", uiOutput(ns("content")))
  )
}

mod_scholarship_server <- function(id, rdm_data, resident_id) {
  moduleServer(id, function(input, output, session) {

    my_scholarship <- reactive({
      req(rdm_data(), resident_id())
      schol <- rdm_data()$all_forms$scholarship
      if (is.null(schol) || nrow(schol) == 0) return(NULL)
      schol %>% filter(record_id == resident_id())
    })

    output$content <- renderUI({
      req(rdm_data(), resident_id())
      n <- if (!is.null(my_scholarship())) nrow(my_scholarship()) else 0
      # TODO: replace with gmed::display_scholarship() or mod_scholarship_wrapper
      #   display_scholarship(my_scholarship(), session)
      section_placeholder("award-fill",
        paste0(n, " scholarship entries loaded — table view coming soon"))
    })
  })
}
