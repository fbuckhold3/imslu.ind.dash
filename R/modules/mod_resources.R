# mod_resources.R ─ Program Resources
# Link cards to SharePoint, other apps, and program tools.
# Edit resource_links below to add / remove / reorder items.

resource_links <- list(
  list(
    label = "Program SharePoint",
    desc  = "Documents, policies & program information",
    icon  = "microsoft",
    url   = "#",   # TODO: replace with actual SharePoint URL
    external = TRUE
  ),
  list(
    label = "Record an Assessment",
    desc  = "Submit a faculty evaluation of a resident",
    icon  = "clipboard2-check",
    url   = "https://fbuckhold3-imslu-resident-assessment.share.connect.posit.cloud",
    external = TRUE
  ),
  list(
    label = "Faculty Evaluation Form",
    desc  = "Submit your evaluation of a faculty member",
    icon  = "person-check",
    url   = "https://fbuckhold3-imslu-facultyeval.share.connect.posit.cloud",
    external = TRUE
  ),
  list(
    label = "Noon Conference",
    desc  = "Record noon conference attendance",
    icon  = "people-fill",
    url   = "https://fbuckhold3-imslu-at-noon.share.connect.posit.cloud",
    external = TRUE
  )
  # Add more resources here as needed
)

mod_resources_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "gmed-nav-grid",   # reuse the same 3-col grid from the home screen
    lapply(resource_links, function(r) {
      tags$a(
        href   = r$url,
        target = if (isTRUE(r$external)) "_blank" else NULL,
        style  = "text-decoration: none;",
        div(
          class = "gmed-nav-block",
          div(class = "gmed-nav-block-icon",
            tags$i(class = paste0("bi bi-", r$icon))
          ),
          div(
            div(class = "gmed-nav-block-label", r$label),
            div(class = "gmed-nav-block-desc",  r$desc)
          )
        )
      )
    })
  )
}

mod_resources_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Static links — no server logic needed
  })
}
