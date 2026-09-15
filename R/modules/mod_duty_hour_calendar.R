# mod_duty_hour_calendar.R ─ Duty Hours: calendar grid (Phase 2)
#
# Month-at-a-time grid, one cell per day, color-coded by status. Clicking a
# day cell sets the shared `selected_date` reactiveVal (owned by the parent
# orchestrator, mod_duty_hours.R) so mod_duty_hour_confirm shows/edits that
# specific date instead of the queue's oldest. Own month-navigation state
# is local (not shared — no other module needs to know which month is
# displayed).
#
# Click wiring uses a plain onchange/onclick -> Shiny.setInputValue()
# pattern (one shared input id for every cell, the clicked date as the
# value) rather than one actionButton per day — registering up to 31
# separate observeEvent()s per month, recreated on every render, would be
# fragile (duplicate-firing / cleanup risk); one observer on one input
# handles every cell regardless of how many days are in the month.
#
# Status colors (legend rendered in the UI):
#   green  = saved entry exists (confirmed or entered)
#   blue   = Amion default available, not yet confirmed
#   red    = no Amion default AND no saved entry (needs manual entry)
#   gray   = off/vacation/jeopardy (0h by design, not a to-do item)
#   faded  = future date (nothing to confirm yet) or no data at all

.DH_CAL_COLORS <- c(
  confirmed = "#2e9e5b", unconfirmed = "#2a78d6",
  needs_entry = "#c0392b", off = "#ccd3d4", future = "#eef2f3"
)

mod_duty_hour_calendar_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "d-flex justify-content-between align-items-center mb-2",
      actionButton(ns("prev_month"), "‹ Prev", class = "btn btn-sm btn-outline-secondary"),
      h5(textOutput(ns("month_label"), inline = TRUE), style = "margin: 0;"),
      actionButton(ns("next_month"), "Next ›", class = "btn btn-sm btn-outline-secondary")
    ),
    uiOutput(ns("grid")),
    div(class = "d-flex gap-3 mt-2 small text-muted flex-wrap",
      tags$span(tags$span(style = sprintf("display:inline-block;width:10px;height:10px;background:%s;border-radius:2px;margin-right:4px;", .DH_CAL_COLORS[["unconfirmed"]])), "Unconfirmed (Amion default)"),
      tags$span(tags$span(style = sprintf("display:inline-block;width:10px;height:10px;background:%s;border-radius:2px;margin-right:4px;", .DH_CAL_COLORS[["confirmed"]])), "Confirmed"),
      tags$span(tags$span(style = sprintf("display:inline-block;width:10px;height:10px;background:%s;border-radius:2px;margin-right:4px;", .DH_CAL_COLORS[["needs_entry"]])), "Needs entry"),
      tags$span(tags$span(style = sprintf("display:inline-block;width:10px;height:10px;background:%s;border-radius:2px;margin-right:4px;", .DH_CAL_COLORS[["off"]])), "Off/Vacation")
    )
  )
}

#' @param entries_r,amion_blocks_r Shared reactives from the parent
#'   orchestrator (see mod_duty_hours.R).
#' @param selected_date Shared reactiveVal — clicking a cell sets it.
mod_duty_hour_calendar_server <- function(id, resident_id, entries_r, amion_blocks_r, selected_date) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    month_start <- reactiveVal(as.Date(format(Sys.Date(), "%Y-%m-01")))

    observeEvent(input$prev_month, month_start(seq(month_start(), length.out = 2, by = "-1 month")[2]))
    observeEvent(input$next_month, month_start(seq(month_start(), length.out = 2, by = "1 month")[2]))
    observeEvent(input$day_click, {
      req(input$day_click)
      selected_date(as.Date(input$day_click))
    })

    output$month_label <- renderText(format(month_start(), "%B %Y"))

    day_status <- reactive({
      ms <- month_start()
      me <- seq(ms, length.out = 2, by = "1 month")[2] - 1
      db <- amion_blocks_r() |> dplyr::filter(Date >= ms, Date <= me)
      entries <- entries_r()
      covered <- entries$dh_date[entries$dh_date >= ms & entries$dh_date <= me]

      all_days <- seq(ms, me, by = "day")
      vapply(all_days, function(d) {
        if (d > Sys.Date()) return("future")
        if (d %in% covered) return("confirmed")
        rows <- db[db$Date == d, ]
        if (nrow(rows) == 0) return("future")  # no data at all (outside Amion pull range etc.)
        if (all(rows$source %in% c("vacation", "day_off", "jeopardy"))) return("off")
        if (all(is.na(rows$Hours))) return("needs_entry")
        "unconfirmed"
      }, character(1))
    })

    output$grid <- renderUI({
      ms <- month_start()
      me <- seq(ms, length.out = 2, by = "1 month")[2] - 1
      all_days <- seq(ms, me, by = "day")
      status <- day_status()
      lead_blanks <- as.integer(format(ms, "%w"))  # 0 = Sunday

      cells <- lapply(seq_along(all_days), function(i) {
        d <- all_days[i]
        st <- status[i]
        is_future <- st == "future" && d > Sys.Date()
        clickable <- st != "future" || d <= Sys.Date()
        tags$div(
          class = "dh-cal-cell",
          style = sprintf(
            "background:%s; opacity:%s; cursor:%s; border-radius:4px; padding:6px; text-align:center; min-height:44px;",
            .DH_CAL_COLORS[[st]], if (is_future) "0.5" else "1",
            if (clickable && !is_future) "pointer" else "default"
          ),
          onclick = if (clickable && !is_future)
            sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", ns("day_click"), as.character(d))
          else NULL,
          tags$div(format(d, "%e"), style = "font-weight:600; font-size:0.85rem;")
        )
      })

      blanks <- lapply(seq_len(lead_blanks), function(i) tags$div())

      tags$div(
        style = "display:grid; grid-template-columns: repeat(7, 1fr); gap: 4px;",
        tags$div("S", class = "text-muted small text-center"), tags$div("M", class = "text-muted small text-center"),
        tags$div("T", class = "text-muted small text-center"), tags$div("W", class = "text-muted small text-center"),
        tags$div("T", class = "text-muted small text-center"), tags$div("F", class = "text-muted small text-center"),
        tags$div("S", class = "text-muted small text-center"),
        blanks, cells
      )
    })
  })
}
