# roundsui viz gallery
#
# A standalone demo of the Data-Visualization wrapper family: status
# badges, the roundsui_datatable() constructor (populated, empty, and
# row-clickable), and a plotly figure themed via roundsui_plotly_layout().
# Run with:
#
#   shiny::runApp(system.file("examples/viz_gallery", package = "roundsui"))
#
# or, while developing roundsui itself (not yet installed):
#
#   devtools::load_all("roundsui")
#   shiny::runApp("roundsui/inst/examples/viz_gallery")

library(shiny)
library(bslib)

if (!requireNamespace("roundsui", quietly = TRUE)) {
  find_pkg_root <- function(start) {
    dir <- normalizePath(start)
    for (i in 1:6) {
      if (file.exists(file.path(dir, "DESCRIPTION"))) return(dir)
      parent <- dirname(dir)
      if (identical(parent, dir)) break
      dir <- parent
    }
    stop("Could not find roundsui's DESCRIPTION file above ", start)
  }
  devtools::load_all(find_pkg_root(getwd()))
}

have_plotly <- requireNamespace("plotly", quietly = TRUE)

section <- function(title, subtitle = NULL, ...) {
  div(
    class = "gallery-section",
    div(class = "gallery-section__head", h2(title), if (!is.null(subtitle)) p(class = "gallery-section__sub", subtitle)),
    div(class = "gallery-section__body", ...)
  )
}
swatch_card <- function(...) div(class = "gallery-card", ...)

residents <- data.frame(
  Name = c("R. Ahmadi", "J. Okafor", "S. Lindqvist"),
  Level = c("PGY-2", "PGY-1", "PGY-3"),
  `Last Review` = c("2026-06-15", "2026-07-01", NA),
  Status = c("On track", "Watch", "On track"),
  check.names = FALSE
)

ui <- roundsui::roundsui_page(
  title = "roundsui viz gallery",
  tags$head(tags$style(HTML("
    body { background: var(--roundsui-ground); }
    .gallery-shell { max-width: 980px; margin: 0 auto; padding: 32px 20px 64px; display: flex; flex-direction: column; gap: 28px; }
    .gallery-head h1 { font-size: 20px; font-weight: 650; letter-spacing: -0.01em; margin: 0 0 4px; }
    .gallery-head p { color: var(--roundsui-ink-muted); font-size: 13px; margin: 0; }
    .gallery-section__head h2 { font-size: 14px; font-weight: 650; margin: 0 0 2px; text-transform: uppercase; letter-spacing: 0.04em; color: var(--roundsui-ink-muted); }
    .gallery-section__sub { font-size: 12.5px; color: var(--roundsui-ink-faint); margin: 0 0 12px; }
    .gallery-section__body { display: flex; flex-direction: column; gap: 14px; }
    .gallery-row { display: flex; gap: 10px; flex-wrap: wrap; align-items: center; }
    .gallery-card { background: var(--roundsui-surface); border: 1px solid var(--roundsui-border); border-radius: var(--roundsui-radius-md); box-shadow: var(--roundsui-shadow); overflow: hidden; }
    .gallery-card__label { font-size: 11px; text-transform: uppercase; letter-spacing: 0.05em; color: var(--roundsui-ink-faint); padding: 10px 14px 0; }
    .gallery-card__body { padding: 10px 14px 14px; }
  "))),
  div(
    class = "gallery-shell",
    div(class = "gallery-head",
        h1("roundsui — viz gallery"),
        p("Data-Visualization wrapper family: status conventions, table chrome, chart theming.")
    ),

    section("Status",
            "roundsui_status_badge() - pill and dot variants",
            div(class = "gallery-card",
                div(class = "gallery-card__label", "Pill"),
                div(class = "gallery-card__body gallery-row",
                    roundsui::roundsui_status_badge("Complete", status = "complete"),
                    roundsui::roundsui_status_badge("In progress", status = "in-progress"),
                    roundsui::roundsui_status_badge("Incomplete", status = "incomplete"),
                    roundsui::roundsui_status_badge("Pending", status = "pending")
                )
            ),
            div(class = "gallery-card",
                div(class = "gallery-card__label", "Dot"),
                div(class = "gallery-card__body gallery-row",
                    roundsui::roundsui_status_badge("On track", status = "success", variant = "dot"),
                    roundsui::roundsui_status_badge("Below median", status = "warning", variant = "dot"),
                    roundsui::roundsui_status_badge("Needs discussion", status = "danger", variant = "dot")
                )
            )
    ),

    section("Table",
            "roundsui_datatable() - populated (with highlight + row click), and empty",
            swatch_card(
              div(class = "gallery-card__label", "Populated, highlight_columns + row_click_input_id"),
              div(style = "padding: 4px 14px 14px;",
                  DT::DTOutput("residents_table"),
                  div(style = "margin-top: 8px; font-size: 12.5px; color: var(--roundsui-ink-muted); font-family: ui-monospace, monospace;",
                      textOutput("clicked_row", inline = TRUE))
              )
            ),
            swatch_card(
              div(class = "gallery-card__label", "Empty (roundsui_empty_table() wired in automatically)"),
              div(style = "padding: 4px 14px 14px;", DT::DTOutput("empty_table"))
            )
    ),

    if (have_plotly) {
      section("Chart",
              "roundsui_plotly_layout() - chrome applied to an ordinary plotly figure",
              swatch_card(
                div(class = "gallery-card__label", "Milestone trend, three review periods"),
                div(style = "padding: 8px 14px 14px;", plotly::plotlyOutput("demo_chart", height = "260px"))
              )
      )
    } else {
      section("Chart", "plotly not installed in this environment - skipping the chart demo.")
    }
  )
)

server <- function(input, output, session) {
  output$residents_table <- DT::renderDT({
    roundsui::roundsui_datatable(
      residents,
      caption = "residents",
      highlight_columns = "Status",
      row_click_input_id = "clicked_resident"
    )
  })

  format_clicked <- function(x) {
    if (requireNamespace("jsonlite", quietly = TRUE)) {
      jsonlite::toJSON(x, auto_unbox = TRUE)
    } else {
      paste(names(x), unlist(x), sep = "=", collapse = ", ")
    }
  }

  output$clicked_row <- renderText({
    if (is.null(input$clicked_resident)) {
      "Click a row to see its data captured here (no hardcoded column schema)."
    } else {
      paste0("input$clicked_resident = ", format_clicked(input$clicked_resident))
    }
  })

  output$empty_table <- DT::renderDT({
    roundsui::roundsui_datatable(data.frame(), caption = "assessments")
  })

  if (have_plotly) {
    output$demo_chart <- plotly::renderPlotly({
      plotly::plot_ly(
        x = c("P0", "P1", "P2"), y = c(3.5, 4.5, 5.5),
        type = "scatter", mode = "lines+markers",
        line = list(color = roundsui::roundsui_colors()$accent, width = 2.5),
        marker = list(color = roundsui::roundsui_colors()$accent, size = 8)
      ) |>
        roundsui::roundsui_plotly_layout(
          title = list(text = "Subcompetency level over time", font = list(size = 13)),
          yaxis = list(title = "Level", range = c(0, 9))
        )
    })
  }
}

shinyApp(ui, server)
