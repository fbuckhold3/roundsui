# roundsui state gallery
#
# A standalone demo of every Shared Feedback/State component, styled in
# "Ward Notes", so they can be checked visually without wiring into a
# real app first. Run with:
#
#   shiny::runApp(system.file("examples/state_gallery", package = "roundsui"))
#
# or, while developing roundsui itself (not yet installed):
#
#   devtools::load_all("roundsui")
#   shiny::runApp("roundsui/inst/examples/state_gallery")

library(shiny)
library(bslib)

if (!requireNamespace("roundsui", quietly = TRUE)) {
  # Being run straight from source during development, before install:
  # walk up from this app's directory to find the package root (the
  # directory holding DESCRIPTION), then devtools::load_all() it.
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

section <- function(title, subtitle = NULL, ...) {
  div(
    class = "gallery-section",
    div(class = "gallery-section__head",
        h2(title),
        if (!is.null(subtitle)) p(class = "gallery-section__sub", subtitle)
    ),
    div(class = "gallery-section__body", ...)
  )
}

swatch_card <- function(...) {
  div(class = "gallery-card", ...)
}

ui <- page_fluid(
  theme = roundsui::create_roundsui_theme(),
  roundsui::load_roundsui_styles(),
  tags$head(tags$style(HTML("
    body { background: var(--roundsui-ground); }
    .gallery-shell { max-width: 920px; margin: 0 auto; padding: 32px 20px 64px; display: flex; flex-direction: column; gap: 28px; }
    .gallery-head h1 { font-size: 20px; font-weight: 650; letter-spacing: -0.01em; margin: 0 0 4px; }
    .gallery-head p { color: var(--roundsui-ink-muted); font-size: 13px; margin: 0; }
    .gallery-section__head h2 { font-size: 14px; font-weight: 650; margin: 0 0 2px; text-transform: uppercase; letter-spacing: 0.04em; color: var(--roundsui-ink-muted); }
    .gallery-section__sub { font-size: 12.5px; color: var(--roundsui-ink-faint); margin: 0 0 12px; }
    .gallery-section__body { display: flex; flex-direction: column; gap: 14px; }
    .gallery-row { display: grid; grid-template-columns: repeat(auto-fit, minmax(260px, 1fr)); gap: 14px; }
    .gallery-card { background: var(--roundsui-surface); border: 1px solid var(--roundsui-border); border-radius: var(--roundsui-radius-md); box-shadow: var(--roundsui-shadow); overflow: hidden; }
    .gallery-card__label { font-size: 11px; text-transform: uppercase; letter-spacing: 0.05em; color: var(--roundsui-ink-faint); padding: 10px 14px 0; }
    .gallery-toggle { position: fixed; top: 20px; right: 20px; }
  "))),
  tags$script(HTML("
    function roundsuiToggleTheme() {
      var root = document.documentElement;
      var current = root.getAttribute('data-theme');
      var next = current === 'dark' ? 'light' : (current === 'light' ? null : 'dark');
      if (next) { root.setAttribute('data-theme', next); } else { root.removeAttribute('data-theme'); }
    }
  ")),
  div(
    class = "gallery-shell",
    div(
      class = "gallery-toggle",
      tags$button(
        "Toggle theme", type = "button", class = "btn btn-sm btn-outline-secondary",
        onclick = "roundsuiToggleTheme()"
      )
    ),
    div(class = "gallery-head",
        h1("roundsui — state gallery"),
        p("Shared Feedback/State components, \"Ward Notes\" direction. Everything on this page is real roundsui markup, not a mockup.")
    ),

    section("Loading",
            "roundsui_loading_state() and roundsui_loading_overlay()",
            div(class = "gallery-row",
                swatch_card(
                  div(class = "gallery-card__label", "Panel (default)"),
                  roundsui::roundsui_loading_state("Loading milestone data...")
                ),
                swatch_card(
                  div(class = "gallery-card__label", "Compact"),
                  roundsui::roundsui_loading_state("Loading...", compact = TRUE)
                )
            ),
            actionButton("show_overlay", "Preview full-page overlay", class = "btn btn-sm btn-outline-secondary")
    ),

    section("Empty",
            "roundsui_empty_state(), plus the table-row and chart-annotation variants",
            div(class = "gallery-row",
                swatch_card(
                  div(class = "gallery-card__label", "Panel"),
                  roundsui::roundsui_empty_state(
                    "No assessments match the current filters.",
                    icon = icon("inbox")
                  )
                ),
                swatch_card(
                  div(class = "gallery-card__label", "With action"),
                  roundsui::roundsui_empty_state(
                    "No career goals recorded yet.",
                    icon = icon("bullseye"),
                    action = actionButton("add_goals", "Add goals", class = "btn btn-sm btn-primary")
                  )
                ),
                swatch_card(
                  div(class = "gallery-card__label", "Compact"),
                  roundsui::roundsui_empty_state("No ITE scores available yet.", compact = TRUE)
                )
            ),
            swatch_card(
              div(class = "gallery-card__label", "As a DT table (roundsui_empty_table())"),
              div(style = "padding: 4px 14px 14px;", DT::DTOutput("empty_table_demo"))
            )
    ),

    section("Error",
            "roundsui_inline_error(), roundsui_error_modal(), roundsui_notify()",
            div(class = "gallery-row",
                swatch_card(
                  div(class = "gallery-card__label", "Inline"),
                  div(style = "padding: 14px;",
                      roundsui::roundsui_inline_error("Access code not recognized. Check with your coordinator."))
                ),
                swatch_card(
                  div(class = "gallery-card__label", "Modal + notifications"),
                  div(style = "padding: 14px; display: flex; gap: 8px; flex-wrap: wrap;",
                      actionButton("show_modal", "Show error modal", class = "btn btn-sm btn-outline-danger"),
                      actionButton("notify_success", "Success toast", class = "btn btn-sm btn-outline-success"),
                      actionButton("notify_warning", "Warning toast", class = "btn btn-sm btn-outline-warning"),
                      actionButton("notify_error", "Error toast", class = "btn btn-sm btn-outline-danger")
                  )
                )
            )
    )
  )
)

server <- function(input, output, session) {
  output$empty_table_demo <- DT::renderDT({
    DT::datatable(
      roundsui::roundsui_empty_table("No assessments found."),
      options = list(dom = "t", ordering = FALSE),
      rownames = FALSE,
      class = "roundsui-empty-table-row"
    )
  })

  observeEvent(input$show_modal, {
    showModal(roundsui::roundsui_error_modal(
      "Could not save this evaluation. Check your connection and try again.",
      title = "Save failed"
    ))
  })

  observeEvent(input$notify_success, {
    roundsui::roundsui_notify("Evaluation submitted.", type = "success")
  })
  observeEvent(input$notify_warning, {
    roundsui::roundsui_notify("This subcompetency is below cohort median.", type = "warning")
  })
  observeEvent(input$notify_error, {
    roundsui::roundsui_notify("Could not reach REDCap.", type = "error", duration = NULL)
  })

  observeEvent(input$show_overlay, {
    showModal(modalDialog(
      title = "Full-page overlay preview",
      size = "l",
      easyClose = TRUE,
      tags$div(
        style = "position: relative; height: 260px; border-radius: var(--roundsui-radius-md); overflow: hidden; border: 1px solid var(--roundsui-border);",
        tags$div(
          style = "position: absolute; inset: 0;",
          roundsui::roundsui_loading_overlay(brand = "ind.dash", message = "Loading your dashboard...")
        )
      )
    ))
  })
}

shinyApp(ui, server)
