# roundsui shell gallery
#
# A standalone demo of roundsui_page() + roundsui_nav_blocks() as a real
# home screen, with a live width control so the responsive grid can be
# checked without resizing the browser window. Run with:
#
#   shiny::runApp(system.file("examples/shell_gallery", package = "roundsui"))
#
# or, while developing roundsui itself (not yet installed):
#
#   devtools::load_all("roundsui")
#   shiny::runApp("roundsui/inst/examples/shell_gallery")

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

# The same blocks list shape ind.dash/ccc.dashboard already build server-side
# for their own nav grids - this is meant to be a close-to-drop-in swap.
demo_blocks <- list(
  list(id = "milestones", label = "Milestones", desc = "Review ACGME subcompetency progress", icon = "chart-line"),
  list(id = "evaluations", label = "Evaluations", desc = "Faculty and self-assessments", icon = "clipboard-check"),
  list(id = "coaching", label = "Coaching", desc = "Notes from your coach", icon = "comments"),
  list(id = "scholarship", label = "Scholarship", desc = "QI, research, and presentations", icon = "flask"),
  list(id = "schedule", label = "Schedule", desc = "Rotations and conferences", icon = "calendar-days"),
  list(id = "goals", label = "Career Goals", desc = "Set for next academic year", icon = "bullseye", disabled = TRUE)
)

ui <- roundsui::roundsui_page(
  title = "roundsui shell gallery",
  tags$head(tags$style(HTML("
    body { background: var(--roundsui-ground); }
    .gallery-shell { max-width: 1040px; margin: 0 auto; padding: 32px 20px 64px; }
    .gallery-head { margin-bottom: 8px; }
    .gallery-head h1 { font-size: 20px; font-weight: 650; letter-spacing: -0.01em; margin: 0 0 4px; }
    .gallery-head p { color: var(--roundsui-ink-muted); font-size: 13px; margin: 0; }
    .gallery-controls {
      display: flex; align-items: center; gap: 12px; margin: 20px 0 24px;
      background: var(--roundsui-surface); border: 1px solid var(--roundsui-border);
      border-radius: var(--roundsui-radius-md); padding: 12px 16px;
    }
    .gallery-controls label { font-size: 12.5px; font-weight: 600; color: var(--roundsui-ink-muted); white-space: nowrap; }
    .gallery-frame {
      border: 1px dashed var(--roundsui-border-strong); border-radius: var(--roundsui-radius-md);
      padding: 24px; margin: 0 auto; transition: max-width 0.2s ease; overflow: auto;
    }
    .last-click { margin-top: 16px; font-size: 12.5px; color: var(--roundsui-ink-muted); font-family: ui-monospace, monospace; }
  "))),
  div(
    class = "gallery-shell",
    div(class = "gallery-head",
        h1("roundsui — shell gallery"),
        p("roundsui_page() + roundsui_nav_blocks(), the generalized gmed_page()/gmed_nav_blocks() home-screen pattern.")
    ),
    div(class = "gallery-controls",
        tags$label("Preview width"),
        sliderInput("frame_width", NULL, min = 320, max = 1000, value = 1000, step = 20, width = "300px"),
        span(id = "frame_width_label", style = "font-family: ui-monospace, monospace; font-size: 12.5px; color: var(--roundsui-ink-muted);")
    ),
    div(
      class = "gallery-frame",
      style = "max-width: 1000px;",
      id = "gallery_frame",
      roundsui::roundsui_nav_blocks(
        title = "IMSLU Resident Dashboard",
        subtitle = "Welcome back - here's your program at a glance.",
        blocks = demo_blocks,
        input_id = "nav_block"
      )
    ),
    div(class = "last-click", textOutput("last_click", inline = TRUE)),

    h2(style = "font-size: 14px; font-weight: 650; text-transform: uppercase; letter-spacing: 0.04em; color: var(--roundsui-ink-muted); margin: 32px 0 4px;",
       "Card & Resident Panel"),
    p(style = "font-size: 12.5px; color: var(--roundsui-ink-faint); margin: 0 0 12px;",
      "roundsui_card() and roundsui_resident_panel() - generalize gmed_card() (25+ call sites in ccc.dashboard) and gmed_resident_panel()."),

    roundsui::roundsui_resident_panel(
      resident_name = "R. Ahmadi",
      level = "PGY-2",
      coach = "Dr. Bastin"
    ),

    div(
      style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(240px, 1fr)); gap: 16px; margin-top: 16px;",
      roundsui::roundsui_card(
        title = "Review Progress",
        p(style = "margin: 0; color: var(--roundsui-ink-muted); font-size: 13px;",
          "18 of 25 assessments complete this year.")
      ),
      roundsui::roundsui_card(
        p(style = "margin: 0; color: var(--roundsui-ink-muted); font-size: 13px;",
          "A card with no title - just body content.")
      ),
      roundsui::roundsui_card(
        title = "Resident Panel, Partial Fields",
        p(style = "margin: 0 0 10px; color: var(--roundsui-ink-faint); font-size: 12px;",
          "Only 2 of 5 fields supplied - wraps naturally instead of leaving a fixed 12-col grid unbalanced:"),
        roundsui::roundsui_resident_panel(resident_name = "J. Okafor", period = "Period 2")
      )
    )
  ),
  tags$script(HTML("
    Shiny.addCustomMessageHandler('roundsui-set-frame-width', function(px) {
      var el = document.getElementById('gallery_frame');
      if (el) { el.style.maxWidth = px + 'px'; }
    });
  "))
)

server <- function(input, output, session) {
  observeEvent(input$frame_width, {
    session$sendCustomMessage("roundsui-set-frame-width", input$frame_width)
  })

  output$last_click <- renderText({
    if (is.null(input$nav_block)) {
      "Click a tile (or focus one and press Enter/Space) to see its id here."
    } else {
      paste0("input$nav_block = \"", input$nav_block, "\"")
    }
  })
}

shinyApp(ui, server)
