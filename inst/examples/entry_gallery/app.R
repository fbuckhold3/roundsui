# roundsui entry gallery
#
# A standalone demo of the Data-Entry pattern family: choice input with
# an "Other" reveal, the confirm-unchanged Yes/No pattern, filter chips,
# and a validation summary. Run with:
#
#   shiny::runApp(system.file("examples/entry_gallery", package = "roundsui"))
#
# or, while developing roundsui itself (not yet installed):
#
#   devtools::load_all("roundsui")
#   shiny::runApp("roundsui/inst/examples/entry_gallery")

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

section <- function(title, subtitle = NULL, ...) {
  div(
    class = "gallery-section",
    div(class = "gallery-section__head", h2(title), if (!is.null(subtitle)) p(class = "gallery-section__sub", subtitle)),
    div(class = "gallery-section__body", ...)
  )
}
swatch_card <- function(...) div(class = "gallery-card", ...)

career_choices <- "1, Academic medicine | 2, Community practice | 3, Research | 8, Other"

eval_chips <- list(
  list(value = "PGY-1", label = "PGY-1", count = 12),
  list(value = "PGY-2", label = "PGY-2", count = 9),
  list(value = "PGY-3", label = "PGY-3", count = 11)
)

ui <- roundsui::roundsui_page(
  title = "roundsui entry gallery",
  tags$head(tags$style(HTML("
    body { background: var(--roundsui-ground); }
    .gallery-shell { max-width: 820px; margin: 0 auto; padding: 32px 20px 64px; display: flex; flex-direction: column; gap: 28px; }
    .gallery-head h1 { font-size: 20px; font-weight: 650; letter-spacing: -0.01em; margin: 0 0 4px; }
    .gallery-head p { color: var(--roundsui-ink-muted); font-size: 13px; margin: 0; }
    .gallery-section__head h2 { font-size: 14px; font-weight: 650; margin: 0 0 2px; text-transform: uppercase; letter-spacing: 0.04em; color: var(--roundsui-ink-muted); }
    .gallery-section__sub { font-size: 12.5px; color: var(--roundsui-ink-faint); margin: 0 0 12px; }
    .gallery-section__body { display: flex; flex-direction: column; gap: 14px; }
    .gallery-card { background: var(--roundsui-surface); border: 1px solid var(--roundsui-border); border-radius: var(--roundsui-radius-md); box-shadow: var(--roundsui-shadow); overflow: hidden; }
    .gallery-card__label { font-size: 11px; text-transform: uppercase; letter-spacing: 0.05em; color: var(--roundsui-ink-faint); padding: 10px 14px 0; }
    .gallery-card__body { padding: 10px 14px 14px; }
    .gallery-note { font-size: 12.5px; color: var(--roundsui-ink-muted); font-family: ui-monospace, monospace; margin-top: 6px; }
  "))),
  div(
    class = "gallery-shell",
    div(class = "gallery-head",
        h1("roundsui — entry gallery"),
        p("Data-Entry pattern family: REDCap-choice-driven inputs, confirm-unchanged, filter chips, validation summary.")
    ),

    section("Choice input",
            "roundsui_choice_input() - parses a REDCap choice string, auto-detects \"Other\" by label text",
            swatch_card(
              div(class = "gallery-card__label", "Career interests (checkbox, try selecting Other)"),
              div(class = "gallery-card__body",
                  roundsui::roundsui_choice_input("career_path", "Career interests", career_choices),
                  div(class = "gallery-note", textOutput("career_selection", inline = TRUE))
              )
            )
    ),

    section("Confirm unchanged",
            "roundsui_confirm_unchanged() - the mod_career_goals Yes/No pattern, generalized",
            swatch_card(
              div(class = "gallery-card__body",
                  roundsui::roundsui_confirm_unchanged(
                    "goals",
                    question = "Are your career goals the same as last time?",
                    yes_label = "Yes, same goals", no_label = "No, update my goals"
                  ),
                  div(class = "gallery-note", textOutput("confirm_state", inline = TRUE))
              )
            )
    ),

    section("Filter chips",
            "roundsui_filter_chips() - generalizes mod_eval_feedback's PGY/type chip filters",
            swatch_card(
              div(class = "gallery-card__body",
                  # Rendered via uiOutput/renderUI, keyed off input$level_filter, so the
                  # active chip's style actually updates on click - matching how a real
                  # consumer (mod_eval_feedback's own output$pgy_chips) already works.
                  # A static call (as this looked before) sends the right input value on
                  # click but never re-renders to show which chip is now active.
                  uiOutput("level_chips"),
                  div(class = "gallery-note", textOutput("chip_selection", inline = TRUE))
              )
            )
    ),

    section("Validation summary",
            "roundsui_validation_summary() - replaces the comma-mashed 'Please complete: X, Y, Z' pattern",
            swatch_card(
              div(class = "gallery-card__body",
                  actionButton("run_validation", "Submit (with missing fields)", class = "btn btn-primary btn-sm"),
                  div(style = "margin-top: 12px;", uiOutput("validation_demo"))
              )
            )
    )
  )
)

server <- function(input, output, session) {
  output$career_selection <- renderText({
    sel <- input$career_path
    other <- input$career_path_other
    parts <- c(
      if (length(sel) > 0) paste0("selected: ", paste(sel, collapse = ", ")) else "selected: (none)",
      if (!is.null(other) && nzchar(other)) paste0("other: \"", other, "\"")
    )
    paste(parts, collapse = " | ")
  })

  goals_same <- reactiveVal(NULL)
  observeEvent(input$goals_yes, { goals_same(TRUE) })
  observeEvent(input$goals_no, { goals_same(FALSE) })
  output$confirm_state <- renderText({
    val <- goals_same()
    if (is.null(val)) "not answered yet" else if (val) "goals_same = TRUE (yes clicked)" else "goals_same = FALSE (no clicked)"
  })

  output$level_chips <- renderUI({
    roundsui::roundsui_filter_chips(
      chips = eval_chips, input_id = "level_filter",
      all_count = 32, selected = input$level_filter
    )
  })

  output$chip_selection <- renderText({
    sel <- input$level_filter
    paste0("input$level_filter = ", if (is.null(sel) || !nzchar(sel)) '"" (All)' else paste0('"', sel, '"'))
  })

  output$validation_demo <- renderUI({
    input$run_validation
    if (isTRUE(input$run_validation > 0)) {
      roundsui::roundsui_validation_summary(c("Plus comments", "Delta comments", "Overall rating"))
    }
  })
}

shinyApp(ui, server)
