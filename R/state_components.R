#' @title Shared Feedback/State Components ("Ward Notes")
#' @description Loading, empty, and error state components shared across
#'   roundsui-adopting apps. This is roundsui's first component family,
#'   built first because it was the biggest gap found across the rounds
#'   ecosystem: no shared version existed anywhere despite 6+ near-
#'   identical hand-rolled instances (\code{milestone_module.R}'s busy
#'   spinner, \code{ind.dash}'s startup overlay,
#'   \code{datatable_components.R}'s empty-table row,
#'   \code{resident.assessment}'s emoji-\code{showNotification} +
#'   colored-modal pattern used 5+ times, per-chart plotly "no data"
#'   annotations written by hand each time). See the roundsui shape brief
#'   recorded in project memory for the full inventory this replaces.
#' @name state_components
NULL

# ---- Loading ---------------------------------------------------------------

#' roundsui Loading State
#'
#' An inline/panel loading indicator - spinner plus message. Use inside a
#' card body, table container, or chart area while data is being fetched
#' or computed.
#'
#' @param message Character message shown beside/below the spinner.
#' @param compact If \code{TRUE}, renders as a smaller horizontal row
#'   (e.g. for a table cell or narrow panel) instead of the full centered
#'   block.
#' @param id Optional HTML id, for targeting with
#'   \code{shinyjs::show()}/\code{hide()} or similar.
#'
#' @return A shiny UI element.
#' @export
#'
#' @examples
#' \dontrun{
#' roundsui_loading_state("Loading milestone data...")
#' roundsui_loading_state("Loading...", compact = TRUE)
#' }
roundsui_loading_state <- function(message = "Loading\u2026", compact = FALSE, id = NULL) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required for UI components")
  }
  shiny::div(
    id = id,
    class = trimws(paste("roundsui-loading", if (isTRUE(compact)) "roundsui-loading--compact")),
    role = "status",
    `aria-live` = "polite",
    shiny::div(class = "roundsui-spinner"),
    shiny::span(class = "roundsui-loading__message", message)
  )
}

#' roundsui Full-Page Loading Overlay
#'
#' A full-page startup overlay, generalizing the bespoke version shipped
#' on \code{imslu.ind.dash}'s login screen into a shared component. Render
#' once near the top of the app UI; hide it from the server once startup
#' data is ready (e.g. \code{shinyjs::hide(id)}, or by toggling the
#' \code{roundsui-loading-overlay--hidden} class with
#' \code{shinyjs::addClass()}/\code{removeClass()}).
#'
#' @param message Character message shown under the spinner.
#' @param brand Character brand/app name shown above the message. Omit
#'   for no brand line.
#' @param id HTML id for the overlay container, so the server can hide it.
#'
#' @return A shiny UI element.
#' @export
#'
#' @examples
#' \dontrun{
#' roundsui_loading_overlay(brand = "ind.dash", message = "Loading your dashboard...")
#' }
roundsui_loading_overlay <- function(message = "Loading\u2026",
                                      brand = NULL,
                                      id = "roundsui_loading_overlay") {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required for UI components")
  }
  shiny::div(
    id = id,
    class = "roundsui-loading-overlay",
    role = "status",
    `aria-live` = "polite",
    shiny::div(class = "roundsui-spinner"),
    if (!is.null(brand)) shiny::div(class = "roundsui-loading-overlay__brand", brand),
    shiny::div(class = "roundsui-loading-overlay__message", message)
  )
}

# ---- Empty -------------------------------------------------------------

#' roundsui Empty State
#'
#' A styled "nothing here yet" panel for cards, sections, or filtered
#' views. Replaces the several hand-written "No X available"/"No X
#' recorded yet" divs found across \code{gmed} and app-level code (e.g.
#' \code{mod_eval_table.R}, \code{scholarship_display.R},
#' \code{mod_learning_visualizations.R}).
#'
#' @param message Character message describing what's missing.
#' @param icon Optional \code{shiny::icon()} (Font Awesome), e.g.
#'   \code{shiny::icon("inbox")}. Uses a real icon, never a unicode
#'   glyph or emoji stand-in.
#' @param action Optional UI element (e.g. an \code{actionButton}) shown
#'   below the message, for an empty state with a clear next step.
#' @param compact If \code{TRUE}, renders as a smaller horizontal row
#'   instead of the full centered block.
#'
#' @return A shiny UI element.
#' @export
#'
#' @examples
#' \dontrun{
#' roundsui_empty_state("No assessments match the current filters.")
#' roundsui_empty_state(
#'   "No career goals recorded yet.",
#'   icon = shiny::icon("bullseye"),
#'   action = shiny::actionButton("add_goals", "Add goals")
#' )
#' }
roundsui_empty_state <- function(message = "No data available",
                                  icon = NULL,
                                  action = NULL,
                                  compact = FALSE) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required for UI components")
  }
  shiny::div(
    class = trimws(paste("roundsui-empty", if (isTRUE(compact)) "roundsui-empty--compact")),
    if (!is.null(icon)) shiny::div(class = "roundsui-empty__icon", icon),
    shiny::div(class = "roundsui-empty__message", message),
    if (!is.null(action)) shiny::div(class = "roundsui-empty__action", action)
  )
}

#' roundsui Empty Table Data
#'
#' Returns a single-row data.frame carrying a styled "no data" message, in
#' the shape gmed's \code{create_gmed_datatable_tested()} and similar
#' functions already expect (a one-column Message data.frame). Use inside
#' the data-preparation step before handing off to \code{DT::datatable()},
#' so an empty result still renders as a table instead of a blank space.
#' Pair with the \code{roundsui-empty-table-row} CSS class (already
#' applied by \code{gmed}-style table constructors that check for it) for
#' consistent italic/muted styling.
#'
#' @param message Character message.
#' @param column_name Name of the single message column
#'   (default \code{"Message"}).
#'
#' @return A one-row, one-column data.frame.
#' @export
#'
#' @examples
#' \dontrun{
#' display_data <- if (nrow(evals) == 0) {
#'   roundsui_empty_table("No assessments found.")
#' } else {
#'   evals
#' }
#' }
roundsui_empty_table <- function(message = "No data available", column_name = "Message") {
  df <- data.frame(x = message, stringsAsFactors = FALSE)
  names(df) <- column_name
  df
}

#' roundsui Empty Chart Annotation
#'
#' Returns \code{plotly::add_annotations()}-ready parameters for a
#' consistent "no data" state across charts, replacing the hand-written
#' version currently duplicated across gmed's milestone chart functions
#' (\code{create_enhanced_milestone_spider_plot()},
#' \code{milestone_dashboard_ui()}'s server, etc.).
#'
#' @param message Character message.
#' @param color Character hex color for the annotation text. Defaults to
#'   \code{roundsui_colors()$ink_faint}.
#'
#' @return A named list of arguments suitable for splicing into
#'   \code{plotly::add_annotations()} via \code{do.call()} or
#'   \code{rlang::exec()}.
#' @export
#'
#' @examples
#' \dontrun{
#' plotly::plot_ly() |>
#'   plotly::add_annotations(
#'     text = roundsui_empty_chart_annotation()$text,
#'     x = roundsui_empty_chart_annotation()$x,
#'     y = roundsui_empty_chart_annotation()$y
#'   )
#' }
roundsui_empty_chart_annotation <- function(message = "No data available", color = NULL) {
  if (is.null(color)) {
    color <- roundsui_colors()$ink_faint
  }
  list(
    text = message,
    x = 0.5, y = 0.5,
    xref = "paper", yref = "paper",
    showarrow = FALSE,
    font = list(size = 13, color = color),
    xanchor = "center", yanchor = "middle"
  )
}

# ---- Error -------------------------------------------------------------

#' roundsui Inline Error
#'
#' A small inline error message, styled consistently. Replaces
#' \code{gmed::mod_auth}'s bespoke
#' \code{div(class = "gmed-login-error", ...)} pattern and similar
#' one-off inline errors elsewhere in the ecosystem.
#'
#' @param message Character error message.
#' @param icon Optional \code{shiny::icon()}; defaults to a warning
#'   triangle.
#'
#' @return A shiny UI element.
#' @export
#'
#' @examples
#' \dontrun{
#' roundsui_inline_error("Access code not recognized. Check with your coordinator.")
#' }
roundsui_inline_error <- function(message, icon = NULL) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required for UI components")
  }
  if (is.null(icon)) {
    icon <- shiny::icon("triangle-exclamation")
  }
  shiny::div(
    class = "roundsui-inline-error",
    role = "alert",
    shiny::span(class = "roundsui-inline-error__icon", icon),
    shiny::span(message)
  )
}

#' roundsui Error Modal
#'
#' A styled \code{shiny::modalDialog()} for blocking configuration/data
#' errors, replacing the hand-styled
#' \code{modalDialog(h5(..., style = "color:#dc3545"), p(...))} pattern
#' repeated 5+ times in \code{resident.assessment}'s server code.
#'
#' @param message Character error message (body of the modal).
#' @param title Character modal title.
#' @param easy_close Passed to \code{shiny::modalDialog()}'s
#'   \code{easyClose}; default \code{TRUE}.
#'
#' @return A \code{shiny::modalDialog()}, suitable for
#'   \code{shiny::showModal()}.
#' @export
#'
#' @examples
#' \dontrun{
#' shiny::showModal(roundsui_error_modal(
#'   "Could not save this evaluation. Check your connection and try again.",
#'   title = "Save failed"
#' ))
#' }
roundsui_error_modal <- function(message,
                                  title = "Something went wrong",
                                  easy_close = TRUE) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required for UI components")
  }
  shiny::modalDialog(
    title = shiny::div(
      class = "roundsui-error-modal__title",
      shiny::icon("circle-exclamation"),
      title
    ),
    shiny::p(message),
    easyClose = easy_close,
    footer = shiny::modalButton("Close")
  )
}

#' roundsui Notification
#'
#' A thin wrapper around \code{shiny::showNotification()} that applies
#' consistent roundsui styling, replacing the emoji-prefixed
#' \code{showNotification()} calls found throughout
#' \code{resident.assessment} and \code{imslu_rdm_data_entry} (e.g.
#' \code{"[check] ..."}, \code{"[x] Error submitting ...:"}).
#'
#' Shiny's own notification system only distinguishes
#' default/message/warning/error - there's no native "success" channel.
#' \code{type = "success"} is carried by styling this function's own
#' content wrapper rather than shiny's outer notification class, which is
#' why it still needs to go through this helper rather than
#' \code{shiny::showNotification()} directly.
#'
#' @param message Character notification message. No emoji prefix needed
#'   - the styling itself carries the type.
#' @param type One of \code{"message"}, \code{"success"},
#'   \code{"warning"}, \code{"error"} (\code{"danger"} is accepted as a
#'   synonym for \code{"error"}).
#' @param duration Seconds before auto-dismiss; \code{NULL} to require
#'   manual dismissal. Passed through to
#'   \code{shiny::showNotification()}.
#' @param session The Shiny session; defaults to the current reactive
#'   domain.
#'
#' @return Invisibly, the notification id (see
#'   \code{shiny::showNotification()}).
#' @export
#'
#' @examples
#' \dontrun{
#' roundsui_notify("Evaluation submitted.", type = "success")
#' roundsui_notify("Could not reach REDCap.", type = "error", duration = NULL)
#' }
roundsui_notify <- function(message,
                             type = c("message", "success", "warning", "error", "danger"),
                             duration = 4,
                             session = shiny::getDefaultReactiveDomain()) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required for UI components")
  }
  type <- match.arg(type)

  # Shiny's native type controls the outer .shiny-notification-<type>
  # class it applies itself; "success"/"danger" have no native equivalent,
  # so those fall back to "default" at the shiny level and are carried by
  # our own inner content class instead (see roundsui-components.css).
  shiny_type <- switch(type,
    warning = "warning",
    error = ,
    danger = "error",
    "default"
  )

  icon <- switch(type,
    success = shiny::icon("circle-check"),
    warning = shiny::icon("triangle-exclamation"),
    error = ,
    danger = shiny::icon("circle-exclamation"),
    shiny::icon("circle-info")
  )

  content <- shiny::div(
    class = paste0("roundsui-notify-content roundsui-notify-content--", type),
    icon,
    shiny::span(message)
  )

  shiny::showNotification(
    content,
    duration = duration,
    type = shiny_type,
    session = session
  )
}
