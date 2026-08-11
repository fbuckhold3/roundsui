#' @title Theme Setup for roundsui
#' @description bslib theme construction and CSS resource loading for
#'   apps adopting roundsui. Deliberately has zero REDCap or data-loading
#'   dependency — see rounds/PRODUCT.md for why that separation from
#'   \code{gmed} is a load-bearing architecture decision, not an accident.
#' @name theme
NULL

#' Load roundsui CSS
#'
#' Registers the roundsui resource path and returns the \verb{<link>} tags
#' needed to load roundsui's design tokens, shell/navigation, and
#' component styles. Call once
#' near the top of your app's UI. Safe to load alongside
#' \code{gmed::load_gmed_styles()} during migration — roundsui's tokens are
#' prefixed \code{--roundsui-*}, so they don't collide with gmed's
#' \code{--gmed-*}/\code{--ssm-*} custom properties.
#'
#' @return A \code{shiny::tagList()} of \verb{<link>} tags.
#' @export
#'
#' @examples
#' \dontrun{
#' ui <- fluidPage(
#'   load_roundsui_styles(),
#'   # ... app content
#' )
#' }
load_roundsui_styles <- function() {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required to load roundsui styles")
  }
  shiny::addResourcePath("roundsui", system.file("www", package = "roundsui"))
  shiny::tagList(
    shiny::tags$link(
      rel = "stylesheet", type = "text/css",
      href = "roundsui/css/roundsui-tokens.css"
    ),
    shiny::tags$link(
      rel = "stylesheet", type = "text/css",
      href = "roundsui/css/roundsui-shell.css"
    ),
    shiny::tags$link(
      rel = "stylesheet", type = "text/css",
      href = "roundsui/css/roundsui-components.css"
    )
  )
}

#' Create a bslib Theme from roundsui Tokens
#'
#' @param version Bootstrap version. Default 5, matching the ecosystem
#'   convention already used by \code{gmed::create_gmed_theme()}.
#' @param base_font,heading_font Optional \code{bslib::font_google()} (or
#'   similar) overrides. Default to Inter, matching gmed's own
#'   \code{create_gmed_theme()} font convention.
#' @param custom_colors Optional named list overriding individual
#'   \code{roundsui_colors()} values (names must match
#'   \code{roundsui_colors()}'s field names).
#'
#' @return A \code{bslib::bs_theme()} object.
#' @export
#'
#' @examples
#' \dontrun{
#' theme <- create_roundsui_theme()
#' ui <- bslib::page_fluid(theme = theme, load_roundsui_styles(), ...)
#' }
create_roundsui_theme <- function(version = 5,
                                   base_font = NULL,
                                   heading_font = NULL,
                                   custom_colors = NULL) {
  if (!requireNamespace("bslib", quietly = TRUE)) {
    stop("Package 'bslib' is required for theme creation")
  }

  colors <- roundsui_colors()
  if (!is.null(custom_colors)) {
    colors[names(custom_colors)] <- custom_colors
  }

  if (is.null(base_font)) {
    base_font <- bslib::font_google("Inter")
  }
  if (is.null(heading_font)) {
    heading_font <- bslib::font_google("Inter", wght = 650)
  }

  bslib::bs_theme(
    version = version,
    primary = colors$accent,
    success = colors$success,
    warning = colors$warning,
    danger = colors$danger,
    bg = colors$ground,
    fg = colors$ink,
    base_font = base_font,
    heading_font = heading_font
  )
}
