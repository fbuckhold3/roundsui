#' @title Shell & Navigation Components ("Ward Notes")
#' @description Page shell and home-screen navigation, generalizing
#'   \code{gmed::gmed_page()} and \code{gmed::gmed_nav_blocks()} — the
#'   "thin shell, all content server-driven" pattern already consistent
#'   across \code{imslu.ccc.dashboard} and \code{imslu.ind.dash}. This
#'   family keeps that structure and the
#'   \code{Shiny.setInputValue()}-on-click mechanism exactly as-is; what
#'   changes is: a genuinely responsive grid (both apps currently
#'   \code{!important}-override the same fixed 3/4-column grid locally,
#'   byte-for-byte duplicated), Font Awesome instead of Bootstrap Icons
#'   (family 1 already standardized on it, and Shiny/bslib load it with no
#'   extra CDN dependency, unlike \code{bi bi-*}), dropping the 5px
#'   colored \code{border-left} on nav tiles (a named craft-floor refusal,
#'   not a style choice), and keyboard operability on the nav tiles
#'   (the original is click-only with no focus/Enter handling).
#'
#'   \code{gmed::gmed_app_header()} was deliberately not ported — neither
#'   real consumer app uses it anymore, both use \code{gmed_nav_blocks()}'s
#'   own header instead.
#' @name shell
NULL

#' roundsui Page Shell
#'
#' Generalizes \code{gmed::gmed_page()}: applies the roundsui theme and
#' CSS, sets the page title, and optionally wires up \code{shinyjs}. Two
#' redundant dependencies from the original are deliberately dropped: a
#' hardcoded Font Awesome CDN \verb{<link>} (Shiny/bslib already bundle
#' Font Awesome locally — confirmed working via
#' \code{inst/examples/state_gallery}'s icons, no CDN needed) and a
#' hardcoded, version-pinned \code{shinyjs} CDN \verb{<script>} loaded
#' alongside \code{shinyjs::useShinyjs()}, which already inserts
#' \code{shinyjs}'s own bundled JS — loading it twice.
#'
#' @param ... App UI content (typically a single \code{shiny::uiOutput()}
#'   for a server-driven thin shell, matching \code{ccc.dashboard} and
#'   \code{ind.dash}'s existing pattern).
#' @param title Character page/browser-tab title.
#' @param base_font,heading_font Optional \code{bslib::font_google()}
#'   overrides, passed to \code{create_roundsui_theme()}.
#' @param include_shinyjs If \code{TRUE} (default) and the \code{shinyjs}
#'   package is installed, calls \code{shinyjs::useShinyjs()}.
#' @param custom_css,custom_js Optional character vectors of file paths,
#'   included via \code{shiny::includeCSS()}/\code{includeScript()} —
#'   ported directly from \code{gmed_page()}'s existing app-level override
#'   mechanism.
#'
#' @return A \code{bslib::page_fluid()}.
#' @export
#'
#' @examples
#' \dontrun{
#' ui <- roundsui_page(
#'   title = "IMSLU Resident Dashboard",
#'   shiny::uiOutput("main_view")
#' )
#' }
roundsui_page <- function(...,
                           title = "roundsui Application",
                           base_font = NULL,
                           heading_font = NULL,
                           include_shinyjs = TRUE,
                           custom_css = NULL,
                           custom_js = NULL) {
  if (!requireNamespace("bslib", quietly = TRUE)) {
    stop("Package 'bslib' is required for page creation")
  }
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required for page creation")
  }

  bslib::page_fluid(
    theme = create_roundsui_theme(base_font = base_font, heading_font = heading_font),
    load_roundsui_styles(),

    if (isTRUE(include_shinyjs) && requireNamespace("shinyjs", quietly = TRUE)) {
      shinyjs::useShinyjs()
    },

    shiny::tags$head(shiny::tags$title(title)),

    if (!is.null(custom_css)) {
      lapply(custom_css, shiny::includeCSS)
    },
    if (!is.null(custom_js)) {
      lapply(custom_js, shiny::includeScript)
    },

    ...
  )
}

#' roundsui Navigation Blocks
#'
#' A home-screen grid of clickable navigation tiles, generalizing
#' \code{gmed::gmed_nav_blocks()}. Each block fires
#' \code{Shiny.setInputValue(input_id, block$id, {priority: 'event'})} on
#' click or Enter/Space when focused — the exact mechanism
#' \code{ccc.dashboard} and \code{ind.dash} already build their
#' server-side navigation on, so porting an app's existing \code{blocks}
#' list across is close to a drop-in swap.
#'
#' @param blocks A list of lists, each with:
#'   \describe{
#'     \item{id}{Character, the value sent to \code{input[[input_id]]}.}
#'     \item{label}{Character, the tile's bold label.}
#'     \item{desc}{Character, a short description line.}
#'     \item{icon}{Character Font Awesome icon name, passed to
#'       \code{shiny::icon()} (e.g. \code{"chart-line"}, not
#'       \code{"bi-graph-up"} — this is the one field that changes shape
#'       from \code{gmed_nav_blocks()}, since roundsui standardizes on
#'       Font Awesome).}
#'     \item{disabled}{Optional logical; if \code{TRUE}, renders a
#'       non-interactive "Coming soon" tile instead.}
#'   }
#' @param title Character page header title.
#' @param subtitle Optional character page header subtitle.
#' @param input_id Character Shiny input id the tiles write to on click.
#'
#' @return A \code{shiny::tagList()}.
#' @export
#'
#' @examples
#' \dontrun{
#' roundsui_nav_blocks(
#'   title = "IMSLU Resident Dashboard",
#'   blocks = list(
#'     list(id = "milestones", label = "Milestones", desc = "Review your progress",
#'          icon = "chart-line"),
#'     list(id = "coaching", label = "Coaching", desc = "Notes from your coach",
#'          icon = "comments"),
#'     list(id = "goals", label = "Career Goals", desc = "Coming next year",
#'          icon = "bullseye", disabled = TRUE)
#'   )
#' )
#' }
roundsui_nav_blocks <- function(blocks,
                                 title = "Dashboard",
                                 subtitle = NULL,
                                 input_id = "nav_block") {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required for UI components")
  }

  shiny::tagList(
    shiny::div(
      class = "roundsui-page-header",
      shiny::tags$h2(title),
      if (!is.null(subtitle)) shiny::tags$p(subtitle)
    ),
    shiny::div(
      class = "roundsui-nav-grid",
      lapply(blocks, function(b) {
        if (isTRUE(b$disabled)) {
          shiny::div(
            class = "roundsui-nav-block roundsui-nav-block--disabled",
            `aria-disabled` = "true",
            shiny::div(class = "roundsui-nav-block__icon", shiny::icon(b$icon)),
            shiny::div(class = "roundsui-nav-block__label", b$label),
            shiny::div(class = "roundsui-nav-block__desc", b$desc),
            shiny::div(class = "roundsui-nav-block__soon", "Coming soon")
          )
        } else {
          click_js <- sprintf(
            "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
            input_id, b$id
          )
          shiny::div(
            class = "roundsui-nav-block",
            role = "button",
            tabindex = "0",
            `aria-label` = paste0(b$label, if (!is.null(b$desc)) paste0(" - ", b$desc)),
            onclick = click_js,
            onkeypress = sprintf(
              "if(event.key==='Enter'||event.key===' '){event.preventDefault();%s}",
              click_js
            ),
            shiny::div(class = "roundsui-nav-block__icon", shiny::icon(b$icon)),
            shiny::div(class = "roundsui-nav-block__label", b$label),
            shiny::div(class = "roundsui-nav-block__desc", b$desc)
          )
        }
      })
    )
  )
}
