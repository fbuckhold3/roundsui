#' @title Design Tokens for roundsui ("Ward Notes")
#' @description Color tokens for roundsui's "Ward Notes" visual system,
#'   confirmed via \code{/impeccable new-work} on 2026-08-11. Restrained
#'   color strategy: neutrals plus one accent, with conventional (not
#'   reinvented) status colors. Mirrors the CSS custom properties in
#'   \code{inst/www/css/roundsui-tokens.css} — keep the two in sync.
#' @name tokens
NULL

#' roundsui Color Palette ("Ward Notes")
#'
#' Institution-neutral by design (this package intentionally carries no
#' SSM Health / SLUCare branding, unlike gmed's original palette).
#'
#' @return Named list of roundsui design tokens (hex strings).
#' @export
#'
#' @examples
#' colors <- roundsui_colors()
#' colors$accent  # "#3355D8"
roundsui_colors <- function() {
  list(
    # Ground / surface
    ground = "#F6F7FA",
    surface = "#FFFFFF",
    surface_2 = "#FBFBFD",
    border = "#E2E5EC",
    border_strong = "#CBD0DB",

    # Ink
    ink = "#13161F",
    ink_muted = "#5B6472",
    ink_faint = "#8991A0",

    # Accent — reserved for primary actions, active nav, focus rings, and
    # chart emphasis. Never a broad surface fill (Restrained strategy).
    accent = "#3355D8",
    accent_ink = "#FFFFFF",
    accent_tint = "#EAEFFC",

    # Status — kept conventional on purpose.
    success = "#16A34A",
    success_tint = "#E7F6EC",
    warning = "#C2760A",
    warning_tint = "#FBF0DF",
    danger = "#D33A2C",
    danger_tint = "#FBEAE8"
  )
}
