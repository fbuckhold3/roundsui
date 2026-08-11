#' @title Data-Visualization Wrapper Components ("Ward Notes")
#' @description Wrapper-layer helpers over the ecosystem's existing plotly
#'   and DT investment — status conventions, table chrome, and chart
#'   theming — not a chart-type or table-engine replacement. See the
#'   roundsui shape brief in project memory for the inventory this
#'   consolidates:
#'   \itemize{
#'     \item \code{gmed::gmed_status_badge()} (text pill, CSS classes) and
#'       \code{gmed:::create_status_indicator()} (dot + hardcoded hex +
#'       unicode glyph) did the same job two inconsistent ways —
#'       \code{roundsui_status_badge()} is one function, two display
#'       variants.
#'     \item \code{gmed::create_gmed_datatable_tested()} /
#'       \code{create_gmed_datatable()} / \code{create_styled_dt()} /
#'       \code{datatable_with_click()} were four overlapping DT
#'       constructors in one file — one of them
#'       (\code{create_gmed_datatable <- create_gmed_datatable_tested})
#'       is a dead alias silently overwritten two lines later by a second
#'       \code{create_gmed_datatable <- function(...)} definition.
#'       \code{roundsui_datatable()} is the one constructor.
#'     \item No shared plotly chrome existed at all —
#'       \code{roundsui_plotly_layout()} is new, not a consolidation.
#'   }
#' @name viz
NULL

# ---- Status ----------------------------------------------------------------

#' roundsui Status Badge
#'
#' A colored status indicator, in two display variants. Replaces both
#' \code{gmed::gmed_status_badge()} (text pill) and
#' \code{gmed:::create_status_indicator()} (small dot + glyph, which used
#' hardcoded hex colors and raw unicode characters \code{✓}/\code{○}/
#' \code{⏳} instead of a real icon).
#'
#' @param label Character text shown next to/inside the indicator.
#' @param status One of \code{"complete"}, \code{"incomplete"},
#'   \code{"in-progress"}, \code{"pending"}, \code{"success"},
#'   \code{"warning"}, \code{"danger"}. The clinical-workflow statuses
#'   (\code{"complete"}/\code{"incomplete"}/\code{"in-progress"}/
#'   \code{"pending"}) map onto the same success/warning/danger/neutral
#'   tokens as the generic ones, so callers can use whichever vocabulary
#'   fits the call site.
#' @param variant \code{"pill"} (default; a labeled pill, for table cells
#'   and lists) or \code{"dot"} (a small icon-only indicator with the
#'   label as adjacent text, for denser layouts).
#'
#' @return A shiny UI element.
#' @export
#'
#' @examples
#' \dontrun{
#' roundsui_status_badge("Complete", status = "complete")
#' roundsui_status_badge("Below cohort median", status = "warning", variant = "dot")
#' }
roundsui_status_badge <- function(label,
                                   status = c("complete", "incomplete", "in-progress",
                                              "pending", "success", "warning", "danger"),
                                   variant = c("pill", "dot")) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required for UI components")
  }
  status <- match.arg(status)
  variant <- match.arg(variant)

  # Clinical-workflow vocabulary maps onto the same semantic tokens.
  semantic <- switch(status,
    complete = "success",
    "in-progress" = "warning",
    incomplete = "danger",
    pending = "neutral",
    status
  )

  icon_name <- switch(semantic,
    success = "circle-check",
    warning = "circle-half-stroke",
    danger = "circle-xmark",
    "clock" # neutral/pending - verified against the bundled Font Awesome
            # Free set; "circle-dashed" (an earlier choice here) doesn't
            # exist in Free and rendered as an invisible glyph
  )

  if (variant == "dot") {
    shiny::span(
      class = paste0("roundsui-status-dot roundsui-status-dot--", semantic),
      shiny::icon(icon_name),
      shiny::span(class = "roundsui-status-dot__label", label)
    )
  } else {
    shiny::span(
      class = paste0("roundsui-status-badge roundsui-status-badge--", semantic),
      shiny::icon(icon_name),
      shiny::span(label)
    )
  }
}

# ---- Table -------------------------------------------------------------

#' roundsui DataTable
#'
#' A single, consistent DT constructor, replacing \code{gmed}'s four
#' overlapping ones. Handles the empty-data case via
#' \code{roundsui_empty_table()} automatically, applies consistent
#' null-value rendering and pagination language, and supports a generic
#' \code{highlight_columns} (no name-sniffing magic for "Plus"/"Delta" or
#' date columns — that's app-specific business logic, not a table-chrome
#' concern).
#'
#' @param data A data frame.
#' @param caption Optional character caption, also used to build the
#'   empty-state message ("No <caption> available") when \code{data} has
#'   zero rows.
#' @param page_length Rows per page. Default 10.
#' @param searchable Whether to show the search box. Default \code{TRUE}.
#' @param highlight_columns Optional character vector of column names to
#'   tint with the roundsui accent (e.g. columns the user should notice
#'   first).
#' @param row_click_input_id Optional character Shiny input id. When set,
#'   clicking a row sends that row's full data (as a named list, keyed by
#'   column name — not a hardcoded column schema, unlike
#'   \code{gmed::datatable_with_click()}) to
#'   \code{input[[row_click_input_id]]}.
#'
#' @return A \code{DT::datatable()}.
#' @export
#'
#' @examples
#' \dontrun{
#' roundsui_datatable(evaluations, caption = "evaluations")
#' roundsui_datatable(
#'   residents, caption = "residents",
#'   row_click_input_id = "selected_resident"
#' )
#' }
roundsui_datatable <- function(data,
                                caption = NULL,
                                page_length = 10,
                                searchable = TRUE,
                                highlight_columns = NULL,
                                row_click_input_id = NULL) {
  if (!requireNamespace("DT", quietly = TRUE)) {
    stop("Package 'DT' is required for datatable creation")
  }

  if (is.null(data) || nrow(data) == 0) {
    empty_message <- if (!is.null(caption)) {
      paste0("No ", tolower(caption), " available")
    } else {
      "No data available"
    }
    # caption is intentionally not passed through to DT here - it already
    # feeds the empty_message text above, and DT renders `caption` as its
    # own visible <caption> element, which duplicated the same words
    # right next to the message row rather than adding information.
    return(DT::datatable(
      roundsui_empty_table(empty_message),
      options = list(dom = "t", ordering = FALSE, paging = FALSE, searching = FALSE),
      rownames = FALSE,
      class = "roundsui-empty-table-row"
    ))
  }

  callback <- if (!is.null(row_click_input_id)) {
    # DT::datatable()'s `callback` is the BODY of a function(table) {...}
    # DT wraps itself (its own default is JS("return table;")) - passing a
    # full function(table) {...} declaration here double-wraps it into an
    # unnamed nested function statement, which is invalid JS ("Function
    # statements require a function name") and silently breaks the whole
    # table client-side with no server-side error to show for it.
    #
    # Hand-built JSON array of column names below avoids taking a
    # jsonlite dependency just to serialize a character vector.
    escaped_cols <- gsub('(["\\\\])', "\\\\\\1", names(data))
    cols_json <- paste0("[", paste0('"', escaped_cols, '"', collapse = ","), "]")
    DT::JS(sprintf(
      "table.on('click', 'tbody tr', function() {
        table.$('tr.roundsui-row-selected').removeClass('roundsui-row-selected');
        $(this).addClass('roundsui-row-selected');
        var rowData = table.row(this).data();
        var cols = %s;
        var payload = {};
        for (var i = 0; i < cols.length; i++) { payload[cols[i]] = rowData[i]; }
        Shiny.setInputValue('%s', payload, {priority: 'event'});
      });
      return table;",
      cols_json,
      row_click_input_id
    ))
  } else {
    DT::JS("return table;")
  }

  dt <- DT::datatable(
    data,
    caption = caption,
    rownames = FALSE,
    escape = FALSE,
    class = "roundsui-datatable cell-border stripe hover",
    callback = callback,
    options = list(
      pageLength = page_length,
      dom = if (searchable) "frtip" else "rtip",
      scrollX = TRUE,
      autoWidth = TRUE,
      columnDefs = list(
        list(
          targets = "_all",
          render = DT::JS(
            "function(data, type, row) {
              if (data === null || data === '' || data === 'Not provided') {
                return '<span class=\"roundsui-dt-null\">Not provided</span>';
              }
              return data;
            }"
          )
        )
      ),
      language = list(
        search = "Search:",
        lengthMenu = "Show _MENU_ entries",
        info = "Showing _START_ to _END_ of _TOTAL_ entries",
        infoEmpty = "No entries available",
        infoFiltered = "(filtered from _MAX_ total entries)",
        paginate = list(first = "First", last = "Last", `next` = "Next", previous = "Previous")
      )
    )
  )

  if (!is.null(highlight_columns)) {
    highlight_cols <- intersect(highlight_columns, names(data))
    if (length(highlight_cols) > 0) {
      dt <- DT::formatStyle(
        dt,
        columns = highlight_cols,
        backgroundColor = roundsui_colors()$accent_tint,
        fontWeight = "600"
      )
    }
  }

  dt
}

# ---- Chart chrome ---------------------------------------------------------

#' roundsui Plotly Layout
#'
#' Applies consistent Ward Notes chrome (typography, gridlines, hover
#' styling) to a plotly figure. Piped in after building the figure's own
#' traces — this does not build charts itself; gmed's actual chart
#' functions (spider plots, stacked bars, etc.) keep their own trace
#' logic and pipe through this for shared chrome, per the "wrapper layer,
#' not a library swap" scope.
#'
#' @param p A plotly object.
#' @param ... Additional named arguments passed through to
#'   \code{plotly::layout()}, layered on top of (and able to override)
#'   the roundsui defaults.
#'
#' @return The plotly object, with layout applied.
#' @export
#'
#' @examples
#' \dontrun{
#' plotly::plot_ly(x = 1:5, y = c(2, 4, 3, 5, 6), type = "scatter", mode = "lines") |>
#'   roundsui_plotly_layout(title = "Milestone trend")
#' }
roundsui_plotly_layout <- function(p, ...) {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Package 'plotly' is required for chart theming")
  }
  colors <- roundsui_colors()
  overrides <- list(...)

  # plotly::layout(p, xaxis = default, ..., ) does NOT merge a caller-
  # supplied `yaxis`/`xaxis` in `...` with the default above it - passing
  # both silently drops one instead of layering (confirmed: a caller's
  # yaxis = list(range = c(0, 9)) never took effect this way). Each
  # themed field is merged explicitly with utils::modifyList() instead, so
  # a caller can override e.g. just `range` on yaxis without losing the
  # gridline/tick theming, and anything else in `...` (title, etc.) still
  # passes through untouched.
  or_empty <- function(x) if (is.null(x)) list() else x

  axis_defaults <- list(
    gridcolor = colors$border, zerolinecolor = colors$border_strong,
    linecolor = colors$border_strong, tickfont = list(color = colors$ink_muted)
  )

  themed <- list(
    font = utils::modifyList(
      list(family = "Inter, -apple-system, sans-serif", size = 12, color = colors$ink_muted),
      or_empty(overrides$font)
    ),
    plot_bgcolor = "rgba(0,0,0,0)",
    paper_bgcolor = "rgba(0,0,0,0)",
    xaxis = utils::modifyList(axis_defaults, or_empty(overrides$xaxis)),
    yaxis = utils::modifyList(axis_defaults, or_empty(overrides$yaxis)),
    hoverlabel = utils::modifyList(
      list(bgcolor = colors$surface, bordercolor = colors$border,
           font = list(color = colors$ink, family = "Inter, -apple-system, sans-serif")),
      or_empty(overrides$hoverlabel)
    ),
    legend = utils::modifyList(list(font = list(color = colors$ink_muted)), or_empty(overrides$legend))
  )

  remaining <- overrides[!names(overrides) %in% names(themed)]

  do.call(plotly::layout, c(list(p), themed, remaining))
}
