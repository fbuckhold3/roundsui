#' @title Data-Entry Pattern Components ("Ward Notes")
#' @description The last component family in the original shape brief.
#'   Generalizes reusable UI *patterns* found duplicated across the
#'   ecosystem's entry forms - never the REDCap-coupled logic underneath.
#'   Deliberately NOT ported here, by architecture (see rounds/PRODUCT.md):
#'   \code{gmed}'s \code{submit_overwrite_data()}/\code{submit_additive_data()}
#'   (data-layer, not UI), and each app's actual per-field validation logic
#'   (which fields are required for which eval type is app-specific
#'   business logic; roundsui only standardizes how the *result* of that
#'   logic is displayed).
#' @name entry
NULL

# ---- Choice input --------------------------------------------------------

#' Parse a REDCap-style pipe-delimited choice string
#'
#' \code{"1, Option A | 2, Option B | 99, Other"} becomes a named
#' character vector suitable for \code{choices=} in
#' \code{checkboxGroupInput()}/\code{radioButtons()} (names are the
#' labels shown, values are the REDCap codes submitted).
#'
#' @param choices_string Character, REDCap data-dictionary choices format.
#' @return A named character vector, or \code{character(0)} if
#'   \code{choices_string} is empty/\code{NA}.
#' @keywords internal
#' @noRd
.roundsui_parse_choices <- function(choices_string) {
  if (is.null(choices_string) || length(choices_string) == 0 ||
      is.na(choices_string) || !nzchar(trimws(choices_string))) {
    return(character(0))
  }
  items <- trimws(strsplit(choices_string, "\\|")[[1]])
  codes <- trimws(sub(",.*$", "", items))
  labels <- trimws(sub("^[^,]*,", "", items))
  stats::setNames(codes, labels)
}

#' roundsui Choice Input
#'
#' A checkbox/radio group driven directly by a REDCap data-dictionary
#' choices string, with an "Other" free-text reveal - generalizes the
#' pattern in \code{gmed::mod_career_goals} (career path, fellowship, and
#' track fields all repeat this by hand). One improvement over the
#' original: the "Other" choice is detected by matching its label text
#' (case-insensitively) against \code{"other"}, instead of
#' \code{mod_career_goals}'s hardcoded per-field magic numbers
#' (\code{input.x.includes('8')}, \code{.includes('12')}, ...) - fragile,
#' and silently wrong if a REDCap dictionary's coding ever changes.
#'
#' @param input_id Character Shiny input id for the choice widget. Used
#'   exactly as given (no internal namespacing) - if calling from inside
#'   a Shiny module, pass \code{ns("field_name")} yourself, matching every
#'   other roundsui function that takes an id.
#' @param label Optional character field label shown above the widget.
#' @param choices_string Character, REDCap-format choices
#'   (\code{"1, Label | 2, Label"}).
#' @param type \code{"checkbox"} (default, multi-select via
#'   \code{checkboxGroupInput()}) or \code{"radio"}
#'   (\code{radioButtons()}).
#' @param other_input_id Character Shiny input id for the "Other" reveal
#'   text field. Default \code{paste0(input_id, "_other")}.
#' @param other_placeholder Character placeholder text for the "Other"
#'   field.
#' @param selected Optional pre-selected value(s), passed through to the
#'   underlying widget.
#'
#' @return A shiny UI element.
#' @export
#'
#' @examples
#' \dontrun{
#' roundsui_choice_input(
#'   "career_path", "Career interests",
#'   "1, Academic medicine | 2, Community practice | 3, Research | 8, Other"
#' )
#' }
roundsui_choice_input <- function(input_id,
                                   label = NULL,
                                   choices_string,
                                   type = c("checkbox", "radio"),
                                   other_input_id = paste0(input_id, "_other"),
                                   other_placeholder = "Please specify",
                                   selected = NULL) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required for UI components")
  }
  type <- match.arg(type)
  choices <- .roundsui_parse_choices(choices_string)

  if (length(choices) == 0) {
    return(shiny::div(class = "roundsui-choice-input__empty", "No choices configured."))
  }

  other_idx <- grep("other", names(choices), ignore.case = TRUE)
  other_code <- if (length(other_idx) > 0) unname(choices[other_idx[1]]) else NULL

  input_widget <- if (type == "checkbox") {
    shiny::checkboxGroupInput(input_id, NULL, choices = choices, selected = selected)
  } else {
    shiny::radioButtons(input_id, NULL, choices = choices, selected = if (is.null(selected)) FALSE else selected)
  }

  other_reveal <- if (!is.null(other_code)) {
    condition <- if (type == "checkbox") {
      sprintf("input.%s && input.%s.includes('%s')", input_id, input_id, other_code)
    } else {
      sprintf("input.%s == '%s'", input_id, other_code)
    }
    shiny::conditionalPanel(
      condition = condition,
      shiny::textInput(other_input_id, NULL, placeholder = other_placeholder)
    )
  }

  shiny::div(
    class = "roundsui-choice-input",
    if (!is.null(label)) shiny::tags$label(class = "roundsui-choice-input__label", label),
    input_widget,
    other_reveal
  )
}

# ---- Confirm unchanged --------------------------------------------------

#' roundsui Confirm-Unchanged Prompt
#'
#' The "Are your career goals the same as last time?" Yes/No pattern from
#' \code{gmed::mod_career_goals}, generalized to any periodic-snapshot
#' data (career goals, a recurring self-assessment, a rotation checklist,
#' ...). This is UI only - roundsui owns no reactive state, matching
#' every other roundsui function; wire the two buttons up the same way
#' you would any \code{actionButton()}:
#' \code{observeEvent(input[[paste0(id, "_yes")]], ...)} and
#' \code{..._no}.
#'
#' @param id Character base id; buttons are \code{paste0(id, "_yes")} and
#'   \code{paste0(id, "_no")}.
#' @param question Character prompt text.
#' @param yes_label,no_label Character button labels.
#'
#' @return A shiny UI element.
#' @export
#'
#' @examples
#' \dontrun{
#' roundsui_confirm_unchanged(
#'   "career_goals",
#'   question = "Are your career goals the same as last time?",
#'   yes_label = "Yes, same goals", no_label = "No, update my goals"
#' )
#' # in the server:
#' observeEvent(input$career_goals_yes, { goals_same(TRUE) })
#' observeEvent(input$career_goals_no, { goals_same(FALSE) })
#' }
roundsui_confirm_unchanged <- function(id,
                                        question = "Are these the same as last time?",
                                        yes_label = "Yes, same",
                                        no_label = "No, update") {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required for UI components")
  }
  shiny::div(
    class = "roundsui-confirm-unchanged",
    shiny::p(class = "roundsui-confirm-unchanged__question", question),
    shiny::div(
      class = "roundsui-confirm-unchanged__actions",
      shiny::actionButton(paste0(id, "_yes"), yes_label, icon = shiny::icon("check"), class = "btn btn-success"),
      shiny::actionButton(paste0(id, "_no"), no_label, icon = shiny::icon("pen"), class = "btn btn-primary")
    )
  )
}

# ---- Filter chips --------------------------------------------------------

#' roundsui Filter Chips
#'
#' A row of toggleable filter chips with optional counts, generalizing
#' the PGY-level/assessment-type chip filters in
#' \code{gmed::mod_eval_feedback} - originally ~50 lines of per-chip
#' inline hex styling (\code{"#eee"}, \code{"#555"}, ...) rebuilt by hand
#' for each of the two filter rows in that file.
#'
#' @param chips A list of lists, each with \code{value}, \code{label},
#'   and optional \code{count} (integer, shown as a small badge).
#' @param input_id Character Shiny input id; clicking a chip sends its
#'   \code{value} to \code{input[[input_id]]} (matching
#'   \code{roundsui_nav_blocks()}'s click mechanism). Clicking "All"
#'   sends \code{""}.
#' @param all_label Character label for the "show everything" chip.
#' @param all_count Optional integer count shown on the "All" chip.
#' @param selected Character, the currently active chip's \code{value}
#'   (\code{""} or \code{NULL} for "All").
#'
#' @return A shiny UI element.
#' @export
#'
#' @examples
#' \dontrun{
#' roundsui_filter_chips(
#'   chips = list(
#'     list(value = "PGY-1", label = "PGY-1", count = 12),
#'     list(value = "PGY-2", label = "PGY-2", count = 9),
#'     list(value = "PGY-3", label = "PGY-3", count = 11)
#'   ),
#'   input_id = "level_filter",
#'   all_count = 32,
#'   selected = input$level_filter
#' )
#' }
roundsui_filter_chips <- function(chips,
                                   input_id,
                                   all_label = "All",
                                   all_count = NULL,
                                   selected = NULL) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required for UI components")
  }
  is_selected <- function(value) !is.null(selected) && nzchar(selected) && identical(selected, value)
  is_all_active <- is.null(selected) || !nzchar(selected)

  chip_tag <- function(value, label, count, active) {
    shiny::tags$button(
      type = "button",
      class = trimws(paste("roundsui-chip", if (active) "roundsui-chip--active")),
      `aria-pressed` = tolower(as.character(active)),
      onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", input_id, value),
      label,
      if (!is.null(count)) shiny::span(class = "roundsui-chip__count", count)
    )
  }

  shiny::div(
    class = "roundsui-chip-row",
    role = "group",
    chip_tag("", all_label, all_count, is_all_active),
    lapply(chips, function(ch) chip_tag(ch$value, ch$label, ch$count, is_selected(ch$value)))
  )
}

# ---- Validation summary --------------------------------------------------

#' roundsui Validation Summary
#'
#' A structured display for a missing-required-fields list, replacing
#' the ecosystem's repeated pattern (found in \code{resident.assessment}
#' and elsewhere) of mashing a character vector into one comma sentence:
#' \code{paste("Please complete the following required fields:",
#' paste(missing_fields, collapse = ", "))}. Takes the same
#' \code{missing_fields} character vector every app's own validation
#' function already returns - only the display changes.
#'
#' @param missing_fields Character vector of human-readable field labels.
#'   Returns \code{NULL} (renders nothing) when empty.
#' @param title Character heading shown above the list.
#'
#' @return A shiny UI element, or \code{NULL} when \code{missing_fields}
#'   is empty.
#' @export
#'
#' @examples
#' \dontrun{
#' missing <- validate_evaluation_form(input, "day", field_names)
#' if (length(missing) > 0) showModal(modalDialog(roundsui_validation_summary(missing)))
#' }
roundsui_validation_summary <- function(missing_fields, title = "Please complete the following:") {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required for UI components")
  }
  if (is.null(missing_fields) || length(missing_fields) == 0) {
    return(NULL)
  }
  shiny::div(
    class = "roundsui-validation-summary",
    role = "alert",
    shiny::div(
      class = "roundsui-validation-summary__title",
      shiny::icon("triangle-exclamation"),
      title
    ),
    shiny::tags$ul(
      class = "roundsui-validation-summary__list",
      lapply(missing_fields, shiny::tags$li)
    )
  )
}
