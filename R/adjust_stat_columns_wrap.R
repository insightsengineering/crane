#' Adjust Statistics Columns Wrapping
#'
#' @description
#' Toggles standard whitespace to non-breaking spaces (`\u00A0`) dynamically
#' in all visible statistics columns of a `gtsummary` table's body (or vice versa).
#' This forces the layout engine to keep statistics (e.g., "12.5 ( 95%)")
#' on a single line when protected. Column headers and labels remain unaffected.
#' For the rare cases when protecting creates ugly squashed label column
#' protection can be reversed using the same function.
#'
#' @param tbl (`gtsummary`)\cr
#'   A `gtsummary` object.
#' @param mode (`character(1)`)\cr
#'   Either `"protect"` (replaces whitespace with non-breaking spaces) or
#'   `"unprotect"` (replaces non-breaking spaces with standard spaces).
#'   Defaults to `"protect"`.
#'
#' @return A modified `gtsummary` object.
#'
#' @export
adjust_stat_columns_wrap <- function(tbl, mode = c("protect", "unprotect")) {
  # Robustly match the argument (defaults to "protect" if not provided)
  mode <- rlang::arg_match(mode)

  if (!inherits(tbl, "gtsummary")) {
    cli::cli_abort("Argument {.arg tbl} must be a {.cls gtsummary} object.")
  }

  # set the wrapping mode for the table to theme_gtsummary_roche
  attr(tbl, "wrap_mode") <- mode

  # Setup the regex/strings based on the selected mode
  if (mode == "protect") {
    change_from <- " "
    change_to <- "\u00A0"
  } else if (mode == "unprotect") {
    change_from <- "\u00A0"
    change_to <- " "
  }

  # 1. Dynamically identify statistics columns
  visible_cols <- tbl$table_styling$header$column[!tbl$table_styling$header$hide]

  # Select standard statistics columns across all gtsummary table types based on
  # regex
  pattern <- paste0(
    "^(stat|add_stat|p\\.value|q\\.value|ci|estimate",
    "|std\\.error|conf\\.low|conf\\.high)"
  )
  stat_cols <- visible_cols[grepl(pattern, visible_cols)]

  if (length(stat_cols) == 0) {
    return(tbl)
  }

  # 2. Inject or remove non-breaking spaces ONLY into the table body.
tbl <- tbl |>
    gtsummary::modify_table_body(
      ~ .x |>
        dplyr::mutate(
          dplyr::across(
            dplyr::all_of(stat_cols),
            ~ if (is.character(.x)) gsub(change_from, change_to, .x) else .x
          )
        )
    )

  tbl
}
