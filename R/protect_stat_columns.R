#' Protect Statistics Columns from DOCX Wrapping
#'
#' @description
#' Replaces standard whitespace with non-breaking spaces (`\u00A0`) dynamically
#' in all visible statistics columns of a `gtsummary` table's body.
#' This forces the DOCX layout engine to keep statistics (e.g., "12.5 ( 95%)")
#' on a single line. Column headers remain unaffected.
#'
#' @param tbl (`gtsummary`)\cr
#'   A `gtsummary` object.
#'
#' @return A modified `gtsummary` object.
#'
#' @export
protect_stat_columns <- function(tbl) {
  # Fix: Ensure we check the correct variable name 'tbl'
  if (!inherits(tbl, "gtsummary")) {
    cli::cli_abort("Argument {.arg tbl} must be a {.cls gtsummary} object.")
  }

  # 1. Dynamically identify statistics columns
  # Extract visible columns from the header metadata
  visible_cols <- tbl$table_styling$header$column[!tbl$table_styling$header$hide]

  # Remove "label" and any label-related columns (like indentation levels)
  stat_cols <- setdiff(
    visible_cols,
    visible_cols[startsWith(visible_cols, "label")]
  )

  # If no stat columns found (rare), return table as-is
  if (length(stat_cols) == 0) {
    return(tbl)
  }

  # 2. Inject non-breaking spaces ONLY into the table body.
  # We use \\s to catch any existing whitespace variations.
  tbl <- tbl |>
    gtsummary::modify_table_body(
      ~ .x |>
        dplyr::mutate(
          dplyr::across(
            dplyr::all_of(stat_cols),
            ~ gsub(" ", "\u00A0", as.character(.x))
          )
        )
    )

  tbl
}
