#' Add Blank Row
#'
#' @description
#' Add a blank row below each variable group defined by `variables` or below each
#' specified `row_numbers`. A blank row will not be added to the bottom of the table.
#'
#' *NOTE*: For HTML flextable output (which includes the RStudio IDE Viewer),
#'         the blank rows do not render. But they will appear when the
#'         table is rendered to Word.
#'
#' @param x (`gtsummary`)\cr
#'   a 'gtsummary' table. The table must include a column named `'variable'`
#'   in `x$table_body`.
#' @param variables,row_numbers ([`tidy-select`][dplyr::dplyr_tidy_select] or `integer`)\cr
#'   variables or row numbers at which a blank row needs to be added. If none is selected,
#'   `variables = everything()` is used, which adds a blank row after every variable.
#'
#' @returns updated 'gtsummary' table.
#'
#' @examples
#' # Example 1 ----------------------------------
#' # Default to every variable used
#' trial |>
#'   tbl_roche_summary(
#'     by = trt,
#'     include = c(age, marker, grade),
#'     nonmissing = "always"
#'   ) |>
#'   add_blank_row()
#'
#' # Example 2 ----------------------------------
#' trial |>
#'   tbl_roche_summary(
#'     by = trt,
#'     include = c(age, marker, grade),
#'     nonmissing = "always"
#'   ) |>
#'   add_blank_row(variable = age)
#'
#' # Example 3 ----------------------------------
#' trial |>
#'   tbl_roche_summary(
#'     by = trt, statistic = age ~ "{mean}",
#'     include = c(age, grade),
#'     nonmissing = "no"
#'   ) |>
#'   add_blank_row(age, row_numbers = c(1, 1)) # can add multiple blank lines
#'
#' @export
add_blank_row <- function(x, variables = NULL, row_numbers = NULL) {
  set_cli_abort_call()
  # check inputs ---------------------------------------------------------------
  check_class(x, "gtsummary")

  # if the table doesn't have a 'variable' column, user must specify `row_numbers`
  if (!"variable" %in% names(x$table_body) && is_empty(row_numbers)) {
    cli::cli_abort(
      "The {.arg row_numbers} argument must be specified when the passed table does
       not contain a {.val variable} column in {.code x$table_body}.",
      call = get_cli_abort_call()
    )
  }

  # process inputs -------------------------------------------------------------
  if ("variable" %in% names(x$table_body)) {
    cards::process_selectors(
      data = scope_table_body(x$table_body),
      variables = {{ variables }}
    )
    # if variables and row_number is NULL, use everything()
    if (is_empty(variables) && is_empty(row_numbers)) {
      variables <- unique(x$table_body$variable) # Same as everything()?
      cards::process_selectors(
        data = scope_table_body(x$table_body),
        variables = {{ variables }}
      )
    }
  }

  # check that row_numbers is integerish and in bounds
  check_integerish(row_numbers, allow_empty = TRUE)

  # check that row_numbers are in bounds
  check_range(
    row_numbers,
    include_bounds = c(TRUE, TRUE),
    range = c(1L, nrow(x$table_body)),
    message = c("Argument {.arg row_numbers} is out of bounds.",
      i = "Must be between {.val {1}} and {.val {nrow(x$table_body)}}."
    ),
    envir = current_env(),
    allow_empty = TRUE
  )

  updated_call_list <- append(x$call_list, list(add_blank_row = match.call()))

  # add blank row --------------------------------------------------------------
  # get row indices where to add blank row
  if (!is_empty(variables)) {
    row_indices <-
      x$table_body["variable"] |>
      dplyr::mutate(row_number = dplyr::row_number()) |>
      dplyr::filter(variable %in% variables) |>
      dplyr::filter(.by = "variable", dplyr::row_number() == dplyr::n()) |>
      deframe() |>
      rev()
  } else {
    row_indices <- NULL
  }

  # if row_numbers is specified, add those to the row indices
  if (!is_empty(row_numbers)) {
    row_indices <- c(row_indices, "row_sep" = row_numbers + seq(0, length(row_numbers) - 1))
  }


  # cycle through the row indices and add a blank row after each
  for (i in seq_along(row_indices)) {
    # don't add a blank row to the bottom of the table
    if (row_indices[i] == nrow(x$table_body)) next

    # if the table has a 'variable' column, add a blank row after the specified row_indices
    if ("variable" %in% names(x$table_body)) {
      x <- x |>
        gtsummary::modify_table_body(
          ~ .x |>
            dplyr::add_row(
              dplyr::tibble(variable = names(row_indices)[i]),
              .after = row_indices[i]
            )
        )
    } else {
      # if no variable column, add a blank row after the specified row_numbers
      x <- x |>
        gtsummary::modify_table_body(
          ~ .x |>
            dplyr::add_row(
              setNames(dplyr::tibble(NA), colnames(.x)[1]),
              .after = row_indices[i]
            )
        )
    }
  }

  # return table ---------------------------------------------------------------
  x$call_list <- updated_call_list
  x
}
