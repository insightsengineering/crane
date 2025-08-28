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
#' @param variables,row_numbers,variable_level ([`tidy-select`][dplyr::dplyr_tidy_select] or `integer`)\cr
#'   - `variables`: When a table contains variable summaries, use this argument
#'                  to add blank rows below the specified variable block.
#'  - `row_numbers`: Add blank rows after each row number specified.
#'  - `variable_level`: A single column name in `x$table_body` and blank rows
#'                      will be added after each unique level.
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
#'   add_blank_row(variables = everything())
#'
#' # Example 2 ----------------------------------
#' trial |>
#'   tbl_roche_summary(
#'     by = trt,
#'     include = c(age, marker, grade),
#'     nonmissing = "always"
#'   ) |>
#'   add_blank_row(variables = age)
#' @export
add_blank_row <- function(x, variables = NULL, row_numbers = NULL, variable_level = NULL) {
  set_cli_abort_call()
  # check inputs ---------------------------------------------------------------
  check_class(x, "gtsummary")

  # process inputs -------------------------------------------------------------
  check_class(x, "gtsummary")
  if ("variable" %in% names(x$table_body)) {
    cards::process_selectors(gtsummary::scope_table_body(x$table_body), variables = {{ variables }})
  }
  else if (tryCatch(!is_empty(variables), error = \(e) TRUE)) {
    cli::cli_abort(
      "The {.arg variables} argument cannot be specified when {.code x$table_body}
       does not have a column named {.val variable}.",
      call = get_cli_abort_call()
    )
  }

  cards::process_selectors(x$table_body, variable_level = {{ variable_level }})
  check_scalar(
    variable_level,
    allow_empty = TRUE,
    message = "The {.arg variable_level} argument may only select a single column when specified."
  )

  # can only specify one of `variable`, `row_numbers`, and `variable_level`
  if (sum(c(!is_empty(variables), !is_empty(row_numbers), !is_empty(variable_level))) != 1L) {
    cli::cli_abort(
      "One and only one of the following arguments may be specified: {.arg variables}, {.arg row_numbers}, and {.arg variable_level}",
      call = get_cli_abort_call()
    )
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
      dplyr::mutate(....crane_row_number.... = dplyr::row_number()) |>
      dplyr::filter(.data$variable %in% variables) |>
      dplyr::filter(.by = "variable", dplyr::row_number() == dplyr::n()) |>
      deframe()
  }
  else if (!is_empty(variable_level)) {
    row_indices <-
      x$table_body[variable_level] |>
      dplyr::mutate(....crane_row_number.... = dplyr::row_number()) |>
      dplyr::filter(.by = any_of(variable_level), dplyr::row_number() == dplyr::n()) |>
      deframe()
  }
  else {
    row_indices <- row_numbers
  }

  # remove the last row if specified, and sort and reverse the order
  row_indices <- setdiff(row_indices, nrow(x$table_body)) |> sort() |> unique() |> rev()

  # cycle through the row indices and add a blank row after each
  for (i in seq_along(row_indices)) {
    x <- x |>
      gtsummary::modify_table_body(
        ~ .x |>
          dplyr::add_row(
            setNames(dplyr::tibble(NA), colnames(.x)[1]),
            .after = row_indices[i]
          )
      )
  }

  # return table ---------------------------------------------------------------
  x$call_list <- updated_call_list
  x
}
