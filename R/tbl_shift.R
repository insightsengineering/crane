#' Shift Table
#'
#' Typical use is tabulating post-baseline measurement stratified by the
#' baseline measurement.
#'
#' @inheritParams tbl_roche_summary
#' @param strata ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Stratifying variable. Typically the baseline grade.
#' @param variable ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Variable to tabulate. Typically the post-baseline grade.
#' @param by ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Variable to report results by. Typical value is the treatment arm.
#' @param data_header (`data.frame`)\cr
#'   Data frame used to calculate the Ns in the table header.
#'   Only include the columns needed to merge with `data`: these are typically
#'   the `'USUBJID'` and the treatment arm only, e.g `ADSL[c("USUBJID", "ARM")]`.
#' @param strata_location (`string`)\cr
#'   Specifies the location where the individual stratum levels will be printed.
#'   Must be one of `c("new_column", "header")`.
#'   `"new_column"`: stratum labels are placed in a new column to the left of the tabulated results.
#'   `"header"`: stratum labels are placed in a header row above the tabulations.
#' @param strata_label (`string`)\cr
#'   A glue-string that inserts stratum level. Default is `'{strata}'`, and
#'   `{n}` is also available to insert.
#' @param header (`string`)\cr
#'   String that is passed to `gtsummary::modify_header(all_stat_cols() ~ header)`.
#' @param label ([`formula-list-selector`][gtsummary::syntax])\cr
#'   Used to specify the labels for the `strata` and `variable` columns.
#'   Default is to use the column label attribute.
#' @param nonmissing,nonmissing_text,... Argument passed to `tbl_roche_summary()`.
#'   See details below for call details to `tbl_roche_summary()`.
#'
#' @returns a 'gtsummary' table
#' @export
#'
#' @details
#' Broadly, this function is a wrapper for chunk below with some additional
#' calls to `gtsummary::modify_*()` function to update the table's
#' headers, indentation, column alignment, etc.
#'
#' ```r
#' gtsummary::tbl_strata2(
#'   data = data,
#'   strata = strata,
#'    ~ tbl_roche_summary(.x, include = variable, by = by)
#' )
#' ```
#'
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' # subsetting ADLB on one PARAM, and the highest grade
#' adlb <- pharmaverseadam::adlb[c("USUBJID", "TRT01A", "PARAMCD", "ATOXGRH", "BTOXGRH", "VISITNUM")]|>
#'   mutate(TRT01A = factor(TRT01A)) |>
#'   filter(PARAMCD == "CHOLES") |>
#'   slice_max(by = c(USUBJID, PARAMCD), order_by = ATOXGRH, n = 1L, with_ties = FALSE) |>
#'   labelled::set_variable_labels(
#'     BTOXGRH = "Baseline  \nNCI-CTCAE Grade",
#'     ATOXGRH = "Post-baseline  \nNCI-CTCAE Grade"
#'   )
#'
#' # Example 1 ----------------------------------
#' # tabulate baseline grade by worst grade
#' tbl_shift(
#'   data = adlb,
#'   strata = BTOXGRH,
#'   variable = ATOXGRH,
#'   by = TRT01A,
#'   data_header =
#'     pharmaverseadam::adsl[c("USUBJID", "TRT01A")] |>
#'     filter(TRT01A != "Screen Failure")
#' )
#'
#' # Example 2 ----------------------------------
#' # same as Ex1, but with the stratifying variable levels in header rows
#' tbl_shift(
#'   data = adlb,
#'   strata = BTOXGRH,
#'   variable = ATOXGRH,
#'   strata_location = "header",
#'   by = TRT01A,
#'   data_header =
#'     pharmaverseadam::adsl[c("USUBJID", "TRT01A")] |>
#'     filter(TRT01A != "Screen Failure")
#' )
#'
#' # Example 3 ----------------------------------
#' # Include the treatment variable in a new column
#' adlb |>
#'   right_join(
#'     pharmaverseadam::adsl[c("USUBJID", "TRT01A")] |>
#'       filter(TRT01A != "Screen Failure"),
#'     by = c("USUBJID", "TRT01A")
#'   ) |>
#'   tbl_shift(
#'     data = ,
#'     strata = TRT01A,
#'     variable = BTOXGRH,
#'     by = ATOXGRH,
#'     header = "{level}",
#'     strata_label = "{strata}, N={n}",
#'     label = list(TRT01A = "Actual Treatment"),
#'     percent = "cell",
#'     nonmissing = "no"
#'   ) |>
#'   modify_spanning_header(all_stat_cols() ~ "Worst Post-baseline NCI-CTCAE Grade")
tbl_shift <- function(data,
                      strata,
                      variable,
                      by = NULL,
                      data_header = NULL,
                      strata_location = c("new_column", "header"),
                      strata_label = "{strata}",
                      header = "{level}  \nN = {n}",
                      label = NULL,
                      nonmissing = "always",
                      nonmissing_text = "Total",
                      ...
                      ) {
  set_cli_abort_call()
  # check inputs ---------------------------------------------------------------
  check_not_missing(data)
  check_not_missing(strata)
  check_not_missing(variable)
  check_data_frame(data)
  check_data_frame(data_header, allow_empty = TRUE)
  strata_location <- arg_match(strata_location)
  check_string(header)
  check_string(strata_label)
  cards::process_selectors(data, strata = {{ strata }}, variable = {{ variable }}, by = {{ by }})
  check_scalar(strata, message = "The {.arg strata} argument must select exactly one variable.")
  check_scalar(variable, message = "The {.arg variable} argument must select exactly one variable.")
  check_scalar(by, allow_empty = TRUE, message = "The {.arg by} argument must select exactly one variable or none.")
  cards::process_formula_selectors(data[c(strata, variable)], label = label)
  if (!is_empty(data_header) && any(!names(data_header) %in% names(data))) {
    cli::cli_abort(
      c("The data frame passed in the {.arg data_header} argument should only
        include columns that will be used to merge with {.arg data}.",
        i = "Typcially, this means only including {.val {c('USUBJID', 'ARM')}}
        (if {.val ARM} is the columns passed in the {.arg by} argument)."),
      call = get_cli_abort_call()
    )
  }

  # build stratified table -----------------------------------------------------
  # first get the label for the variable and the strata variable
  strata_var_label <- label[[strata]] %||% attr(data[[strata]], "label") %||% strata
  variable_var_label <- label[[variable]] %||% attr(data[[variable]], "label") %||% variable

  # if there is a `by` variable, make it a factor to ensure all levels appear in tbls
  if (!is.factor(data[[by]])) {
    old_by_label <- attr(data[[by]], "label")
    data[[by]] <- factor(data[[by]])
    attr(data[[by]], "label") <- old_by_label
  }

  gtsummary::tbl_strata2(
    data = data,
    strata = all_of(strata),
    .tbl_fun =
      \(data, stratum) {
        # if `data_header` was passed, then merge it with the primary data
        if (!is_empty(data_header)) {
          data <-
            dplyr::right_join(
              data,
              data_header,
              by = names(data_header)
            )
        }

        # Glue the stratum level
        stratum <- glue::glue_data(.x = list(strata = stratum, n = nrow(data)), strata_label)

        # build cross table
        tbl <-
          tbl_roche_summary(
            data = data,
            by = any_of(by),
            include = all_of(variable),
            nonmissing = nonmissing,
            nonmissing_text = nonmissing_text,
            label = list(stratum) |> set_names(variable), # if we keep the ..., users may try to specify this arg which would cause an error
            ...
          ) |>
          gtsummary::modify_header(all_stat_cols() ~ header)

        # If new column, add the column, update indentation and alignment
        if (strata_location == "new_column") {
          tbl <- tbl |>
            gtsummary::remove_row_type(type = "header") |>
            gtsummary::modify_table_body(
              ~ .x |>
                mutate(
                  .before = "label",
                  label0 = ifelse(dplyr::row_number() == 1L, .env$stratum, NA_character_)
                )
            ) |>
            gtsummary::modify_column_alignment(columns = c("label", "label0"), align = "left") |>
            gtsummary::modify_indent(columns = label, indent = 0L) |>
            gtsummary::modify_header(label0 = strata_var_label, label = variable_var_label)
        }
        # If not new column, update column header
        else if (strata_location == "header") {
          tbl <- tbl  |>
            gtsummary::modify_header(
              label = paste(strata_var_label, variable_var_label, sep = "  \n\U00A0\U00A0\U00A0\U00A0")
            )
        }
      },
    .combine_with = "tbl_stack",
    .combine_args = list(group_header = NULL)
  ) %>%
    structure(., class = c("tbl_shift", class(.)))
}
