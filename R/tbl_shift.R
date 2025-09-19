#' Shift Table
#'
#' Typical use is tabulating post-baseline measurement stratified by the
#' baseline measurement.
#'
#' @inheritParams tbl_roche_summary
#' @inheritParams gtsummary::add_overall
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
#' @param x (`tbl_shift`)\cr
#'   Object of class `'tbl_shift'`.
#' @param col_label (`string`)\cr
#'   String indicating the column label. Default is `"All Participants \nN = {gtsummary::style_number(n)}"`
#'
#' @returns a 'gtsummary' table
#' @name tbl_shift
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
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true")
#' library(dplyr, warn.conflicts = FALSE)
#'
#' # subsetting ADLB on one PARAM, and the highest grade
#' No ATOXGRH or BOTOXGRH in cards::ADLB, generating these variables here
#'adlb <- cards::ADLB |>
#'  dplyr::mutate(
#'    ATOXGRH = factor(dplyr::case_when(
#'      LBNRIND == "HIGH" & A1HI >= 0 & A1HI <= 30 ~ "0",
#'      LBNRIND == "HIGH" & A1HI >= 31 & A1HI <= 100 ~ "1",
#'      LBNRIND == "HIGH" & A1HI >= 100 & A1HI <= 300 ~ "2",
#'      LBNRIND == "HIGH" & A1HI >= 400 & A1HI <= 500 ~ "3",
#'      TRUE ~ NA_character_
#'    )),
#'    BTOXGRH = factor(dplyr::case_when(
#'      LBNRIND == "HIGH" & BR2A1HI >= 0 & BR2A1HI <= 0.40 ~ "0",
#'      LBNRIND == "HIGH" & BR2A1HI >= 0.41 & BR2A1HI <= 0.80 ~ "1",
#'      LBNRIND == "HIGH" & BR2A1HI >= 0.81 & BR2A1HI <= 1.0 ~ "2",
#'      LBNRIND == "HIGH" & BR2A1HI >= 1.1 & BR2A1HI <= 1.50 ~ "3",
#'      TRUE ~ NA_character_
#'    ))
#'  ) |>
#'  dplyr::select("USUBJID", "TRTA", "PARAM", "PARAMCD", "ATOXGRH", "BTOXGRH", "VISITNUM") |>
#'  dplyr::mutate(TRTA = factor(TRTA)) |>
#'  dplyr::filter(PARAMCD %in% c("CHOL", "GLUC")) |>
#'  dplyr::slice_max(by = c(USUBJID, PARAMCD), order_by = ATOXGRH, n = 1L, with_ties = FALSE) |>
#'  labelled::set_variable_labels(
#'    BTOXGRH = "Baseline  \nNCI-CTCAE Grade",
#'    ATOXGRH = "Post-baseline  \nNCI-CTCAE Grade"
#'  )
#' adsl <- cards::ADSL[c("USUBJID", "TRTA")] |>
#' dplyr::mutate(TRTA = factor(TRTA))
#'
#' # Example 1 ----------------------------------
#' # tabulate baseline grade by worst grade
#' tbl_shift(
#'   data = filter(adlb, PARAMCD %in% "CHOL"),
#'   strata = BTOXGRH,
#'   variable = ATOXGRH,
#'   by = TRTA,
#'   data_header = adsl
#' )
#'
#' # Example 2 ----------------------------------
#' # same as Ex1, but with the stratifying variable levels in header rows
#' adlb |>
#'   filter(PARAMCD %in% "CHOL") |>
#'   labelled::set_variable_labels(
#'     BTOXGRH = "Baseline NCI-CTCAE Grade",
#'     ATOXGRH = "Post-baseline NCI-CTCAE Grade"
#'   ) |>
#'   tbl_shift(
#'     data = ,
#'     strata = BTOXGRH,
#'     variable = ATOXGRH,
#'     strata_location = "header",
#'     by = TRTA,
#'     data_header = adsl
#'   )
#'
#' # Example 3 ----------------------------------
#' # same as Ex2, but with two labs
#' adlb |>
#'   labelled::set_variable_labels(
#'     BTOXGRH = "Baseline NCI-CTCAE Grade",
#'     ATOXGRH = "Post-baseline NCI-CTCAE Grade"
#'   ) |>
#'   tbl_strata_nested_stack(
#'     strata = PARAM,
#'     ~ .x |>
#'       tbl_shift(
#'         strata = BTOXGRH,
#'         variable = ATOXGRH,
#'         strata_location = "header",
#'         by = TRTA,
#'         data_header = adsl
#'       )
#'   ) |>
#'   # Update header with Lab header and indentation (the '\U00A0' character adds whitespace)
#'   modify_header(
#'     label = "Lab  \n\U00A0\U00A0\U00A0\U00A0
#'              Baseline NCI-CTCAE Grade  \n\U00A0\U00A0\U00A0\U00A0\U00A0\U00A0\U00A0\U00A0
#'              Post-baseline NCI-CTCAE Grade"
#'   )
#'
#' # Example 4 ----------------------------------
#' # Include the treatment variable in a new column
#' filter(adlb, PARAMCD %in% "CHOL") |>
#'   right_join(
#'     cards::ADSL[c("USUBJID", "TRTA")],
#'     by = c("USUBJID", "TRTA")
#'   ) |>
#'   tbl_shift(
#'     strata = TRTA,
#'     variable = BTOXGRH,
#'     by = ATOXGRH,
#'     header = "{level}",
#'     strata_label = "{strata}, N={n}",
#'     label = list(TRTA = "Actual Treatment"),
#'     percent = "cell",
#'     nonmissing = "no"
#'   ) |>
#'   modify_spanning_header(all_stat_cols() ~ "Worst Post-baseline NCI-CTCAE Grade")
NULL

#' @rdname tbl_shift
#' @export
tbl_shift <- function(data,
                      variable,
                      strata = NULL,
                      by = NULL,
                      data_header = NULL,
                      strata_location = c("new_column", "header"),
                      strata_label = "{strata}",
                      header = "{level}  \nN = {n}",
                      label = NULL,
                      nonmissing = "always",
                      nonmissing_text = "Total",
                      ...) {
  set_cli_abort_call()
  # check inputs ---------------------------------------------------------------
  check_not_missing(data)
  check_not_missing(variable)
  check_data_frame(data)
  check_data_frame(data_header, allow_empty = TRUE)
  strata_location <- arg_match(strata_location)
  check_string(header)
  check_string(strata_label)
  cards::process_selectors(data, strata = {{ strata }}, variable = {{ variable }}, by = {{ by }})
  check_scalar(strata, allow_empty = TRUE, message = "The {.arg strata} argument must select exactly one variable or none.")
  check_scalar(variable, message = "The {.arg variable} argument must select exactly one variable.")
  check_scalar(by, allow_empty = TRUE, message = "The {.arg by} argument must select exactly one variable or none.")
  cards::process_formula_selectors(data[c(strata, variable)], label = label)
  if (!is_empty(data_header) && any(!names(data_header) %in% names(data))) {
    cli::cli_abort(
      c("The data frame passed in the {.arg data_header} argument should only
        include columns that will be used to merge with {.arg data}.",
        i = "Based on the other inputs, this likely means only including {.val {c('USUBJID', by)}}."
      ),
      call = get_cli_abort_call()
    )
  }

  tbl_shift_inputs <- as.list(environment())

  # replace strata colum with an overall if empty ------------------------------
  if (is_empty(strata)) {
    strata <- "...overall...strata..."
    data[[strata]] <- "All Participants"
    attr(data[[strata]], "label") <- "Cohort"
  }

  # build stratified table -----------------------------------------------------
  # first get the label for the variable and the strata variable
  strata_var_label <- label[[strata]] %||% attr(data[[strata]], "label") %||% strata
  variable_var_label <- label[[variable]] %||% attr(data[[variable]], "label") %||% variable

  # if there is a `by` variable, make it a factor to ensure all levels appear in tbls
  if (!is_empty(by) && !is.factor(data[[by]])) {
    cli::cli_inform(c("i" = "Converting column {.val {by}} to a factor."))
    old_by_label <- attr(data[[by]], "label")
    data[[by]] <- factor(data[[by]])
    attr(data[[by]], "label") <- old_by_label
  }

  x <-
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
            tbl <- tbl |>
              gtsummary::modify_header(
                label = paste(strata_var_label, variable_var_label, sep = "  \n\U00A0\U00A0\U00A0\U00A0")
              )
          }
        },
      .combine_with = "tbl_stack",
      .combine_args = list(group_header = NULL)
    )

  # final prep of table --------------------------------------------------------
  x$inputs <- tbl_shift_inputs
  x$call_list <- list(tbl_shift = match.call())

  x %>%
    structure(., class = c("tbl_shift", class(.)))
}

#' @rdname tbl_shift
#' @export
add_overall.tbl_shift <- function(x,
                                  col_label = "All Participants  \n(N = {gtsummary::style_number(n)})",
                                  last = FALSE, ...) {
  # check inputs ---------------------------------------------------------------
  set_cli_abort_call()
  check_dots_empty(call = get_cli_abort_call())
  check_string(col_label)
  check_scalar_logical(last)
  if (is_empty(x$inputs$by)) {
    cli::cli_inform(
      c("Original table was not stratified, and overall column cannot be added.",
        i = "Table has been returned unaltered."
      )
    )
    return(x)
  }

  # build overall table --------------------------------------------------------
  tbl_overall <-
    x$inputs |>
    utils::modifyList(list(by = NULL)) |>
    do.call("tbl_shift", args = _)

  # check the tbls have the same structure before merging
  if (!identical(
    dplyr::select(x$table_body, any_of(c("label0", "label"))),
    dplyr::select(tbl_overall$table_body, any_of(c("label0", "label")))
  )) {
    cli::cli_inform(
      c("!" = "The structures of the original table and the overall table are not identical,
         and the resulting table may be malformed.")
    )
  }

  # merge tables ---------------------------------------------------------------
  gtsummary::tbl_merge(
    tbls = list(x, tbl_overall),
    tab_spanner = FALSE,
    merge_vars = c("variable", "row_type", "var_label", "label0", "label")
  )
}
