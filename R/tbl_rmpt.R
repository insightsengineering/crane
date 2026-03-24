#' Risk Management Plan Table (RMPT)
#'
#' @description
#' Creates a summary table showing participant counts and person-time exposure
#' across categories of exposure duration. The table displays both:
#' - Number and percentage of participants in each exposure duration category
#' - Total person-time (sum of exposure durations) for each category
#'
#' By default, the table does not stratify by treatment arms. Please refer to the RMP Best Pracice documents for guidance.
#'
#' Total person-time is computed by summing up the exposure duration (e.g., `AVAL`) across all participants within each category.
#' The unit can be days, months or years depending on the use-case.
#'
#'
#' @inheritParams tbl_roche_summary
#' @inheritParams gtsummary::add_overall
#' @param data (`data.frame`)\cr
#'   A data frame containing the exposure data. Typically ADEX dataset filtered
#'   to one row per subject (e.g., `PARAMCD == 'TDURD'` and `PARCAT1 == 'OVERALL'`).
#' @param variable ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Categorical variable defining exposure duration categories (e.g., `AVAL_CAT`).
#'   This variable should be a factor with ordered levels.
#' @param aval ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Continuous variable containing exposure duration values (e.g., `AVAL`).
#'   Used to calculate person-time by summing across participants.
#' @param by ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Variable to report results by. Typically the treatment arm (e.g., `TRT01A` or `TRTA`).
#'   Default is `NULL` for unstratified analysis.
#' @param id ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   String identifying the unique subjects. Default is `'USUBJID'`.
#' @param denominator (`data.frame`)\cr
#'   Data set used to compute the header counts and percentages (typically ADSL).
#'   Should contain columns for `id` and `by` variables.
#' @param label (`string`)\cr
#'   Label for the exposure duration variable that appears in the table header.
#'   Default is `"Duration of exposure"`.
#' @param participant_footnote (`string`)\cr
#'   Footnote text for participant count columns. Default explains participant counting.
#' @param person_time_footnote (`string`)\cr
#'   Footnote text for person-time columns. Default explains person-time calculation.
#' @param x (`tbl_rmpt`)\cr
#'   Object of class `'tbl_rmpt'`.
#' @param col_label (`string`)\cr
#'   String indicating the column label for overall column.
#'   Default is `"All Participants  \n(N = {style_roche_number(n)})"`.
#'
#' @return A gtsummary table.
#' @name tbl_rmpt
#'
#' @examples
#' # Create example exposure data
#'
#' df_adsl <- pharmaverseadam::adsl |> dplyr::filter(SAFFL == "Y")
#' df_adex <- pharmaverseadam::adex |>
#'   dplyr::filter(PARAMCD == 'TDURD', PARCAT1 == 'OVERALL', SAFFL == "Y") |>
#'   dplyr::mutate(
#'     AVAL_MONTH = AVAL / 30.4375,
#'     AVAL_CAT = factor(
#'       dplyr::case_when(
#'         AVAL_MONTH < 1 ~ "< 1 month",
#'         AVAL_MONTH >= 1 & AVAL_MONTH < 3 ~ "1 to <3 months",
#'         AVAL_MONTH >= 3 & AVAL_MONTH < 6 ~ "3 to <6 months",
#'         TRUE ~ ">=6 months"
#'       ),
#'       levels = c("< 1 month", "1 to <3 months", "3 to <6 months", ">=6 months")
#'     )
#'   ) |>
#'   dplyr::select(
#'     USUBJID,
#'     SEX,
#'     ETHNIC,
#'     RACE,
#'     AGEGR1,
#'     AVAL,
#'     AVAL_MONTH,
#'     AVAL_CAT,
#'     TRT01A
#'   )
#' # Example 1 --------------------------------
#' # Create basic RMPT table
#' tbl_rmpt(
#'   data = df_adex,
#'   variable = AVAL_CAT,
#'   aval = AVAL,
#'   by = TRT01A,
#'   denominator = df_adsl
#' )
#'
#' # Example 2 --------------------------------
#' # Add overall column at the end
#' tbl_rmpt(
#'   data = df_adex,
#'   variable = AVAL_CAT,
#'   aval = AVAL,
#'   by = TRT01A,
#'   denominator = df_adsl
#' ) |>
#'   add_overall(last = TRUE)
#'
#' # Example 3 --------------------------------
#' # Customize labels and footnotes
#' tbl_rmpt(
#'   data = df_adex,
#'   variable = AVAL_CAT,
#'   aval = AVAL,
#'   by = TRT01A,
#'   denominator = df_adsl,
#'   label = "Treatment Exposure Duration",
#'   participant_footnote = "Number of patients in each category",
#'   person_time_footnote = "Total person-time in days"
#' )
#'
#' # Example 3 --------------------------------
#' # RMPT table for another variable (e.g. ethnicity), stratified by treatment arm
#' tbl_rmpt(
#'  data = df_adex,
#'  variable = ETHNIC,
#'  aval = AVAL,
#'  by = TRT01A,
#'  denominator = df_adsl
#')
#'
NULL

#' @rdname tbl_rmpt
#' @export
tbl_rmpt <- function(
  data,
  variable,
  aval,
  by = NULL,
  id = "USUBJID",
  denominator,
  label = "Duration of exposure",
  participant_footnote = "Number of participants in each exposure duration category",
  person_time_footnote = "Person time is the sum of exposure duration across all participants"
) {
  set_cli_abort_call()

  # check inputs ---------------------------------------------------------------
  check_not_missing(data)
  check_not_missing(variable)
  check_not_missing(aval)
  check_not_missing(denominator)
  check_data_frame(data)
  check_data_frame(denominator)
  check_string(label)
  check_string(participant_footnote)
  check_string(person_time_footnote)

  cards::process_selectors(
    data,
    variable = {{ variable }},
    aval = {{ aval }},
    by = {{ by }},
    id = {{ id }}
  )
  check_scalar(variable)
  check_scalar(aval)
  check_scalar(by, allow_empty = TRUE)
  check_scalar(id)

  # Save function inputs
  tbl_rmpt_inputs <- as.list(environment())

  # Create non-missing indicator variable --------------------------------------
  data[[".non_missing_aval"]] <- !is.na(data[[aval]])

  # Pivot data wide for category-level summaries ------------------------------
  df_wide <- data |>
    tidyr::pivot_wider(
      id_cols = all_of(c(id, by)),
      names_from = all_of(variable),
      values_from = all_of(c(aval, ".non_missing_aval")),
      names_sort = TRUE
    ) |>
    # Add in denominator for the header Ns
    dplyr::right_join(
      denominator[c(id, by)],
      by = c(id, by),
      relationship = "many-to-one"
    )

  # Build participant count table (by category) -------------------------------
  tbl_count_cat <- df_wide |>
    dplyr::select(all_of(by), starts_with(".non_missing_aval")) |>
    dplyr::rename_with(~ str_remove(., "^\\.non_missing_aval_")) |>
    gtsummary::tbl_summary(
      by = all_of(by),
      missing = "no",
      percent = denominator
    )

  # Build participant count table (overall) -----------------------------------
  tbl_count_overall <- data |>
    gtsummary::tbl_summary(
      by = all_of(by),
      include = ".non_missing_aval",
      label = list(.non_missing_aval = "Total"),
      missing = "no",
      percent = denominator
    )

  # Stack category and overall count tables
  tbl_count <- gtsummary::tbl_stack(
    list(tbl_count_cat, tbl_count_overall),
    quiet = TRUE
  )

  # Build person-time table (by category) -------------------------------------
  tbl_pt_cat <- df_wide |>
    dplyr::select(all_of(by), starts_with(paste0(aval, "_"))) |>
    dplyr::rename_with(~ str_remove(., paste0("^", aval, "_"))) |>
    labelled::remove_var_label() |>
    tbl_roche_summary(
      by = all_of(by),
      statistic = everything() ~ "{sum}",
      type = everything() ~ "continuous",
      nonmissing = "no"
    )

  # Build person-time table (overall) -----------------------------------------
  tbl_pt_overall <- data |>
    tbl_roche_summary(
      include = all_of(aval),
      by = all_of(by),
      statistic = everything() ~ "{sum}",
      type = everything() ~ "continuous",
      label = list(Total = "Total") |> set_names(aval),
      nonmissing = "no"
    )

  # Stack category and overall person-time tables
  tbl_pt <- gtsummary::tbl_stack(
    list(tbl_pt_cat, tbl_pt_overall),
    quiet = TRUE
  )

  # Merge count and person-time tables -----------------------------------------
  rmpt_tbl <-
    gtsummary::tbl_merge(
      list(tbl_count, tbl_pt),
      tab_spanner = FALSE,
      quiet = TRUE,
      merge_vars = "label"
    ) |>
    # Modify column headers
    gtsummary::modify_header(
      gtsummary::all_stat_cols() & ends_with("_1") ~ "Participants",
      gtsummary::all_stat_cols() & ends_with("_2") ~ "Person time",
      label = label
    ) |>
    gtsummary::modify_spanning_header(
      gtsummary::all_stat_cols() ~ "{level}  \n(N = {n})"
    ) |>
    # Sort stat columns to group by treatment arm
    gtsummary::modify_table_body(
      \(.x) {
        stat_cols <- dplyr::select(.x, gtsummary::all_stat_cols()) |>
          names() |>
          sort()
        dplyr::relocate(.x, all_of(stat_cols), .after = "label")
      }
    ) |>
    # Add footnotes
    gtsummary::modify_footnote_header(
      footnote = participant_footnote,
      columns = gtsummary::all_stat_cols() & ends_with("_1"),
      replace = TRUE
    ) |>
    gtsummary::modify_footnote_header(
      footnote = person_time_footnote,
      columns = gtsummary::all_stat_cols() & ends_with("_2"),
      replace = TRUE
    )

  # Return table ---------------------------------------------------------------
  rmpt_tbl$call_list <- list(tbl_rmpt = match.call())
  rmpt_tbl$inputs <- tbl_rmpt_inputs
  # Combine ARDs from both component tables
  rmpt_tbl$cards <- list(
    tbl_rmpt = list(
      tbl_count = gtsummary::gather_ard(tbl_count),
      tbl_pt = gtsummary::gather_ard(tbl_pt)
    )
  )

  rmpt_tbl |>
    structure(class = c("tbl_rmpt", "gtsummary"))
}

#' @rdname tbl_rmpt
#' @export
add_overall.tbl_rmpt <- function(
  x,
  last = FALSE,
  col_label = "All Participants  \n(N = {style_roche_number(n)})",
  ...
) {
  # check inputs ---------------------------------------------------------------
  set_cli_abort_call()
  check_dots_empty(call = get_cli_abort_call())
  check_scalar_logical(last)

  if (is_empty(x$inputs$by)) {
    cli::cli_inform(
      c(
        "Original table was not stratified, and overall columns cannot be added.",
        i = "Table has been returned unaltered."
      )
    )
    return(x)
  }

  # Build overall table --------------------------------------------------------
  tbl_overall <-
    x$inputs |>
    utils::modifyList(list(by = NULL)) |>
    (\(args_list) do.call("tbl_rmpt", args = args_list))() |>
    gtsummary::modify_spanning_header(gtsummary::all_stat_cols() ~ col_label)

  # Check the tables have the same structure before merging
  if (
    !identical(
      dplyr::select(x$table_body, any_of(c("label0", "label"))),
      dplyr::select(tbl_overall$table_body, any_of(c("label0", "label")))
    )
  ) {
    cli::cli_inform(
      c(
        "!" = "The structures of the original table and the overall table are not identical,
         and the resulting table may be malformed."
      )
    )
  }

  # Merge tables ---------------------------------------------------------------
  merged_tbl <- if (isTRUE(last)) {
    gtsummary::tbl_merge(
      tbls = list(x, tbl_overall),
      tab_spanner = FALSE,
      merge_vars = c("variable", "row_type", "var_label", "label0", "label"),
      quiet = TRUE
    )
  } else {
    gtsummary::tbl_merge(
      tbls = list(tbl_overall, x),
      tab_spanner = FALSE,
      merge_vars = c("variable", "row_type", "var_label", "label0", "label"),
      quiet = TRUE
    )
  }

  # Correct ARD structure
  merged_tbl[["cards"]] <-
    list(
      tbl_rmpt = x$cards$tbl_rmpt,
      add_overall = tbl_overall$cards$tbl_rmpt
    )

  merged_tbl |>
    structure(class = class(x))
}
