#' Change from Baseline
#'
#' Typical use is tabulating changes from baseline
#' measurement of an Analysis Variable.
#' @inheritParams tbl_roche_summary
#' @inheritParams gtsummary::add_overall
#' @param analysis_variable (`string`)\cr
#'  String identifying the analysis values. Default is `AVAL`.
#' @param change_variable (`string`)\cr
#'  String identifying the change from baseline values. Default is `CHG`.
#' @param id (`string`)\cr
#'  String identifying the unique subjects. Default is `USUBJID`.
#' @param test_variable (`string`)\cr
#'  String identifying the column containing lab test codes.
#' @param test_cd (`string`)\cr
#'  String identifying the lab test code to compute the change from baseline. This must be a value contained in `test_variable`.
#' @param baseline_level (`string`)\cr
#'  String identifying baseline level in the `visit` variable.
#' @param header_label (`string`)\cr
#'  String identifying the column header.
#' @param denominator (`string`)\cr
#'  Data set used to compute the header counts (typically `ADSL`).
#' @param visit (`string`)\cr
#'  String for the visit variable. Default is
#'  `VISIT`.
#' @param analysis_date (`string`)\cr
#'  String identifying the visit or analysis sequence number. Default is
#'  `VISITNUM`.
#' @return a gtsummary table
#' @name tbl_baseline_chg
#'
#' @examples
#' theme_gtsummary_roche()
#' library(dplyr, warn.conflicts = FALSE)
#' df <- cards::ADLB
#' df <- df[!grepl("unscheduled", df$VISIT, ignore.case = TRUE), ]
#'
#' tbl_baseline_chg(
#'   data = df,
#'   test_variable = "PARAMCD",
#'   test_cd = "SODIUM",
#'   baseline_level = "SCREENING 1",
#'   by = "TRTA",
#'   denominator = cards::ADSL
#' )
NULL

#' @rdname tbl_baseline_chg
#' @export
tbl_baseline_chg <- function(data,
                             analysis_variable = "AVAL",
                             change_variable = "CHG",
                             by = NULL,
                             id = "USUBJID",
                             visit = "VISIT",
                             test_variable,
                             test_cd,
                             analysis_date = "VISITNUM",
                             header_label = "{level}  \n(N = {n})",
                             baseline_level,
                             denominator) {
  set_cli_abort_call()

  # check inputs ---------------------------------------------------------------
  check_not_missing(data)
  check_not_missing(analysis_variable)
  check_not_missing(change_variable)
  check_not_missing(test_variable)
  check_not_missing(test_cd)
  check_not_missing(baseline_level)
  check_not_missing(denominator)

  # ---- Type and content checks ----
  check_data_frame(data)
  check_data_frame(denominator)
  check_string(analysis_variable)
  check_string(change_variable)
  check_string(id)
  check_string(visit)
  check_string(test_variable)
  check_string(test_cd)
  check_string(analysis_date)
  check_string(header_label)
  check_scalar(baseline_level, message = "The {.arg baseline_level} must be a scalar (single value).")

  # Allow `by` to be NULL or a string
  check_scalar(by, allow_empty = TRUE, message = "The {.arg by} argument must select exactly one variable or none.")

  # Check that `by` exists in data if not NULL
  if (!is.null(by) && !by %in% names(data)) {
    cli::cli_abort("The variable {.val {by}} specified in {.arg by} is not found in {.arg data}.")
  }

  # Check that `baseline_level` is one of the visit values
  if (!is.null(baseline_level) && !(baseline_level %in% data[[visit]])) {
    cli::cli_warn("The {.arg baseline_level} {.val {baseline_level}} is not found in the {.val {visit}} variable.")
  }
  cards::process_selectors(data, visit = {{ visit }}, test_variable = {{ test_variable }}, analysis_variable = {{ analysis_variable }}, change_variable = {{ change_variable }}, by = {{ by }}, analysis_date = {{ analysis_date }})
  tbl_baseline_inputs <- as.list(environment())

  # build summary table -----------------------------------------------------
  # if there is a `by` variable, make it a factor to ensure all levels appear in tbls
  if (!is_empty(by) && !is.factor(data[[by]])) {
    cli::cli_inform(c("i" = "Converting column {.val {by}} to a factor."))
    old_by_label <- attr(data[[by]], "label")
    data[[by]] <- factor(data[[by]])
    attr(data[[by]], "label") <- old_by_label
  }

  df_change_baseline <-
    # filter lab results data
    data |>
    dplyr::arrange(id, visit, analysis_date) |>
    dplyr::filter(
      .by = c(id, visit),
      .data[[test_variable]] == test_cd,
      dplyr::row_number() == 1L
    ) |>
    dplyr::mutate(
      visit = fct_reorder(.data[[visit]], .data[[analysis_date]])
    ) |>
    tidyr::pivot_wider(
      id_cols = c(id, test_variable, by),
      names_from = visit,
      values_from = c(analysis_variable, change_variable),
      names_sort = TRUE
    ) |>
    # add in denominator for the header Ns
    dplyr::right_join(
      gtsummary::select(denominator, c(id, by)),
      relationship = "many-to-one"
    )

  # Build results tables ----------------------------------------------------
  # Summary of AVAL
  tbl_aval <-
    df_change_baseline |>
    dplyr::select(by, starts_with(analysis_variable)) |>
    dplyr::rename_with(~ str_remove(., paste0("^", analysis_variable, "_"))) %>%
    # after reshape all column labels are the same, so changing them to the variable name
    labelled::remove_var_label() |>
    tbl_roche_summary(
      by = by,
      nonmissing = "always", # include the non-missing count in summary
      # round mean/sd/median/min/max,
      type = everything() ~ "continuous2",
      digits = list(
        all_continuous() ~ list(
          mean = 2, sd = 2, median = 2,
          min = 1, max = 1
        )
      )
    )

  # Building a table change values at each visit
  tbl_chg <-
    df_change_baseline |>
    dplyr::select(by, starts_with(change_variable)) |>
    dplyr::rename_with(~ str_remove(., paste0("^", change_variable, "_"))) %>%
    # after reshape all column labels are the same, so changing them to the variable name
    labelled::remove_var_label() |>
    # using `tbl_roche_summary()` as the default continuous variable summary matches our spec
    tbl_roche_summary(
      by = by,
      nonmissing = "always", # include the non-missing count in summary
      # round mean/sd/median/min/max
      type = everything() ~ "continuous2",
      digits =
        list(
          all_continuous() ~ list(
            mean = 2, sd = 2, median = 2,
            min = 1, max = 1
          )
        ),
      include = -baseline_level # Remove the baseline visit from summary
    )

  # Merge tables together
  baseline_chg_tbl <-
    list(tbl_aval, tbl_chg) |>
    gtsummary::tbl_merge(tab_spanner = FALSE) |>
    gtsummary::modify_spanning_header(gtsummary::all_stat_cols() ~ header_label) |>
    gtsummary::modify_header(
      gtsummary::all_stat_cols() & ends_with("_1") ~ "Value at Visit",
      gtsummary::all_stat_cols() & ends_with("_2") ~ "Change from Baseline",
      label = "Visit"
    ) |>
    # sort the stat columns together within treatment group
    gtsummary::modify_table_body(
      \(.x) {
        stat_cols <- dplyr::select(.x, gtsummary::all_stat_cols()) |>
          names() |>
          sort()
        dplyr::relocate(.x, all_of(stat_cols), .after = "label")
      }
    )

  # return tbl -----------------------------------------------------------------
  baseline_chg_tbl[["call_list"]] <- list(tbl_baseline_chg = match.call())
  baseline_chg_tbl$inputs <- tbl_baseline_inputs
  baseline_chg_tbl |>
    structure(class = c("tbl_baseline_chg", "gtsummary"))
}

#' @rdname tbl_baseline_chg
#' @export
add_overall.tbl_baseline_chg <- function(x,
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
    utils::modifyList(list(header_label = col_label)) |>
    (\(args_list) do.call("tbl_baseline_chg", args = args_list))()

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
  merged_tbl <- if (isTRUE(last)) {
    gtsummary::tbl_merge(
      tbls = list(x, tbl_overall),
      tab_spanner = FALSE,
      merge_vars = c("variable", "row_type", "var_label", "label0", "label")
    )
  } else {
    gtsummary::tbl_merge(
      tbls = list(tbl_overall, x),
      tab_spanner = FALSE,
      merge_vars = c("variable", "row_type", "var_label", "label0", "label")
    )
  }

  return(merged_tbl)
}
