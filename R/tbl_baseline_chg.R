#' Change from Baseline
#'
#' @description Computes Change from Baseline
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
#' @param denominator (`string`)\cr
#'  Data set used to compute the header counts (typically `ADSL`).
#' @param digits ([`formula-list-selector`][gtsummary::syntax])\cr
#'  Specifies how summary statistics are rounded. Values may be either integer(s) or function(s). If not specified,
#'  default formatting is assigned via `label_style_number()` for the `n` statistic and
#'  `label_style_number(digits=1, scale=100)` for the `p` statistic.
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
#' ----------------------------------
#' adlb <- cards::ADLB
#' adsl <- cards::ADSL
#' df <- adlb |>
#'   dplyr::filter(!str_detect(VISIT, regex("unscheduled", ignore_case = TRUE)))
#'
#' tbl_baseline_chg(
#'   data = df,
#'   test_variable = "PARAMCD",
#'   test_cd = "SODIUM",
#'   baseline_level = "SCREENING 1",
#'   by = "TRTA",
#'   denominator = adsl
#' )
NULL

#' @rdname tbl_baseline_chg
#' @export
tbl_baseline_chg <- function(data,
                             analysis_variable = AVAL,
                             change_variable = CHG,
                             by = NULL,
                             id = USUBJID,
                             visit = VISIT,
                             test_variable,
                             test_cd,
                             analysis_date = VISITNUM,
                             baseline_level,
                             denominator,
                             header = "{level}  \nN = {n}",
                             nonmissing = "always",
                             nonmissing_text = "Total",
                             ...) {
  set_cli_abort_call()

  # check inputs ---------------------------------------------------------------
  check_not_missing(data)
  check_not_missing(analysis_variable)
  check_not_missing(change_variable)
  check_data_frame(data)
  cards::process_selectors(data,
    analysis_variable = {{ analysis_variable }}, by = {{ by }},
    change_variable = {{ change_variable }}, visit = {{ visit }}, analysis_date = {{ analysis_date }},
    id = {{ id }}, test_variable = {{ test_variable }}
  )
  check_scalar(analysis_variable, message = "The {.arg analysis_variable} argument must select exactly one variable.")
  check_scalar(change_variable, message = "The {.arg change_variable} argument must select exactly one variable.")
  check_scalar(by, allow_empty = TRUE, message = "The {.arg by} argument must select exactly one variable or none.")

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
      VISIT_fct = forcats::fct_reorder(.data[[visit]], .data[[analysis_date]])
    ) |>
    tidyr::pivot_wider(
      id_cols = c(id, test_variable, by),
      names_from = VISIT_fct,
      values_from = c(analysis_variable, change_variable),
      names_sort = TRUE
    ) |>
    # add in denominator for the header Ns
    suppressMessages(dplyr::right_join(
      dplyr::select(denominator, c(id, by)),
      relationship = "many-to-one"
    ))

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
    gtsummary::modify_spanning_header(all_stat_cols() ~ "{level}  \n(N = {n})") |>
    gtsummary::modify_header(
      all_stat_cols() & ends_with("_1") ~ "Value at Visit",
      all_stat_cols() & ends_with("_2") ~ "Change from Baseline",
      label = ""
    ) |>
    # sort the stat columns together within treatment group
    gtsummary::modify_table_body(
      \(.x) {
        stat_cols <- dplyr::select(.x, all_stat_cols()) |>
          names() |>
          sort()
        dplyr::relocate(.x, all_of(stat_cols), .after = "label")
      }
    )

  baseline_chg_tbl |>
    structure(class = c("tbl_baseline_chg", "gtsummary"))
}
