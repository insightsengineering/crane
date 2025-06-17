#' Change from Baseline
#'
#' @description Computes Change from Baseline
#' @param digits ([`formula-list-selector`][gtsummary::syntax])\cr
#'  Specifies how summary statistics are rounded. Values may be either integer(s) or function(s). If not specified,
#'  default formatting is assigned via `label_style_number()` for the `n` statistic and
#'  `label_style_number(digits=1, scale=100)` for the `p` statistic.
#' @returns a gtsummary table
#' @name tbl_baseline_chg
#'
#' @examples
#' # Example 1 ----------------------------------
NULL
#' @export
tbl_baseline_chg <- function(data,
                             variables,
                             denominator,
                             by = NULL,
                             id = "USUBJID",
                             visit = "VISIT",
                             visit_seq = "VISITNUM",
                             test_code = "LBTESTCD",
                             analysis_date = "ADT",
                             test_subset = "ALB",
                             digits = everything() ~ list(
                               n = label_style_number(),
                               p = label_style_number(digits = 1, scale = 100))) {
  # check inputs ---------------------------------------------------------------
  set_cli_abort_call()
  check_not_missing(data)
  check_not_missing(variables)
  check_not_missing(denominator)
  check_data_frame(data)
  cards::process_selectors(
    data,
    variables = {{ variables }},
    by = {{ by }},
    id = {{ id }}
  )
  check_scalar(by, allow_empty = TRUE)
  check_scalar(id)

  # Reshaping data frame to one line per USUBJID per LBTEST
  df <-
    # filter lab results data
    data |>
    # keeping the first LAB within USUBJID and VISIT and removing LAB tests not included in summary
    arrange(id, visit, analysis_date) |>
    filter(
      .by = c(id, visit),
      .data[[test_code]] == test_subset, row_number() == 1L, grepl("SCREENING|WEEK", .data[[visit]])
    ) |>
    dplyr::mutate(
      VISIT_fct = fct_reorder(.data[[visit]], .data[[visit_seq]])
    ) |>
    tidyr::pivot_wider(
      id_cols = c(id, test_code,  any_of(by)),
      names_from = VISIT_fct,
      values_from = c("AVAL", "CHG"),
      names_sort = TRUE
    ) |>
    # add in ADSL for the header Ns
    right_join(
      select(denominator, c(id, any_of(by))),
      relationship = "many-to-one"
    )

  # Build lab results tables ----------------------------------------------------
  # Summary of AVAL for this lab test
  tbl_aval <-
    df |>
    select(any_of(by), starts_with("AVAL_")) |>
    rename_with(~ str_remove(., "^AVAL_")) %>%
    # after reshape all column labels are the same, so changing them to the variable name
    labelled::remove_var_label() |>
    tbl_demographics(
      by = any_of(by),
      # round mean/sd/median/min/max
      digits = list(
        all_continuous() ~ list(
          mean = 2, sd = 2, median = 2,
          min = 1, max = 1
        )
      )
    )

  # Building a table change values at each visit
  tbl_chg <-
    df |>
    select(any_of(by), starts_with("CHG_")) |>
    rename_with(~ str_remove(., "^CHG_")) %>%
    # after reshape all column labels are the same, so changing them to the variable name
    labelled::remove_var_label() |>
    # using `tbl_demographics()` as the default continuous variable summary matches our spec
    tbl_demographics(
      by = any_of(by),
      # round mean/sd/median/min/max
      digits =
        list(
          all_continuous() ~ list(
            mean = 2, sd = 2, median = 2,
            min = 1, max = 1
          )
        ),
      include = -"SCREENING 1" # Remove the baseline visit from summary
    )

  # combine into single table ---------------------------------
  tbl_final <-
    list(tbl_aval, tbl_chg) |>
    tbl_merge(tab_spanner = FALSE) |>
    # add spanning header with treatment labels
    modify_spanning_header(all_stat_cols() ~ "{level}  \n(N = {n})") |>
    modify_header(
      all_stat_cols() & ends_with("_1") ~ "Value at Visit", # after the merge, values from the first table end with `_1`
      all_stat_cols() & ends_with("_2") ~ "Change from Baseline", # after the merge, values from the first table end with `_2`
      label = ""
    ) |>
    # sort the stat columns together within treatment group
    modify_table_body(
      \(.x) {
        stat_cols <- select(.x, all_stat_cols()) |>
          names() |>
          sort()
        dplyr::relocate(.x, all_of(stat_cols), .after = "label")
      }
    )


  tbl_final |>
    structure(class = c("tbl_baseline_chg", "gtsummary"))
}
