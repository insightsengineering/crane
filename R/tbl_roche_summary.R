#' Roche Summary Table
#'
#' @description
#' This is a thin wrapper of [`gtsummary::tbl_summary()`] with the following differences:
#' - Default summary type for continuous variables is `'continuous2'`.
#' - Number of non-missing observations, when requested, is added for each variable
#'   and placed on the row under the variable label/header.
#' - The `tbl_summary(missing*)` arguments have been renamed to
#'   `tbl_roche_summary(nonmissing*)` with updated default values.
#' - The default footnotes from `tbl_summary()` are removed.
#' - Cells with `"0 (0.0%)"` are converted to `"0"` with `gtsummary::modify_post_fmt_fun()`.
#'
#' @inheritParams gtsummary::tbl_summary
#' @param nonmissing,nonmissing_text,nonmissing_stat
#'   Arguments dictating how and if missing values are presented:
#'   - `nonmissing`: must be one of `c("always", "ifany", "no")`
#'   - `nonmissing_text`: string indicating text shown on non-missing row. Default is `"n"`
#'   - `nonmissing_stat`: statistic to show on non-missing row. Default is `"{N_nonmiss}"`.
#'     Possible values are `N_nonmiss`, `N_miss`, `N_obs`, `p_nonmiss` `p_miss`.
#'
#' @return a 'gtsummary' table
#' @export
#'
#' @examples
#' # Example 1 ----------------------------------
#' trial |>
#'   tbl_roche_summary(
#'     by = trt,
#'     include = c(age, grade),
#'     nonmissing = "always"
#'   ) |>
#'   add_overall()
tbl_roche_summary <- function(data,
                              by = NULL,
                              label = NULL,
                              statistic =
                                list(
                                  gtsummary::all_continuous() ~ c("{mean} ({sd})", "{median}", "{min} - {max}"),
                                  gtsummary::all_categorical() ~ "{n} ({p}%)"
                                ),
                              digits = NULL,
                              type = NULL,
                              value = NULL,
                              nonmissing = c("no", "always", "ifany"),
                              nonmissing_text = "n",
                              nonmissing_stat = "{N_nonmiss}",
                              sort = gtsummary::all_categorical(FALSE) ~ "alphanumeric",
                              percent = c("column", "row", "cell"),
                              include = everything()) {
  set_cli_abort_call()
  if (is.character(percent)) percent <- arg_match(percent, error_call = get_cli_abort_call())
  nonmissing <- arg_match(nonmissing, error_call = get_cli_abort_call())

  # execute `tbl_summary()` code with theme/defaults ---------------------------
  x <-
    gtsummary::with_gtsummary_theme(
      x = tbl_roche_summary_theme,
      expr =
        gtsummary::tbl_summary(
          data = data,
          by = {{ by }},
          label = label,
          statistic = statistic,
          digits = digits,
          type = type,
          value = value,
          missing = nonmissing,
          missing_text = nonmissing_text,
          missing_stat = nonmissing_stat,
          sort = sort,
          percent = percent,
          include = {{ include }}
        ),
      msg_ignored_elements =
        paste(
          "Theme element(s) {.val {elements}} utilized internally",
          "by {.code tbl_roche_summary()} and cannot be modified.\n",
          "Use {.code gtsummary::tbl_summary()} if you",
          "wish to modify these theme elements."
        )
    ) |>
    # remove default footnote
    gtsummary::remove_footnote_header(columns = everything()) |>
    # remove the default "Characteristic" header, remove bold from stat headers
    gtsummary::modify_header(
      label = "",
      gtsummary::all_stat_cols() ~ "{level}  \n(N = {n})"
    ) |>
    # convert "0 (0.0%)" to "0"
    modify_zero_recode() |>
    # sort the missing row to just below the header row
    gtsummary::modify_table_body(
      function(.x) {
        .x |>
          # add column to retain variable order (arrange() forces alphanumeric sorting)
          dplyr::mutate(
            variable_order = match(variable, unique(variable))
          ) |>
          dplyr::arrange(
            variable_order,
            # `desc()` makes TRUE (label) come before FALSE
            desc(row_type == "label"),
            row_type != "missing"
          ) |>
          dplyr::select(-variable_order) # clean up
      }
    ) |>
    # assign class
    structure(class = c("tbl_roche_summary", "tbl_summary", "gtsummary"))

  # return table ---------------------------------------------------------------
  x$inputs$missing <- NULL
  x$inputs$missing_stat <- NULL
  x$inputs$missing_text <- NULL

  x$inputs <- x$inputs |>
    append(
      list(
        nonmissing = nonmissing,
        nonmissing_stat = nonmissing_stat,
        nonmissing_text = nonmissing_text
      )
    )
  x$call_list <- list(tbl_roche_summary = match.call())
  x
}

# creating theme for tbl_roche_summary summaries -------------------------------
tbl_roche_summary_theme <- list("tbl_summary-str:default_con_type" = "continuous2")
