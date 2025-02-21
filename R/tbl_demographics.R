#' Demographics Table
#'
#' @description
#' This is a thin wrapper of [`gtsummary::tbl_summary()`] with the following differences:
#' - Default summary type for continuous variables is `'continuous2'`.
#' - Number of non-missing observations is added for each variable by default
#'   and placed on the row under the header.
#' - The `tbl_summary(missing*)` arguments have been renamed to
#'   `tbl_demographics(nonmissing*)` with updated default values.
#' - The default footnotes from `tbl_summary()` are removed.
#'
#' @inheritParams gtsummary::tbl_summary
#' @param nonmissing,nonmissing_text,nonmissing_stat
#'   Arguments dictating how and if missing values are presented:
#'   - `nonmissing`: must be one of `c("always", "ifany", "no")`
#'   - `nonmissing_text`: string indicating text shown on non-missing row. Default is `"n"`
#'   - `nonmissing_stat`: statistic to show on non-missing row. Default is `"{N_nonmiss}"`.
#'     Possible values are `N_nonmiss`, `N_miss`, `N_obs`,, `p_nonmiss` `p_miss`.
#'
#' @return a 'gtsummary' table
#' @export
#'
#' @examples
#' # Example 1 ----------------------------------
#' gtsummary::trial |>
#'   tbl_demographics(
#'     by = trt,
#'     include = c(age, grade)
#'   )
tbl_demographics <- function(data,
                             by = NULL,
                             label = NULL,
                             statistic =
                               list(
                                 all_continuous() ~ c("{mean} ({sd})", "{median}", "{min} - {max}"),
                                 all_categorical() ~ "{n} ({p}%)"
                               ),
                             digits = NULL,
                             type = NULL,
                             value = NULL,
                             nonmissing = c("always", "ifany", "no"),
                             nonmissing_text = "n",
                             nonmissing_stat = "{N_nonmiss}",
                             sort = all_categorical(FALSE) ~ "alphanumeric",
                             percent = c("column", "row", "cell"),
                             include = everything()) {
  set_cli_abort_call()
  percent <- arg_match(percent, error_call = get_cli_abort_call())
  nonmissing <- arg_match(nonmissing, error_call = get_cli_abort_call())

  # execute `tbl_summary()` code with theme/defaults ---------------------------
  x <-
    gtsummary::with_gtsummary_theme(
      x = tbl_demographics_theme,
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
          "by {.code tbl_demographics()} and cannot be modified.\n",
          "Use {.code gtsummary::tbl_summary()} if you",
          "wish to modify these theme elements."
        )
    ) |>
    # remove default footnote
    gtsummary::remove_footnote_header(columns = everything()) |>
    # sort the missing row to just below the header row
    gtsummary::modify_table_body(
      function(.x) {
        .x |>
          dplyr::mutate(
            .by = "variable",
            dplyr::across(dplyr::everything(), ~ .[order(row_type != "label", row_type != "missing")])
          )
      }
    ) |>
    # assign class
    structure(class = c("tbl_demographics", "tbl_summary", "gtsummary"))

  # return table ---------------------------------------------------------------
  x$call_list <- list(tbl_demographics = match.call())
  x
}

# creating theme for tbl_demographic summaries ---------------------------------
tbl_demographics_theme <- list("tbl_summary-str:default_con_type" = "continuous2")
