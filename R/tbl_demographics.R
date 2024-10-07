#' Demographics Table
#'
#' @description
#' This is a thin wrapper of `gtsummary::tbl_summary()`.
#' The function is meant for `'continuous2'` and `'categorical'` summary types only.
#'
#' - Continuous variables default to type `'continuous2'`.
#' - The number of non-missing is placed on the first row for all summaries.
#' - The default formatting function for percentages is `gtsummary::label_style_number(digits = 1, scale = 100)`.
#' - The `gtsummary::add_stat_label()` is run automatically after the function's execution.
#'
#' @inheritParams gtsummary::tbl_summary
#'
#' @return a 'gtsummary' table
#' @export
#'
#' @examples
#' theme_gtsummary_roche()
#' tbl <-
#'   trial |>
#'   tbl_demographics(
#'     by = trt,
#'     include = c(age, grade)
#'   )
#'
#' tbl
#'
#' # extract ARD from table
#' gather_ard(tbl)
#'
#' reset_gtsummary_theme()
tbl_demographics <- function(data,
                             by = NULL,
                             label = NULL,
                             statistic =
                               list(all_continuous() ~ c("{mean} ({sd})", "{median} ({p25}, {p75})", "{min}, {max}"),
                                    all_categorical() ~ "{n} ({p}%)"),
                             digits = NULL,
                             type = NULL,
                             value = NULL,
                             sort = all_categorical(FALSE) ~ "alphanumeric",
                             percent = c("column", "row", "cell"),
                             include = everything()) {
  percent <- match.arg(percent)

  # execute `tbl_summary()` code with theme/defaults ---------------------------
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
        missing = "always",
        missing_text = "n",
        missing_stat = "{N_nonmiss}",
        sort = sort,
        percent = percent,
        include = {{ include }}
      ),
    msg_ignored_elements =
      paste("Theme element(s) {.val {elements}} utilized internally",
            "by {.code tbl_demographics()} and cannot be modified.\n",
            "Use {.code gtsummary::tbl_summary()} if you",
            "wish to modify these theme elements.")
  ) |>
    gtsummary::modify_table_body(
      function(.x) .x |>
        dplyr::mutate(
          .by = "variable",
          dplyr::across(dplyr::everything(), ~.[order(row_type != "label", row_type != "missing")])
        )
    ) |>
    gtsummary::add_stat_label() |>
    structure(class = c("tbl_demographics", "tbl_summary", "gtsummary"))
}

# creating theme for gtreg summaries -------------------------------------------
tbl_demographics_theme <-
  list(
    "tbl_summary-str:default_con_type" = "continuous2",
    "tbl_summary-fn:percent_fun" = gtsummary::label_style_number(digits = 1, scale = 100)
  )
