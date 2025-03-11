#' Survival Quantiles
#'
#' Create a gtsummary table with Kaplan-Meier estimated survival quantiles.
#' If you must further customize the way these results are presented,
#' see the Details section below for the full details.
#'
#' @param data (`data.frame`)\cr
#'   A data frame
#' @param y (`string` or `expression`)\cr
#'   A string or expression with the survival outcome, e.g. `survival::Surv(time, status)`.
#'   The default value is
#'   `survival::Surv(time = AVAL, event = 1 - CNSR, type = "right", origin = 0)`.
#' @param by ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   A single column from `data`. Summary statistics will be stratified by this variable.
#'   Default is `NULL`, which returns results for the unstratified model.
#' @param header (`string`)\cr
#'   String for the header of the survival quantile chunks.
#'   Default is `"Time to event"`.
#' @param estimate_fun (`function`)\cr
#'   Function used to round and format the estimates in the table.
#'   Default is `label_style_number(digits = 1)`.
#' @param method.args (named `list`)\cr
#'   Named list of arguments that will be passed to `survival::survfit()`.
#'
#'   Note that this list may contain non-standard evaluation components, and
#'   must be handled similarly to tidyselect inputs by using
#'   rlang's embrace operator `{{ . }}` or `!!enquo()` when programming with this
#'   function.
#'
#' @returns a gtsummary table
#' @export
#'
#' @section ARD-first:
#'
#' This function is a helper for creating a common summary.
#' But if you need to modify the appearance of this table, you may need to build
#' it from ARDs.
#'
#' Here's the general outline for creating this table directly from ARDs.
#' 1. Create an ARD of survival quantiles using `cardx::ard_survival_survfit()`.
#' 2. Construct an ARD of the minimum and maximum survival times using `cards::ard_continuous()`.
#' 3. Combine the ARDs and build summary table with `gtsummary::tbl_ard_summary()`.
#'
#' ```r
#' # get the survival quantiles with 95% CI
#' ard_surv_quantiles <-
#'   cardx::ard_survival_survfit(
#'     x = cards::ADTTE,
#'     y = Surv_CNSR(),
#'     variables = "TRTA",
#'     probs = c(0.25, 0.50, 0.75)
#'   ) |>
#'   # modify the shape of the ARD to look like a
#'   # 'continuous' result to feed into `tbl_ard_summary()`
#'   dplyr::mutate(
#'     stat_name = paste0(.data$stat_name, 100 * unlist(.data$variable_level)),
#'     variable_level = list(NULL)
#'   )
#'
#' # get the min/max followup time
#' ard_surv_min_max <-
#'   cards::ard_continuous(
#'     data = cards::ADTTE,
#'     variables = AVAL,
#'     by = "TRTA",
#'     statistic = everything() ~ cards::continuous_summary_fns(c("min", "max"))
#'   )
#'
#' # stack the ARDs and pass them to `tbl_ard_summary()`
#' cards::bind_ard(
#'   ard_surv_quantiles,
#'   ard_surv_min_max
#' ) |>
#'   tbl_ard_summary(
#'     by = "TRTA",
#'     type = list(prob = "continuous2", AVAL = "continuous"),
#'     statistic = list(
#'       prob = c("{estimate50}", "({conf.low50}, {conf.high50})", "{estimate25}, {estimate75}"),
#'       AVAL = "{min} to {max}"
#'     ),
#'     label = list(
#'       prob = "Time to event",
#'       AVAL = "Range"
#'     )
#'   ) |>
#'   # directly modify the labels in the table to match spec
#'   modify_table_body(
#'     ~ .x |>
#'       mutate(
#'         label = dplyr::case_when(
#'           .data$label == "Survival Probability" ~ "Median",
#'           .data$label == "(CI Lower Bound, CI Upper Bound)" ~ "95% CI",
#'           .data$label == "Survival Probability, Survival Probability" ~ "25% and 75%-ile",
#'           .default = .data$label
#'         )
#'       )
#'   ) |>
#'   # update indentation to match spec
#'   modify_column_indent(columns = "label", rows = label == "95% CI", indent = 8L) |>
#'   modify_column_indent(columns = "label", rows = .data$label == "Range", indent = 4L) |>
#'   # remove default footnotes
#'   remove_footnote_header(columns = all_stat_cols())
#' ```
#'
#' @examples
#' # Example 1 ----------------------------------
#' tbl_survfit_quantiles(
#'   data = cards::ADTTE,
#'   by = "TRTA"
#' )
#'
#' # Example 2: unstratified analysis -----------
#' tbl_survfit_quantiles(data = cards::ADTTE)
tbl_survfit_quantiles <- function(data,
                                  y = "survival::Surv(time = AVAL, event = 1 - CNSR, type = 'right', origin = 0)",
                                  by = NULL,
                                  header = "Time to event",
                                  estimate_fun = label_style_number(digits = 1),
                                  method.args = list(conf.int = 0.95)) {
  method.args <- enquo(method.args)

  # check inputs ---------------------------------------------------------------
  set_cli_abort_call()
  check_not_missing(data)
  check_string(header)
  cards::process_selectors(data, by = {{ by }})
  check_class(estimate_fun, "function")
  if (length(by) > 1L) {
    cli::cli_abort(
      "The {.arg by} argument must be empty or a single stratifying variable name.",
      call = get_cli_abort_call()
    )
  }
  if ("time" %in% by) {
    cli::cli_abort(
      "The {.arg by} column cannot be named {.val time}.",
      call = get_cli_abort_call()
    )
  }
  y <- .expr_as_string({{ y }}) # convert y to string (if not already)
  func_inputs <- as.list(environment())

  # subset data on complete row ------------------------------------------------
  form <- glue("{y} ~ {ifelse(is_empty(by), 1, cardx::bt(by))}") |> stats::as.formula()
  data <- data[stats::complete.cases(data[all.vars(form)]), ]

  # get survival quantiles -----------------------------------------------------
  ard_surv_quantiles <-
    cardx::ard_survival_survfit(
      x = data,
      y = y,
      variables = any_of(by),
      probs = c(0.25, 0.50, 0.75),
      method.args = !!method.args
    ) |>
    cards::update_ard_fmt_fn(
      stat_names = c("estimate", "conf.low", "conf.high"),
      fmt_fn = estimate_fun
    )
  # get the confidence level
  conf.level <- call_args(method.args)[["conf.int"]] %||% 0.95

  # calculate range of followup times ------------------------------------------
  df_time <-
    stats::model.frame(
      formula = form,
      data = data
    ) |>
    stats::setNames(c("time", by)) |>
    dplyr::mutate(time = .data$time[, 1])

  ard_followup_range <-
    cards::ard_continuous(
      df_time,
      variables = "time",
      by = any_of(by),
      statistic = everything() ~ cards::continuous_summary_fns(c("min", "max"))
    ) |>
    cards::update_ard_fmt_fn(
      stat_names = c("min", "max"),
      fmt_fn = estimate_fun
    )

  # calculate ARD for by vars
  if (!is_empty(by)) {
    ard_by <- cards::ard_categorical(data, variables = all_of(by))
  }
  ard_n <- cards::ard_total_n(data)

  # build gtsummary table ------------------------------------------------------
  tbl_survift_quantiles <-
    dplyr::bind_rows(
      ard_surv_quantiles |>
        dplyr::mutate(
          stat_name = paste0(.data$stat_name, 100 * unlist(.data$variable_level)),
          variable_level = list(NULL)
        ),
      ard_followup_range,
      if (!is_empty(by)) ard_by,  # styler: off
      ard_n
    ) |>
    gtsummary::tbl_ard_summary(
      by = any_of(by),
      type = list(prob = "continuous2", time = "continuous"),
      statistic = list(
        prob = c("{estimate50}", "({conf.low50}, {conf.high50})", "{estimate25}, {estimate75}"),
        time = "{min} to {max}"
      ),
      label = list(
        prob = header,
        time = "Range"
      )
    ) |>
    gtsummary::modify_header(
      all_stat_cols() ~ "**{level}**  \nN = {n}",
      label = ""
    ) |>
    gtsummary::modify_table_body(
      ~ .x |>
        mutate(
          label = dplyr::case_when(
            .data$label == "Survival Probability" ~ "Median",
            .data$label == "(CI Lower Bound, CI Upper Bound)" ~ glue("{gtsummary::style_number(conf.level, scale = 100)}% CI"),
            .data$label == "Survival Probability, Survival Probability" ~ "25% and 75%-ile",
            .default = .data$label
          )
        )
    ) |>
    gtsummary::modify_column_indent(
      columns = "label",
      rows = .data$label == glue("{gtsummary::style_number(conf.level, scale = 100)}% CI"),
      indent = 8L
    ) |>
    gtsummary::modify_column_indent(
      columns = "label",
      rows = .data$label == "Range",
      indent = 4L
    ) |>
    gtsummary::remove_footnote_header(columns = all_stat_cols())

  # return tbl -----------------------------------------------------------------
  tbl_survift_quantiles$cards <-
    dplyr::bind_rows(
      ard_surv_quantiles,
      ard_followup_range,
      if (!is_empty(by)) ard_by,  # styler: off
      ard_n
    )
  tbl_survift_quantiles$inputs <- func_inputs
  tbl_survift_quantiles
}

.expr_as_string <- function(x) {
  x <- enquo(x)
  # if a character was passed, return it as is
  if (tryCatch(is.character(eval_tidy(x)), error = \(e) FALSE)) x <- eval_tidy(x) # styler: off
  # otherwise, convert expr to string
  else x <- expr_deparse(quo_get_expr(x))  # styler: off
  x
}
