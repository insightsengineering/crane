#' Survival Quantiles
#'
#' Create a gtsummary table with Kaplan-Meier estimated survival quantiles.
#' If you must further customize the way these results are presented,
#' see the Details section below for the full details.
#'
#' @inheritParams gtsummary::add_overall.tbl_summary
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
#'   Default is `label_roche_number(digits = 1)`.
#' @param method.args (named `list`)\cr
#'   Named list of arguments that will be passed to `survival::survfit()`.
#'
#'   Note that this list may contain non-standard evaluation components, and
#'   must be handled similarly to tidyselect inputs by using
#'   rlang's embrace operator `{{ . }}` or `!!enquo()` when programming with this
#'   function.
#' @param x (`tbl_survfit_quantiles`)\cr
#'   A stratified 'tbl_survfit_quantiles' object.
#'
#' @returns a gtsummary table
#' @name tbl_survfit_quantiles
#'
#' @section ARD-first:
#'
#' This function is a helper for creating a common summary.
#' But if you need to modify the appearance of this table, you may need to build
#' it from ARDs.
#'
#' Here's the general outline for creating this table directly from ARDs.
#' 1. Create an ARD of survival quantiles using `cardx::ard_survival_survfit()`.
#' 2. Construct an ARD of the minimum and maximum survival times using `cards::ard_mvsummary()`.
#' 3. Combine the ARDs and build summary table with `gtsummary::tbl_ard_summary()`.
#'
#' ```r
#' # get the survival quantiles with 95% CI
#' ard_surv_quantiles <-
#'   cardx::ard_survival_survfit(
#'     x = cards::ADTTE,
#'     y = survival::Surv(time = AVAL, event = 1 - CNSR, type = 'right', origin = 0),
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
#' # get the min/max followup time with censor flags
#' df_time <- cards::ADTTE |>
#'   dplyr::mutate(time = AVAL, status = 1 - CNSR)
#'
#' # 1. Create a simple helper to check if an extreme time is censored
#' get_cens_flag <- function(data, val) {
#'   is_cens <- all(data$status[data$time == val] == 0, na.rm = TRUE)
#'   if (isTRUE(is_cens)) "*" else ""
#' }
#'
#' # 2. Get the min/max followup time with censor flags
#' df_time <- cards::ADTTE |>
#'   dplyr::mutate(time = AVAL, status = 1 - CNSR)
#'
#' ard_surv_min_max <-
#'   cards::ard_mvsummary(
#'     df_time,
#'     variables = "time",
#'     by = "TRTA",
#'     statistic = list(
#'       time = list(
#'         min = function(x, data, ...) min(data$time, na.rm = TRUE),
#'         max = function(x, data, ...) max(data$time, na.rm = TRUE),
#'         min_cens = function(x, data, ...) get_cens_flag(data, min(data$time, na.rm = TRUE)),
#'         max_cens = function(x, data, ...) get_cens_flag(data, max(data$time, na.rm = TRUE))
#'       )
#'     )
#'   )
#'
#' # 3. Stack the ARDs and format the table
#' cards::bind_ard(
#'   ard_surv_quantiles,
#'   ard_surv_min_max
#' ) |>
#'   tbl_ard_summary(
#'     by = "TRTA",
#'     type = list(prob = "continuous2", time = "continuous"),
#'     statistic = list(
#'       prob = c("{estimate50}", "({conf.low50}, {conf.high50})", "{estimate25}, {estimate75}"),
#'       time = "{min}{min_cens} to {max}{max_cens}"
#'     ),
#'     label = list(
#'       prob = "Time to event",
#'       time = "Range"
#'     )
#'   ) |>
#'   # cleanly rename the default ARD statistics
#'   modify_table_body(
#'     ~ .x |>
#'       dplyr::mutate(
#'         label = dplyr::case_match(
#'           label,
#'           "Survival Probability" ~ "Median",
#'           "(CI Lower Bound, CI Upper Bound)" ~ "95% CI",
#'           "Survival Probability, Survival Probability" ~ "25% and 75%-ile",
#'           .default = label
#'         )
#'       )
#'   ) |>
#'   # update indentation to match spec
#'   modify_indent(columns = "label", rows = label == "95% CI", indent = 8L) |>
#'   modify_indent(columns = "label", rows = label == "Range", indent = 4L) |>
#'   remove_footnote_header(columns = all_stat_cols())
#' ```
#'
#' @examples
#' # Example 1 ----------------------------------
#' tbl_survfit_quantiles(
#'   data = cards::ADTTE,
#'   by = "TRTA",
#'   estimate_fun = label_roche_number(digits = 1, na = "NE")
#' ) |>
#'   add_overall(last = TRUE, col_label = "**All Participants**  \nN = {n}")
#'
#' # Example 2: unstratified analysis -----------
#' tbl_survfit_quantiles(data = cards::ADTTE)
NULL

#' @export
#' @rdname tbl_survfit_quantiles
tbl_survfit_quantiles <- function(data,
                                  y = "survival::Surv(time = AVAL, event = 1 - CNSR, type = 'right', origin = 0)",
                                  by = NULL,
                                  header = "Time to event",
                                  estimate_fun = label_roche_number(digits = 1, na = "NE"),
                                  method.args = list(conf.int = 0.95, conf.type = "plain")) {
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
    cards::update_ard_fmt_fun(
      stat_names = c("estimate", "conf.low", "conf.high"),
      fmt_fun = estimate_fun
    )

  # calculate range of followup times ------------------------------------------
  df_time <-
    stats::model.frame(
      formula = form,
      data = data
    ) |>
    stats::setNames(c("surv_obj", by)) |>
    dplyr::mutate(
      time = .data$surv_obj[, 1],
      status = .data$surv_obj[, 2]
    )

  ard_followup_range <-
    cards::ard_mvsummary(
      df_time,
      variables = "time",
      by = any_of(by),
      statistic = list(
        time = list(
          min = function(x, data, ...) min(data$time, na.rm = TRUE),
          max = function(x, data, ...) max(data$time, na.rm = TRUE),
          min_cens = function(x, data, ...) .get_censoring_flag(data, "min"),
          max_cens = function(x, data, ...) .get_censoring_flag(data, "max")
        )
      )
    ) |>
    cards::update_ard_fmt_fun(
      stat_names = c("min", "max"),
      fmt_fun = estimate_fun
    ) |>
    cards::update_ard_fmt_fun(
      stat_names = c("min_cens", "max_cens"),
      fmt_fun = as.character
    )

  # calculate ARD for by vars
  if (!is_empty(by)) {
    ard_by <- cards::ard_tabulate(data, variables = all_of(by))
  }
  ard_n <- cards::ard_total_n(data)

  # get the confidence level
  conf.level <-
    ard_surv_quantiles |>
    dplyr::filter(.data$stat_name == "conf.level") |>
    dplyr::pull("stat") |>
    unlist()

  # build gtsummary table ------------------------------------------------------
  res <-
    dplyr::bind_rows(
      ard_surv_quantiles |>
        # remove model-wide stats
        dplyr::filter(.data$variable == "prob") |>
        dplyr::mutate(
          stat_name = paste0(.data$stat_name, 100 * unlist(.data$variable_level)),
          variable_level = list(NULL)
        ),
      ard_followup_range,
      case_switch(!is_empty(by) ~ ard_by),
      ard_n
    ) |>
    gtsummary::tbl_ard_summary(
      by = any_of(by),
      type = list(prob = "continuous2", time = "continuous"),
      statistic = list(
        prob = c("{estimate50}", "({conf.low50}, {conf.high50})", "{estimate25}, {estimate75}"),
        time = "{min}{min_cens} to {max}{max_cens}"
      ),
      label = list(
        prob = header,
        time = "Range"
      )
    ) |>
    gtsummary::modify_header(
      gtsummary::all_stat_cols() ~ "{level}  \n(N = {n})",
      label = ""
    ) |>
    gtsummary::modify_table_body(
      ~ .x |>
        dplyr::mutate(
          label = dplyr::case_when(
            .data$label == "Survival Probability" ~ "Median",
            .data$label == "(CI Lower Bound, CI Upper Bound)" ~ glue("{style_roche_number(conf.level, scale = 100)}% CI"),
            .data$label == "Survival Probability, Survival Probability" ~ "25% and 75%-ile",
            .default = .data$label
          )
        )
    ) |>
    gtsummary::modify_indent(
      columns = "label",
      rows = .data$label == glue("{style_roche_number(conf.level, scale = 100)}% CI"),
      indent = 8L
    ) |>
    gtsummary::modify_indent(
      columns = "label",
      rows = .data$label == "Range",
      indent = 4L
    ) |>
    gtsummary::remove_footnote_header(columns = gtsummary::all_stat_cols())

  # return tbl -----------------------------------------------------------------
  res$cards <-
    list(
      tbl_survfit_quantiles =
        dplyr::bind_rows(
          ard_surv_quantiles,
          ard_followup_range,
          if (!is_empty(by)) ard_by,  # styler: off
          ard_n
        )
    )

  res$inputs <- func_inputs
  res[["call_list"]] <- list(tbl_survfit_quantiles = match.call())
  res |>
    structure(class = c("tbl_survfit_quantiles", "gtsummary"))
}

#' Check for Censoring at Extreme Times
#'
#' @description
#' Helper function to evaluate whether all observations at a given extreme time
#' point (minimum or maximum) are censored. This flag is used to append
#' asterisks to survival quantiles in summary tables.
#'
#' @param data (`data.frame`)\cr
#'   A data frame containing the survival data with columns `time` and `status`.
#' @param type (`character`)\cr
#'   A string indicating which extreme to evaluate, either `"min"` or `"max"`.
#'
#' @return A character string. Returns `"*"` if all events at the evaluated time
#'   are censored, and `""` otherwise.
#' @noRd
.get_censoring_flag <- function(data, type = c("min", "max")) {
  type <- match.arg(type)

  # Use dynamic function matching to prevent repeating the subsetting logic
  # for both boundaries
  fun <- match.fun(type)
  val <- fun(data$time, na.rm = TRUE)

  # Evaluate true censoring by ensuring all records tied at the boundary time
  # strictly have an event status of 0
  is_cens <- all(
    data$status[data$time == val & !is.na(data$time)] == 0,
    na.rm = TRUE
  )

  if (isTRUE(is_cens)) "*" else ""
}

#' @export
#' @rdname tbl_survfit_quantiles
add_overall.tbl_survfit_quantiles <- function(x,
                                              last = FALSE,
                                              col_label = "All Participants  \nN = {style_roche_number(N)}",
                                              ...) {
  set_cli_abort_call()
  rlang::check_dots_empty(call = get_cli_abort_call())

  do.call(
    what = getNamespace("gtsummary")[["add_overall.tbl_summary"]],
    args = list(x = x, last = last, col_label = col_label)
  )
}

.expr_as_string <- function(x) {
  x <- enquo(x)
  # if a character was passed, return it as is
  if (tryCatch(is.character(eval_tidy(x)), error = \(e) FALSE)) x <- eval_tidy(x) # styler: off
  # otherwise, convert expr to string
  else x <- expr_deparse(quo_get_expr(x))  # styler: off
  x
}
