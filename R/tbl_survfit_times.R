#' Survival Times
#'
#' Create a gtsummary table with Kaplan-Meier estimated survival estimates
#' and specified times.
#'
#' @inheritParams tbl_survfit_quantiles
#' @inheritParams cardx::ard_survival_survfit
#' @param label (`string`)\cr
#'   Label to appear in the header row. Default is `"Time {time}"`, where
#'   the glue syntax injects the time estimate into the label.
#' @param statistic (`character`)\cr
#'   Character vector of the statistics to report.
#'   May use any of the following statistics:
#'   `c(n.risk, estimate, std.error, conf.low, conf.high)`,
#'   Default is `c("{n.risk}", "{estimate}%", "{conf.low}%, {conf.high}%")`
#' @param estimate_fun (`function`) \cr
#'   Function used to style/round the `c(estimate, conf.low, conf.high)` statistics.
#'
#' @returns a gtsummary table
#' @name tbl_survfit_times
#'
#' @examples
#' tbl_survfit_times(
#'   data = cards::ADTTE,
#'   by = "TRTA",
#'   times = c(6, 12)
#' )
NULL

#' @rdname tbl_survfit_times
#' @export
tbl_survfit_times <- function(data,
                              times,
                              y = "survival::Surv(time = AVAL, event = 1 - CNSR, type = 'right', origin = 0)",
                              by = NULL,
                              label = "Time {time}",
                              statistic = c("{n.risk}", "{estimate}%", "{conf.low}%, {conf.high}%"),
                              estimate_fun = label_style_number(digits = 1, scale = 100),
                              method.args = list(conf.int = 0.95)) {
  # check inputs ---------------------------------------------------------------
  method.args <- enquo(method.args)
  set_cli_abort_call()
  check_not_missing(data)
  check_not_missing(times)
  check_data_frame(data)
  check_numeric(times)
  check_range(times, range = c(0, Inf))
  check_string(label)
  check_class(statistic, "character")
  check_class(estimate_fun, "function")
  cards::process_selectors(data, by = {{ by }})
  check_scalar(by, allow_empty = TRUE)

  y <- .expr_as_string({{ y }}) # convert y to string (if not already)
  func_inputs <- as.list(environment())

  # subset data on complete row ------------------------------------------------
  form <- glue("{y} ~ {ifelse(is_empty(by), 1, cardx::bt(by))}") |> stats::as.formula()
  data <- data[stats::complete.cases(data[all.vars(form)]), ]

  # get survival quantiles -----------------------------------------------------
  browser()
  ard_surv_times <-
    cardx::ard_survival_survfit(
      x = data,
      y = y,
      variables = any_of(by),
      times = times,
      method.args = !!method.args
    ) |>
    cards::update_ard_fmt_fn(
      stat_names = c("estimate", "conf.low", "conf.high"),
      fmt_fn = estimate_fun
    ) |>
    cards::update_ard_fmt_fn(
      stat_names = c("n.risk", "n.censor", "cum.risk", "cum.censor"),
      fmt_fn = gtsummary::label_style_number()
    )

  # calculate ARD for by vars
  if (!is_empty(by)) {
    ard_by <- cards::ard_categorical(data, variables = all_of(by))
  }
  ard_n <- cards::ard_total_n(data)

  # get the confidence level
  conf.level <-
    ard_surv_times |>
    dplyr::filter(.data$stat_name == "conf.level") |>
    dplyr::pull("stat") |>
    unlist()

  # build gtsummary table ------------------------------------------------------
  tbl <-
    dplyr::bind_rows(
      ard_surv_times |>
        # remove model-wide stats
        dplyr::filter(.data$variable == "time") |>
        dplyr::mutate(
          variable = paste0(.data$variable, unlist(.data$variable_level)),
          variable_level = NULL
        ),
      # TODO: This is causing an error in `tbl_ard_summary()` and I need to investigate
      # case_switch(!is_empty(by) ~ ard_by),
      ard_n
    ) |>
    gtsummary::tbl_ard_summary(
      by = any_of(by),
      type = starts_with("time") ~ "continuous2",
      statistic =  starts_with("time") ~ statistic,
      label =
        map(times, ~ glue::glue_data(list(time = .x), label)) |>
        set_names(paste0("time", times))
    ) |>
    # TODO: add this back once `ard_by` is added back
    # gtsummary::modify_header(
    #   all_stat_cols() ~ "**{level}**  \nN = {n}",
    #   label = ""
    # ) |>
    gtsummary::modify_table_body(
      ~ .x |>
        mutate(
          label = dplyr::case_when(
            .data$label == "Number of Subjects at Risk" ~ "Patients remaining at risk",
            .data$label == "(CI Lower Bound, CI Upper Bound)" ~ glue("{gtsummary::style_number(conf.level, scale = 100)}% CI"),
            .data$label == "Survival Probability, Survival Probability" ~ "25% and 75%-ile",
            .default = .data$label
          )
        )
    )

  # return tbl -----------------------------------------------------------------
  tbl$cards <-
    list(
      tbl_survfit_times =
        dplyr::bind_rows(
          ard_surv_times,
          ard_followup_range,
          if (!is_empty(by)) ard_by,  # styler: off
          ard_n
        )
    )
  tbl[["call_list"]] <- list(tbl_survfit_times = match.call())
  tbl$inputs <- func_inputs
  tbl |>
    structure(class = c("tbl_survfit_times", "gtsummary"))
}

#' @export
#' @rdname tbl_survfit_times
add_overall.tbl_survfit_times <- function(x,
                                          last = FALSE,
                                          col_label = "**Overall**  \nN = {gtsummary::style_number(N)}",
                                          ...) {
  # TODO: we can just replace this with `add_overall.tbl_survfit_times <- add_overall.tbl_survfit_qunatiles`
  set_cli_abort_call()
  rlang::check_dots_empty(call = get_cli_abort_call())

  do.call(
    what = getNamespace("gtsummary")[["add_overall.tbl_summary"]],
    args = list(x = x, last = last, col_label = col_label)
  )
}
