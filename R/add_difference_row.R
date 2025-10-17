#' Add difference rows between groups
#'
#' @description
#' `r lifecycle::badge('experimental')`\cr
#' Adds difference to tables created by [`tbl_survfit_times()`] as additional rows.
#' This function is often useful when there are more than two groups to compare.
#'
#' Pairwise differences are calculated relative to the specified
#' `by` variable's specified reference level.
#'
#' @inheritParams gtsummary::add_difference.tbl_summary
#' @param reference (`string`)\cr
#'   Value of the `tbl_survfit_times(by)` variable value that is the reference for
#'   each of the difference calculations. For factors, use the character
#'   level.
#' @param statistic ([`formula-list-selector`][gtsummary::syntax])\cr
#'   Specifies summary statistics to display for each time.  The default is
#'   `everything() ~ c("{estimate}", "({conf.low}, {conf.high})", "{p.value}")`.
#'   The statistics available to include are `"estimate"`, `"std.error"`,
#'   `"statistic"`, `"conf.low"`, `"conf.high"`, `"p.value"`.
#'
#' @export
#' @return a gtsummary table
#' @name tbl_survfit_times
#'
#' @examples
#' # Example 2 - Survival Differences -----------
#' tbl_survfit_times(
#'   data = cards::ADTTE,
#'   by = "TRTA",
#'   times = c(30, 60),
#'   label = "Day {time}"
#' ) |>
#'   add_difference_row(reference = "Placebo")
NULL

#' @rdname tbl_survfit_times
#' @export
add_difference_row.tbl_survfit_times <- function(x,
                                                 reference,
                                                 statistic = c("{estimate}", "({conf.low}, {conf.high})", "{p.value}"),
                                                 conf.level = 0.95,
                                                 pvalue_fun = label_roche_pvalue(),
                                                 estimate_fun = label_roche_number(digits = 2, scale = 100),
                                                 ...) {
  # check inputs ---------------------------------------------------------------
  set_cli_abort_call()
  check_dots_empty(call = get_cli_abort_call())
  check_not_missing(reference)
  check_string(reference)
  check_range(conf.level, range = c(0, 1))
  check_class(statistic, "character")
  check_class(pvalue_fun, "function")
  check_class(estimate_fun, "function")

  # check that input `x` has a `by` var and it has 2+ levels
  if (is_empty(x$inputs$by)) {
    "Cannot run {.fun add_difference_row} when {.code tbl_survfit_times()} does not include a {.arg by} argument." |>
      cli::cli_abort(call = get_cli_abort_call())
  }

  # check reference level is appropriate
  lst_by_levels <-
    x$table_styling$header |>
    dplyr::filter(grepl(pattern = "^stat_\\d*[1-9]\\d*$", x = .data$column)) |>
    dplyr::select("column", "modify_stat_level") |>
    deframe() |>
    lapply(FUN = as.character)
  if (!as.character(reference) %in% unlist(lst_by_levels)) {
    cli::cli_abort(
      "The {.arg reference} argument must be one of {.val {unlist(lst_by_levels)}}.",
      call = get_cli_abort_call()
    )
  }

  func_inputs <- as.list(environment())
  by <- x$inputs$by
  y <- x$inputs$y
  times <- x$inputs$times
  data <- x$inputs$data
  form <- glue("{y} ~ {ifelse(is_empty(by), 1, cardx::bt(by))}") |> stats::as.formula()

  # subset data on complete row ------------------------------------------------
  data <- data[stats::complete.cases(data[all.vars(form)]), ]

  # add reference level to the first position in factor
  data[[by]] <- fct_relevel(data[[by]], reference, after = 0L)
  ref_col <- names(lst_by_levels)[lst_by_levels == reference]

  # move reference column to first position
  x <- x |>
    gtsummary::modify_table_body(
      ~ .x |>
        dplyr::relocate(ref_col, .after = label)
    )

  # calculate survival difference ----------------------------------------------------------------
  card <-
    cardx::ard_survival_survfit_diff(
      x = rlang::inject(survival::survfit(!!form, data = data)),
      times = times,
      conf.level = conf.level
    ) |>
    dplyr::filter(!.data$stat_name %in% c("method", "reference_level")) |>
    cards::update_ard_fmt_fun(
      stat_names = c("estimate", "conf.low", "conf.high"),
      fmt_fun = estimate_fun
    ) |>
    cards::update_ard_fmt_fun(
      stat_names = "p.value",
      fmt_fun = pvalue_fun
    ) |>
    dplyr::mutate(
      variable = paste0(.data$variable, unlist(.data$variable_level)),
      variable_level = NULL
    )

  ard_surv_diff <-
    cards::bind_ard(
      card |>
        dplyr::filter(
          unlist(.data$group1_level) == unlist(card$group1_level)[1]
        ) |>
        dplyr::mutate(
          group1_level = as.list(factor(reference, levels = levels(data[[by]]))),
          stat = list(NULL)
        ),
      card
    )

  # build gtsummary table ------------------------------------------------------
  tbl_surv_diff <-
    ard_surv_diff |>
    gtsummary::tbl_ard_summary(
      by = any_of(by),
      type = starts_with("time") ~ "continuous2",
      statistic = starts_with("time") ~ statistic
    ) |>
    gtsummary::remove_row_type(type = "header") |>
    gtsummary::modify_table_body(
      ~ .x |>
        dplyr::mutate(
          label = dplyr::case_when(
            .data$label == "Survival Difference" ~ "Difference in Event Free Rate",
            .data$label == "(CI Lower Bound, CI Upper Bound)" ~ glue("{style_roche_number(conf.level, scale = 100)}% CI"),
            .data$label == "p-value" ~ "p-value (Z-test)",
            .default = .data$label
          ),
          # update row_type
          row_type = "difference_row",
          !!ref_col := NA
        )
    ) |>
    gtsummary::modify_indent(columns = label, rows = row_type == "difference_row", indent = 8L) |>
    gtsummary::modify_indent(columns = label, rows = label == "Difference in Event Free Rate", indent = 4L) |>
    gtsummary::modify_missing_symbol(
      columns =
        x$table_styling$header |>
        dplyr::filter(.data$modify_stat_level == .env$reference) |>
        dplyr::pull("column"),
      rows = .data$row_type == "difference_row",
      symbol = "\U2014"
    )

  x <-
    tbl_stack(
      tbls = list(x, tbl_surv_diff),
      quiet = TRUE
    ) |>
    # move survival difference sections under each section for each matching survival time
    gtsummary::modify_table_body(
      ~ .x |>
        dplyr::mutate(
          variable = as.factor(variable),
          idx_row = dplyr::row_number()
        ) |>
        dplyr::arrange(variable, idx_row) |>
        dplyr::mutate(
          variable = as.character(variable)
        )
    )

  # add info to table ----------------------------------------------------------
  x$call_list[["add_difference_row"]] <- match.call()
  x$cards[["add_difference_row"]] <- card

  # print warnings/errors from calculations
  x$cards[["add_difference_row"]] |> cards::print_ard_conditions()

  x
}
