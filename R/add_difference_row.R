#' @describeIn tbl_survfit_times
#'
#' Adds survival differences between groups as additional rows to tables created by [`tbl_survfit_times()`].
#'
#' Difference statistics are calculated using [cardx::ard_survival_survfit_diff()]
#' for all `tbl_survfit_times(times)` variable values, using `survfit` formula:
#' ```r
#' survival::survfit(y ~ by, data = data)
#' ```
#' where `y`, `by` and `data` are the inputs of the same names to the `tbl_survfit_times()` object `x`.
#'
#' Pairwise differences are calculated relative to the specified `by` variable's specified reference level.
#'
#' @inheritParams gtsummary::add_difference.tbl_summary
#' @param reference (`string`)\cr
#'   Value of the `tbl_survfit_times(by)` variable value that is the reference for
#'   each of the difference calculations. For factors, use the character
#'   level. The reference column will appear as the leftmost column in the table.
#' @param pvalue_fun (`function`)\cr
#'   Function to round and format the `p.value` statistic. Default is [label_roche_pvalue()].
#'   The function must have a numeric vector input, and return a string that is the
#'   rounded/formatted p-value (e.g. `pvalue_fun = label_style_pvalue(digits = 3)`).
#'
#' @export
#' @order 3
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

  # check that input `x` has a `by` var with 2+ levels
  if (is_empty(x$inputs$by)) {
    cli::cli_abort(
      "Cannot run {.fun add_difference_row} when {.code tbl_survfit_times()} does not include a {.arg by} argument.",
      call = get_cli_abort_call()
    )
  }

  lst_by_levels <-
    x$table_styling$header |>
    dplyr::filter(grepl(pattern = "^stat_\\d*[1-9]\\d*$", x = .data$column)) |>
    dplyr::select("column", "modify_stat_level") |>
    deframe() |>
    lapply(FUN = as.character)

  # check reference level is appropriate
  if (!as.character(reference) %in% unlist(lst_by_levels)) {
    cli::cli_abort(
      "The {.arg reference} argument must be one of {.val {unlist(lst_by_levels)}}.",
      call = get_cli_abort_call()
    )
  }

  # get function inputs --------------------------------------------------------
  func_inputs <- as.list(environment())
  by <- x$inputs$by
  y <- x$inputs$y
  times <- x$inputs$times
  data <- x$inputs$data
  form <- glue("{y} ~ {cardx::bt(by)}") |> stats::as.formula()

  # add reference level to the first position in factor
  data[[by]] <- fct_relevel(data[[by]], reference, after = 0L)
  ref_col <- names(lst_by_levels)[lst_by_levels == reference]

  # move reference column to first position in `x`
  x <- x |>
    gtsummary::modify_table_body(
      ~ .x |> dplyr::relocate(all_of(ref_col), .after = "label")
    )

  # calculate survival difference ----------------------------------------------
  survfit_diff_ard_fun <- function(data, variable, ...) {
    cardx::ard_survival_survfit_diff(
      x = rlang::inject(survival::survfit(!!form, data = data)),
      times = as.numeric(variable),
      conf.level = conf.level
    ) |>
      dplyr::filter(!.data$stat_name %in% c("method", "reference_level"))
  }

  # difference to be calculated for each time
  for (t in times) data[[as.character(t)]] <- NA

  # create difference rows
  tbl_surv_diff <-
    data |>
    # create dummy table to add difference rows to
    gtsummary::tbl_summary(by = any_of(by), include = as.character(times), missing = "no") |>
    gtsummary::add_difference_row(
      reference = reference,
      statistic = everything() ~ statistic,
      test = everything() ~ survfit_diff_ard_fun,
      conf.level = conf.level,
      pvalue_fun = pvalue_fun,
      estimate_fun = everything() ~ estimate_fun
    )

  # build gtsummary table ------------------------------------------------------
  tbl_surv_diff <-
    tbl_surv_diff |>
    # remove time labels
    gtsummary::modify_table_body(
      ~ .x |>
        # remove dummy table label rows
        dplyr::filter(row_type != "label") |>
        dplyr::mutate(
          # match variable names to `x`
          variable = paste0("time", .data$variable),
          # add default labels
          label = dplyr::case_when(
            .data$label == "Survival Difference" ~ "Difference in Event Free Rates",
            .data$label == "(CI Lower Bound, CI Upper Bound)" ~ glue("{style_roche_number(conf.level, scale = 100)}% CI"),
            .data$label == "p-value" ~ "p-value (Z-test)",
            .default = .data$label
          )
        )
    ) |>
    # indent rows
    gtsummary::modify_indent(columns = "label", rows = .data$row_type == "difference_row", indent = 8L) |>
    gtsummary::modify_indent(columns = "label", rows = .data$label == "Difference in Event Free Rates", indent = 4L)

  # remove ARD for dummy table rows
  tbl_surv_diff$cards$tbl_summary <- NULL

  # add difference rows into tbl_survfit_times table
  x <-
    gtsummary::tbl_stack(
      tbls = list(x, tbl_surv_diff),
      quiet = TRUE
    ) |>
    # move survival difference rows under each section for each matching survival time
    gtsummary::modify_table_body(
      \(x) {
        x |>
          dplyr::mutate(
            variable_f = factor(gsub("-row_difference", "", .data$variable), levels = unique(x$variable)),
            idx_row = dplyr::row_number()
          ) |>
          dplyr::arrange(dplyr::pick("variable_f", "idx_row")) |>
          dplyr::select(-"variable_f", -"idx_row")
      }
    )

  # add info to table ----------------------------------------------------------
  x$call_list <- list(
    "tbl_survfit_times" = x$tbls[[1]]$call_list,
    "add_difference_row" = match.call()
  )
  x$cards <- lapply(x$tbls, \(x) x$cards) |> unlist(recursive = FALSE)
  x$inputs <- list(
    "tbl_survfit_times" = x$tbls[[1]]$inputs,
    "add_difference_row" = func_inputs
  )

  x |>
    structure(class = c("tbl_survfit_times", "gtsummary"))
}
