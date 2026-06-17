#' @describeIn tbl_survfit_times
#'
#' Adds survival differences between groups as additional rows to tables created by [`tbl_survfit_times()`].
#'
#' Difference statistics are calculated using [cardx::ard_survival_survfit_diff()]
#' for all specified timepoints, extracting the formula and dataset directly from the provided `fit` object.
#'
#' Pairwise differences are calculated relative to the specified `by` variable's specified reference level.
#'
#' @inheritParams gtsummary::add_difference.tbl_summary
#' @param fit (`survfit`)\cr
#'   The original `survival::survfit()` model used to extract the survival times.
#' @param times (`numeric`)\cr
#'   Numeric vector of times at which to calculate the differences. If `NULL` (default), the function
#'   will attempt to infer the times dynamically from the table inputs.
#' @param reference (`string`)\cr
#'   Value of the `by` variable that is the reference for each of the difference calculations. 
#'   For factors, use the character level. The reference column will appear as the leftmost column in the table.
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
#' fit <- survival::survfit(survival::Surv(AVAL, 1 - CNSR) ~ TRTA, data = cards::ADTTE)
#' surv_df <- get_surv_times_df(fit, times = c(30, 60))
#'
#' tbl_survfit_times(surv_df, label = "Day {time}") |>
#'   add_difference_row(fit = fit, reference = "Placebo")
add_difference_row.tbl_survfit_times <- function(x,
                                                 fit,
                                                 reference,
                                                 times = NULL,
                                                 statistic = c("{estimate}", "({conf.low}, {conf.high})", "{p.value}"),
                                                 conf.level = 0.95,
                                                 pvalue_fun = label_roche_pvalue(),
                                                 estimate_fun = label_roche_number(digits = 2, scale = 100),
                                                 ...) {
  # check inputs ---------------------------------------------------------------
  set_cli_abort_call()
  check_dots_empty(call = get_cli_abort_call())
  check_class(fit, "survfit")
  check_not_missing(reference)
  check_string(reference)
  check_range(conf.level, range = c(0, 1))
  check_class(statistic, "character")
  check_class(pvalue_fun, "function")
  check_class(estimate_fun, "function")

  # extract formula and data from fit ------------------------------------------
  form <- fit$call$formula |> stats::as.formula()
  by <- all.vars(form[[3]])
  
  if (length(by) == 0) {
    cli::cli_abort(
      "Cannot run {.fun add_difference_row} when the {.arg fit} model does not include a strata/by variable.",
      call = get_cli_abort_call()
    )
  }
  by <- by[1] # Take primary stratification variable

  data <- eval(fit$call$data, envir = parent.frame())
  if (is.null(data)) {
    cli::cli_abort(
      "Could not extract data from the {.arg fit} object. Ensure the model was called with a data frame in the data argument.",
      call = get_cli_abort_call()
    )
  }

  # Infer times if not explicitly provided -------------------------------------
  if (is.null(times)) {
    # Extract the original dataframe passed to tbl_survfit_times
    surv_df <- x$inputs$data
    if (is.null(surv_df)) surv_df <- x$inputs[[1]] 
    
    if (!is.null(surv_df) && "Time" %in% colnames(surv_df)) {
      times <- as.numeric(unique(surv_df$Time))
      times <- times[!is.na(times)]
    }
    
    if (is.null(times) || length(times) == 0) {
      cli::cli_abort(
        "Could not infer {.arg times} dynamically from the table inputs. Please provide the {.arg times} argument explicitly.",
        call = get_cli_abort_call()
      )
    }
  }

  # extract reference levels from table ----------------------------------------
  lst_by_levels <-
    x$table_styling$header |>
    dplyr::filter(grepl(pattern = "^stat_\\d*[1-9]\\d*$", x = .data$column)) |>
    dplyr::select("column", "modify_stat_level") |>
    tibble::deframe() |>
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

  # add reference level to the first position in factor
  data[[by]] <- forcats::fct_relevel(data[[by]], reference, after = 0L)
  ref_col <- names(lst_by_levels)[lst_by_levels == reference]

  # move reference column to first position in `x`
  x <- x |>
    gtsummary::modify_table_body(
      ~ .x |> dplyr::relocate(dplyr::all_of(ref_col), .after = "label")
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
    gtsummary::tbl_summary(by = dplyr::any_of(by), include = as.character(times), missing = "no") |>
    gtsummary::add_difference_row(
      reference = reference,
      statistic = gtsummary::everything() ~ statistic,
      test = gtsummary::everything() ~ survfit_diff_ard_fun,
      conf.level = conf.level,
      pvalue_fun = pvalue_fun,
      estimate_fun = gtsummary::everything() ~ estimate_fun
    )

  # build gtsummary table ------------------------------------------------------
  tbl_surv_diff <-
    tbl_surv_diff |>
    # remove time labels
    gtsummary::modify_table_body(
      ~ .x |>
        # remove dummy table label rows
        dplyr::filter(.data$row_type != "label") |>
        dplyr::mutate(
          # match variable names to `x`
          variable = paste0("time", .data$variable),
          # add default labels
          label = dplyr::case_when(
            .data$label == "Survival Difference" ~ "Difference in Event Free Rates",
            .data$label == "(CI Lower Bound, CI Upper Bound)" ~ glue::glue("{style_roche_number(conf.level, scale = 100)}% CI"),
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