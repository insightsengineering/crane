case_switch <- function(..., .default = NULL) {
  dots <- dots_list(...)

  for (f in dots) {
    if (isTRUE(eval(f_lhs(f), envir = attr(f, ".Environment")))) {
      return(eval(f_rhs(f), envir = attr(f, ".Environment")))
    }
  }

  return(.default)
}

# function to estimate default decimal places to display for tbl_roche_summary()
.guess_roche_summary_digits <- function(data, include) {
  set_cli_abort_call()
  cards::process_selectors(data, include = {{ include }})

  stat_meas <- c("mean", "sd", "var", "median", "p25", "p50", "p75") # dp = max(DP) + 1
  stat_fixed_cts <- c("min", "max", "sum", "n") # dp = max(DP)
  stat_fixed_cat <- c("n", "N", "N_obs", "N_miss", "N_nonmiss") # dp = 0
  stat_pct <- c("p", "p_miss", "p_nonmiss") # dp = 1

  lapply(
    include,
    function(var) {
      # continuous variables
      if (is.numeric(data[[var]])) {
        # get max digits for variable
        max_dp <- max(vapply(data[[var]], .count_dp, FUN.VALUE = numeric(1)))
        c(
          rep(list(label_roche_number(digits = max_dp + 1)), length(stat_meas)),
          rep(list(label_roche_number(digits = max_dp)), length(stat_fixed_cts))
        ) |>
          stats::setNames(c(stat_meas, stat_fixed_cts))
        # non-continuous variables
      } else {
        c(
          rep(list(label_roche_number(digits = 0)), length(stat_fixed_cat)),
          rep(list(label_roche_percent(digits = 1, scale = 100)), length(stat_pct))
        ) |>
          stats::setNames(c(stat_fixed_cat, stat_pct))
      }
    }
  ) |>
    stats::setNames(include)
}

# function to count decimal places in a number
.count_dp <- function(x) {
  if (is.na(x)) {
    return(0)
  } else if (abs(x - round(x)) > .Machine$double.eps^0.5) {
    nchar(strsplit(format(x, scientific = FALSE, trim = FALSE), ".", fixed = TRUE)[[1]][[2]])
  } else {
    return(0)
  }
}

#' Internal Calculation Helper for Abnormality Tabulation
#'
#' @description
#' Acts as the calculation engine for baseline-stratified abnormality summaries.
#' It computes the numerator (`n`, unique patients with the abnormality),
#' denominator (`N`, total unique patients in the subset), and proportion (`p`)
#' using `cards::ard_mvsummary`. It then forcefully applies the specified
#' baseline tier label to the resulting ARD object.
#'
#' @param df (`data.frame`)\cr
#'   A pre-filtered data frame containing the clinical results for a specific baseline status tier.
#' @param group_label (`character`)\cr
#'   The label to assign to the calculated statistics (e.g., `"Not Low"`, `"Total"`).
#'   This will overwrite the `variable_level` and `level` columns in the output ARD.
#' @param abn_val (`character` vector)\cr
#'   A vector of values defining the target abnormality in the post-baseline column
#'   (e.g., `c("LOW", "LOW LOW")`).
#' @param postbaseline (`character`)\cr
#'   The column name containing the post-baseline reference range indicators.
#' @param id (`character`)\cr
#'   The column name for the unique subject identifier.
#' @param by (`character` vector or `NULL`)\cr
#'   Optional column names for grouping variables (e.g., `"TRT01A"`).
#'
#' @return An ARD data frame containing the customized `abnormal` statistic (`n`, `N`, `p`),
#'   or `NULL` if the input `df` has zero rows.
#'
#' @keywords internal
#' @noRd
.calc_abnormal_logic <- function(df, group_label, abn_val, postbaseline, id, by) {
  if (nrow(df) == 0) {
    return(NULL)
  }

  cards::ard_mvsummary(
    data = df,
    variables = all_of(postbaseline),
    by = any_of(by),
    statistic = ~ list(
      abnormal = \(x, data, ...) {
        n_abn <- data |>
          dplyr::filter(.data[[postbaseline]] %in% abn_val) |>
          dplyr::pull(all_of(id)) |>
          dplyr::n_distinct()

        N_total <- data |>
          dplyr::pull(all_of(id)) |>
          dplyr::n_distinct()

        dplyr::tibble(n = n_abn, N = N_total, p = n / N)
      }
    )
  ) |>
    # FORCE level and variable_level to character strings.
    dplyr::mutate(
      variable_level = as.character(group_label),
      level = as.character(group_label)
    )
}
