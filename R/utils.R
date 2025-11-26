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

  digits <- list()
  stat_meas <- c("mean", "sd", "var", "median", "sum", "p25", "p50", "p75") # dp = max(DP) + 1
  stat_fixed_cts <- c("min", "max", "n") # dp = max(DP)
  stat_fixed_cat <- c("n", "N", "N_obs", "N_miss", "N_nonmiss") # dp = 0
  stat_pct <- c("p", "p_miss", "p_nonmiss") # dp = 1

  for (var in include) {
    if (var %in% names(data)) {
      # continuous variables
      if (is.numeric(data[[var]])) {
        # get max digits for variable
        max_dp <- max(vapply(data[[var]], .count_dp, FUN.VALUE = numeric(1)))

        digits[[var]] <- c(
          rep(list(label_roche_number(digits = max_dp + 1)), length(stat_meas)),
          rep(list(label_roche_number(digits = max_dp)), length(stat_fixed_cts))
        ) |>
          stats::setNames(c(stat_meas, stat_fixed_cts))
        # non-continuous variables
      } else {
        digits[[var]] <- c(
          rep(list(label_roche_number(digits = 0)), length(stat_fixed_cat)),
          rep(list(label_roche_percent(digits = 1, scale = 100)), length(stat_pct))
        ) |>
          stats::setNames(c(stat_fixed_cat, stat_pct))
      }
    }
  }
  digits
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
