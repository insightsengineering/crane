#' Generate Table of Pairwise Cox-PH and Log-Rank Results
#'
#' @description
#' This function performs pairwise comparisons of treatment arms using the
#' **Cox Proportional Hazards model** and calculates the corresponding
#' **log-rank p-value**. Each comparison tests a non-reference group
#' against a specified reference group.
#'
#' @param model_formula (`formula`)\cr
#'   A `formula` object specifying the survival model,
#'   typically in the form `Surv(time, status) ~ arm + covariates`.
#' @param data (`data.frame`)\cr
#'   A `data.frame` containing the survival data,
#'  including time, status, and the arm variable.
#' @param arm (`character`)\cr
#'   A single character string specifying the name of the column in `data`
#'   that contains the grouping/treatment
#'   **arm variable**. This column **must be a factor**
#'   for correct stratification and comparison.
#' @param ref_group (`character` or `NULL`)\cr
#'   A single character string specifying the level of the `arm` variable
#'   to be used as the **reference group** for
#'   all pairwise comparisons. If `NULL` (the default),
#'   the **first unique level** of the `arm` column is automatically
#'   selected as the reference group.
#' @param ties (`character`)\cr
#'   A string specifying the method for tie handling in the Cox model.
#'   Must be one of "exact", "efron", or "breslow".
#' @param test (`character`)\cr
#'   A string specifying the type of test to compute the p-value.
#'   Must be one of "log-rank", "gehan-breslow" (wilcoxon), "tarone", "peto", 
#'   "modpeto" (prentice), "fleming-harrington", or "likelihood-ratio".
#'
#' @return A `data.frame` with the results of the pairwise comparisons.
#' The columns include:
#' \itemize{
#'   \item `arm`: (rownames of the `data.frame`) The comparison arm (group)
#'   being tested against the reference group.
#'   \item `hr`: The Hazard Ratio (HR) for the comparison arm vs.
#'   the reference arm, formatted to two decimal places.
#'   \item `ci`: The 95% confidence interval for the HR, presented as
#'   a string in the format "(lower, upper)", with values formatted
#'   to two decimal places.
#'   \item `pval`: The log-rank p-value for the comparison.
#' }
#'
#' @details The function iterates through each unique arm
#'   (excluding the reference group). For each iteration, it filters the data
#'   to include only the current comparison arm and the reference arm, and then:
#'   \itemize{
#'     \item Fits a Cox model using `survival::coxph`.
#'     \item Performs a log-rank test using `survival::survdiff`.
#'   }
#'   The Hazard Ratio and its 95% confidence interval are extracted from the
#'   Cox model summary, and the p-value is extracted from the log-rank test.
#'
#' @seealso `annotate_gg_km()`, `gg_km()`, `survival` and `coin`
#'   package functions `survival::coxph`, `survival::survdiff`,
#'   and `coin::logrank_test`.
#'
#' @examples
#' # Example data setup (assuming 'time' is event time, 'status'
#' # is event indicator (1=event), and 'arm' is the treatment group)
#' library(dplyr) # For better data handling
#'
#' # Prepare data in a modern dplyr-friendly way
#' surv_data <- survival::lung |>
#'   mutate(
#'     arm = factor(sample(c("A", "B", "C"), n(), replace = TRUE)),
#'     status = status - 1 # Convert status to 0/1
#'   ) |>
#'   filter(if_all(everything(), ~ !is.na(.)))
#'
#' formula <- survival::Surv(time, status) ~ arm
#'
#' # Example 1: Default usage (ties = "exact", test = "log-rank")
#' results_default <- get_cox_pairwise_df(
#'   model_formula = formula,
#'   data = surv_data,
#'   arm = "arm",
#'   ref_group = "A"
#' )
#' print(results_default)
#'
#' # Example 2: Using Breslow ties and the Wilcoxon test
#' results_wilcoxon <- get_cox_pairwise_df(
#'   model_formula = formula,
#'   data = surv_data,
#'   arm = "arm",
#'   ref_group = "A",
#'   ties = "breslow",
#'   test = "wilcoxon"
#' )
#' print(results_wilcoxon)
#'
#' # Example 3: Using Efron ties and the Likelihood-Ratio test
#' results_lr <- get_cox_pairwise_df(
#'   model_formula = formula,
#'   data = surv_data,
#'   arm = "arm",
#'   ref_group = "A",
#'   ties = "efron",
#'   test = "likelihood-ratio"
#' )
#' print(results_lr)
#'
#' @export
get_cox_pairwise_df <- function(
  model_formula,
  data,
  arm,
  ref_group = NULL,
  ties = c("exact", "efron", "breslow"),
  test = c(
    "log-rank",
    "gehan-breslow",
    "tarone",
    "peto",
    "prentice",
    "fleming-harrington", 
    "likelihood-ratio"
  )
) {
  set_cli_abort_call()
  # Input checks
  if (!rlang::is_formula(model_formula)) {
    cli::cli_abort(
      "{.arg model_formula} must be a {.cls formula}.",
      call = get_cli_abort_call()
    )
  }
  if (!is.factor(data[[arm]])) {
    cli::cli_abort(
      "Column {.arg {data}[[\"{.var {arm}}\"]]} must be a {.cls factor}.",
      call = get_cli_abort_call()
    )
  }

  ties <- match.arg(ties)
  test <- match.arg(test)

  # Determine reference and comparison groups
  ref_group <- if (!is.null(ref_group)) {
    ref_group
  } else {
    levels(data[[arm]])[1]
  }
  comp_group <- setdiff(levels(data[[arm]]), ref_group)

  res <- c()
  for (current_arm in comp_group) {
    subset_arm <- c(ref_group, current_arm)
    if (length(subset_arm) != 2) {
      cli::cli_abort(
        paste0(
          "{.arg subset_arm} must contain exactly 2 arms/groups ",
          "(current length is {length(subset_arm)})."
        ),
        call = get_cli_abort_call()
      )
    }
    comp_df <- data[as.character(data[[arm]]) %in% subset_arm, ]
    suppressWarnings(
      coxph_ans <- survival::coxph(
        formula = model_formula,
        data = comp_df,
        ties = ties
      ) |> summary()
    )

    log_rank_pvalue <- .estimate_p_value(model_formula, comp_df, test)

    conf_int_row <- paste0(arm, current_arm)
    current_row <- data.frame(
      hr = sprintf("%.2f", coxph_ans$conf.int[conf_int_row, 1]),
      ci = paste0(
        "(",
        sprintf("%.2f", coxph_ans$conf.int[conf_int_row, 3]),
        ", ",
        sprintf("%.2f", coxph_ans$conf.int[conf_int_row, 4]),
        ")"
      ),
      pval = log_rank_pvalue
    )
    rownames(current_row) <- current_arm
    res <- rbind(res, current_row)
  }
  names(res) <- c(
    "HR",
    "95% CI",
    paste0("p-value (", names(log_rank_pvalue), ")")
  )
  res
}

#' Estimate p value based on chosen test type
#' Using coin and survival packages
#' @keywords internal
.estimate_p_value <- function(formula, data, test) {
  test_type <- switch(test,
    "log-rank" = "logrank",
    "gehan-breslow" = "Gehan-Breslow",
    "tarone" = "Tarone-Ware",
    "peto" = "Peto-Peto",
    "prentice" = "Prentice",
    "fleming-harrington" = "Fleming-Harrington",
    "likelihood-ratio" = "lr"
  )

  if (test_type != "lr") {
    test_result <- coin::logrank_test(
      formula = formula,
      data = data,
      type = test_type
    )
    p_value <- coin::pvalue(test_result)
  } else if (test_type == "lr") {
    # SAS LR test assumes an exponential distribution
    # fit the model
    fit_cov <- survival::survreg(formula, data = data, dist = "exponential")

    # fit the null model
    null_formula <- stats::update(formula, ~ 1)
    fit_null <- survival::survreg(null_formula, data = data, dist = "exponential")

    # calculate the difference and estimate the statistics
    lrt_stat <- 2 * (fit_cov$loglik[2] - fit_null$loglik[1])
    df <- fit_cov$df - fit_null$df
    p_value <- stats::pchisq(lrt_stat, df, lower.tail = FALSE)
  }

  if (test == "log-rank") {
    names(p_value) <- test
  } else {
    names(p_value) <- tools::toTitleCase(test)
  }

  p_value
}
