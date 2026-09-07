#' @describeIn add_proportion_difference
#'
#' Add an odds-ratio row (with confidence interval) comparing each non-reference
#' group to `reference`. Unstratified odds ratios come from logistic regression
#' ([stats::glm()], via [cardx::ard_regression_basic()]); when `strata` is
#' supplied the common (CMH) odds ratio from [stats::mantelhaen.test()] is used.
#'
#' @param or_label (`string`)\cr
#'   label for the odds-ratio row. Default `"Odds Ratio (95% CI)"`.
#'
#' @export
add_proportion_odds_ratio <- function(x,
                                      reference = NULL,
                                      strata = NULL,
                                      conf.level = 0.95,
                                      or_label = NULL,
                                      estimate_fun = label_roche_ratio()) {
  set_cli_abort_call()
  check_class(x, "tbl_proportion")
  check_range(conf.level, range = c(0, 1))
  check_class(estimate_fun, "function")

  meta <- .check_proportion_comparison(x, strata, test = if (is.null(strata)) "chisq" else "cmh")
  reference <- reference %||% meta$lvls[1]
  .check_reference(reference, meta$lvls)
  or_label <- or_label %||% glue("Odds Ratio ({style_roche_number(conf.level, scale = 100)}% CI)")

  cmp_lvls <- setdiff(meta$lvls, reference)
  stats_by_level <- lapply(
    cmp_lvls,
    function(lv) {
      .odds_ratio_one(
        data = meta$data, variable = meta$variable, by = meta$by,
        value = meta$value, reference = reference, level = lv,
        strata = strata, conf.level = conf.level
      )
    }
  )
  names(stats_by_level) <- cmp_lvls

  # single OR row: reuse the block formatter, estimate row only ----------------
  rows <- .format_comparison_block(
    x = x, reference = reference, stats_by_level = stats_by_level,
    header = or_label, diff_label = or_label, ci_label = or_label,
    pvalue_label = or_label, estimate_fun = estimate_fun, pvalue_fun = estimate_fun,
    block = "..or..", rows_spec = "or"
  )

  x |>
    gtsummary::modify_table_body(~ dplyr::bind_rows(.x, rows)) |>
    structure(class = c("tbl_proportion", "gtsummary"))
}

# Odds ratio (level vs reference) with CI. Logistic GLM when unstratified;
# CMH common odds ratio when a stratification variable is supplied.
.odds_ratio_one <- function(data, variable, by, value, reference, level,
                            strata, conf.level) {
  sub <- data[data[[by]] %in% c(reference, level), , drop = FALSE]
  sub$.success <- as.integer(sub[[variable]] == value)
  sub[[by]] <- stats::relevel(factor(sub[[by]], levels = c(reference, level)), ref = reference)

  if (is.null(strata)) {
    ok <- stats::complete.cases(sub$.success, sub[[by]])
    sub <- sub[ok, , drop = FALSE]
    ard <- cardx::construct_model(
      data = sub,
      formula = stats::reformulate(response = ".success", termlabels = by),
      method = "glm",
      method.args = list(family = stats::binomial)
    ) |>
      cardx::ard_regression_basic(exponentiate = TRUE, conf.level = conf.level) |>
      dplyr::filter(.data$variable == .env$by)
    get_stat <- function(nm) {
      v <- ard$stat[ard$stat_name == nm]
      if (length(v) == 0) NA_real_ else unlist(v)[1]
    }
    list(estimate = get_stat("estimate"), conf.low = get_stat("conf.low"), conf.high = get_stat("conf.high"))
  } else {
    ok <- stats::complete.cases(sub$.success, sub[[by]], sub[[strata]])
    sub <- sub[ok, , drop = FALSE]
    arr <- table(
      factor(sub$.success, levels = c(1L, 0L)),
      factor(sub[[by]], levels = c(level, reference)),
      sub[[strata]]
    )
    mh <- stats::mantelhaen.test(arr, correct = FALSE, conf.level = conf.level)
    list(estimate = unname(mh$estimate), conf.low = mh$conf.int[1], conf.high = mh$conf.int[2])
  }
}
