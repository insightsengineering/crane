#' Proportion Comparisons
#'
#' @description
#' Add between-group comparison rows to a [tbl_proportion()] table.
#'
#' - `add_proportion_difference()` adds a block with the difference in response
#'   rates, its confidence interval, and a test p-value.
#' - `add_proportion_odds_ratio()` adds an odds-ratio row (with confidence
#'   interval).
#'
#' Both verbs compare every non-reference group against `reference`. Supplying
#' `strata` switches the p-value (and odds ratio) to their stratified
#' (Cochran-Mantel-Haenszel) counterparts, so an unstratified and a stratified
#' block can be stacked by calling the verb twice.
#'
#' All statistical choices (test, method, confidence level, continuity
#' correction) live on these functions. Row wording is overridable through the
#' label arguments.
#'
#' @param x (`tbl_proportion`)\cr
#'   a table created by [tbl_proportion()] on a binary variable (built with the
#'   `value` argument).
#' @param reference (`string`)\cr
#'   value of the `by` variable used as reference. Defaults to the first level.
#' @param test (`string`)\cr
#'   test for the p-value: `"chisq"` (Chi-squared, default), `"fisher"`
#'   (Fisher's exact), or `"cmh"` (Cochran-Mantel-Haenszel, requires `strata`).
#' @param strata (`string`)\cr
#'   optional stratification variable; when supplied the p-value uses CMH and
#'   the default header becomes `"Stratified Analysis"`. Default `NULL`.
#' @param conf.level (`scalar`)\cr
#'   confidence level. Default `0.95`.
#' @param correct (`flag`)\cr
#'   continuity correction for the difference confidence interval (the
#'   "Wald, with continuity correction" interval from [stats::prop.test()]).
#'   Default `TRUE`.
#' @param test_correct (`flag`)\cr
#'   Yates continuity correction for the chi-squared test p-value. Default
#'   `FALSE`, matching the NEST 1.0 catalog (RSPT01). Has no effect on the
#'   Fisher or CMH tests.
#' @param header,diff_label,pvalue_label,ci_label (`string`)\cr
#'   row labels. `header` defaults to `"Unstratified Analysis"` /
#'   `"Stratified Analysis"`; `pvalue_label` and `ci_label` are derived from
#'   `test`, `conf.level` and `correct` when `NULL`.
#' @param estimate_fun,pvalue_fun (`function`)\cr
#'   formatters for the difference/CI values and the p-value.
#'
#' @returns the input `tbl_proportion` with comparison rows appended.
#' @name add_proportion_difference
#' @seealso [tbl_proportion()]
#'
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'   arm = factor(sample(c("A", "B", "C"), 200, TRUE), levels = c("A", "B", "C")),
#'   rsp = sample(c(TRUE, FALSE), 200, TRUE)
#' )
#' tbl_proportion(df, "rsp", "arm", value = TRUE) |>
#'   add_proportion_difference(reference = "A") |>
#'   add_proportion_odds_ratio(reference = "A")
#'
#' @export
add_proportion_difference <- function(x,
                                      reference = NULL,
                                      test = c("chisq", "fisher", "cmh"),
                                      strata = NULL,
                                      conf.level = 0.95,
                                      correct = TRUE,
                                      test_correct = FALSE,
                                      header = NULL,
                                      diff_label = "Difference in Response rate (%)",
                                      pvalue_label = NULL,
                                      ci_label = NULL,
                                      estimate_fun = label_roche_number(digits = 1),
                                      pvalue_fun = label_roche_pvalue()) {
  set_cli_abort_call()
  check_class(x, "tbl_proportion")
  check_range(conf.level, range = c(0, 1))
  check_class(estimate_fun, "function")
  check_class(pvalue_fun, "function")
  check_string(diff_label)
  check_scalar_logical(correct)
  check_scalar_logical(test_correct)
  test <- arg_match(test)
  if (!is.null(strata)) test <- "cmh"

  meta <- .check_proportion_comparison(x, strata, test)
  reference <- reference %||% meta$lvls[1]
  .check_reference(reference, meta$lvls)

  header <- header %||% if (is.null(strata)) "Unstratified Analysis" else "Stratified Analysis"
  ci_label <- ci_label %||%
    glue("{style_roche_number(conf.level, scale = 100)}% CI ({if (correct) 'Wald, with continuity correction' else 'Wald'})")
  pvalue_label <- pvalue_label %||% .default_test_label(test)

  # per-level statistics -------------------------------------------------------
  cmp_lvls <- setdiff(meta$lvls, reference)
  stats_by_level <- lapply(
    cmp_lvls,
    function(lv) {
      .proportion_diff_one(
        data = meta$data, variable = meta$variable, by = meta$by,
        value = meta$value, reference = reference, level = lv,
        strata = strata, test = test, conf.level = conf.level,
        correct = correct, test_correct = test_correct
      )
    }
  )
  names(stats_by_level) <- cmp_lvls

  # build the comparison rows using the reliable column -> level map -----------
  rows <- .format_comparison_block(
    x = x, reference = reference, stats_by_level = stats_by_level,
    header = header, diff_label = diff_label, ci_label = ci_label,
    pvalue_label = pvalue_label, estimate_fun = estimate_fun, pvalue_fun = pvalue_fun,
    block = "..diff.."
  )

  x |>
    gtsummary::modify_table_body(~ dplyr::bind_rows(.x, rows)) |>
    gtsummary::modify_indent(
      columns = "label",
      rows = .data$variable %in% "..diff.." & .data$label %in% c(diff_label, pvalue_label),
      indent = 4L
    ) |>
    gtsummary::modify_indent(
      columns = "label",
      rows = .data$variable %in% "..diff.." & .data$label %in% ci_label,
      indent = 8L
    ) |>
    structure(class = c("tbl_proportion", "gtsummary"))
}

# Map stat columns to their by-level using the header's `modify_stat_level`.
# This is the robust column mapping (no parsing of markdown header labels).
.stat_col_level_map <- function(x) {
  x$table_styling$header |>
    dplyr::filter(grepl("^stat_\\d+$", .data$column)) |>
    dplyr::select("column", "modify_stat_level") |>
    tibble::deframe()
}

# Assemble a header row plus one estimate/CI/p-value block into table-body rows,
# placing each comparison statistic in the correct stat column.
.format_comparison_block <- function(x, reference, stats_by_level, header,
                                     diff_label, ci_label, pvalue_label,
                                     estimate_fun, pvalue_fun, block,
                                     rows_spec = c("estimate", "ci", "pvalue")) {
  col_map <- .stat_col_level_map(x)
  stat_cols <- names(col_map)
  template <- x$table_body[1, , drop = FALSE]
  template[stat_cols] <- NA_character_

  mk_row <- function(label, filler) {
    r <- template
    r$variable <- block
    r$row_type <- "level"
    r$label <- label
    for (col in stat_cols) {
      lvl <- col_map[[col]]
      r[[col]] <- if (identical(lvl, reference) || is.null(stats_by_level[[lvl]])) {
        NA_character_
      } else {
        filler(stats_by_level[[lvl]])
      }
    }
    r
  }

  hdr <- template
  hdr$variable <- block
  hdr$row_type <- "label"
  hdr$label <- header

  parts <- list(hdr)
  if ("estimate" %in% rows_spec) {
    parts <- c(parts, list(mk_row(diff_label, function(s) estimate_fun(s$estimate))))
  }
  if ("ci" %in% rows_spec) {
    parts <- c(parts, list(mk_row(ci_label, function(s) {
      if (is.na(s$conf.low)) NA_character_ else glue("({estimate_fun(s$conf.low)}, {estimate_fun(s$conf.high)})")
    })))
  }
  if ("pvalue" %in% rows_spec) {
    parts <- c(parts, list(mk_row(pvalue_label, function(s) pvalue_fun(s$p.value))))
  }
  if ("or" %in% rows_spec) {
    # single estimate (CI) row; the header IS the row (no separate header line)
    parts <- list(mk_row(header, function(s) {
      if (is.na(s$estimate)) {
        NA_character_
      } else {
        glue("{estimate_fun(s$estimate)} ({estimate_fun(s$conf.low)}, {estimate_fun(s$conf.high)})")
      }
    }))
  }
  dplyr::bind_rows(parts)
}

# Shared checks for the comparison verbs; returns pulled metadata.
.check_proportion_comparison <- function(x, strata, test) {
  by <- x$inputs$by
  if (is_empty(by)) {
    cli::cli_abort(
      "Cannot add a comparison when {.fun tbl_proportion} was built without a
       {.arg by} variable.",
      call = get_cli_abort_call()
    )
  }
  if (is.null(x$inputs$value)) {
    cli::cli_abort(
      "Comparisons require a binary summary. Rebuild {.fun tbl_proportion} with
       the {.arg value} argument.",
      call = get_cli_abort_call()
    )
  }
  if (test == "cmh" && is.null(strata)) {
    cli::cli_abort(
      "The {.val cmh} test requires a {.arg strata} variable.",
      call = get_cli_abort_call()
    )
  }
  data <- x$inputs$data
  data[[by]] <- factor(data[[by]])
  list(
    data = data, by = by, variable = x$inputs$variable,
    value = x$inputs$value, lvls = levels(data[[by]])
  )
}

.check_reference <- function(reference, lvls) {
  if (!reference %in% lvls) {
    cli::cli_abort(
      "The {.arg reference} {.val {reference}} is not a level of the grouping variable.",
      call = get_cli_abort_call()
    )
  }
}

.default_test_label <- function(test) {
  switch(test,
    chisq = "p-value (Chi-Squared Test)",
    fisher = "p-value (Fisher's Exact Test)",
    cmh = "p-value (Cochran-Mantel-Haenszel Test)",
    "p-value"
  )
}

# Difference, CI and p-value for one comparison (level - reference). Uses
# stats::prop.test / mantelhaen.test so the CI matches the "Wald, with
# correction" convention without re-implementing interval math.
.proportion_diff_one <- function(data, variable, by, value, reference, level,
                                 strata, test, conf.level, correct, test_correct) {
  sub <- data[data[[by]] %in% c(reference, level), , drop = FALSE]
  sub$.success <- sub[[variable]] == value
  sub[[by]] <- factor(sub[[by]], levels = c(level, reference)) # level first

  if (is.null(strata)) {
    ok <- stats::complete.cases(sub$.success, sub[[by]])
    sub <- sub[ok, , drop = FALSE]
    tab <- table(sub[[by]], factor(sub$.success, levels = c(TRUE, FALSE)))
    pt <- stats::prop.test(tab, conf.level = conf.level, correct = correct)
    est <- unname(pt$estimate[1] - pt$estimate[2]) * 100
    pval <- switch(test,
      chisq = stats::chisq.test(tab, correct = test_correct)$p.value,
      fisher = stats::fisher.test(tab)$p.value,
      pt$p.value
    )
    list(estimate = est, conf.low = pt$conf.int[1] * 100, conf.high = pt$conf.int[2] * 100, p.value = pval)
  } else {
    ok <- stats::complete.cases(sub$.success, sub[[by]], sub[[strata]])
    sub <- sub[ok, , drop = FALSE]
    arr <- table(factor(sub$.success, levels = c(TRUE, FALSE)), sub[[by]], sub[[strata]])
    mh <- stats::mantelhaen.test(arr, correct = correct, conf.level = conf.level)
    p_lvl <- mean(sub$.success[sub[[by]] == level], na.rm = TRUE)
    p_ref <- mean(sub$.success[sub[[by]] == reference], na.rm = TRUE)
    list(estimate = (p_lvl - p_ref) * 100, conf.low = NA_real_, conf.high = NA_real_, p.value = mh$p.value)
  }
}
