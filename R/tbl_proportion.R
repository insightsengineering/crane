#' Proportion Summary
#'
#' @description
#' Build a gtsummary table of proportions (with confidence intervals) for a
#' binary or multinomial variable, split by a grouping variable. The estimate
#' and its confidence interval are rendered on two rows, matching the layout of
#' binary-outcome / response summary tables (e.g. RSPT01).
#'
#' The table is a plain `gtsummary` object, so the usual `modify_*()` verbs
#' apply. Between-group comparisons are added with [add_proportion_difference()]
#' (difference in rates, confidence interval, p-value) and
#' [add_proportion_odds_ratio()] (odds ratio).
#'
#' All statistical choices, the confidence-interval `method`, the `conf.level`,
#' and (for comparisons) the `test`, live on these functions rather than in the
#' calling template. Cosmetic choices, factor labels, category wording,
#' indentation, live in the template.
#'
#' @param data (`data.frame`)\cr
#'   analysis data.
#' @param variable (`string`)\cr
#'   name of the variable to summarize. May be a logical/binary flag (use
#'   `value = TRUE`) or a factor/character with several levels (leave `value`
#'   `NULL` to report every level).
#' @param by (`string`)\cr
#'   name of the grouping (arm) variable. When `NULL`, a single-column summary
#'   is produced and comparison verbs are not applicable.
#' @param value (`scalar`)\cr
#'   for a binary summary, the level counted as a "success" (e.g. `TRUE`). When
#'   `NULL` (default) every level of `variable` is reported, each on its own
#'   estimate/CI row pair.
#' @param method (`string`)\cr
#'   confidence-interval method passed to [cardx::ard_categorical_ci()]. One of
#'   `"waldcc"` (Wald with continuity correction, the default), `"wald"`,
#'   `"clopper-pearson"`, `"wilson"`, `"wilsoncc"`, `"agresti-coull"`,
#'   `"jeffreys"`, `"strat_wilson"`, `"strat_wilsoncc"`.
#' @param conf.level (`scalar`)\cr
#'   confidence level for the interval. Default is `0.95`.
#' @param statistic (`character`)\cr
#'   two-element character vector of glue templates for the estimate row and the
#'   CI row. Default `c("{n} ({p}%)", "({conf.low}, {conf.high})")`.
#' @param label (`string` or named `list`)\cr
#'   for a binary summary, the label of the estimate row (default
#'   `"Responders"`). For a multinomial summary, an optional named list mapping
#'   level values to labels; unmapped levels use the level value itself.
#' @param ci_label (`string`)\cr
#'   label of the confidence-interval row. Default is built from `conf.level`
#'   and `method`, e.g. `"95% CI (Wald, with correction)"`.
#' @param estimate_fun (`function`)\cr
#'   formatter applied to the CI bounds. Default
#'   `label_roche_number(digits = 1, scale = 100)`.
#'
#' @returns a `tbl_proportion`/`gtsummary` table.
#' @name tbl_proportion
#' @seealso [add_proportion_difference()], [add_proportion_odds_ratio()]
#'
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'   arm = factor(sample(c("A", "B"), 100, TRUE), levels = c("A", "B")),
#'   rsp = sample(c(TRUE, FALSE), 100, TRUE)
#' )
#'
#' # binary responder summary
#' tbl_proportion(df, variable = "rsp", by = "arm", value = TRUE)
#'
#' # multinomial breakdown
#' df$cat <- factor(sample(c("CR", "PR", "SD"), 100, TRUE))
#' tbl_proportion(df, variable = "cat", by = "arm")
#'
#' @export
tbl_proportion <- function(data,
                           variable,
                           by = NULL,
                           value = NULL,
                           method = c(
                             "waldcc", "wald", "clopper-pearson", "wilson",
                             "wilsoncc", "agresti-coull", "jeffreys",
                             "strat_wilson", "strat_wilsoncc"
                           ),
                           conf.level = 0.95,
                           statistic = c("{n} ({p}%)", "({conf.low}, {conf.high})"),
                           label = "Responders",
                           ci_label = NULL,
                           estimate_fun = label_roche_number(digits = 1, scale = 100)) {
  # check inputs ---------------------------------------------------------------
  set_cli_abort_call()
  check_not_missing(data)
  check_not_missing(variable)
  check_data_frame(data)
  check_string(variable)
  check_range(conf.level, range = c(0, 1))
  check_class(statistic, "character")
  check_class(estimate_fun, "function")
  method <- arg_match(method)
  check_scalar(by, allow_empty = TRUE)
  if (!is_empty(by)) {
    check_string(by)
    if (!by %in% names(data)) {
      cli::cli_abort(
        "The {.arg by} {.val {by}} is not a column in {.arg data}.",
        call = get_cli_abort_call()
      )
    }
  }
  if (length(statistic) != 2L) {
    cli::cli_abort(
      "The {.arg statistic} argument must be a length-2 character vector
       (estimate row, CI row).",
      call = get_cli_abort_call()
    )
  }
  if (!variable %in% names(data)) {
    cli::cli_abort(
      "The {.arg variable} {.val {variable}} is not a column in {.arg data}.",
      call = get_cli_abort_call()
    )
  }

  func_inputs <- as.list(environment())

  # default CI row label -------------------------------------------------------
  ci_label <- ci_label %||% .default_ci_label(conf.level, method)

  # compute ARDs ---------------------------------------------------------------
  by_arg <- if (is_empty(by)) NULL else by
  ard_n <- rlang::inject(
    cards::ard_categorical(data, variables = all_of(variable), by = any_of(!!by_arg))
  )
  ci_value <- if (is.null(value)) list() else stats::setNames(list(value), variable)
  ard_ci <- rlang::inject(cardx::ard_categorical_ci(
    data,
    variables = all_of(variable),
    by = any_of(!!by_arg),
    method = method,
    conf.level = conf.level,
    value = ci_value
  )) |>
    cards::update_ard_fmt_fun(
      stat_names = c("conf.low", "conf.high"),
      fmt_fun = estimate_fun
    )

  # levels to keep: success level for binary, all levels otherwise -------------
  keep_lvls <- if (is.null(value)) {
    ard_n |>
      dplyr::filter(.data$stat_name == "n") |>
      dplyr::pull("variable_level") |>
      unlist() |>
      as.character() |>
      unique()
  } else {
    as.character(value)
  }
  keep_lvls <- keep_lvls[!is.na(keep_lvls)]

  # combine and reshape: one continuous2 block per kept level ------------------
  ard <-
    dplyr::bind_rows(
      ard_n |> dplyr::filter(.data$stat_name %in% c("n", "p")),
      ard_ci |> dplyr::filter(.data$stat_name %in% c("conf.low", "conf.high"))
    ) |>
    dplyr::filter(!vapply(.data$variable_level, is.null, logical(1))) |>
    dplyr::mutate(.lvl = as.character(unlist(.data$variable_level))) |>
    dplyr::filter(.data$.lvl %in% keep_lvls) |>
    dplyr::mutate(
      variable = paste0(.env$variable, "__", .data$.lvl),
      variable_level = NULL,
      .lvl = NULL,
      context = "continuous"
    )

  # variable order and labels --------------------------------------------------
  var_order <- paste0(variable, "__", keep_lvls)
  lab_map <-
    if (is.null(value)) {
      user_lab <- if (is.list(label)) label else list()
      stats::setNames(
        lapply(keep_lvls, function(l) if (l %in% names(user_lab)) user_lab[[l]] else l),
        var_order
      )
    } else {
      stats::setNames(list(if (is.list(label)) label[[1]] else label), var_order)
    }

  # build gtsummary table ------------------------------------------------------
  # `continuous2` renders each block as a label row plus one row per statistic.
  # Collapse so the estimate sits on the label row and only the CI remains as an
  # indented child row, matching the response-summary layout.
  stat_cols <- NULL
  tbl <-
    rlang::inject(gtsummary::tbl_ard_summary(
      ard,
      by = !!by_arg,
      type = everything() ~ "continuous2",
      statistic = everything() ~ statistic,
      label = lab_map
    )) |>
    gtsummary::modify_table_body(function(.x) {
      stat_cols <<- grep("^stat_", names(.x), value = TRUE)
      .x |>
        dplyr::mutate(variable = factor(.data$variable, levels = var_order)) |>
        dplyr::arrange(.data$variable) |>
        dplyr::group_by(.data$variable) |>
        dplyr::group_modify(~ {
          lab_row <- .x[.x$row_type == "label", , drop = FALSE]
          est_row <- .x[.x$row_type == "level", , drop = FALSE][1, , drop = FALSE]
          ci_row <- .x[.x$row_type == "level", , drop = FALSE][2, , drop = FALSE]
          # move estimate onto the label row
          lab_row[stat_cols] <- est_row[stat_cols]
          # relabel the CI child row
          ci_row$label <- ci_label
          dplyr::bind_rows(lab_row, ci_row)
        }) |>
        dplyr::ungroup() |>
        dplyr::mutate(variable = as.character(.data$variable))
    })

  # populate per-column N so `(N = {n})` headers resolve after stacking --------
  tbl <- .set_proportion_header_n(tbl, data, by)

  # attach metadata ------------------------------------------------------------
  tbl$inputs <- func_inputs
  tbl$call_list <- list(tbl_proportion = match.call())
  tbl |>
    structure(class = c("tbl_proportion", class(tbl)))
}

# Add `modify_stat_n` (and total N) to the header so callers can use the
# standard `{n}` / `{N}` glue elements, e.g. "**{level}** (N = {n})".
.set_proportion_header_n <- function(tbl, data, by) {
  header <- tbl$table_styling$header
  if (!"modify_stat_n" %in% names(header)) header$modify_stat_n <- NA_integer_
  if (!"modify_stat_N" %in% names(header)) header$modify_stat_N <- NA_integer_
  stat_rows <- grepl("^stat_\\d+$", header$column)
  if (is_empty(by)) {
    n_by_col <- rep(nrow(data), sum(stat_rows))
  } else {
    counts <- table(factor(data[[by]]))
    lvl <- header$modify_stat_level[stat_rows]
    n_by_col <- as.integer(counts[lvl])
  }
  header$modify_stat_n[stat_rows] <- n_by_col
  header$modify_stat_N <- nrow(data)
  tbl$table_styling$header <- header
  tbl
}

# Build the default CI row label from the confidence level and CI method.
.default_ci_label <- function(conf.level, method) {
  pct <- style_roche_number(conf.level, scale = 100)
  method_lab <- switch(method,
    waldcc = "Wald, with correction",
    wald = "Wald",
    `clopper-pearson` = "Clopper-Pearson",
    wilson = "Wilson",
    wilsoncc = "Wilson, with correction",
    `agresti-coull` = "Agresti-Coull",
    jeffreys = "Jeffreys",
    strat_wilson = "stratified Wilson",
    strat_wilsoncc = "stratified Wilson, with correction",
    method
  )
  glue("{pct}% CI ({method_lab})")
}
