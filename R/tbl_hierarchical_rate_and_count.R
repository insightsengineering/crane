#' Hierarchical Rates and Counts
#'
#' @description
#' A mix of adverse event rates (from `gtsummary::tbl_hierarchical()`) and counts
#' (from `gtsummary::tbl_hierarchical_count()`).
#' The function produces additional summary rows for the higher level nesting
#' variables providing both rates and counts.
#'
#' When a hierarchical summary is filtered, the summary rows no longer provide
#' useful/consistent information.
#' When creating a filtered summary, use `gtsummary::tbl_hierarchical()` or
#' `gtsummary::tbl_hierarchical_count()` directly, followed by a call to
#' `gtsummary::filter_hierarchical()`.
#'
#' @details
#' ## Factor levels and zero-rows
#'
#' When the first variable in `variables` is a factor, the function respects
#' its levels and emits zero-rows for any levels not observed in the data.
#' Each unobserved level gets a header row (with `NA` stats), a rate summary
#' row (`"0"`), and a count summary row (`"0"`).
#'
#' This is useful when the set of expected categories is predefined but some
#' may not be present in the data. To exclude categories with no observations,
#' drop unused levels before calling the function (e.g., `droplevels()`).
#'
#' Ensures consistent output structure for empty datasets, removing the
#' need for manual workarounds in reporting templates.
#'
#' @inheritParams gtsummary::tbl_hierarchical
#' @inheritParams gtsummary::add_overall.tbl_hierarchical
#' @param variables ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Hierarchical variables to summarize. Must be 2 or 3 variables.
#'   Typical inputs are `c(AEBODSYS, AEDECOD)` for an SOC/AE summary
#'   or `c(AEBODSYS, AEHLT, AEDECOD)` for an SOC/HLT/AE summary.
#'
#'   Variables must be specified in the nesting order.
#' @param digits ([`formula-list-selector`][gtsummary::syntax])\cr
#'  Specifies how summary statistics are rounded. Values may be either integer(s) or function(s).
#'  If a theme is applied, the `digits` specifications of the theme is applied.
#' @param label_overall_rate (`string`)\cr
#'  String for the overall rate summary. Default is
#'  `"Total number of participants with at least one adverse event"`.
#' @param label_overall_count (`string`)\cr
#'  String for the overall count summary. Default is
#'  `"Overall total number of events"`.
#' @param label_rate (`string`)\cr
#'  String for the rate summary. Default is `"Overall total number of events"`.
#'  `"Total number of participants with at least one adverse event"`.
#' @param label_count (`string`)\cr
#'  String for the overall count summary. Default is `"Total number of events"`.
#' @param sort
#'   _Optional_ arguments passed to `gtsummary::sort_hierarchical(sort)`.
#' @param x (`tbl_hierarchical_rate_and_count`)\cr
#'   a stratified 'tbl_hierarchical_rate_and_count' table
#'
#' @returns a gtsummary table
#' @name tbl_hierarchical_rate_and_count
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true")
#' # Example 1 ----------------------------------
#' cards::ADAE[c(1, 2, 3, 8, 16), ] |>
#'   tbl_hierarchical_rate_and_count(
#'     variables = c(AEBODSYS, AEDECOD),
#'     denominator = cards::ADSL,
#'     by = TRTA
#'   ) |>
#'   add_overall(last = TRUE)
#'
#' # Example 2: factor with unobserved levels ----------------------------------
#' # Adding an unobserved SOC level produces zero-rows automatically
#' cards::ADAE[c(1, 2, 3, 8, 16), ] |>
#'   dplyr::mutate(
#'     AEBODSYS = factor(AEBODSYS, levels = c(unique(AEBODSYS), "UNOBSERVED SOC"))
#'   ) |>
#'   tbl_hierarchical_rate_and_count(
#'     variables = c(AEBODSYS, AEDECOD),
#'     denominator = cards::ADSL,
#'     by = TRTA
#'   )
NULL

#' @rdname tbl_hierarchical_rate_and_count
#' @export
tbl_hierarchical_rate_and_count <- function(data,
                                            variables,
                                            denominator,
                                            by = NULL,
                                            id = "USUBJID",
                                            label = NULL,
                                            digits = NULL,
                                            sort = NULL,
                                            label_overall_rate = "Total number of participants with at least one adverse event",
                                            label_overall_count = "Overall total number of events",
                                            label_rate = "Total number of participants with at least one adverse event",
                                            label_count = "Total number of events") {
  # check inputs ---------------------------------------------------------------
  set_cli_abort_call()
  check_not_missing(data)
  check_not_missing(variables)
  check_not_missing(denominator)
  check_data_frame(data)
  cards::process_selectors(
    data,
    variables = {{ variables }},
    by = {{ by }},
    id = {{ id }}
  )
  if (!length(variables) %in% seq(2, 3)) {
    msg_pt1 <- "The {.arg variables} argument must select 2 or 3 columns"
    if (length(variables) == 1L) {
      cli::cli_abort(
        c(msg_pt1,
          i = "For a single variable summary, use {.code gtsummary::hierarchical(variables={.val {variables}})}"
        )
      )
    }
    cli::cli_abort(
      c(msg_pt1,
        i = "Columns select are typically {.code c(AEBODSYS, AEDECOD)} or {.code c(AEBODSYS, AEHLT, AEDECOD)}"
      )
    )
  }
  check_scalar(by, allow_empty = TRUE)
  check_scalar(id)

  # saving function inputs
  tbl_hierarchical_rate_and_count_inputs <- as.list(environment())

  # handle 0-row data with factor levels ---------------------------------------
  # gtsummary::tbl_hierarchical() cannot process empty data. When the first
  # variable is a factor we build a scaffold table with zero-rows for all levels.
  if (nrow(data) == 0L && is.factor(data[[variables[1]]])) {
    return(
      .build_empty_scaffold(
        data = data,
        variables = variables,
        by = by,
        denominator = denominator,
        label_overall_rate = label_overall_rate,
        label_overall_count = label_overall_count,
        label_rate = label_rate,
        label_count = label_count,
        label = label,
        inputs = tbl_hierarchical_rate_and_count_inputs
      )
    )
  }

  # process labels -------------------------------------------------------------
  df_variables <- data[variables] |> dplyr::mutate(..ard_hierarchical_overall.. = data[[variables[1]]])

  # add a default label to the overall row - we update it later to say counts
  attr(df_variables[["..ard_hierarchical_overall.."]], "label") <- label_overall_rate

  cards::process_formula_selectors(df_variables, label = label)

  # fill in unspecified labels
  cards::fill_formula_selectors(
    df_variables,
    label = lapply(names(df_variables), \(x) attr(df_variables[[x]], "label") %||% x) |> stats::setNames(names(df_variables))
  )

  # build AE rates table -------------------------------------------------------
  tbl_rates <-
    gtsummary::tbl_hierarchical(
      data = data,
      variables = all_of(variables),
      include = all_of(variables),
      by = all_of(by),
      denominator = denominator,
      id = all_of(id),
      overall_row = TRUE,
      label = label,
      digits = digits
    ) |>
    gtsummary::remove_footnote_header()

  # apply sort if specified by user
  if (!is.null(sort)) {
    tbl_rates <- tbl_rates |>
      gtsummary::sort_hierarchical(sort = sort)
  }

  # finally, add the row numbers
  tbl_rates <- gtsummary::modify_table_body(tbl_rates, ~ dplyr::mutate(.x, ord = dplyr::row_number()))

  # build AE counts table ------------------------------------------------------
  tbl_count <-
    gtsummary::tbl_hierarchical_count(
      data = data,
      variables = all_of(variables),
      include = all_of(variables),
      by = all_of(by),
      overall_row = TRUE,
      label = label,
      digits = tbl_rates$inputs$digits
    )

  # if a sort occurred, merge in the tbl_rates$table_body, to put rows in same order
  if (!is.null(sort)) {
    # get the merge variables
    merge_vars <- tbl_rates$table_body |>
      dplyr::select(cards::all_ard_groups(), cards::all_ard_variables()) |>
      names() |>
      union(c("row_type", "label"))

    # merge in the tbl_rates to remove removed rows/re-order rows to match tbl_rates
    tbl_count$table_body <-
      dplyr::left_join(
        tbl_rates$table_body[merge_vars],
        tbl_count$table_body,
        by = merge_vars
      )
  }

  tbl_count <- tbl_count |>
    # save row order and remove AE level counts
    gtsummary::modify_table_body(
      ~ .x |>
        dplyr::mutate(
          ord = dplyr::row_number()
        ) |>
        dplyr::filter(!.data$variable %in% rev(.env$variables)[1])
    ) |>
    # relabel the overall counts row
    gtsummary::modify_table_body(
      ~ .x |>
        dplyr::mutate(
          label =
            ifelse(
              .data$label == label_overall_rate,
              label_overall_count,
              .data$label
            )
        )
    )


  # combine rates and counts into single table ---------------------------------
  tbl_final <-
    tbl_rates |>
    gtsummary::modify_table_body(
      \(table_body) {
        dplyr::bind_rows(
          # these are the blank rows with the SOC/HLT label
          dplyr::select(table_body, -gtsummary::all_stat_cols()) |>
            dplyr::filter(.data$variable %in% rev(.env$variables)[-1]),
          # these are the rows with all the AE rates on them.
          #    the first row below the SOC/HLT header is renamed to `label_rate`
          dplyr::mutate(
            table_body,
            .by = cards::all_ard_groups(),
            label =
              ifelse(
                .data$group1 %in% .env$variables[1] & dplyr::row_number() == 1L,
                label_rate,
                .data$label
              )
          ),
          # these are the rows with the counts. We only report the SOC/HLT counts
          tbl_count$table_body |>
            dplyr::mutate(
              label =
                ifelse(
                  .data$variable %in% rev(.env$variables)[-1],
                  label_count,
                  .data$label
                )
            )
        ) |>
          dplyr::arrange(.data$ord)
      }
    ) |>
    # indent rate/count summary rows under each hierarchical variable
    .apply_hierarchical_indent(variables, label_rate, label_count) |>
    # convert "0 (0.0%)" to "0"
    modify_zero_recode()

  # append zero-rows for unobserved factor levels ------------------------------
  # When variables[1] is a factor, emit zero-rows for levels with no events.
  # gtsummary::tbl_hierarchical() drops unobserved strata levels, so we
  # re-inject them here to keep the output complete.
  tbl_final <- .inject_unobserved_levels(
    tbl_final,
    data = data,
    variables = variables,
    label_rate = label_rate,
    label_count = label_count
  )

  # return final table ---------------------------------------------------------
  tbl_final$call_list <- list(tbl_hierarchical_rate_and_count = match.call())
  tbl_final$cards <-
    list(
      tbl_hierarchical_rate_and_count =
        list(
          tbl_hierarchical = tbl_rates$cards$tbl_hierarchical,
          tbl_hierarchical_count = tbl_count$cards$tbl_hierarchical_count
        )
    )
  tbl_final$inputs <- tbl_hierarchical_rate_and_count_inputs

  tbl_final |>
    structure(class = c("tbl_hierarchical_rate_and_count", "gtsummary")) |>
    modify_header_rm_md()
}

#' @rdname tbl_hierarchical_rate_and_count
#' @export
add_overall.tbl_hierarchical_rate_and_count <- function(x,
                                                        last = FALSE,
                                                        col_label = "All Participants  \n(N = {style_roche_number(N)})", ...) {
  do.call(
    what = asNamespace("gtsummary")[["add_overall.tbl_hierarchical"]],
    args = list(x = x, last = last, col_label = col_label)
  )
}

# Inject zero-rows for unobserved factor levels in the first hierarchical
# variable. Returns the table unchanged if variables[1] is not a factor or
# all levels are already present.
#' @keywords internal
.inject_unobserved_levels <- function(tbl,
                                      data,
                                      variables,
                                      label_rate,
                                      label_count) {
  top_var <- variables[1L]
  col_vals <- data[[top_var]]

  # only act when the column is a factor with defined levels

  if (!is.factor(col_vals)) {
    return(tbl)
  }

  observed <- unique(as.character(col_vals))
  all_levels <- levels(col_vals)
  missing_levels <- setdiff(all_levels, observed)

  if (length(missing_levels) == 0L) {
    return(tbl)
  }

  cli::cli_inform(
    c("i" = "Adding zero-rows for {length(missing_levels)} unobserved level{?s} in {.val {top_var}}.")
  )

  stat_cols <- grep("^stat_", names(tbl$table_body), value = TRUE)

  # build zero-rows: header + rate + count per missing level
  zero_rows <- map(missing_levels, \(lvl) {
    header <- stats::setNames(
      rep(NA_character_, length(stat_cols)), stat_cols
    )
    summary_row <- stats::setNames(
      rep("0", length(stat_cols)), stat_cols
    )

    dplyr::bind_rows(
      # SOC/basket header row
      tibble::tibble(
        row_type = "level",
        group1 = top_var,
        group1_level = lvl,
        var_label = NA_character_,
        variable = top_var,
        label = lvl,
        !!!header
      ),
      # rate summary row
      tibble::tibble(
        row_type = "level",
        group1 = top_var,
        group1_level = lvl,
        var_label = NA_character_,
        variable = top_var,
        label = label_rate,
        !!!summary_row
      ),
      # count summary row
      tibble::tibble(
        row_type = "level",
        group1 = top_var,
        group1_level = lvl,
        var_label = NA_character_,
        variable = top_var,
        label = label_count,
        !!!summary_row
      )
    )
  }) |>
    dplyr::bind_rows()

  # assign ord values that place zero-rows after existing rows
  max_ord <- max(tbl$table_body$ord, na.rm = TRUE)
  zero_rows$ord <- seq(max_ord + 1L, length.out = nrow(zero_rows))

  tbl |>
    gtsummary::modify_table_body(~ dplyr::bind_rows(.x, zero_rows))
}

# Build a scaffold table when data has 0 rows but variables[1] is a factor.
# Produces a gtsummary object with correct column headers and zero-rows for
# every factor level.
#' @keywords internal
.build_empty_scaffold <- function(data,
                                  variables,
                                  by,
                                  denominator,
                                  label_overall_rate,
                                  label_overall_count,
                                  label_rate,
                                  label_count,
                                  label,
                                  inputs) {
  top_var <- variables[1L]
  all_levels <- levels(data[[top_var]])

  # determine stat columns from denominator
  if (length(by) > 0L) {
    stat_cols <- paste0("stat_", seq_along(unique(denominator[[by]])))
  } else {
    stat_cols <- "stat_0"
  }

  zero_val <- stats::setNames(rep("0", length(stat_cols)), stat_cols)
  na_val <- stats::setNames(rep(NA_character_, length(stat_cols)), stat_cols)

  # overall rate row
  rows <- list(
    tibble::tibble(
      row_type = "level",
      group1 = "..ard_hierarchical_overall..",
      group1_level = NA_character_,
      var_label = NA_character_,
      variable = "..ard_hierarchical_overall..",
      label = label_overall_rate,
      !!!zero_val
    )
  )

  # overall count row (unless "remove")
  if (!identical(label_overall_count, "remove")) {
    rows <- c(rows, list(
      tibble::tibble(
        row_type = "level",
        group1 = "..ard_hierarchical_overall..",
        group1_level = NA_character_,
        var_label = NA_character_,
        variable = "..ard_hierarchical_overall..",
        label = label_overall_count,
        !!!zero_val
      )
    ))
  }

  # per-level rows: header + rate + count
  for (lvl in all_levels) {
    rows <- c(rows, list(
      tibble::tibble(
        row_type = "level",
        group1 = top_var,
        group1_level = lvl,
        var_label = NA_character_,
        variable = top_var,
        label = lvl,
        !!!na_val
      ),
      tibble::tibble(
        row_type = "level",
        group1 = top_var,
        group1_level = lvl,
        var_label = NA_character_,
        variable = top_var,
        label = label_rate,
        !!!zero_val
      ),
      tibble::tibble(
        row_type = "level",
        group1 = top_var,
        group1_level = lvl,
        var_label = NA_character_,
        variable = top_var,
        label = label_count,
        !!!zero_val
      )
    ))
  }

  body <- dplyr::bind_rows(rows)
  body$ord <- seq_len(nrow(body))


  # build scaffold from tbl_summary to get correct column headers with N counts.
  # The body is replaced entirely, so the summarized variable is irrelevant.
  # Use the first column of the denominator as a throwaway include variable.
  include_var <- names(denominator)[1L]
  tbl_scaffold <- rlang::inject(
    gtsummary::tbl_summary(
      data = denominator,
      include = dplyr::all_of(include_var),
      by = !!if (length(by) > 0L) by else NULL
    )
  ) |>
    gtsummary::modify_table_body(\(x) body)

  # process user-supplied labels for the header
  df_labels <- data[variables]
  cards::process_formula_selectors(df_labels, label = label)
  cards::fill_formula_selectors(
    df_labels,
    label = lapply(
      names(df_labels),
      \(x) attr(df_labels[[x]], "label") %||% x
    ) |>
      stats::setNames(names(df_labels))
  )

  header_label <- paste0(
    label[[variables[1L]]],
    "  \n",
    paste0(rep("\U00A0", 4L), collapse = ""),
    label[[variables[length(variables)]]]
  )

  tbl_scaffold <- tbl_scaffold |>
    gtsummary::modify_header(label ~ header_label) |>
    .apply_hierarchical_indent(variables, label_rate, label_count,
      include_variable_indent = TRUE
    )

  tbl_scaffold$call_list <- list(tbl_hierarchical_rate_and_count = match.call())
  tbl_scaffold$cards <- list(tbl_hierarchical_rate_and_count = list())
  tbl_scaffold$inputs <- inputs

  tbl_scaffold |>
    structure(class = c("tbl_hierarchical_rate_and_count", "gtsummary")) |>
    modify_header_rm_md()
}

# Apply hierarchical indentation to rate/count summary rows.
#
# This helper centralises the indent logic for `tbl_hierarchical_rate_and_count()`
# so that every code path (normal build, 0-row scaffold, injected zero-rows)
# produces identical indentation.
#
# The indentation scheme has two layers:
#
# 1. **Per-variable indent** (set by `gtsummary:::brdg_hierarchical()`):
#    Each variable's rows are indented at `(depth - 1) * 4` spaces, where
#    `depth` is the 1-based position in `variables`. For a 3-variable
#    hierarchy `c(SOC, HLT, PT)`:
#      - SOC labels:  0 spaces  (depth 1)
#      - HLT labels:  4 spaces  (depth 2)
#      - PT  labels:  8 spaces  (depth 3)
#    The overall row (`..ard_hierarchical_overall..`) is always at 0.
#    In the normal path, `brdg_hierarchical()` already sets these rules.
#    The scaffold path skips `brdg_hierarchical()`, so it needs them
#    explicitly <U+2014> controlled by `include_variable_indent = TRUE`.
#
# 2. **Rate/count summary indent** (set by this function, always):
#    The rate and count summary rows inserted by
#    `tbl_hierarchical_rate_and_count()` sit one level deeper than their
#    parent variable's label. The indent is `depth * 4`:
#      - SOC rate/count:  4 spaces  (depth 1)
#      - HLT rate/count:  8 spaces  (depth 2)
#    The last variable (e.g., PT) has no rate/count rows <U+2014> it is the leaf.
#    This generalises to any number of hierarchical levels.
#
# @param tbl A gtsummary table object.
# @param variables Character vector of hierarchical variable names.
# @param label_rate,label_count Label strings identifying rate/count rows.
# @param include_variable_indent If `TRUE`, also apply the per-variable
#   indent rules (layer 1). Set to `TRUE` when the table was not built via
#   `gtsummary::tbl_hierarchical()` (e.g., the 0-row scaffold path).
#   The normal path leaves this `FALSE` because `brdg_hierarchical()` has
#   already set those rules.
# @returns The table with indent styling applied.
# @keywords internal
.apply_hierarchical_indent <- function(tbl,
                                       variables,
                                       label_rate,
                                       label_count,
                                       include_variable_indent = FALSE) {
  # Layer 1: per-variable indent (mirrors brdg_hierarchical loop)
  if (include_variable_indent) {
    for (i in seq_along(variables)) {
      tbl <- gtsummary::modify_indent(
        tbl,
        columns = "label",
        rows = .data$variable == !!variables[i],
        indent = (i - 1L) * 4L
      )
    }
    tbl <- gtsummary::modify_indent(
      tbl,
      columns = "label",
      rows = .data$variable == "..ard_hierarchical_overall..",
      indent = 0L
    )
  }

  # Layer 2: rate/count summary rows one level deeper than their variable.
  # Only non-leaf variables (all except the last) have rate/count rows.
  non_leaf <- variables[-length(variables)]
  for (i in seq_along(non_leaf)) {
    tbl <- gtsummary::modify_indent(
      tbl,
      columns = "label",
      rows = .data$variable == !!non_leaf[i] &
        .data$label %in% c(.env$label_rate, .env$label_count),
      indent = i * 4L
    )
  }

  tbl
}
