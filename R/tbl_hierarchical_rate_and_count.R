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
#' @inheritParams gtsummary::tbl_hierarchical
#' @inheritParams gtsummary::add_overall.tbl_hierarchical
#' @param variables ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Hierarchical variables to summarize. Must be 2 or 3 variables.
#'   Typical inputs are `c(AEBODSYS, AEDECOD)` for an SOC/AE summary
#'   or `c(AEBODSYS, AEHLT, AEDECOD)` for an SOC/HLT/AE summary.
#'
#'   Variables must be specified in the nesting order.
#' @param digits ([`formula-list-selector`][gtsummary::syntax])\cr
#'  Specifies how summary statistics are rounded. Values may be either integer(s) or function(s). If not specified,
#'  default formatting is assigned via `label_style_number()` for the `n` statistic and
#'  `label_roche_percent(digits=1)` for the `p` statistic.
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
#' @examples
#' # Example 1 ----------------------------------
#' cards::ADAE[c(1, 2, 3, 8, 16), ] |>
#'   tbl_hierarchical_rate_and_count(
#'     variables = c(AEBODSYS, AEDECOD),
#'     denominator = cards::ADSL,
#'     by = TRTA
#'   ) |>
#'   add_overall(last = TRUE)
NULL

#' @rdname tbl_hierarchical_rate_and_count
#' @export
tbl_hierarchical_rate_and_count <- function(data,
                                            variables,
                                            denominator,
                                            by = NULL,
                                            id = "USUBJID",
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
  if (!length(variables) %in% 2:3) {
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
      label = list(..ard_hierarchical_overall.. = label_overall_rate),
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
      # this label needs to match tbl_rates. We update it later to say counts
      label = list(..ard_hierarchical_overall.. = label_overall_rate),
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
    # indent the SOC overall stats
    gtsummary::modify_indent(
      columns = "label",
      rows = .data$variable %in% .env$variables[1] & .data$label %in% c(label_rate, label_count)
    ) |>
    # indent the HLT overall stats (if not present, nothing will happen)
    gtsummary::modify_indent(
      columns = "label",
      rows = .data$variable %in% .env$variables[-c(1L, length(.env$variables))] & .data$label %in% c(label_rate, label_count),
      indent = 8L
    ) |>
    # convert "0 (0.0%)" to "0"
    gtsummary::modify_post_fmt_fun(
      fmt_fun = ~ ifelse(. == "0 (0.0%)", "0", .),
      columns = gtsummary::all_stat_cols()
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
add_overall.tbl_hierarchical_rate_and_count <- function(x, last = FALSE, col_label = "All Participants  \nN = {style_number(N)}", ...) {
  do.call(
    what = asNamespace("gtsummary")[["add_overall.tbl_hierarchical"]],
    args = list(x = x, last = last, col_label = col_label)
  )
}
