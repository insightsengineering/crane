#' AE Rates and Counts
#'
#' @description
#' A mix of adverse event rates (from `tbl_hierarchical()`) and counts
#' (from `tbl_hierarchical_count()`).
#'
#' @inheritParams gtsummary::tbl_hierarchical
#' @inheritParams gtsummary::add_overall.tbl_hierarchical
#' @param ae ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   A single column name with the adverse event terms.
#' @param soc ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   A single column name with the system organ class.
#' @param hlt ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   An _optional_ single column name with the higher level terms.
#' @param digits ([`formula-list-selector`][syntax])\cr
#'  Specifies how summary statistics are rounded. Values may be either integer(s) or function(s). If not specified,
#'  default formatting is assigned via `label_style_number()` for statistics `n` and `N`, and
#'  `label_style_number(digits=1, scale=100)` for statistic `p`.
#' @param filter,sort
#'   _Optional_ arguments passed to `gtsummary::filter_hierarchical(filter)`
#'   and `gtsummary::sort_hierarchical(sort)`.
#'   All filtering and sorting is applied to the _rates-related_ statistics, and
#'   not the counts.
#'
#' @returns a gtsummary table
#' @name tbl_ae_rate_and_count
#'
#' @examples
#' # Example 1 ----------------------------------
#' cards::ADAE |>
#'   tbl_ae_rate_and_count(
#'     denominator = cards::ADSL |> dplyr::rename(TRTA = TRT01A),
#'     by = TRTA,
#'     ae = AEDECOD,
#'     soc = AEBODSYS,
#'     # Keep rows where AEs across the row have an overall prevalence of greater than 10%
#'     filter = sum(n) / sum(N) > 0.10
#'   ) |>
#'   add_overall(last = TRUE)
NULL

#' @rdname tbl_ae_rate_and_count
#' @export
tbl_ae_rate_and_count <- function(data, ae, soc, denominator,
                                  hlt = NULL, by = NULL, id = "USUBJID",
                                  digits = everything() ~ list(
                                    n = label_style_number(),
                                    p = label_style_number(digits = 1, scale = 100)
                                  ),
                                  filter = NULL, sort = NULL) {
  # check inputs ---------------------------------------------------------------
  set_cli_abort_call()
  check_not_missing(data)
  check_not_missing(ae)
  check_not_missing(soc)
  check_not_missing(denominator)
  check_data_frame(data)
  filter <- enquo(filter)

  cards::process_selectors(data,
    ae = {{ ae }},
    soc = {{ soc }},
    hlt = {{ hlt }},
    by = {{ by }},
    id = {{ id }}
  )
  check_scalar(ae)
  check_scalar(soc)
  check_scalar(hlt, allow_empty = TRUE)
  check_scalar(by, allow_empty = TRUE)
  check_scalar(id)

  # saving function inputs
  tbl_ae_rate_and_count_inputs <- as.list(environment())

  # build AE rates table -------------------------------------------------------
  tbl_rates <-
    gtsummary::tbl_hierarchical(
      data = data,
      variables = all_of(c(soc, hlt, ae)),
      by = all_of(by),
      denominator = denominator,
      id = all_of(id),
      overall_row = TRUE,
      label = list(..ard_hierarchical_overall.. = "Total number of patients with at least one adverse event"),
      digits = digits
    ) |>
    gtsummary::remove_footnote_header()

  # apply filter or sort if specified by user
  if (!quo_is_null(filter)) {
    tbl_rates <- tbl_rates |>
      gtsummary::filter_hierarchical(filter = !!filter)
  }
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
      variables = all_of(c(soc, hlt, ae)),
      by = all_of(by),
      overall_row = TRUE,
      # this label needs to match tbl_rates. We update it later to say counts
      label = list(..ard_hierarchical_overall.. = "Total number of patients with at least one adverse event"),
      digits = tbl_rates$inputs$digits
    )

  # if a filter or sort occurred, merge in the tbl_rates$table_body, to put rows in same order
  if (!quo_is_null(filter) || !is.null(sort)) {
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
        dplyr::filter(!.data$variable %in% .env$ae)
    ) |>
    # relabel the overall counts row
    gtsummary::modify_table_body(
      ~ .x |>
        dplyr::mutate(
          label =
            ifelse(
              .data$label == "Total number of patients with at least one adverse event",
              "Overall total number of events",
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
          dplyr::select(table_body, -all_stat_cols()) |>
            dplyr::filter(.data$variable %in% c(.env$soc, .env$hlt)),
          # these are the rows with all the AE rates on them.
          #    the first row below the SOC/HLT header is renamed to "total number..."
          dplyr::mutate(
            table_body,
            .by = cards::all_ard_groups(),
            label =
              ifelse(
                .data$group1 %in% .env$soc & dplyr::row_number() == 1L,
                "Total number of patients with at least one adverse event",
                .data$label
              )
          ),
          # these are the rows with the counts. We only report the SOC/HLT counts
          tbl_count$table_body |>
            dplyr::mutate(
              label =
                ifelse(
                  .data$variable %in% c(.env$soc, .env$hlt),
                  "Total number of events",
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
      rows = .data$variable %in% .env$soc & .data$label %in% c("Total number of patients with at least one adverse event", "Total number of events")
    ) |>
    # indent the HLT overall stats (if not present, nothing will happen)
    gtsummary::modify_indent(
      columns = "label",
      rows = .data$variable %in% .env$hlt & .data$label %in% c("Total number of patients with at least one adverse event", "Total number of events"),
      indent = 8L
    ) |>
    # convert "0 (0.0%)" to "0"
    gtsummary::modify_post_fmt_fun(
      fmt_fun = ~ ifelse(. == "0 (0.0%)", "0", .),
      columns = all_stat_cols()
    )

  # return final table ---------------------------------------------------------
  tbl_final$call_list <- list(tbl_ae_rate_and_count = match.call())
  tbl_final$cards <-
    list(
      tbl_ae_rate_and_count =
        list(
          tbl_hierarchical = tbl_rates$cards$tbl_hierarchical,
          tbl_hierarchical_count = tbl_count$cards$tbl_hierarchical_count
        )
    )
  tbl_final$inputs <- tbl_ae_rate_and_count_inputs

  tbl_final |>
    structure(class = c("tbl_ae_rate_and_count", "gtsummary"))
}

#' @rdname tbl_ae_rate_and_count
#' @export
add_overall.tbl_ae_rate_and_count <- asNamespace("gtsummary")[["add_overall.tbl_hierarchical"]]
