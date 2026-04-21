#' Hierarchical Exposure-Adjusted Incidence Rates
#'
#' @description
#' A wrapper function for [gtsummary::tbl_hierarchical()] to calculate exposure-adjusted 
#' incidence rates of adverse events (or other clinical events) across a hierarchy.
#' 
#' The function calculates the incidence rate per specified person-years (default 100 PY)
#' using the total time at risk for each patient. Incidence estimates and confidence 
#' intervals are generated using [cardx::ard_incidence_rate()].
#'
#' @inheritParams gtsummary::tbl_hierarchical
#' @param variables ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   A character vector or tidy-selector of hierarchical columns in `data` 
#'   (e.g., system organ class and preferred term).
#' @param by ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   A single column name in `data` to stratify the summary table by (e.g., treatment arm).
#' @param time ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   A column name in `denominator` specifying the pre-calculated time-to-event 
#'   or total follow-up time (in years) for each subject.
#' @param event_count (`string`)\cr
#'   A string specifying the column name to use for the number of events internally 
#'   in the calculated ARD. Defaults to `"count"`.
#' @param rate_multiplier (`numeric(1)`)\cr
#'   A numeric scalar multiplier to scale the incidence rate. Defaults to `100` 
#'   (i.e., calculating events per 100 Person-Years).
#' @param digits (`numeric(1)`)\cr
#'   An integer specifying the number of decimal places to round the incidence 
#'   estimates, confidence intervals, and person-years to. Defaults to `2`.
#'
#' @details
#' Prior to passing data to this function, the event-level dataset (`data`) should 
#' typically be filtered to keep only the absolute first occurrence of any event for 
#' each subject at each level of the hierarchy. 
#' 
#' Furthermore, the time-to-event (`time`) must be pre-calculated and present in the 
#' `denominator` dataset. For subjects experiencing an event, this is typically the 
#' time from treatment start to event onset. For subjects without an event, it is 
#' the time from treatment start to treatment end, death, or study discontinuation.
#'
#' @returns a gtsummary table of class `"tbl_hierarchical_incidence_rate"`.
#' @name tbl_hierarchical_incidence_rate
#'
#' @examples
#' # Dummy datasets to demonstrate incidence rate calculation
#' adae <- data.frame(
#'   USUBJID = c("1", "1", "2", "3"),
#'   ARM = c("A", "A", "B", "B"),
#'   AESOC = c("SOC1", "SOC1", "SOC2", "SOC1"),
#'   AEDECOD = c("PT1", "PT2", "PT3", "PT1")
#' )
#'
#' adsl <- data.frame(
#'   USUBJID = c("1", "2", "3", "4"),
#'   ARM = c("A", "B", "B", "A"),
#'   tte = c(1.5, 0.5, 2.0, 1.0)
#' )
#'
#' # Example 1 ----------------------------------
#' tbl_hierarchical_incidence_rate(
#'   data = adae,
#'   denominator = adsl,
#'   variables = c(AESOC, AEDECOD),
#'   by = ARM,
#'   time = tte,
#'   rate_multiplier = 100,
#'   label = list(
#'     AESOC = "System Organ Class",
#'     AEDECOD = "Preferred Term",
#'     "..ard_hierarchical_overall.." = "All Adverse Events"
#'   )
#' )
#'
#' @export
tbl_hierarchical_incidence_rate <- function(data,
                                            denominator,
                                            variables,
                                            by = NULL,
                                            id = USUBJID,
                                            time = tte,
                                            event_count = "count",
                                            rate_multiplier = 100,
                                            digits = 2,
                                            label = NULL) {

  # 1. Base input validation
  check_data_frame(data)
  check_data_frame(denominator)
  check_string(event_count)
  check_numeric(rate_multiplier)
  check_scalar(rate_multiplier)
  check_numeric(digits)
  check_scalar(digits)

  # 2. Process Tidy-Select Arguments
  cards::process_selectors(
    data,
    variables = {{ variables }},
    by = {{ by }},
    id = {{ id }}
  )
  cards::process_selectors(
    denominator,
    time = {{ time }}
  )
  
  # Ensure the variables vector contains at least two hierarchical levels
  check_length(variables, 2)

  # 3. Auto-extract Labels
  cards::process_formula_selectors(data[variables], label = label)
  cards::fill_formula_selectors(
    data[variables],
    label = lapply(variables, \(x) attr(data[[x]], "label") %||% x) |> 
      stats::setNames(variables)
  )

  # Build the base hierarchical framework
  tbl_base <- gtsummary::tbl_hierarchical(
    data = data,
    by = dplyr::all_of(by),
    variables = dplyr::all_of(variables),
    id = dplyr::all_of(id),
    denominator = denominator,
    label = label,
    overall_row = TRUE
  ) |>
    gtsummary::sort_hierarchical()

  # 4. Closure wrapper for ARD generation
  calc_ard <- function(df_ae, strat_vars = NULL) {
    grp_vars <- c(id, by, strat_vars)

    df_ae |>
      dplyr::summarise(
        .by = dplyr::all_of(grp_vars),
        count = length(!!rlang::sym(id))
      ) |>
      dplyr::right_join(denominator, by = c(id, by)) |>
      dplyr::mutate(
        count = dplyr::coalesce(count, 0L),
        time_var = .data[[time]]
      ) |>
      cardx::ard_incidence_rate(
        time = "time_var",
        count = "count",
        by = dplyr::any_of(by),
        strata = dplyr::all_of(strat_vars),
        unit_label = "years"
      ) |>
      dplyr::filter(
        stat_name %in% c(
          "n_events", "tot_person_time", "estimate", "conf.low", "conf.high"
        )
      ) |>
      dplyr::mutate(
        stat = lapply(seq_along(stat), function(i) {
          if (stat_name[[i]] %in% c("estimate", "conf.low", "conf.high")) {
            stat[[i]] * rate_multiplier
          } else {
            stat[[i]]
          }
        })
      ) |>
      cards::update_ard_fmt_fun(
        stat_names = c("estimate", "tot_person_time", "conf.low", "conf.high"),
        fmt_fun = crane::label_roche_number(digits = digits)
      )
  }

  ard_overall <- calc_ard(data) |>
    dplyr::mutate(variable = "..ard_hierarchical_overall..")

  ard_lvl1 <- calc_ard(data, strat_vars = variables[1]) |>
    dplyr::select(-cards::all_ard_variables()) |>
    dplyr::rename(
      variable = dplyr::if_else(length(by) > 0, "group2", "group1"),
      variable_level = dplyr::if_else(
        length(by) > 0, "group2_level", "group1_level"
      )
    )

  ard_lvl2 <- calc_ard(data, strat_vars = variables) |>
    dplyr::select(-cards::all_ard_variables()) |>
    dplyr::rename(
      variable = dplyr::if_else(length(by) > 0, "group3", "group2"),
      variable_level = dplyr::if_else(
        length(by) > 0, "group3_level", "group2_level"
      )
    )

  ard_n <- cards::bind_ard(
    cards::ard_tabulate(denominator, variables = dplyr::any_of(by)),
    cardx::ard_total_n(denominator)
  )

  tbl_stat_labels <- list(
    "{n_events}" = "No. of\nAEs",
    "{tot_person_time}" = "PY",
    "{estimate}" = paste0("AE Rate\nper\n", rate_multiplier, " PY"),
    "({conf.low}, {conf.high})" = "95% CI"
  )

  tbls_rates <- lapply(names(tbl_stat_labels), function(stat) {
    list(
      gtsummary::tbl_ard_summary(
        ard_overall, by = dplyr::all_of(by), statistic = ~stat
      ) |>
        gtsummary::modify_table_body(~ .x |> dplyr::mutate(
          row_type = "level",
          var_label = NA,
          label = label[["..ard_hierarchical_overall.."]],
          group1 = "..ard_hierarchical_overall.."
        )),
      cards::bind_ard(ard_n, ard_lvl1, ard_lvl2) |>
        gtsummary::tbl_ard_hierarchical(
          by = dplyr::all_of(by),
          variables = dplyr::all_of(variables),
          statistic = ~stat
        )
    ) |>
      gtsummary::tbl_stack(attr_order = 2:1, quiet = TRUE) |>
      gtsummary::modify_header(
        gtsummary::all_stat_cols() ~ tbl_stat_labels[[stat]]
      )
  })

  tbl_final <- c(list(tbl_base), tbls_rates) |>
    gtsummary::tbl_merge(tab_spanner = FALSE) |>
    gtsummary::modify_table_body(\(x) {
      col_order <- sort(names(dplyr::select(x, gtsummary::all_stat_cols())))
      dplyr::relocate(x, dplyr::all_of(col_order), .after = "label")
    }) |>
    gtsummary::remove_footnote_header(tidyselect::everything()) |>
    # 5. Clean up "Zero" display to match rate_by_grade functionality
    gtsummary::modify_post_fmt_fun(
      fmt_fun = ~ ifelse(
        . %in% c("0.00", "0 (0.00, 0.00)", "0 (NA, NA)", "0.0"), "0", .
      ),
      columns = gtsummary::all_stat_cols()
    )

  # 6. Preserve function call and environment for gtsummary compatibility
  tbl_final$call_list <- list(tbl_hierarchical_incidence_rate = match.call())
  tbl_final$inputs <- as.list(environment())
  
  tbl_final |>
    structure(class = c("tbl_hierarchical_incidence_rate", "gtsummary"))
}