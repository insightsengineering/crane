#' Hierarchical Exposure-Adjusted Incidence Rates
#'
#' @description
#' A wrapper function for [gtsummary::tbl_hierarchical()] to calculate exposure-adjusted
#' incidence rates of adverse events (or other clinical events) across a hierarchy.
#'
#' The function calculates the incidence rate per specified person-time dynamically.
#' For subjects experiencing an event, Person-Years is calculated from `start_date` to
#' `event_date`. For subjects without an event, it is calculated from `start_date` to `end_date`.
#'
#' @inheritParams gtsummary::tbl_hierarchical
#' @param variables ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   A character vector or tidy-selector of hierarchical columns in `data`
#'   (e.g., system organ class and preferred term).
#' @param by ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   A single column name in `data` to stratify the summary table by (e.g., treatment arm).
#' @param start_date ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   A column name in `denominator` specifying the treatment start date.
#' @param end_date ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   A column name in `denominator` specifying the treatment end date or follow-up cutoff.
#' @param event_date ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   A column name in `data` specifying the onset date of the event.
#' @param n_person_time (`numeric(1)`)\cr
#'   A numeric scalar multiplier to scale the incidence rate. Defaults to `100`.
#' @param unit_label (`string`)\cr
#'   Label for the unit of estimated person-time output. Defaults to `"years"`.
#' @param conf.level (`numeric(1)`)\cr
#'   Confidence level for the calculated interval. Default is `0.95`. Passed directly
#'   to [cardx::ard_incidence_rate()].
#' @param conf.type (`string`)\cr
#'   Confidence interval type. Default is `"normal"`. Passed directly to
#'   [cardx::ard_incidence_rate()].
#' @param digits (`numeric(1)`)\cr
#'   An integer specifying the number of decimal places to round the incidence
#'   estimates, confidence intervals, and person-years to. Defaults to `2`.
#'
#' @returns a gtsummary table of class `"tbl_hierarchical_incidence_rate"`.
#' @name tbl_hierarchical_incidence_rate
#'
#' @examples
#' # Dummy denominator dataset with treatment start and end dates
#' adsl <- data.frame(
#'   USUBJID = paste0("PT", sprintf("%02d", 1:5)),
#'   ARM = c("Treatment", "Treatment", "Placebo", "Placebo", "Placebo"),
#'   TRTSDT = as.Date(rep("2023-01-01", 5)),
#'   TRTEDT = as.Date(c("2023-12-31", "2023-06-30", "2023-12-31", "2023-08-15", "2023-10-31"))
#' )
#'
#' # Dummy AE dataset with onset dates (subset to first occurrences)
#' adae <- data.frame(
#'   USUBJID = c("PT02", "PT05", "PT05"),
#'   AESOC = c("Cardiac", "Nervous", "Cardiac"),
#'   ARM = c("Treatment", "Placebo", "Placebo"),
#'   AEDECOD = c("Tachycardia", "Headache", "Palpitations"),
#'   AESTDTC = c("2023-04-15", "2023-09-10", "2023-10-01")
#' )
#'
#' # Build the hierarchical incidence rate table
#' tbl_hierarchical_incidence_rate(
#'   data = adae,
#'   denominator = adsl,
#'   variables = c(AESOC, AEDECOD),
#'   by = ARM,
#'   start_date = TRTSDT,
#'   end_date = TRTEDT,
#'   event_date = AESTDTC,
#'   n_person_time = 100,
#'   unit_label = "years",
#'   label = list(
#'     AESOC = "MedDRA System Organ Class",
#'     AEDECOD = "MedDRA Preferred Term",
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
                                            start_date = TRTSDT,
                                            end_date = TRTEDT,
                                            event_date = AESTDTC,
                                            n_person_time = 100,
                                            unit_label = "years",
                                            conf.level = 0.95,
                                            conf.type = "normal",
                                            digits = 2,
                                            label = NULL) {
  # 1. Base input validation
  check_data_frame(data)
  check_data_frame(denominator)
  check_numeric(n_person_time)
  check_scalar(n_person_time)
  check_string(unit_label)
  check_numeric(conf.level)
  check_scalar(conf.level)
  check_string(conf.type)
  check_numeric(digits)
  check_scalar(digits)

  # 2. Extract the overall label BEFORE `cards` processes the dataset
  # `cards` will mess it up if you do it later
  overall_label <- "All Adverse Events"
  if (is.list(label) && "..ard_hierarchical_overall.." %in% names(label)) {
    overall_label <- label[["..ard_hierarchical_overall.."]]
  }

  # 3. Process Tidy-Select Arguments
  cards::process_selectors(
    data,
    variables = {{ variables }},
    by = {{ by }},
    id = {{ id }},
    event_date = {{ event_date }}
  )
  cards::process_selectors(
    denominator,
    start_date = {{ start_date }},
    end_date = {{ end_date }}
  )

  check_length(variables, 2)

  # 4. Auto-extract Labels for Dataset Columns
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
    gtsummary::sort_hierarchical() |>
    gtsummary::modify_header(
      gtsummary::all_stat_cols() ~ "No. of\nParticipants\nwith AE (%)"
    ) |>
    gtsummary::modify_table_body(~ .x |> dplyr::mutate(
      label = ifelse(
        dplyr::row_number() == 1,
        .env$overall_label,
        label
      )
    ))

  # 5. Generate ARDs  using the helper function and date variables
  ard_overall <- .ard_incidence_rate(
    data = data, denominator = denominator, id = id, by = by,
    start_date = start_date, end_date = end_date, event_date = event_date,
    n_person_time = n_person_time, unit_label = unit_label,
    conf.level = conf.level, conf.type = conf.type, digits = digits
  ) |>
    dplyr::mutate(variable = "..ard_hierarchical_overall..")

  ard_lvl1 <- .ard_incidence_rate(
    data = data, denominator = denominator, id = id, by = by,
    strata_vars = variables[1],
    start_date = start_date, end_date = end_date, event_date = event_date,
    n_person_time = n_person_time, unit_label = unit_label,
    conf.level = conf.level, conf.type = conf.type, digits = digits
  ) |>
    dplyr::select(-cards::all_ard_variables()) |>
    dplyr::rename(
      variable = dplyr::if_else(length(by) > 0, "group2", "group1"),
      variable_level = dplyr::if_else(
        length(by) > 0,
        "group2_level",
        "group1_level"
      )
    )

  ard_lvl2 <- .ard_incidence_rate(
    data = data, denominator = denominator, id = id, by = by,
    strata_vars = variables,
    start_date = start_date, end_date = end_date, event_date = event_date,
    n_person_time = n_person_time, unit_label = unit_label,
    conf.level = conf.level, conf.type = conf.type, digits = digits
  ) |>
    dplyr::select(-cards::all_ard_variables()) |>
    dplyr::rename(
      variable = dplyr::if_else(length(by) > 0, "group3", "group2"),
      variable_level = dplyr::if_else(
        length(by) > 0,
        "group3_level",
        "group2_level"
      )
    )

  ard_n <- cards::bind_ard(
    cards::ard_tabulate(denominator, variables = dplyr::any_of(by)),
    cardx::ard_total_n(denominator)
  )

  # Format the header labels based on the unit_label
  pt_abbr <- switch(tolower(unit_label),
    "years" = "PY",
    "months" = "PM",
    "days" = "PD",
    "weeks" = "PW",
    "time" = "PT",
    paste0("Person-", tools::toTitleCase(unit_label))
  )

  tbl_stat_labels <- list(
    "{n_events}" = "No. of\nAEs",
    "{tot_person_time}" = pt_abbr,
    "{estimate}" = paste0("AE Rate\nper\n", n_person_time, " ", pt_abbr),
    "({conf.low}, {conf.high})" = paste0(conf.level * 100, "% CI")
  )

  tbls_rates <- lapply(names(tbl_stat_labels), function(stat) {
    list(
      gtsummary::tbl_ard_summary(
        ard_overall,
        by = dplyr::all_of(by), statistic = ~stat
      ) |>
        gtsummary::modify_table_body(~ .x |> dplyr::mutate(
          row_type = "level",
          var_label = NA,
          label = .env$overall_label,
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
    gtsummary::remove_footnote_header(tidyselect::everything())
  tbl_final$call_list <- list(tbl_hierarchical_incidence_rate = match.call())
  tbl_final$inputs <- as.list(environment())

  tbl_final |> structure(
    class = c(
      "tbl_hierarchical_incidence_rate",
      "gtsummary"
    )
  )
}

#' Internal Helper for Incidence Rate ARDs
#' @keywords internal
#' @noRd
.ard_incidence_rate <- function(data,
                                denominator,
                                id,
                                by,
                                start_date,
                                end_date,
                                event_date,
                                strata_vars = NULL,
                                n_person_time = 100,
                                unit_label = "years",
                                conf.level = 0.95,
                                conf.type = "normal",
                                digits = 2) {
  # Data manipulation ----------------------------------------------------------
  if (!is.null(strata_vars) && length(strata_vars) > 0) {
    # expand the denominator to include all patients x events
    # to calculate time to event also for patients who do not have AE reported
    # End and start date are used for those
    unique_strata <- data |>
      dplyr::select(dplyr::all_of(strata_vars)) |>
      dplyr::distinct() |>
      dplyr::mutate(..dummy.. = 1L)

    expanded_denom <- denominator |>
      dplyr::mutate(..dummy.. = 1L) |>
      dplyr::full_join(
        unique_strata,
        by = "..dummy..",
        relationship = "many-to-many"
      ) |>
      dplyr::select(-"..dummy..")
  } else {
    expanded_denom <- denominator
  }

  grp_vars <- c(id, by, strata_vars)
  # Summarize events -----------------------------------------------------------
  # function to get the first date and format it
  safe_min_date <- function(x) {
    x_char <- as.character(x)
    x_char[x_char == "" | x_char == "NA"] <- NA_character_
    d <- as.Date(substr(x_char, 1, 10), format = "%Y-%m-%d")

    # If all dates are missing for this patient/group, return NA quietly
    if (all(is.na(d))) {
      return(as.Date(NA))
    }
    min(d, na.rm = TRUE)
  }

  ae_counts <- data |>
    dplyr::summarise(
      .by = dplyr::all_of(grp_vars),
      count = length(.data[[id]]),
      # Safely find the earliest valid date, returning NA if none exist
      ae_onset = safe_min_date(.data[[event_date]])
    )

  # Join counts onto the expanded grid and calculate Time-to-Event -------------
  merged_data <- expanded_denom |>
    dplyr::left_join(ae_counts, by = grp_vars) |>
    dplyr::mutate(
      count = dplyr::coalesce(count, 0L),
      # Parse start and end dates once to keep the if_else clean
      start_dt = as.Date(
        substr(
          as.character(.data[[start_date]]), 1, 10
        ),
        format = "%Y-%m-%d"
      ),
      end_dt = as.Date(
        substr(
          as.character(.data[[end_date]]), 1, 10
        ),
        format = "%Y-%m-%d"
      ),
      time_var = dplyr::if_else(
        # Check for valid dates that occur ON or AFTER the start date
        count > 0 & !is.na(ae_onset) & ae_onset >= start_dt,
        # Valid AE Date: Onset date - Start date + 1
        as.numeric(difftime(ae_onset, start_dt, units = "days") + 1) / 365.25,
        # No AE or Bad/Prior Date: End date - Start date + 1
        as.numeric(difftime(end_dt, start_dt, units = "days") + 1) / 365.25
      )
    )

  # Compute incidence rate -----------------------------------------------------
  merged_data |>
    cardx::ard_incidence_rate(
      time = "time_var",
      count = "count",
      by = dplyr::any_of(by),
      strata = dplyr::all_of(strata_vars),
      n_person_time = n_person_time,
      conf.level = conf.level,
      conf.type = conf.type,
      unit_label = unit_label
    ) |>
    dplyr::filter(
      stat_name %in% c(
        "n_events",
        "tot_person_time",
        "estimate",
        "conf.low",
        "conf.high"
      )
    ) |>
    cards::update_ard_fmt_fun(
      stat_names = c("estimate", "tot_person_time", "conf.low", "conf.high"),
      fmt_fun = crane::label_roche_number(digits = digits)
    )
}
