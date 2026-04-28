#' Hierarchical Exposure-Adjusted Incidence Rates
#'
#' @description
#' A wrapper function for [gtsummary::tbl_hierarchical()] to calculate
#' exposure-adjusted incidence rates of adverse events (or other clinical
#' events) across a hierarchy.
#'
#' The function calculates the incidence rate per specified person-time
#' dynamically. For subjects experiencing an event, Person-Years is calculated
#' from `start_date` to `event_date`. For subjects without an event, it is
#' calculated from `start_date` to `end_date`.
#'
#' @inheritParams gtsummary::tbl_hierarchical
#' @param variables ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   A character vector or tidy-selector of hierarchical columns in `data`
#'   (e.g., system organ class and preferred term).
#' @param by ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   A single column name in `data` to stratify the summary table by
#'   (e.g., treatment arm).
#' @param start_date ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   A column name in `denominator` specifying the treatment start date.
#' @param end_date ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   A column name in `denominator` specifying the treatment end date or
#'   follow-up cutoff.
#' @param event_date ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   A column name in `data` specifying the onset date of the event.
#' @param event_type (`string`)\cr
#'   Type of the events to be counted. Can be `"first_event"` or `"all"`.
#'   Default is `"first_event"`.
#' @param n_person_time (`numeric(1)`)\cr
#'   A numeric scalar multiplier to scale the incidence rate. Defaults to `100`.
#' @param unit_label (`string`)\cr
#'   Label for the unit of estimated person-time output. Defaults to `"years"`.
#'   Known abbreviations ("years", "months", "weeks", "days") are parsed into
#'   standard acronyms (e.g., "PY"). Custom strings will be formatted to
#'   Title Case and prefixed with "Person-" (e.g., `"decades"` becomes
#'   `"Person-Decades"`).
#' @param conf.level (`numeric(1)`)\cr
#'   Confidence level for the calculated interval. Default is `0.95`. Passed
#'   directly to [cardx::ard_incidence_rate()].
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
#'   TRTEDT = as.Date(c(
#'     "2023-12-31", "2023-06-30", "2023-12-31", "2023-08-15", "2023-10-31"
#'   ))
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
                                            id = "USUBJID",
                                            start_date = "TRTSDT",
                                            end_date = "TRTEDT",
                                            event_date = "AESTDTC",
                                            event_type = c("first_event", "all"),
                                            n_person_time = 100,
                                            unit_label = "years",
                                            conf.level = 0.95,
                                            conf.type = "normal",
                                            digits = 2,
                                            label = NULL) {
  # 1. Base Input Validation (Missingness and standard classes)
  event_type <- rlang::arg_match(event_type)

  check_not_missing(data)
  check_not_missing(denominator)
  check_not_missing(variables)
  check_data_frame(data)
  check_data_frame(denominator)
  check_string(event_type)
  check_scalar(event_type)
  check_numeric(n_person_time)
  check_scalar(n_person_time)
  check_string(unit_label)
  check_numeric(conf.level)
  check_scalar(conf.level)
  check_string(conf.type)
  check_numeric(digits)
  check_scalar(digits)
  if (!is.null(label) && !is.list(label)) {
    cli::cli_abort("{.arg label} must be a list or NULL.")
  }

  # 2. Process Tidy-Select Arguments (Evaluates unquoted inputs to strings)
  cards::process_selectors(
    data,
    variables = {{ variables }}, by = {{ by }}, id = {{ id }},
    event_date = {{ event_date }}
  )
  cards::process_selectors(
    denominator,
    start_date = {{ start_date }}, end_date = {{ end_date }}
  )

  # 3. Post-Processing Validation (Strict lengths and dimensions)
  check_length(variables, 2)
  check_scalar(by, allow_empty = TRUE)
  check_scalar(id)
  check_scalar(start_date)
  check_scalar(end_date)
  check_scalar(event_date)

  # Save inputs and call arguments
  tbl_final <- list()
  tbl_final$call_list <- list(tbl_hierarchical_incidence_rate = match.call())
  tbl_final$inputs <- as.list(environment())

  # 4. Extract overall label BEFORE `cards` processes the dataset
  overall_label <- "All Adverse Events"
  if (is.list(label) && "..ard_hierarchical_overall.." %in% names(label)) {
    overall_label <- label[["..ard_hierarchical_overall.."]]
  }

  # 5. Auto-extract Labels for Dataset Columns
  cards::process_formula_selectors(data[variables], label = label)
  cards::fill_formula_selectors(
    data[variables],
    label = lapply(
      variables, \(x) attr(data[[x]], "label") %||% x
    ) |> stats::setNames(variables)
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
      label = dplyr::if_else(
        dplyr::row_number() == 1, .env$overall_label, .data$label
      )
    ))

  # 6. Generate ARDs cleanly using explicit piping
  ard_overall <- .prep_incidence_rate_data(
    data, denominator, id, by, start_date,
    end_date, event_date, event_type, NULL
  ) |>
    .compute_incidence_rate_ard(
      by, NULL, digits,
      n_person_time = n_person_time, unit_label = unit_label,
      conf.level = conf.level, conf.type = conf.type
    )

  ard_lvl1 <- .prep_incidence_rate_data(
    data, denominator, id, by, start_date,
    end_date, event_date, event_type, variables[1]
  ) |>
    .compute_incidence_rate_ard(
      by, variables[1], digits,
      n_person_time = n_person_time,
      unit_label = unit_label, conf.level = conf.level, conf.type = conf.type
    )

  ard_lvl2 <- .prep_incidence_rate_data(
    data, denominator, id, by, start_date,
    end_date, event_date, event_type, variables
  ) |>
    .compute_incidence_rate_ard(
      by, variables, digits,
      n_person_time = n_person_time,
      unit_label = unit_label, conf.level = conf.level, conf.type = conf.type
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
          row_type = "level", var_label = NA, label = .env$overall_label,
          group1 = "..ard_hierarchical_overall.."
        )),
      cards::bind_ard(ard_n, ard_lvl1, ard_lvl2) |>
        gtsummary::tbl_ard_hierarchical(
          by = dplyr::all_of(by), variables = dplyr::all_of(variables),
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

  class(tbl_final) <- c("tbl_hierarchical_incidence_rate", class(tbl_final))

  tbl_final
}

# ==============================================================================
# EXTERNAL HELPERS (Strictly split by single responsibility)
# ==============================================================================

#' Internal Helper 1: Data Preparation
#' @keywords internal
#' @noRd
.prep_incidence_rate_data <- function(data, denominator, id, by, start_date,
                                      end_date, event_date, event_type,
                                      strata_vars) {
  if (!is.null(strata_vars) && length(strata_vars) > 0) {
    unique_strata <- data |>
      dplyr::select(dplyr::all_of(strata_vars)) |>
      dplyr::distinct()

    expanded_denom <- denominator |> dplyr::cross_join(unique_strata)
  } else {
    expanded_denom <- denominator
  }

  grp_vars <- c(id, by, strata_vars)

  ae_counts <- data |>
    dplyr::summarise(
      .by = dplyr::all_of(grp_vars),
      count = length(.data[[id]]),
      ae_onset = .safe_min_date(.data[[event_date]])
    )

  expanded_denom |>
    dplyr::left_join(ae_counts, by = grp_vars) |>
    dplyr::mutate(
      count = dplyr::coalesce(.data$count, 0L),
      start_dt = .format_date(.data[[start_date]]),
      end_dt = .format_date(.data[[end_date]]),
      time_var = dplyr::if_else(
        event_type == "first_event" & .data$count > 0 & !is.na(.data$ae_onset) &
          .data$ae_onset >= .data$start_dt,
        (as.numeric(.data$ae_onset - .data$start_dt) + 1) / 365.24,
        (as.numeric(.data$end_dt - .data$start_dt) + 1) / 365.24
      )
    )
}

#' Internal Helper 2: ARD Computation and Column Renaming
#' @keywords internal
#' @noRd
.compute_incidence_rate_ard <- function(merged_data, by, strata_vars,
                                        digits, ...) {
  res <- merged_data |>
    cardx::ard_incidence_rate(
      time = "time_var", count = "count", by = dplyr::any_of(by),
      strata = dplyr::all_of(strata_vars), ...
    ) |>
    dplyr::filter(
      .data$stat_name %in% c(
        "n_events", "tot_person_time", "estimate", "conf.low", "conf.high"
      )
    ) |>
    cards::update_ard_fmt_fun(
      stat_names = c("estimate", "tot_person_time", "conf.low", "conf.high"),
      fmt_fun = crane::label_roche_number(digits = digits)
    )

  # Dynamically rename columns internally based on group levels
  if (is.null(strata_vars)) {
    res <- res |> dplyr::mutate(variable = "..ard_hierarchical_overall..")
  } else {
    grp_idx <- length(by) + length(strata_vars)
    rename_lookup <- c(
      "variable" = paste0("group", grp_idx),
      "variable_level" = paste0("group", grp_idx, "_level")
    )
    res <- res |>
      dplyr::select(-cards::all_ard_variables()) |>
      dplyr::rename(dplyr::all_of(rename_lookup))
  }

  res
}

#' Date parsing helper
#' @keywords internal
#' @noRd
.safe_min_date <- function(x) {
  x_char <- as.character(x)
  x_char[x_char == "" | x_char == "NA"] <- NA_character_
  d <- .format_date(x_char)

  if (all(is.na(d))) {
    return(as.Date(NA))
  }
  min(d, na.rm = TRUE)
}

#' Date formatting helper
#' @keywords internal
#' @noRd
.format_date <- function(x) {
  as.Date(substr(x, 1, 10), format = "%Y-%m-%d")
}
