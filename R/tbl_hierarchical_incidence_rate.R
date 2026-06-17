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
#' @param overall_row (`logical(1)`)\cr
#'   Whether to include an overall summary row aggregating across all
#'   hierarchical levels. Default is `TRUE`. The overall row label can be
#'   customized via `label = list("..ard_hierarchical_overall.." = "Custom Label")`.
#' @param spanning_label (`string`)\cr
#'   A [glue][glue::glue]-style template for the per-arm spanning headers when
#'   `by` is supplied. Available substitutions are `{level}` (the arm level) and
#'   `{n}` (the arm participant count). Default is `"{level}  \\n(N = {n})"`,
#'   matching the column-header convention used elsewhere in the package.
#' @param x (`tbl_hierarchical_incidence_rate`)\cr
#'   A table created with [tbl_hierarchical_incidence_rate()].
#' @param last (`logical(1)`)\cr
#'   When `add_overall()` is used, whether to place the overall columns after
#'   (`TRUE`) or before (`FALSE`) the stratified columns. Default is `FALSE`.
#' @param col_label (`string`)\cr
#'   A [glue][glue::glue]-style template for the overall spanning header added by
#'   `add_overall()`. Resolved against the column header context (e.g. `{N}`,
#'   `{n}`). Default is `"All Participants  \\n(N = {style_roche_number(N)})"`.
#' @param ... These dots are for future extensions and must be empty.
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
#' # Custom overall row label
#' tbl_hierarchical_incidence_rate(
#'   data = adae,
#'   denominator = adsl,
#'   variables = c(AESOC, AEDECOD),
#'   by = ARM,
#'   start_date = TRTSDT,
#'   end_date = TRTEDT,
#'   event_date = AESTDTC,
#'   label = list("..ard_hierarchical_overall.." = "Any Adverse Event")
#' )
#'
#' # Without the overall row
#' tbl_hierarchical_incidence_rate(
#'   data = adae,
#'   denominator = adsl,
#'   variables = c(AESOC, AEDECOD),
#'   by = ARM,
#'   start_date = TRTSDT,
#'   end_date = TRTEDT,
#'   event_date = AESTDTC,
#'   overall_row = FALSE
#' )
#'
#' # Add an unstratified "Overall" column pooling all treatment arms
#' tbl_hierarchical_incidence_rate(
#'   data = adae,
#'   denominator = adsl,
#'   variables = c(AESOC, AEDECOD),
#'   by = ARM,
#'   start_date = TRTSDT,
#'   end_date = TRTEDT,
#'   event_date = AESTDTC
#' ) |>
#'   add_overall()
#'
#' # Customize the per-arm spanning headers with a glue template
#' tbl_hierarchical_incidence_rate(
#'   data = adae,
#'   denominator = adsl,
#'   variables = c(AESOC, AEDECOD),
#'   by = ARM,
#'   start_date = TRTSDT,
#'   end_date = TRTEDT,
#'   event_date = AESTDTC,
#'   spanning_label = "{level} (N = {n})"
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
                                            overall_row = TRUE,
                                            spanning_label = "{level}  \n(N = {n})",
                                            label = NULL) {
  set_cli_abort_call()
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
  check_scalar_logical(overall_row)
  check_string(spanning_label)
  if (!is.null(label) && !is.list(label)) {
    cli::cli_abort("{.arg label} must be a list or NULL.", call = get_cli_abort_call())
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

  # Save inputs and call arguments for downstream methods (e.g. add_overall())
  inputs <- as.list(environment())

  # Extract overall label BEFORE `cards` processes the dataset
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
    overall_row = overall_row
  ) |>
    gtsummary::sort_hierarchical() |>
    gtsummary::modify_header(
      gtsummary::all_stat_cols() ~ "No. of\nParticipants\nwith AE (%)"
    )

  if (overall_row) {
    # `tbl_hierarchical(overall_row = TRUE)` generates a default label for the
    # overall row; override it with the user-specified label (if any).
    tbl_base <- tbl_base |>
      gtsummary::modify_table_body(~ .x |> dplyr::mutate(
        label = dplyr::if_else(
          dplyr::row_number() == 1, .env$overall_label, .data$label
        )
      ))

    # Generate the overall (unstratified-across-hierarchy) ARD
    ard_overall <- .prep_incidence_rate_data(
      data, denominator, id, by, start_date,
      end_date, event_date, event_type, NULL
    ) |>
      .compute_incidence_rate_ard(
        by, NULL, digits,
        n_person_time = n_person_time, unit_label = unit_label,
        conf.level = conf.level, conf.type = conf.type
      )
  }

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

  ard_n <- cardx::ard_total_n(denominator)
  if (!is.null(by)) {
    ard_n_by <- rlang::exec(cards::ard_tabulate, data = denominator, variables = by)
    ard_n <- cards::bind_ard(ard_n_by, ard_n)
  }
  ard_hierarchical_combined <- cards::bind_ard(ard_n, ard_lvl1, ard_lvl2)

  class(ard_hierarchical_combined) <- c(
    "ard_stack_hierarchical",
    class(ard_hierarchical_combined)
  )
  attr(ard_hierarchical_combined, "args") <- list(variables = variables, by = by)

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
    tbl_hier <- gtsummary::tbl_ard_hierarchical(
      cards = ard_hierarchical_combined,
      by = dplyr::all_of(by),
      variables = dplyr::all_of(variables),
      statistic = ~stat
    )

    if (overall_row) {
      tbl_overall_stat <- gtsummary::tbl_ard_summary(
        ard_overall,
        by = dplyr::all_of(by), statistic = ~stat
      ) |>
        gtsummary::modify_table_body(~ .x |> dplyr::mutate(
          row_type = "level", var_label = NA, label = .env$overall_label,
          group1 = "..ard_hierarchical_overall.."
        ))

      tbl_hier <- list(tbl_overall_stat, tbl_hier) |>
        gtsummary::tbl_stack(attr_order = 2:1, quiet = TRUE)
    }

    tbl_hier |>
      gtsummary::modify_header(
        gtsummary::all_stat_cols() ~ tbl_stat_labels[[stat]]
      )
  })

  # Extract arm-to-column mapping before merge for spanning headers
  arm_header_map <- tbl_base$table_styling$header |>
    dplyr::filter(grepl("^stat_", .data$column)) |>
    dplyr::select("column", "label", "modify_stat_level", "modify_stat_n")

  tbl_final <- c(list(tbl_base), tbls_rates) |>
    gtsummary::tbl_merge(tab_spanner = FALSE) |>
    gtsummary::modify_table_body(\(x) {
      col_order <- sort(names(dplyr::select(x, gtsummary::all_stat_cols())))
      dplyr::relocate(x, dplyr::all_of(col_order), .after = "label")
    }) |>
    gtsummary::remove_footnote_header(tidyselect::everything())

  # Apply per-arm spanning headers. The label is a glue template resolved with
  # `level` (arm level) and `n` (arm participant count), e.g. "Placebo  \n(N = 3)".
  if (!is.null(by) && nrow(arm_header_map) > 0L) {
    spanning_formulas <- lapply(seq_len(nrow(arm_header_map)), function(i) {
      arm_idx <- sub("^stat_", "", arm_header_map$column[i])
      prefix <- paste0("stat_", arm_idx, "_")
      spanner_label <- glue::glue_data(
        list(
          level = arm_header_map$modify_stat_level[i],
          n = arm_header_map$modify_stat_n[i]
        ),
        spanning_label
      )
      rlang::new_formula(
        lhs = rlang::expr(tidyselect::starts_with(!!prefix)),
        rhs = rlang::expr(!!as.character(spanner_label))
      )
    })

    tbl_final <- rlang::inject(
      gtsummary::modify_spanning_header(tbl_final, !!!spanning_formulas)
    )
  }

  # Save inputs and call arguments for downstream methods (e.g. add_overall())
  tbl_final$inputs <- inputs
  tbl_final$call_list <- list(tbl_hierarchical_incidence_rate = match.call())

  class(tbl_final) <- c("tbl_hierarchical_incidence_rate", class(tbl_final))

  tbl_final
}

#' @rdname tbl_hierarchical_incidence_rate
#' @export
add_overall.tbl_hierarchical_incidence_rate <- function(
    x,
    last = FALSE,
    col_label = "All Participants  \n(N = {style_roche_number(N)})",
    ...) {
  # check inputs ---------------------------------------------------------------
  set_cli_abort_call()
  check_dots_empty(call = get_cli_abort_call())
  check_string(col_label)
  check_scalar_logical(last)

  if (is_empty(x$inputs[["by"]])) {
    cli::cli_inform(
      c("Original table was not stratified, and overall columns cannot be added.",
        i = "Table has been returned unaltered."
      )
    )
    return(x)
  }

  # build overall table --------------------------------------------------------
  # rebuild with `by = NULL`; the unstratified table has a single panel set
  # (stat_0_1 .. stat_0_5) that we splice into the stratified table.
  args_overall <- utils::modifyList(x$inputs, list(by = NULL), keep.null = TRUE)
  tbl_overall <- do.call(tbl_hierarchical_incidence_rate, args_overall)

  overall_stat_cols <- grep("^stat_", names(tbl_overall$table_body), value = TRUE)

  # check the tbls have the same structure before merging
  if (!identical(x$table_body$label, tbl_overall$table_body$label)) {
    cli::cli_inform(
      c("!" = "The structures of the original table and the overall table are not
         identical, and the resulting table may be malformed.")
    )
  }

  # rename overall columns to stat_0_1 .. stat_0_n
  new_col_names <- paste0("stat_0_", seq_along(overall_stat_cols))
  overall_body <- tbl_overall$table_body |>
    dplyr::select(dplyr::all_of(overall_stat_cols)) |>
    stats::setNames(new_col_names)

  x$table_body <- dplyr::bind_cols(x$table_body, overall_body)

  # copy header styling for the overall columns, renaming to stat_0_*
  overall_header <- tbl_overall$table_styling$header |>
    dplyr::filter(grepl("^stat_", .data$column))
  overall_header$column <- new_col_names
  overall_header$spanning_header <- NA_character_

  x$table_styling$header <- dplyr::bind_rows(x$table_styling$header, overall_header)

  # spanning header for overall columns -- `col_label` is a glue template
  # resolved by gtsummary against the column header context (`N`, `n`, ...).
  x <- gtsummary::modify_spanning_header(
    x,
    tidyselect::starts_with("stat_0_") ~ col_label
  )

  # reorder columns: overall first unless `last = TRUE` --------------------------
  if (isTRUE(last)) {
    x <- x |>
      gtsummary::modify_table_body(\(body) {
        overall_cols <- grep("^stat_0_", names(body), value = TRUE)
        dplyr::relocate(body, dplyr::all_of(overall_cols), .after = dplyr::last_col())
      })
  } else {
    x <- x |>
      gtsummary::modify_table_body(\(body) {
        stat_cols <- sort(grep("^stat_", names(body), value = TRUE))
        dplyr::relocate(body, dplyr::all_of(stat_cols), .after = "label")
      })
  }

  # correct ARD structure ------------------------------------------------------
  # the table is a `tbl_merge` of several panels, so ARDs live in `$tbls`
  # (no single `$cards` slot). Append the overall panels' inner tables so
  # `gather_ard()` recurses into the overall ARDs as well.
  x$tbls <- c(
    x$tbls,
    stats::setNames(
      tbl_overall$tbls,
      paste0("add_overall_", seq_along(tbl_overall$tbls))
    )
  )

  x$call_list <- c(x$call_list, list(add_overall = match.call()))
  x
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
  res <- rlang::exec(
    cardx::ard_incidence_rate,
    data = merged_data,
    time = "time_var",
    count = "count",
    by = by,
    strata = strata_vars,
    ...
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
    grp_name <- paste0("group", grp_idx)
    grp_lvl <- paste0("group", grp_idx, "_level")

    res <- res |>
      dplyr::mutate(
        variable = .data[[grp_name]],
        variable_level = .data[[grp_lvl]]
      ) |>
      dplyr::select(-dplyr::all_of(c(grp_name, grp_lvl)))
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
