#' ARD Tabulate Abnormality by Baseline Status
#'
#' @description
#' This function creates an Analysis Results Data (ARD) object counting participants
#' with abnormal assessments, stratified by their baseline status. For each abnormality
#' (e.g., "Low", "High"), it calculates statistics for three tiers:
#' 1. Patients **Not Abnormal** at baseline.
#' 2. Patients **Abnormal** at baseline.
#' 3. **Total** (all patients with a post-baseline assessment).
#'
#' @param data (`data.frame`)\cr
#'   A data frame containing the clinical results.
#' @param postbaseline ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Column name of the post-baseline reference range indicator (e.g., `ANRIND`).
#' @param baseline ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Column name of the baseline reference range indicator (e.g., `BNRIND`).
#' @param abnormal (`list`)\cr
#'   A named list of abnormalities (e.g., `list(Low = c("LOW", "LOW LOW"))`).
#'   The name is used as the level label, and the vector contains the values in
#'   `postbaseline` and `baseline` that define the abnormality.
#' @param id ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Column name for the subject identifier. Defaults to `"USUBJID"`.
#' @param by ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Optional column names for grouping variables (e.g., `TRTA`).
#' @param strata ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Optional column names for additional stratification.
#'
#' @return An ARD data frame of class 'card'.
#' @export
#'
#' @examples
#' # Example usage with ADLB-like data
#' adlb <- cards::ADLB
#' ard_tabulate_abnormal_by_baseline(
#'   data = adlb,
#'   postbaseline = LBNRIND,
#'   baseline = BNRIND,
#'   abnormal = list(Low = "LOW", High = "HIGH"),
#'   by = TRTA
#' ) %>% tbl_ard_summary(by = TRTA)
ard_tabulate_abnormal_by_baseline <- function(data,
                                              postbaseline,
                                              baseline,
                                              abnormal,
                                              id = "USUBJID",
                                              by = NULL,
                                              strata = NULL) {
  postbaseline_quo <- enquo(postbaseline)
  postbaseline_name <- as_label(postbaseline_quo)

  cards::process_selectors(
    data,
    postbaseline = {{ postbaseline }},
    baseline = {{ baseline }},
    id = {{ id }},
    by = {{ by }},
    strata = {{ strata }}
  )

  if (!is.list(abnormal) || is.null(names(abnormal)) || any(names(abnormal) == "")) {
    cli::cli_abort(
      "{.arg abnormal} must be a named list (e.g., {.code list(Low = 'LOW')}).",
      call = get_cli_abort_call()
    )
  }

  for (col in c(postbaseline, baseline)) {
    if (is.numeric(data[[col]])) {
      cli::cli_abort(
        "Column {.var {col}} cannot be numeric. It must be a character or factor indicator.",
        call = get_cli_abort_call()
      )
    }
  }

  # 5. Check if 'abnormal' levels exist in the data (Warning only)
  all_data_levels <- unique(c(as.character(data[[postbaseline]]), as.character(data[[baseline]])))
  all_abn_levels <- unlist(abnormal)
  missing_levels <- setdiff(all_abn_levels, all_data_levels)

  if (length(missing_levels) > 0) {
    cli::cli_warn(
      "The following levels in {.arg abnormal} were not found in the data: {.val {missing_levels}}"
    )
  }

  # 6. Check for Overlap in Logic
  # It's confusing if the same value is defined as both "Low" and "High"
  if (length(all_abn_levels) != length(unique(all_abn_levels))) {
    cli::cli_inform(
      "Note: Some levels appear in multiple {.arg abnormal} categories."
    )
  }

  # 2. Internal Calculation Helper
  calc_logic <- function(df, group_label, abn_val) {
    if (nrow(df) == 0) {
      return(NULL)
    }

    cards::ard_mvsummary(
      data = df,
      variables = all_of(postbaseline),
      by = any_of(by),
      statistic = ~ list(
        abnormal = \(x, data, ...) {
          n_abn <- data |>
            dplyr::filter(.data[[postbaseline]] %in% abn_val) |>
            dplyr::pull(all_of(id)) |>
            dplyr::n_distinct()

          N_total <- data |>
            dplyr::pull(all_of(id)) |>
            dplyr::n_distinct()

          dplyr::tibble(n = n_abn, N = N_total, p = n / N)
        }
      )
    ) |>
      # FORCE level and variable_level to character strings.
      # This fixes the 'Can't combine list and character' error.
      dplyr::mutate(
        variable_level = as.character(group_label),
        level = as.character(group_label)
      )
  }

  current_gp <- unique(data[[postbaseline_name]])

  # 3. Loop through abnormalities (Low, High)
  map(names(abnormal)[unlist(lapply(abnormal, function(x) {
    current_gp %in% x
  }))], function(abn_name) {
    abn_val <- abnormal[[abn_name]]

    # Tier: Not [Abnormal] at baseline
    res_not_abn <- data |>
      dplyr::filter(!(.data[[baseline]] %in% abn_val) | is.na(.data[[baseline]])) |>
      calc_logic(paste("Not", abn_name), abn_val)

    # Tier: [Abnormal] at baseline
    res_is_abn <- data |>
      dplyr::filter(.data[[baseline]] %in% abn_val) |>
      calc_logic(abn_name, abn_val)

    # Tier: Total
    res_total <- data |>
      calc_logic(paste("Total"), abn_val)

    cards::bind_ard(res_not_abn, res_is_abn, res_total)
  }) |>
    cards::bind_ard() |>
    dplyr::mutate(
      context = "abnormal_by_baseline",
      # stat_label = "n/N (%)"
      stat_label = dplyr::coalesce(.data$stat_label, .data$stat_name),
    ) |>
    # FINAL STEP: Ensure all ARD indexing columns are characters
    # This ensures compatibility with gtsummary's internal bind_rows calls
    dplyr::mutate(dplyr::across(
      dplyr::any_of(c("variable", "variable_level", "context", "stat_name", "stat_label", "level")),
      as.character
    )) |>
    cards::as_card(check = FALSE) -> ret
  class(ret$variable_level) <- "list"
  return(ret)
}
