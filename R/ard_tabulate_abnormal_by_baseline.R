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
  # 1. Capture names and process selectors
  postbaseline_name <- as_label(enquo(postbaseline))
  baseline_name <- as_label(enquo(baseline))
  # id_name <- as_label(enquo(id))

  cards::process_selectors(
    data,
    postbaseline = {{ postbaseline }},
    baseline = {{ baseline }},
    id = {{ id }},
    by = {{ by }},
    strata = {{ strata }}
  )

  if (nrow(data) == 0) {
    return(NULL)
  }

  # There was an lapply statement before that had a bug
  # I changed it to for loop cause the lapply thing was harder to read
  current_gp <- as.character(unique(data[[postbaseline_name]]))
  names_present <- c(rep(FALSE, length(abnormal)))
  for (i in seq_along(abnormal)) {
    names_present[[i]] <- any(current_gp %in% abnormal[[i]])
  }

  valid_abnormal_names <- names(abnormal)[names_present]

  stats_for_tbl <- map(valid_abnormal_names, function(abn_name) {
    abn_val <- abnormal[[abn_name]]
    # Tier: Not [Abnormal] at baseline
    res_not_abn <- data |>
      dplyr::filter(!(.data[[baseline_name]] %in% abn_val) & !is.na(.data[[baseline_name]])) |>
      .calc_abnormal_logic(paste("Not", abn_name), abn_val, postbaseline_name, id, by)

    # Tier: [Abnormal] at baseline
    res_is_abn <- data |>
      dplyr::filter(.data[[baseline_name]] %in% abn_val & !is.na(.data[[baseline_name]])) |>
      .calc_abnormal_logic(abn_name, abn_val, postbaseline_name, id, by)

    # Tier: Total
    res_total <- data |>
      .calc_abnormal_logic("Total", abn_val, postbaseline_name, id, by)

    cards::bind_ard(res_not_abn, res_is_abn, res_total) |>
      dplyr::mutate(variable = abn_name)
  }) |>
    cards::bind_ard() |>
    dplyr::mutate(
      context = "abnormal_by_baseline",
      stat_label = dplyr::coalesce(.data$stat_label, .data$stat_name)
    ) |>
    # Final cleanup of types
    dplyr::mutate(dplyr::across(
      dplyr::any_of(c("variable", "variable_level", "context", "stat_name", "stat_label", "level")),
      as.character
    )) |>
    cards::as_card(check = FALSE)
  class(stats_for_tbl$variable_level) <- "list"

  stats_for_tbl
}
