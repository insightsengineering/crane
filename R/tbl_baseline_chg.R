#' Change from Baseline
#'
#' Typical use is tabulating changes from baseline
#' measurement of an Analysis Variable.
#' @inheritParams tbl_roche_summary
#' @inheritParams gtsummary::add_overall
#' @param analysis_variable (`string`)\cr
#'  String identifying the analysis values. Default is `'AVAL'`.
#' @param change_variable (`string`)\cr
#'  String identifying the change from baseline values. Default is `'CHG'`.
#' @param id (`string`)\cr
#'  String identifying the unique subjects. Default is `'USUBJID'`.
#' @param visit (`string`)\cr
#'  String for the visit variable. Default is
#'  `'AVISIT'`. If there are more than one entry for each visit and subject,
#'  only the first row is kept.
#' @param visit_number (`string`)\cr
#'  String identifying the visit or analysis sequence number. Default is
#'  `'AVISITN'`.
#' @param baseline_level (`string`)\cr
#'  String identifying baseline level in the `visit` variable.
#' @param denominator (`string`)\cr
#'  Data set used to compute the header counts (typically `ADSL`).
#'
#' @return A gtsummary table.
#' @name tbl_baseline_chg
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true") || identical(Sys.getenv("IN_PKGDOWN"), "true")
#' theme_gtsummary_roche()
#'
#' df <- cards::ADLB |>
#'   dplyr::mutate(AVISIT = trimws(AVISIT)) |>
#'   dplyr::filter(
#'     AVISIT != "End of Treatment",
#'     PARAMCD %in% c("SODIUM", "K")
#'   )
#'
#' tbl_baseline_chg(
#'   data = df |> dplyr::filter(PARAMCD == "SODIUM"),
#'   baseline_level = "Baseline",
#'   by = "TRTA",
#'   denominator = cards::ADSL
#' )
#'
#' tbl_baseline_chg(
#'   data = df |> dplyr::filter(PARAMCD == "K"),
#'   baseline_level = "Baseline",
#'   by = "TRTA",
#'   denominator = cards::ADSL
#' ) |>
#'   add_overall(last = TRUE, col_label = "All Participants")
#'
#' # Split by PARAM
#' tbl_strata(
#'   data = df,
#'   strata = PARAMCD,
#'   .tbl_fun = ~ tbl_baseline_chg(
#'     data = .x,
#'     baseline_level = "Baseline",
#'     by = "TRTA",
#'     denominator = cards::ADSL
#'   ),
#'   .combine_with = "tbl_stack",
#'   .combine_args = list(group_header = NULL, quiet = TRUE)
#' ) |>
#' tbl_split_by_rows(variable_level = ends_with("lbl"))
#'
NULL

#' @rdname tbl_baseline_chg
#' @export
tbl_baseline_chg <- function(data,
                             baseline_level,
                             denominator,
                             by = NULL,
                             digits = NULL,
                             id = "USUBJID",
                             visit = "AVISIT",
                             visit_number = "AVISITN",
                             analysis_variable = "AVAL",
                             change_variable = "CHG") {
  set_cli_abort_call()

  # check inputs ---------------------------------------------------------------
  check_not_missing(data)
  check_not_missing(denominator)

  # ---- Type and content checks ----
  check_data_frame(data)
  check_data_frame(denominator)
  check_string(baseline_level)
  check_not_missing(id)
  check_not_missing(visit)
  check_not_missing(visit_number)
  check_not_missing(analysis_variable)
  check_not_missing(change_variable)
  cards::process_selectors(
    data,
    by = {{ by }}, id = {{ id }}, visit = {{ visit }}, visit_number = {{ visit_number }},
    analysis_variable = {{ analysis_variable }}, change_variable = {{ change_variable }}
  )
  check_scalar(by, allow_empty = TRUE)
  check_scalar(id)
  check_scalar(visit)
  check_scalar(visit_number)
  check_scalar(analysis_variable)
  check_scalar(change_variable)

  # Check that `baseline_level` is one of the visit values
  if (!(baseline_level %in% data[[visit]])) {
    cli::cli_abort("The {.arg baseline_level} {.val {baseline_level}} is not found in the {.val {visit}} variable.")
  }
  tbl_baseline_inputs <- as.list(environment())

  # build summary table -----------------------------------------------------
  # if there is a `by` variable, make it a factor to ensure all levels appear in tbls
  if (!is_empty(by) && !is.factor(data[[by]])) {
    cli::cli_inform(c("i" = "Converting column {.val {by}} to a factor."))
    old_by_label <- attr(data[[by]], "label")
    data[[by]] <- factor(data[[by]])
    attr(data[[by]], "label") <- old_by_label
  }

  # warn if there are multiple entries per visit per subject
  if (anyDuplicated(data[c(id, visit)]) > 0L) {
    cli::cli_abort(
      c("Columns {.val {c(id, visit)}} do not uniquely identify the rows in {.arg data}.",
        i = "See row number {.val {anyDuplicated(data[c(id, visit)])}}."
      ),
      call = get_cli_abort_call()
    )
  }

  df_change_baseline <-
    # filter lab results data
    data |>
    dplyr::arrange(id, visit_number) |>
    dplyr::mutate(
      visit = fct_reorder(.data[[visit]], .data[[visit_number]])
    ) |>
    tidyr::pivot_wider(
      id_cols = all_of(c(id, by)),
      names_from = visit,
      values_from = all_of(c(analysis_variable, change_variable)),
      names_sort = TRUE
    ) |>
    # add in denominator for the header Ns
    dplyr::right_join(
      denominator[c(id, by)],
      by = c(id, by),
      relationship = "many-to-one"
    )

  # Build results tables ----------------------------------------------------
  # Summary of AVAL
  tbl_aval <-
    df_change_baseline |>
    dplyr::select(all_of(by), starts_with(analysis_variable)) |>
    dplyr::rename_with(~ str_remove(., paste0("^", analysis_variable, "_"))) %>%
    # after reshape all column labels are the same, so changing them to the variable name
    labelled::remove_var_label() |>
    tbl_roche_summary(
      by = any_of(by),
      nonmissing = "always", # include the non-missing count in summary
      # round mean/sd/median/min/max,
      type = everything() ~ "continuous2",
      digits = digits
    )

  # Building a table change values at each visit
  tbl_chg <-
    df_change_baseline |>
    dplyr::select(all_of(by), starts_with(change_variable)) |>
    dplyr::rename_with(~ str_remove(., paste0("^", change_variable, "_"))) %>%
    # after reshape all column labels are the same, so changing them to the variable name
    labelled::remove_var_label() |>
    # using `tbl_roche_summary()` as the default continuous variable summary matches our spec
    tbl_roche_summary(
      by = any_of(by),
      nonmissing = "always", # include the non-missing count in summary
      # round mean/sd/median/min/max
      type = everything() ~ "continuous2",
      digits = digits,
      include = everything() & !all_of(baseline_level) # Remove the baseline visit from summary
    )

  # Merge tables together
  baseline_chg_tbl <-
    list(tbl_aval, tbl_chg) |>
    gtsummary::tbl_merge(tab_spanner = FALSE, quiet = TRUE) |>
    gtsummary::modify_header(
      gtsummary::all_stat_cols() & ends_with("_1") ~ "Value at Visit",
      gtsummary::all_stat_cols() & ends_with("_2") ~ "Change from Baseline",
      label = "Visit"
    ) |>
    gtsummary::modify_spanning_header(gtsummary::all_stat_cols() ~ "{level}  \n(N = {n})") |>
    # sort the stat columns together within treatment group
    gtsummary::modify_table_body(
      \(.x) {
        stat_cols <- dplyr::select(.x, gtsummary::all_stat_cols()) |>
          names() |>
          sort()
        dplyr::relocate(.x, all_of(stat_cols), .after = "label")
      }
    )

  # return tbl -----------------------------------------------------------------
  baseline_chg_tbl[["call_list"]] <- list(tbl_baseline_chg = match.call())
  baseline_chg_tbl$inputs <- tbl_baseline_inputs
  # styler: off
  baseline_chg_tbl$cards$tbl_baseline_chg <-
    cards::bind_ard(
      gtsummary::gather_ard(tbl_aval)$tbl_summary %>%
        {case_switch(
          !"variable_level" %in% names(.) ~ dplyr::mutate(., variable_level = list(NULL), .after = "variable"),
          .default = .
        )} |>
        dplyr::mutate(
          variable_level =
            ifelse(
              !.data$variable %in% c(by, "..ard_total_n.."),
              as.list(.data$variable),
              .data$variable_level
            ),
          variable =
            ifelse(
              !.data$variable %in% c(by, "..ard_total_n.."),
              analysis_variable,
              .data$variable
            )
        ),
      gtsummary::gather_ard(tbl_chg)$tbl_summary %>%
        {case_switch(
          !"variable_level" %in% names(.) ~ dplyr::mutate(., variable_level = list(NULL), .after = "variable"),
          .default = .
        )} |>
        dplyr::mutate(
          variable_level =
            ifelse(
              !.data$variable %in% c(by, "..ard_total_n.."),
              as.list(.data$variable),
              .data$variable_level
            ),
          variable =
            ifelse(
              !.data$variable %in% c(by, "..ard_total_n.."),
              change_variable,
              .data$variable
            )
        ),
      .update = TRUE,
      .quiet = TRUE
    )
  # styler: on
  baseline_chg_tbl |>
    structure(class = c("tbl_baseline_chg", "gtsummary"))
}

#' @rdname tbl_baseline_chg
#' @export
add_overall.tbl_baseline_chg <- function(x,
                                         last = FALSE, col_label = "All Participants  \n(N = {style_roche_number(n)})", ...) {
  # check inputs ---------------------------------------------------------------
  set_cli_abort_call()
  check_dots_empty(call = get_cli_abort_call())
  check_scalar_logical(last)
  if (is_empty(x$inputs$by)) {
    cli::cli_inform(
      c("Original table was not stratified, and overall columns cannot be added.",
        i = "Table has been returned unaltered."
      )
    )
    return(x)
  }

  # build overall table --------------------------------------------------------
  tbl_overall <-
    x$inputs |>
    utils::modifyList(list(by = NULL)) |>
    (\(args_list) do.call("tbl_baseline_chg", args = args_list))() |>
    gtsummary::modify_spanning_header(gtsummary::all_stat_cols() ~ col_label)

  # check the tbls have the same structure before merging
  if (!identical(
    dplyr::select(x$table_body, any_of(c("label0", "label"))),
    dplyr::select(tbl_overall$table_body, any_of(c("label0", "label")))
  )) {
    cli::cli_inform(
      c("!" = "The structures of the original table and the overall table are not identical,
         and the resulting table may be malformed.")
    )
  }

  # merge tables ---------------------------------------------------------------
  merged_tbl <- if (isTRUE(last)) {
    gtsummary::tbl_merge(
      tbls = list(x, tbl_overall),
      tab_spanner = FALSE,
      merge_vars = c("variable", "row_type", "var_label", "label0", "label"),
      quiet = TRUE
    )
  } else {
    gtsummary::tbl_merge(
      tbls = list(tbl_overall, x),
      tab_spanner = FALSE,
      merge_vars = c("variable", "row_type", "var_label", "label0", "label"),
      quiet = TRUE
    )
  }

  # correct ARD structure
  merged_tbl[["cards"]] <-
    list(
      tbl_baseline_chg = x$cards$tbl_baseline_chg,
      add_overall = tbl_overall$cards$tbl_baseline_chg
    )

  merged_tbl |>
    structure(class = class(x))
}
