#' Subgroup Analyses
#'
#' Function adapted from `gtforester::tbl_subgroups()`.
#'
#' @inheritParams gtsummary::tbl_strata
#' @importFrom cards ard_tabulate_value
#' @param rsp ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Variable to use in responder rate calculations.
#' @param by ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Variable to make comparison between groups.
#' @param subgroups ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Variables to perform stratified analyses for.
#' @param time_to_event ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Variable to use in time-to-event analyses. If specified, the mid table will
#'   show the median time-to-event instead of responder rates.
#'
#' @returns a 'gtsummary' table
#'
#' @examples
#' # prepare sample data
#' df_adtte <- data.frame(
#'   time = rexp(10, rate = 0.1),
#'   status = sample(c(0, 1), 10, replace = TRUE),
#'   arm = sample(c("Arm A", "Arm B"), 10, replace = TRUE),
#'   grade = sample(c("I", "II"), 10, replace = TRUE),
#'   strata = sample(c("1", "2"), 10, replace = TRUE)
#' ) |>
#'   mutate(arm = relevel(factor(arm), ref = "Arm A")) # Set Reference
#'
#' # logistic regression -------------------------------------------------------
#' df_adtte |>
#'   tbl_roche_subgroups(
#'     rsp = "status",
#'     by = "arm",
#'     subgroups = c("grade"),
#'     .tbl_fun =
#'       ~ glm(status ~ arm, data = .x) |>
#'         tbl_regression(
#'          show_single_row = arm,
#'          exponentiate = TRUE #, tidy_fun = broom.helpers::tidy_parameters
#'        )
#'  ) |>
#'  modify_header(starts_with("estimate") ~ "**Odds Ratio**")
#'
#' # coxph regression ----------------------------------------------------------
#' # please use browser() inside .tbl_fun to check if the coxph model throws an error
#' # and use tryCatch to modify the input/output accordingly
#' df_adtte |>
#'   tbl_roche_subgroups(
#'     rsp = status,
#'     by = arm,
#'     time_to_event = time, # Specify time variable for time-to-event analyses (different mid table)
#'     subgroups = c(grade, strata),
#'     ~ survival::coxph( # Please use coxph for time-to-event analyses
#'       survival::Surv(time, status) ~ arm,
#'       data = .x,
#'       ties = "exact"
#'     ) |> # Exact Ties
#'       tbl_regression(
#'         show_single_row = arm,
#'         exponentiate = TRUE # Get Hazard Ratios
#'       )
#'   ) |>
#'   modify_header(starts_with("estimate") ~ "**Hazard Ratio**")
#'
#' @export
tbl_roche_subgroups <- function(data, rsp, by, subgroups, .tbl_fun, time_to_event = NULL) {
  set_cli_abort_call()

  # check inputs ---------------------------------------------------------------
  check_not_missing(data)
  check_not_missing(rsp)
  check_not_missing(by)
  check_not_missing(subgroups)
  check_not_missing(.tbl_fun)
  check_data_frame(data)
  cards::process_selectors(data, rsp = {{ rsp }}, by = {{ by }}, subgroups = {{ subgroups }},
                           time_to_event = {{ time_to_event }})
  check_string(rsp)
  check_string(by)
  check_string(time_to_event, allow_empty = TRUE)
  check_binary(data[[rsp]])
  check_class(subgroups, "character")
  check_class(.tbl_fun, c("formula", "function"))

  # Augment data with a dummy variable for the 'All Participants' row
  overall_rowname <- "All Participants"
  data_aug <- data %>% dplyr::mutate(..overall.. = overall_rowname)
  all_vars <- c("..overall..", subgroups)

  # subgroup analyses
  roche_subgroups_tbl <-
    all_vars |>
    map(
      \(x) {
        list(
          # total n
          gtsummary::tbl_strata(
            data = data_aug,
            strata = x,
            .tbl_fun = ~ .x |>
              gtsummary::tbl_summary(include = rsp, statistic = everything() ~ "{N}", missing = "no"),
            .combine_with = "tbl_stack",
            .combine_args = if (x == "..overall..") {
              list(group_header = NULL, quiet = TRUE)
            } else {
              list(quiet = TRUE)
            }
          ) |>
            gtsummary::modify_header(stat_0 ~ "**Total n**"),
          # responder or time-to-event median statistics
          gtsummary::tbl_strata(
            data = data_aug,
            strata = x,
            .tbl_fun =
              ~ .x |>
                tbl_strata(
                  strata = by,
                  .tbl_fun = ~ .make_mid_tbl(dt = .x, vars = c("rsp" = rsp, "time_to_event" = time_to_event))
                  ),
            .combine_with = "tbl_stack",
            .combine_args = if (x == "..overall..") {
              list(group_header = NULL, quiet = TRUE)
            } else {
              list(quiet = TRUE)
            }
          ),
          # comparison statistics
          gtsummary::tbl_strata(
            data = data_aug,
            strata = x,
            .tbl_fun = .tbl_fun,
            .combine_with = "tbl_stack",
            .combine_args = if (x == "..overall..") {
              list(group_header = NULL, quiet = TRUE)
            } else {
              list(quiet = TRUE)
            }
          ) |>
            gtsummary::modify_header(estimate = "**estimate**")
        ) |>
          gtsummary::tbl_merge(tab_spanner = FALSE, merge_vars = c("groupname_col", "tbl_id1", "row_type")) |>
          gtsummary::modify_column_hide(c("label_2", "label_3")) |>
          # add a header row for the variable
          gtsummary::modify_table_body(
            function(table_body) {
              if (any(grepl(pattern = overall_rowname, table_body))) {
                return(
                  table_body |>
                    dplyr::mutate(label_1 = overall_rowname)
                )
              }
              dplyr::bind_rows(
                dplyr::tibble(
                  variable = x,
                  row_type = "label",
                  label_1 = attr(data[[x]], "label") %||% x
                ),
                table_body |>
                  dplyr::mutate(
                    variable = x,
                    label_1 = .data$groupname_col,
                    row_type = "level"
                  ) |>
                  dplyr::select(-.data$groupname_col)
              )
            }
          )
      }
    ) |>
    # stack overall and subgroup analyses
    gtsummary::tbl_stack() |>
    gtsummary::modify_header(label_1 ~ "**Baseline Risk Factors**") |>
    gtsummary::remove_footnote_header() |>
    gtsummary::remove_abbreviation()


  attr(roche_subgroups_tbl, "by") <- levels(factor(data[[by]]))

  roche_subgroups_tbl <- roche_subgroups_tbl |>
    structure(class = c("tbl_roche_subgroups", "gtsummary"))

  return(roche_subgroups_tbl)
}


# Define a function to apply the .tbl_fun to each subgroup
.make_mid_tbl <- function(dt, vars) {
  if ("time_to_event" %in% names(vars) && length(vars[["time_to_event"]]) > 0) {
    out <- dt |>
      # Use ard_continuous instead of ard_tabulate_value
      cards::ard_continuous(
        variables = dplyr::all_of(vars[["time_to_event"]]),
        stat_label = cards::everything() ~ list(N = "n", median = "Median")
        # ard_continuous calculates N, mean, median, min, max, etc. by default
      ) |>
      gtsummary::tbl_ard_wide_summary(
        include = dplyr::all_of(vars[["time_to_event"]]),
        # Call the specific continuous statistics here
        statistic = c("{N}", "{median}")
      ) |>
      gtsummary::modify_header(stat_2 ~ "**Median (Months)**")
  } else {
    out <- dt |>
      cards::ard_tabulate_value(
        variables = dplyr::all_of(vars[["rsp"]]),
        stat_label = cards::everything() ~ list(N = "n", n = "responders", p = "response")
      ) |>
      gtsummary::tbl_ard_wide_summary(
        include = dplyr::all_of(vars[["rsp"]]),
        statistic = "{N} ({p} %)"
      ) |>
      # Explicitly renames the column header itself
      gtsummary::modify_header(gtsummary::all_stat_cols() ~ "**n Response (%)**")
  }

  out
}
