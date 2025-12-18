#' Subgroup Analyses
#'
#' Function adapted from `gtforester::tbl_subgroups()`.
#'
#' @inheritParams gtsummary::tbl_strata
#' @importFrom cards ard_tabulate_value
#' @param rsp ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Variable to use in responder rate calculations.
#' @param subgroups ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Variables to perform stratified analyses for.
#'
#' @returns a 'gtsummary' table
#' @export
#'
#' @examples
#' tbl <-
#'   trial |>
#'   tbl_roche_subgroups(
#'   rsp = "response",
#'   by = "trt",
#'   subgroups = c("grade", "stage"),
#'   .tbl_fun =
#'     ~ glm(response ~ trt, data = .x) |>
#'       tbl_regression(
#'         show_single_row = trt,
#'         exponentiate = TRUE,
#'         tidy_fun = broom.helpers::tidy_parameters
#'       )
#'   )
#'
#' tbl
tbl_roche_subgroups <- function(data, rsp, by, subgroups, .tbl_fun) {
  set_cli_abort_call()

  # check inputs ---------------------------------------------------------------
  check_not_missing(data)
  check_not_missing(rsp)
  check_not_missing(by)
  check_not_missing(subgroups)
  check_not_missing(.tbl_fun)
  check_data_frame(data)
  cards::process_selectors(data, rsp = {{ rsp }}, by = {{ by }}, subgroups = {{ subgroups }})
  check_string(rsp)
  check_string(by)
  check_binary(data[[rsp]])
  check_class(subgroups, "character")
  check_class(.tbl_fun, c("formula", "function"))

  # overall analyses
  tbl_overall <-
    list(
      # total n
      data |>
        tbl_summary(include = rsp, statistic = everything() ~ "{N}", missing = "no") |>
        modify_header(stat_0 ~ "**Total n**"),
      # responder statistics
      data |>
        tbl_strata(
          strata = by,
          .tbl_fun = ~ .x |>
            ard_tabulate_value(
              variables = rsp,
              stat_label = everything() ~ list(N = "n", n = "Responders", p = "Response (%)")
            ) |>
            tbl_ard_wide_summary(
              include = rsp,
              statistic = c("{N}", "{n}", "{p}")
            )
        ),
      # comparison statistics
      tbl_strata(
        data = data |> mutate(dummy = "..overall.."),
        strata = "dummy",
        .tbl_fun = .tbl_fun,
        .combine_with = "tbl_stack"
      ) |>
        modify_header(estimate = "**Odds Ratio**")
    ) |>
    tbl_merge(tab_spanner = FALSE, merge_vars = c("groupname_col", "tbl_id1", "row_type")) |>
    modify_column_hide(c("label_2", "label_3")) |>
    # add a header row for the variable
    modify_table_body(
      function(table_body) {
        table_body |>
          mutate(label_1 = "All Participants")
      }
    ) |>
    modify_column_hide("groupname_col_3")

  # subgroup analyses
  tbl_subgp <-
    subgroups |>
    purrr::map(
      \(x) {
        list(
          # total n
          tbl_strata(
            data = data,
            strata = x,
            .tbl_fun =
              ~ .x |> tbl_summary(include = rsp, statistic = everything() ~ "{N}", missing = "no") ,
            .combine_with = "tbl_stack",
            .combine_args = list(quiet = TRUE)
          ) |>
            modify_header(stat_0 ~ "**Total n**"),
          # responder statistics
          tbl_strata(
            data = data,
            strata = x,
            .tbl_fun =
              ~ .x |>
              tbl_strata(
                strata = by,
                .tbl_fun = ~ .x |>
                  ard_tabulate_value(
                    variables = rsp,
                    stat_label = everything() ~ list(N = "n", n = "Responders", p = "Response (%)")
                  ) |>
                  tbl_ard_wide_summary(
                    include = rsp,
                    statistic = c("{N}", "{n}", "{p}")
                  )
              ),
            .combine_with = "tbl_stack"
          ),
          # comparison statistics
          tbl_strata(
            data = data,
            strata = x,
            .tbl_fun = .tbl_fun,
            .combine_with = "tbl_stack"
          ) |>
            modify_header(estimate = "**Odds Ratio**")
        ) |>
          tbl_merge(tab_spanner = FALSE, merge_vars = c("groupname_col", "tbl_id1", "row_type")) |>
          modify_column_hide(c("label_2", "label_3")) |>
          # add a header row for the variable
          modify_table_body(
            function(table_body) {
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
    tbl_stack()

  # stack analyses and return formatted table
  list(tbl_overall, tbl_subgp) |>
    tbl_stack(attr_order = 2) |>
    modify_header(label_1 ~ "**Baseline Risk Factors**") |>
    remove_footnote_header() |>
    remove_abbreviation()
}
