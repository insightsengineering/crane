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
#'
#' @returns a 'gtsummary' table
#' @export
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true")
#' tbl <-
#'   trial |>
#'   tbl_roche_subgroups(
#'     rsp = "response",
#'     by = "trt",
#'     subgroups = c("grade", "stage"),
#'     .tbl_fun =
#'       ~ glm(response ~ trt, data = .x) |>
#'         tbl_regression(
#'           show_single_row = trt,
#'           exponentiate = TRUE
#'         )
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

  # Augment data with a dummy variable for the 'All Participants' row
  overall_rowname <- "All Participants"
  data_aug <- data %>% dplyr::mutate(..overall.. = all_part_rowname)
  all_vars <- c("..overall..", subgroups)

  # subgroup analyses
  tbl <-
    all_vars |>
    pmap(
      \(x) {
        list(
          # total n
          tbl_strata(
            data = data_aug,
            strata = x,
            .tbl_fun = ~ .x |>
              tbl_summary(include = rsp, statistic = everything() ~ "{N}", missing = "no"),
            .combine_with = "tbl_stack",
            .combine_args = if (x == "..overall..") {
              list(group_header = NULL, quiet = TRUE)
            } else {
              list(quiet = TRUE)
            }
          ) |>
            modify_header(stat_0 ~ "**Total n**"),
          # responder statistics
          tbl_strata(
            data = data_aug,
            strata = x,
            .tbl_fun =
              ~ .x |>
                tbl_strata(
                  strata = by,
                  .tbl_fun = ~ .x |>
                    ard_tabulate_value(
                      variables = rsp,
                      stat_label = everything() ~ list(N = "n", n = "Responders", p = "response")
                    ) |>
                    tbl_ard_wide_summary(
                      include = rsp,
                      statistic = c("{N} ({p} %)"),
                      label = list(rsp = "n Response (%)")
                    )
                ),
            .combine_with = "tbl_stack",
            .combine_args = if (x == "..overall..") {
              list(group_header = NULL, quiet = TRUE)
            } else {
              list(quiet = TRUE)
            }
          ),
          # comparison statistics
          tbl_strata(
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
            modify_header(estimate = "**Odds Ratio**")
        ) |>
          tbl_merge(tab_spanner = FALSE, merge_vars = c("groupname_col", "tbl_id1", "row_type")) |>
          modify_column_hide(c("label_2", "label_3")) |>
          # add a header row for the variable
          modify_table_body(
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
    tbl_stack()

  # stack analyses and return formatted table
  ret <- tbl |>
    modify_header(label_1 ~ "**Baseline Risk Factors**") |>
    remove_footnote_header() |>
    remove_abbreviation()

  attr(ret, "by") <- levels(factor(data[[by]]))
  return(ret)
}
