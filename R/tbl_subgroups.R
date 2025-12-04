# This file is a direct copy from ddsjoberg/gtforester
# usethis::use_github_file("ddsjoberg/gtforester", "R/tbl_subgroups.R")

#' Subgroup Analyses
#'
#' @param data data frame
#' @param subgroups character vector of column names to perform stratified analyses among
#' @inheritParams gtsummary::tbl_strata
#'
#' @return a gt table
#' @export
#'
#' @examples
#' # Load required packages
#' library(gtsummary)
#' library(dplyr)
#'
#' # Create a base table function:
#' # This function takes a data frame and returns a gtsummary table,
#' # showing the distribution of 'age' and 'marker' grouped by 'response'.
#' tbl_fun_ex <- function(data, ...) {
#'   data %>%
#'     select(age, marker) %>%
#'     tbl_summary(
#'       by = response,
#'       missing = "no",
#'       label = list(age ~ "Patient Age (years)", marker ~ "Tumor Marker"),
#'       statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} / {N} ({p}%)")
#'     ) %>%
#'     add_p() %>%
#'     bold_p(t = 0.05) %>%
#'     # Remove the 'by' variable from the table body, as it's the 'response' column
#'     # which is being stratified on within the tbl_strata call.
#'     # We want the output to be the strata results, not the 'response' column itself.
#'     modify_header(label = "**Subgroup**")
#' }
#'
#' # Use the function on the trial data
#' tbl_subgroups(
#'   data = trial,
#'   subgroups = c("stage", "trt"),
#'   .tbl_fun = tbl_fun_ex
#' )
tbl_subgroups <- function(data, subgroups, .tbl_fun, ...) {
  tbl <-
    subgroups %>%
    purrr::map(
      ~gtsummary::tbl_strata(
        data = data,
        strata = .x,
        .tbl_fun = .tbl_fun,
        ...,
        .combine_with = "tbl_stack"
      ) %>%
        # add a header row for the variable
        gtsummary::modify_table_body(
          function(table_body) {
            dplyr::bind_rows(
              dplyr::tibble(
                variable = .x,
                row_type = "label",
                label = attr(data[[.x]], "label") %||% x
              ),
              table_body %>%
                dplyr::mutate(
                  variable = .x,
                  label = .data$groupname_col,
                  row_type = "level"
                ) %>%
                dplyr::select(-.data$groupname_col)
            )
          }
        )
    ) %>%
    gtsummary::tbl_stack()

  tbl
}
