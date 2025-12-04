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
#' # add example
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
