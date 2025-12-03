# update this, use usethis to improt
# usethis::use_standalone("ddsjoberg/gtforester", "tbl_subgroups")

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
