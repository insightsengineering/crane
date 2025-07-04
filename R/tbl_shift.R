
tbl_shift <- function(data,
                      strata,
                      variable,
                      by = NULL,
                      data_header = NULL, # do i need this? it's really just to get the headers write. should i update the name to data_header?
                      strata_location = c("new_column", "header"),
                      strata_label = "{strata}",
                      header = "{level}  \nN = {n}",
                      nonmissing = "always",
                      nonmissing_text = "Total",
                      digits = NULL) {
  set_cli_abort_call()
  # check inputs ---------------------------------------------------------------
  check_not_missing(data)
  check_not_missing(strata)
  check_not_missing(variable)
  check_data_frame(data)
  check_data_frame(data_header, allow_empty = TRUE)
  strata_location <- arg_match(strata_location)
  check_string(header)
  check_string(strata_label)
  cards::process_selectors(data, strata = {{ strata }}, variable = {{ variable }}, by = {{ by }})
  check_scalar(strata, msg = "The {.arg strata} argument must select exactly one variable.")
  check_scalar(variable, msg = "The {.arg variable} argument must select exactly one variable.")
  check_scalar(by, allow_empty = TRUE, msg = "The {.arg by} argument must select exactly one variable or none.")

  # build stratified table -----------------------------------------------------
  gtsummary::tbl_strata2(
    data = data,
    strata = all_of(strata),
    .tbl_fun =
      \(data, stratum) {
        # if `data_header` was passed, then merge it with the primary data
        if (!is_empty(data_header)) {
          data <-
            dplyr::right_join(
              data,
              data_header,
              by = intersect(names(data), names(data_header)) # TODO: Document this these merging variables!
            )
        }

        # build cross table
        tbl <-
          tbl_roche_summary(
            data = data,
            by = any_of(by),
            include = all_of(variable),
            nonmissing = nonmissing,
            nonmissing_text = nonmissing_text,
            label = list(strata) |> set_names(variable),
            digits = digits
          ) |>
          gtsummary::modify_header(all_stat_cols() ~ header)

        # update table if strata placement is in a new column
        if (strata_location == "new_column") {
          tbl <- tbl |>
            remove_row_type(type = "header") |>
            modify_table_body(
              ~ .x |>
                mutate(
                  .before = "label",
                  label0 = ifelse(row_number() == 1L, .env$strata, NA_character_)
                )
            ) |>
            modify_column_alignment(columns = c(label, label0), align = "left") |>
            modify_indent(columns = label, indent = 0L)
        }
      },
    .combine_with = "tbl_stack",
    .combine_args = list(group_header = NULL)
  )
}
