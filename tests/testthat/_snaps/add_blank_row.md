# add_blank_row() error message

    Code
      add_blank_row(tbl_roche_summary(trial, by = trt, statistic = age ~ "{mean}",
      include = c(age, grade), nonmissing = "no"), variables = age, row_numbers = c(1,
        1))
    Condition
      Error in `add_blank_row()`:
      ! One and only one of the following arguments may be specified: `variables`, `row_numbers`, and `variable_level`

# add_blank_row() errors when no variable

    Code
      add_blank_row(gtsummary::as_gtsummary(gtsummary::trial[1:5, 1:2]), variables = everything())
    Condition
      Error in `add_blank_row()`:
      ! The `variables` argument cannot be specified when `x$table_body` does not have a column named "variable".

