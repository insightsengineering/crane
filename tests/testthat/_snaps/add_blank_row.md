# add_blank_row() errors when no variable

    Code
      add_blank_row(gtsummary::as_gtsummary(gtsummary::trial[1:5, 1:2]))
    Condition
      Error in `add_blank_row()`:
      ! The `row_numbers` argument must be specified when the passed table does not contain a "variable" column in `x$table_body`.

