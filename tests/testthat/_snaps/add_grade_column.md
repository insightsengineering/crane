# add_grade_column() errors when custom_info is missing

    Code
      add_grade_column(tbl)
    Condition
      Error in `add_grade_column()`:
      ! No `custom_info` metadata found on the input table.
      i Ensure the table was created with `tbl_hierarchical_rate_by_grade()`.

# add_grade_column() errors on non-gtsummary input

    Code
      add_grade_column(data.frame(x = 1))
    Condition
      Error in `add_grade_column()`:
      ! `x` must be a <gtsummary> object.

