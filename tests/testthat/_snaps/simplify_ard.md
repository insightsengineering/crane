# simplify_ard() errors on different values for the same statistic

    Code
      simplify_ard(dplyr::bind_rows(ard, ard_other))
    Condition
      Error in `simplify_ard()`:
      ! The ARD has different values for the same statistic in 16 rows.
      i First one: variable = AGE, context = summary, stat_name = N.

# simplify_ard() messaging

    Code
      simplify_ard("not a table")
    Condition
      Error in `simplify_ard()`:
      ! The `x` argument must be a <gtsummary> table, a list of tables or a list of ARDs, not a string.

---

    Code
      simplify_ard(list(), .unlist = "yes")
    Condition
      Error in `simplify_ard()`:
      ! The `.unlist` argument must be a scalar with class <logical>, not a string.

