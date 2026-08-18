# tbl_proportion() binary summary works

    Code
      as.data.frame(tbl)
    Output
                        Characteristic      Placebo          Low         High
      1                     Responders   18 (30.0%)   25 (41.7%)   33 (55.0%)
      2 95% CI (Wald, with correction) (17.6, 42.4) (28.4, 55.0) (41.6, 68.4)

# add_proportion_difference() adds the comparison block

    Code
      as.data.frame(tbl)
    Output
                         Characteristic      Placebo          Low         High
      1                      Responders   18 (30.0%)   25 (41.7%)   33 (55.0%)
      2  95% CI (Wald, with correction) (17.6, 42.4) (28.4, 55.0) (41.6, 68.4)
      3           Unstratified Analysis         <NA>         <NA>         <NA>
      4 Difference in Response rate (%)         <NA>         11.7         25.0
      5  95% CI (Wald, with correction)         <NA> (-7.0, 30.4)  (6.2, 43.8)
      6      p-value (Chi-Squared Test)         <NA>       0.1827       0.0056

# informative errors are raised

    Code
      tbl_proportion(d, "rsp", "arm", value = TRUE, conf.level = 1.5)
    Condition
      Error in `tbl_proportion()`:
      ! The `conf.level` argument must be in the interval `(0, 1)`.

---

    Code
      add_proportion_difference(tbl_proportion(d, "rsp", by = NULL, value = TRUE))
    Condition
      Error in `add_proportion_difference()`:
      ! Cannot add a comparison when `tbl_proportion()` was built without a `by` variable.

---

    Code
      add_proportion_difference(tbl_proportion(d, "rsp", "arm"))
    Condition
      Error in `add_proportion_difference()`:
      ! Comparisons require a binary summary. Rebuild `tbl_proportion()` with the `value` argument.

---

    Code
      add_proportion_difference(tbl_proportion(d, "rsp", "arm", value = TRUE), test = "cmh")
    Condition
      Error in `add_proportion_difference()`:
      ! The "cmh" test requires a `strata` variable.

---

    Code
      add_proportion_difference(tbl_proportion(d, "rsp", "arm", value = TRUE),
      reference = "Z")
    Condition
      Error in `add_proportion_difference()`:
      ! The `reference` "Z" is not a level of the grouping variable.

