# add_difference_row.tbl_survfit_times() works

    Code
      as.data.frame(tbl1)
    Output
                                        Placebo  \n(N = 86) Xanomeline High Dose  \n(N = 84) Xanomeline Low Dose  \n(N = 84)
      1                         Time 60                <NA>                             <NA>                            <NA>
      2      Patients remaining at risk                  59                               14                              20
      3             Event Free Rate (%)                76.8                             24.3                            31.1
      4                          95% CI        (68.2, 86.6)                     (15.8, 37.3)                    (21.9, 44.1)
      5  Difference in Event Free Rates                <NA>                            52.54                           45.77
      6                          95% CI                <NA>                   (38.65, 66.43)                  (31.57, 59.97)
      7                p-value (Z-test)                <NA>                          <0.0001                         <0.0001
      8                        Time 120                <NA>                             <NA>                            <NA>
      9      Patients remaining at risk                  45                                4                               8
      10            Event Free Rate (%)                64.3                              9.2                            14.7
      11                         95% CI        (54.5, 76.0)                      (3.8, 22.1)                     (8.0, 27.1)
      12 Difference in Event Free Rates                <NA>                            55.16                           49.68
      13                         95% CI                <NA>                   (41.76, 68.56)                  (35.70, 63.65)
      14               p-value (Z-test)                <NA>                          <0.0001                         <0.0001

---

    Code
      as.data.frame(tbl3)
    Output
                                                                  Placebo  \n(N = 86) Xanomeline High Dose  \n(N = 84) Xanomeline Low Dose  \n(N = 84)
      1                                                   Time 60                <NA>                             <NA>                            <NA>
      2                                Patients remaining at risk                  59                               14                              20
      3                                       Event Free Rate (%)                76.8                             24.3                            31.1
      4                                                    95% CI        (68.2, 86.6)                     (15.8, 37.3)                    (21.9, 44.1)
      5  Survival Difference (Survival Difference Standard Error)                <NA>                       52.5 (7.1)                      45.8 (7.2)
      6                                 z statistic (p = p-value)                <NA>               741.5 (p = <0.001)              631.8 (p = <0.001)
      7                                                  Time 120                <NA>                             <NA>                            <NA>
      8                                Patients remaining at risk                  45                                4                               8
      9                                       Event Free Rate (%)                64.3                              9.2                            14.7
      10                                                   95% CI        (54.5, 76.0)                      (3.8, 22.1)                     (8.0, 27.1)
      11 Survival Difference (Survival Difference Standard Error)                <NA>                       55.2 (6.8)                      49.7 (7.1)
      12                                z statistic (p = p-value)                <NA>               806.8 (p = <0.001)              696.7 (p = <0.001)

# add_difference_row.tbl_survfit_times() error messaging works

    Code
      add_difference_row(tbl_survfit_times(data = cards::ADTTE, times = c(30, 60)), "Placebo")
    Condition
      Error in `add_difference_row()`:
      ! Cannot run `add_difference_row()` when `tbl_survfit_times()` does not include a `by` argument.

---

    Code
      add_difference_row(tbl_survfit_times(data = cards::ADTTE, by = TRTA, times = c(30, 60)), "No Treatment")
    Condition
      Error in `add_difference_row()`:
      ! The `reference` argument must be one of "Placebo", "Xanomeline High Dose", and "Xanomeline Low Dose".

