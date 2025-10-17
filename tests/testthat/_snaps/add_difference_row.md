# add_difference_row.tbl_survfit_times() works

    Code
      as.data.frame(tbl1)
    Output
                                       Placebo  \n(N = 86) Xanomeline High Dose  \n(N = 84) Xanomeline Low Dose  \n(N = 84)
      1                        Time 30                <NA>                             <NA>                            <NA>
      2     Patients remaining at risk                  69                               38                              42
      3            Event Free Rate (%)                84.4                             53.0                            53.4
      4                         95% CI        (77.0, 92.6)                     (42.8, 65.7)                    (43.4, 65.6)
      5  Difference in Event Free Rate                <NA>                            31.43                           31.07
      6                         95% CI                <NA>                   (17.66, 45.20)                  (17.56, 44.58)
      7               p-value (Z-test)                <NA>                          <0.0001                         <0.0001
      8                        Time 60                <NA>                             <NA>                            <NA>
      9     Patients remaining at risk                  59                               14                              20
      10           Event Free Rate (%)                76.8                             24.3                            31.1
      11                        95% CI        (68.2, 86.6)                     (15.8, 37.3)                    (21.9, 44.1)
      12 Difference in Event Free Rate                <NA>                            52.54                           45.77
      13                        95% CI                <NA>                   (38.65, 66.43)                  (31.57, 59.97)
      14              p-value (Z-test)                <NA>                          <0.0001                         <0.0001

---

    Code
      as.data.frame(tbl3)
    Output
                                                                  Placebo  \n(N = 86) Xanomeline High Dose  \n(N = 84) Xanomeline Low Dose  \n(N = 84)
      1                                                   Time 30                <NA>                             <NA>                            <NA>
      2                                Patients remaining at risk                  69                               38                              42
      3                                       Event Free Rate (%)                84.4                             53.0                            53.4
      4                                                    95% CI        (77.0, 92.6)                     (42.8, 65.7)                    (43.4, 65.6)
      5  Survival Difference (Survival Difference Standard Error)                <NA>                       31.4 (0.1)                      31.1 (0.1)
      6                                 z statistic (p = p-value)                <NA>                 4.5 (p = <0.001)                4.5 (p = <0.001)
      7                                                   Time 60                <NA>                             <NA>                            <NA>
      8                                Patients remaining at risk                  59                               14                              20
      9                                       Event Free Rate (%)                76.8                             24.3                            31.1
      10                                                   95% CI        (68.2, 86.6)                     (15.8, 37.3)                    (21.9, 44.1)
      11 Survival Difference (Survival Difference Standard Error)                <NA>                       52.5 (0.1)                      45.8 (0.1)
      12                                z statistic (p = p-value)                <NA>                 7.4 (p = <0.001)                6.3 (p = <0.001)

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

