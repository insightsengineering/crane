# tbl_survfit_times() works

    Code
      as.data.frame(tbl)
    Output
                                   Placebo  \n(N = 86) Xanomeline High Dose  \n(N = 84) Xanomeline Low Dose  \n(N = 84)
      1                    Time 30                <NA>                             <NA>                            <NA>
      2 Patients remaining at risk                  69                               38                              42
      3        Event Free Rate (%)                84.4                             53.0                            53.4
      4                     95% CI        (77.0, 92.6)                     (42.8, 65.7)                    (43.4, 65.6)
      5                    Time 60                <NA>                             <NA>                            <NA>
      6 Patients remaining at risk                  59                               14                              20
      7        Event Free Rate (%)                76.8                             24.3                            31.1
      8                     95% CI        (68.2, 86.6)                     (15.8, 37.3)                    (21.9, 44.1)

---

    Code
      as.data.frame(tbl)
    Output
                                   Drug A  \n(N = 98) Drug B  \n(N = 102)
      1                    Time 12               <NA>                <NA>
      2 Patients remaining at risk                 89                  88
      3        Event Free Rate (%)               90.8                86.3
      4                     95% CI       (85.3, 96.7)        (79.8, 93.2)
      5                    Time 15               <NA>                <NA>
      6 Patients remaining at risk                 83                  75
      7        Event Free Rate (%)               84.7                73.5
      8                     95% CI       (77.9, 92.1)        (65.4, 82.6)

---

    Code
      as.data.frame(tbl)
    Output
                                   Overall  \n(N = 254)
      1                    Time 30                 <NA>
      2 Patients remaining at risk                  149
      3        Event Free Rate (%)                 64.1
      4                     90% CI         (59.1, 69.4)
      5                    Time 60                 <NA>
      6 Patients remaining at risk                   93
      7        Event Free Rate (%)                 45.7
      8                     90% CI         (40.5, 51.5)

# tbl_survfit_times(by) messaging

    Code
      tbl_survfit_times(data = cards::ADTTE, by = everything(), times = 30)
    Condition
      Error in `tbl_survfit_times()`:
      ! The `by` argument must be length 1 or empty.

---

    Code
      tbl_survfit_times(data = dplyr::rename(gtsummary::trial, time = trt), by = "time", y = "survival::Surv(ttdeath, death)",
      times = 30)
    Condition
      Error in `tbl_survfit_times()`:
      ! The `by` column cannot be named "time".

# add_overall.tbl_survfit_times() works

    Code
      as.data.frame(add_overall(tbl_survfit_times(data = cards::ADTTE, by = "TRTA", times = 30, label = "Day {time}"), last = TRUE, col_label = "**All Participants**  \nN = {n}"))
    Output
                                   Placebo  \n(N = 86) Xanomeline High Dose  \n(N = 84) Xanomeline Low Dose  \n(N = 84) **All Participants**  \nN = 254
      1                     Day 30                <NA>                             <NA>                            <NA>                            <NA>
      2 Patients remaining at risk                  69                               38                              42                             149
      3        Event Free Rate (%)                84.4                             53.0                            53.4                            64.1
      4                     95% CI        (77.0, 92.6)                     (42.8, 65.7)                    (43.4, 65.6)                    (58.2, 70.5)

