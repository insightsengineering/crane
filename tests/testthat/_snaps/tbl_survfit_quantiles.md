# tbl_survfit_quantiles() works

    Code
      as.data.frame(tbl)
    Output
                        Placebo  \n(N = 86) Xanomeline High Dose  \n(N = 84) Xanomeline Low Dose  \n(N = 84)
      1   Time to event                <NA>                             <NA>                            <NA>
      2          Median                  NE                             36.0                            33.0
      3          95% CI            (NE, NE)                     (25.0, 47.0)                    (28.0, 51.0)
      4 25% and 75%-ile            70.0, NE                       14.0, 58.0                      19.0, 80.0
      5           Range        1.0 to 198.0                     1.0 to 189.0                    1.0 to 190.0

---

    Code
      as.data.frame(tbl)
    Output
                        Drug A  \n(N = 98) Drug B  \n(N = 102)
      1   Time to event               <NA>                <NA>
      2          Median               23.5                21.2
      3          95% CI         (21.2, NE)          (18.2, NE)
      4 25% and 75%-ile           17.4, NE            14.5, NE
      5           Range        3.5 to 24.0         5.3 to 24.0

---

    Code
      as.data.frame(tbl)
    Output
                        Overall  \n(N = 254)
      1   Time to event                 <NA>
      2          Median                 51.0
      3          90% CI         (46.0, 68.0)
      4 25% and 75%-ile             22.0, NE
      5           Range         1.0 to 198.0

# tbl_survfit_quantiles(by) messaging

    Code
      tbl_survfit_quantiles(data = cards::ADTTE, by = everything())
    Condition
      Error in `tbl_survfit_quantiles()`:
      ! The `by` argument must be empty or a single stratifying variable name.

---

    Code
      tbl_survfit_quantiles(data = dplyr::rename(gtsummary::trial, time = trt), by = "time", y = "survival::Surv(ttdeath, death)")
    Condition
      Error in `tbl_survfit_quantiles()`:
      ! The `by` column cannot be named "time".

# tbl_survfit_quantiles(estimate_fun)

    Code
      as.data.frame(tbl_survfit_quantiles(data = cards::ADTTE, by = "TRTA", estimate_fun = gtsummary::label_style_number(
        digits = 3)))
    Output
                        Placebo  \n(N = 86) Xanomeline High Dose  \n(N = 84) Xanomeline Low Dose  \n(N = 84)
      1   Time to event                <NA>                             <NA>                            <NA>
      2          Median                  NA                           36.000                          33.000
      3          95% CI            (NA, NA)                 (25.000, 47.000)                (28.000, 51.000)
      4 25% and 75%-ile          70.000, NA                   14.000, 58.000                  19.000, 80.000
      5           Range    1.000 to 198.000                 1.000 to 189.000                1.000 to 190.000

# add_overall.tbl_survfit_quantiles() works

    Code
      as.data.frame(add_overall(tbl_survfit_quantiles(data = cards::ADTTE, by = "TRTA"), last = TRUE, col_label = "**All Participants**  \nN = {n}"))
    Output
                        Placebo  \n(N = 86) Xanomeline High Dose  \n(N = 84) Xanomeline Low Dose  \n(N = 84) **All Participants**  \nN = 254
      1   Time to event                <NA>                             <NA>                            <NA>                            <NA>
      2          Median                  NE                             36.0                            33.0                            51.0
      3          95% CI            (NE, NE)                     (25.0, 47.0)                    (28.0, 51.0)                    (44.0, 70.0)
      4 25% and 75%-ile            70.0, NE                       14.0, 58.0                      19.0, 80.0                        22.0, NE
      5           Range        1.0 to 198.0                     1.0 to 189.0                    1.0 to 190.0                    1.0 to 198.0

