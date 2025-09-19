# tbl_shift(strata_location)

    Code
      as.data.frame(tbl)
    Output
         Baseline  \nNCI-CTCAE Grade Post-baseline  \nNCI-CTCAE Grade Placebo  \nN = 86 Xanomeline High Dose  \nN = 84 Xanomeline Low Dose  \nN = 84
      1                            2                            Total                 1                              0                             0
      2                         <NA>                                0          1 (100%)                              0                             0
      3                         <NA>                                1                 0                              0                             0
      4                         <NA>                                2                 0                              0                             0
      5                         <NA>                                3                 0                              0                             0
      6                           NA                            Total                 0                              0                             0
      7                         <NA>                                0                 0                              0                             0
      8                         <NA>                                1                 0                              0                             0
      9                         <NA>                                2                 0                              0                             0
      10                        <NA>                                3                 0                              0                             0

---

    Code
      as.data.frame(tbl)
    Output
         Baseline  \nNCI-CTCAE Grade  \n    Post-baseline  \nNCI-CTCAE Grade Placebo  \nN = 86 Xanomeline High Dose  \nN = 84 Xanomeline Low Dose  \nN = 84
      1                                                                    2              <NA>                           <NA>                          <NA>
      2                                                                Total                 1                              0                             0
      3                                                                    0          1 (100%)                              0                             0
      4                                                                    1                 0                              0                             0
      5                                                                    2                 0                              0                             0
      6                                                                    3                 0                              0                             0
      7                                                                   NA              <NA>                           <NA>                          <NA>
      8                                                                Total                 0                              0                             0
      9                                                                    0                 0                              0                             0
      10                                                                   1                 0                              0                             0
      11                                                                   2                 0                              0                             0
      12                                                                   3                 0                              0                             0

# tbl_shift(by) messaging

    Code
      tbl <- tbl_shift(data = dplyr::mutate(dplyr::filter(adlb, PARAMCD %in% "CHOL"), TRTA = as.character(TRTA)), strata = BTOXGRH, variable = ATOXGRH, by = TRTA, data_header = adsl,
      strata_location = "new_column")
    Message
      i Converting column "TRTA" to a factor.

# tbl_shift(data_header) messaging

    Code
      tbl_shift(data = dplyr::filter(adlb, PARAMCD %in% "CHOL"), strata = BTOXGRH, variable = ATOXGRH, data_header = dplyr::mutate(adsl, asldfk = TRUE), strata_location = "new_column")
    Condition
      Error in `tbl_shift()`:
      ! The data frame passed in the `data_header` argument should only include columns that will be used to merge with `data`.
      i Based on the other inputs, this likely means only including "USUBJID".

# add_overall.tbl_shift()

    Code
      as.data.frame(tbl)
    Output
         Baseline  \nNCI-CTCAE Grade Post-baseline  \nNCI-CTCAE Grade Placebo  \nN = 86 Xanomeline High Dose  \nN = 84 Xanomeline Low Dose  \nN = 84 Overall  \nN = 254
      1                            2                            Total                 1                              0                             0                  1
      2                         <NA>                                0          1 (100%)                              0                             0           1 (100%)
      3                         <NA>                                1                 0                              0                             0                  0
      4                         <NA>                                2                 0                              0                             0                  0
      5                         <NA>                                3                 0                              0                             0                  0
      6                           NA                            Total                 0                              0                             0                  0
      7                         <NA>                                0                 0                              0                             0                  0
      8                         <NA>                                1                 0                              0                             0                  0
      9                         <NA>                                2                 0                              0                             0                  0
      10                        <NA>                                3                 0                              0                             0                  0

---

    Code
      as.data.frame(tbl)
    Output
         Baseline  \nNCI-CTCAE Grade  \n    Post-baseline  \nNCI-CTCAE Grade Placebo  \nN = 86 Xanomeline High Dose  \nN = 84 Xanomeline Low Dose  \nN = 84 Overall  \nN = 254
      1                                                                    2              <NA>                           <NA>                          <NA>               <NA>
      2                                                                Total                 1                              0                             0                  1
      3                                                                    0          1 (100%)                              0                             0           1 (100%)
      4                                                                    1                 0                              0                             0                  0
      5                                                                    2                 0                              0                             0                  0
      6                                                                    3                 0                              0                             0                  0
      7                                                                   NA              <NA>                           <NA>                          <NA>               <NA>
      8                                                                Total                 0                              0                             0                  0
      9                                                                    0                 0                              0                             0                  0
      10                                                                   1                 0                              0                             0                  0
      11                                                                   2                 0                              0                             0                  0
      12                                                                   3                 0                              0                             0                  0

# add_overall.tbl_shift() messaging

    Code
      tbl <- add_overall(modify_table_body(tbl_shift(data = dplyr::filter(adlb, PARAMCD %in% "CHOL"), strata = BTOXGRH, variable = ATOXGRH, by = TRTA, data_header = adsl, strata_location = "new_column"),
      ~ dplyr::filter(.x, dplyr::row_number() %in% 1:5)))
    Message
      ! The structures of the original table and the overall table are not identical, and the resulting table may be malformed.

---

    Code
      tbl <- add_overall(tbl_shift(data = dplyr::filter(adlb, PARAMCD %in% "CHOL"), strata = BTOXGRH, variable = ATOXGRH, data_header = adsl, strata_location = "new_column"))
    Message
      Original table was not stratified, and overall column cannot be added.
      i Table has been returned unaltered.

# add_overall.tbl_shift(strata=NULL) messaging

    Code
      as.data.frame(tbl)
    Output
                        Cohort Baseline  \nNCI-CTCAE Grade        0 1 2 3
      1 All Participants, N=20                           1        0 0 0 0
      2                   <NA>                           2 1 (100%) 0 0 0
      3                   <NA>                           3        0 0 0 0

