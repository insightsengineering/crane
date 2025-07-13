# tbl_shift(strata_location)

    Code
      as.data.frame(tbl)
    Output
        Baseline  \nNCI-CTCAE Grade Post-baseline  \nNCI-CTCAE Grade Placebo  \nN = 86 Xanomeline High Dose  \nN = 72 Xanomeline Low Dose  \nN = 96
      1                           0                            Total                81                             70                            94
      2                        <NA>                                0        77 (95.1%)                     66 (94.3%)                    91 (96.8%)
      3                        <NA>                                1          2 (2.5%)                       4 (5.7%)                      1 (1.1%)
      4                        <NA>                                2          2 (2.5%)                              0                      2 (2.1%)
      5                           1                            Total                 0                              0                             1
      6                        <NA>                                1                 0                              0                      1 (100%)
      7                           2                            Total                 5                              2                             1
      8                        <NA>                                2          5 (100%)                       2 (100%)                      1 (100%)

---

    Code
      as.data.frame(tbl)
    Output
         Baseline  \nNCI-CTCAE Grade  \n    Post-baseline  \nNCI-CTCAE Grade Placebo  \nN = 86 Xanomeline High Dose  \nN = 72 Xanomeline Low Dose  \nN = 96
      1                                                                    0              <NA>                           <NA>                          <NA>
      2                                                                Total                81                             70                            94
      3                                                                    0        77 (95.1%)                     66 (94.3%)                    91 (96.8%)
      4                                                                    1          2 (2.5%)                       4 (5.7%)                      1 (1.1%)
      5                                                                    2          2 (2.5%)                              0                      2 (2.1%)
      6                                                                    1              <NA>                           <NA>                          <NA>
      7                                                                Total                 0                              0                             1
      8                                                                    1                 0                              0                      1 (100%)
      9                                                                    2              <NA>                           <NA>                          <NA>
      10                                                               Total                 5                              2                             1
      11                                                                   2          5 (100%)                       2 (100%)                      1 (100%)

# tbl_shift(by) messaging

    Code
      tbl <- tbl_shift(data = dplyr::mutate(dplyr::filter(adlb, PARAMCD %in% "CHOLES"), TRT01A = as.character(TRT01A)), strata = BTOXGRH, variable = ATOXGRH, by = TRT01A, data_header = adsl,
      strata_location = "new_column")
    Message
      i Converting column "TRT01A" to a factor.

# tbl_shift(data_header) messaging

    Code
      tbl_shift(data = dplyr::filter(adlb, PARAMCD %in% "CHOLES"), strata = BTOXGRH, variable = ATOXGRH, data_header = dplyr::mutate(adsl, asldfk = TRUE), strata_location = "new_column")
    Condition
      Error in `tbl_shift()`:
      ! The data frame passed in the `data_header` argument should only include columns that will be used to merge with `data`.
      i Based on the other inputs, this likely means only including "USUBJID".

# add_overall.tbl_shift()

    Code
      as.data.frame(tbl)
    Output
        Baseline  \nNCI-CTCAE Grade Post-baseline  \nNCI-CTCAE Grade Placebo  \nN = 86 Xanomeline High Dose  \nN = 72 Xanomeline Low Dose  \nN = 96 Overall  \nN = 254
      1                           0                            Total                81                             70                            94                245
      2                        <NA>                                0        77 (95.1%)                     66 (94.3%)                    91 (96.8%)        234 (95.5%)
      3                        <NA>                                1          2 (2.5%)                       4 (5.7%)                      1 (1.1%)           7 (2.9%)
      4                        <NA>                                2          2 (2.5%)                              0                      2 (2.1%)           4 (1.6%)
      5                           1                            Total                 0                              0                             1                  1
      6                        <NA>                                1                 0                              0                      1 (100%)           1 (100%)
      7                           2                            Total                 5                              2                             1                  8
      8                        <NA>                                2          5 (100%)                       2 (100%)                      1 (100%)           8 (100%)

---

    Code
      as.data.frame(tbl)
    Output
         Baseline  \nNCI-CTCAE Grade  \n    Post-baseline  \nNCI-CTCAE Grade Placebo  \nN = 86 Xanomeline High Dose  \nN = 72 Xanomeline Low Dose  \nN = 96 Overall  \nN = 254
      1                                                                    0              <NA>                           <NA>                          <NA>               <NA>
      2                                                                Total                81                             70                            94                245
      3                                                                    0        77 (95.1%)                     66 (94.3%)                    91 (96.8%)        234 (95.5%)
      4                                                                    1          2 (2.5%)                       4 (5.7%)                      1 (1.1%)           7 (2.9%)
      5                                                                    2          2 (2.5%)                              0                      2 (2.1%)           4 (1.6%)
      6                                                                    1              <NA>                           <NA>                          <NA>               <NA>
      7                                                                Total                 0                              0                             1                  1
      8                                                                    1                 0                              0                      1 (100%)           1 (100%)
      9                                                                    2              <NA>                           <NA>                          <NA>               <NA>
      10                                                               Total                 5                              2                             1                  8
      11                                                                   2          5 (100%)                       2 (100%)                      1 (100%)           8 (100%)

# add_overall.tbl_shift() messaging

    Code
      tbl <- add_overall(modify_table_body(tbl_shift(data = dplyr::filter(adlb, PARAMCD %in% "CHOLES"), strata = BTOXGRH, variable = ATOXGRH, by = TRT01A, data_header = adsl, strata_location = "new_column"),
      ~ dplyr::filter(.x, dplyr::row_number() %in% 1:5)))
    Message
      ! The structures of the original table and the overall table are not identical, and the resulting table may be malformed.

---

    Code
      tbl <- add_overall(tbl_shift(data = dplyr::filter(adlb, PARAMCD %in% "CHOLES"), strata = BTOXGRH, variable = ATOXGRH, data_header = adsl, strata_location = "new_column"))
    Message
      Original table was not stratified, and overall column cannot be added.
      i Table has been returned unaltered.

