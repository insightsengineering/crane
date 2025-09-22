# tbl_shift(strata_location)

    Code
      as.data.frame(tbl)
    Output
        Baseline  \nNCI-CTCAE Grade Post-baseline  \nNCI-CTCAE Grade Placebo  \nN = 86 Xanomeline High Dose  \nN = 84 Xanomeline Low Dose  \nN = 84
      1                        High                            Total                 2                              2                             4
      2                        <NA>                              Low                 0                      1 (50.0%)                     1 (25.0%)
      3                        <NA>                           Normal          2 (100%)                      1 (50.0%)                     3 (75.0%)
      4                         Low                            Total                 3                              3                             1
      5                        <NA>                             High         1 (33.3%)                              0                             0
      6                        <NA>                           Normal         2 (66.7%)                       3 (100%)                      1 (100%)
      7                      Normal                            Total                 2                              2                             1
      8                        <NA>                           Normal          2 (100%)                       2 (100%)                      1 (100%)

---

    Code
      as.data.frame(tbl)
    Output
         Baseline  \nNCI-CTCAE Grade  \n    Post-baseline  \nNCI-CTCAE Grade Placebo  \nN = 86 Xanomeline High Dose  \nN = 84 Xanomeline Low Dose  \nN = 84
      1                                                                 High              <NA>                           <NA>                          <NA>
      2                                                                Total                 2                              2                             4
      3                                                                  Low                 0                      1 (50.0%)                     1 (25.0%)
      4                                                               Normal          2 (100%)                      1 (50.0%)                     3 (75.0%)
      5                                                                  Low              <NA>                           <NA>                          <NA>
      6                                                                Total                 3                              3                             1
      7                                                                 High         1 (33.3%)                              0                             0
      8                                                               Normal         2 (66.7%)                       3 (100%)                      1 (100%)
      9                                                               Normal              <NA>                           <NA>                          <NA>
      10                                                               Total                 2                              2                             1
      11                                                              Normal          2 (100%)                       2 (100%)                      1 (100%)

# tbl_shift(by) messaging

    Code
      tbl <- tbl_shift(data = dplyr::mutate(dplyr::filter(adlb, PARAMCD %in% "CHOL"), TRTA = as.character(TRTA)), strata = BNRIND, variable = ANRIND, by = TRTA, data_header = adsl,
      strata_location = "new_column")
    Message
      i Converting column "TRTA" to a factor.

# tbl_shift(data_header) messaging

    Code
      tbl_shift(data = dplyr::filter(adlb, PARAMCD %in% "CHOL"), strata = BNRIND, variable = ANRIND, data_header = dplyr::mutate(adsl, asldfk = TRUE), strata_location = "new_column")
    Condition
      Error in `tbl_shift()`:
      ! The data frame passed in the `data_header` argument should only include columns that will be used to merge with `data`.
      i Based on the other inputs, this likely means only including "USUBJID".

# add_overall.tbl_shift()

    Code
      as.data.frame(tbl)
    Output
        Baseline  \nNCI-CTCAE Grade Post-baseline  \nNCI-CTCAE Grade Placebo  \nN = 86 Xanomeline High Dose  \nN = 84 Xanomeline Low Dose  \nN = 84 Overall  \nN = 254
      1                        High                            Total                 2                              2                             4                  8
      2                        <NA>                              Low                 0                      1 (50.0%)                     1 (25.0%)          2 (25.0%)
      3                        <NA>                           Normal          2 (100%)                      1 (50.0%)                     3 (75.0%)          6 (75.0%)
      4                         Low                            Total                 3                              3                             1                  7
      5                        <NA>                             High         1 (33.3%)                              0                             0          1 (14.3%)
      6                        <NA>                           Normal         2 (66.7%)                       3 (100%)                      1 (100%)          6 (85.7%)
      7                      Normal                            Total                 2                              2                             1                  5
      8                        <NA>                           Normal          2 (100%)                       2 (100%)                      1 (100%)           5 (100%)

---

    Code
      as.data.frame(tbl)
    Output
         Baseline  \nNCI-CTCAE Grade  \n    Post-baseline  \nNCI-CTCAE Grade Placebo  \nN = 86 Xanomeline High Dose  \nN = 84 Xanomeline Low Dose  \nN = 84 Overall  \nN = 254
      1                                                                 High              <NA>                           <NA>                          <NA>               <NA>
      2                                                                Total                 2                              2                             4                  8
      3                                                                  Low                 0                      1 (50.0%)                     1 (25.0%)          2 (25.0%)
      4                                                               Normal          2 (100%)                      1 (50.0%)                     3 (75.0%)          6 (75.0%)
      5                                                                  Low              <NA>                           <NA>                          <NA>               <NA>
      6                                                                Total                 3                              3                             1                  7
      7                                                                 High         1 (33.3%)                              0                             0          1 (14.3%)
      8                                                               Normal         2 (66.7%)                       3 (100%)                      1 (100%)          6 (85.7%)
      9                                                               Normal              <NA>                           <NA>                          <NA>               <NA>
      10                                                               Total                 2                              2                             1                  5
      11                                                              Normal          2 (100%)                       2 (100%)                      1 (100%)           5 (100%)

# add_overall.tbl_shift() messaging

    Code
      tbl <- add_overall(modify_table_body(tbl_shift(data = dplyr::filter(adlb, PARAMCD %in% "CHOL"), strata = BNRIND, variable = ANRIND, by = TRTA, data_header = adsl, strata_location = "new_column"),
      ~ dplyr::filter(.x, dplyr::row_number() %in% 1:5)))
    Message
      ! The structures of the original table and the overall table are not identical, and the resulting table may be malformed.

---

    Code
      tbl <- add_overall(tbl_shift(data = dplyr::filter(adlb, PARAMCD %in% "CHOL"), strata = BNRIND, variable = ANRIND, data_header = adsl, strata_location = "new_column"))
    Message
      Original table was not stratified, and overall column cannot be added.
      i Table has been returned unaltered.

# add_overall.tbl_shift(strata=NULL) messaging

    Code
      as.data.frame(tbl)
    Output
                        Cohort Baseline  \nNCI-CTCAE Grade     High       Low    Normal
      1 All Participants, N=20                        High        0 2 (10.0%) 6 (30.0%)
      2                   <NA>                         Low 1 (5.0%)         0 6 (30.0%)
      3                   <NA>                      Normal        0         0 5 (25.0%)

