# tbl_shift(strata_location)

    Code
      as.data.frame(tbl)
    Output
        Baseline  \nNCI-CTCAE Grade Post-baseline  \nNCI-CTCAE Grade Placebo  \nN = 86 Xanomeline High Dose  \nN = 84 Xanomeline Low Dose  \nN = 84
      1                        High                            Total                 3                              3                             4
      2                        <NA>                           Normal          3 (100%)                       3 (100%)                      4 (100%)
      3                         Low                            Total                 3                              3                             1
      4                        <NA>                           Normal          3 (100%)                       3 (100%)                      1 (100%)
      5                      Normal                            Total                 1                              1                             1
      6                        <NA>                              Low                 0                              0                      1 (100%)
      7                        <NA>                           Normal          1 (100%)                       1 (100%)                             0

---

    Code
      as.data.frame(tbl)
    Output
         Baseline  \nNCI-CTCAE Grade  \n    Post-baseline  \nNCI-CTCAE Grade Placebo  \nN = 86 Xanomeline High Dose  \nN = 84 Xanomeline Low Dose  \nN = 84
      1                                                                 High              <NA>                           <NA>                          <NA>
      2                                                                Total                 3                              3                             4
      3                                                               Normal          3 (100%)                       3 (100%)                      4 (100%)
      4                                                                  Low              <NA>                           <NA>                          <NA>
      5                                                                Total                 3                              3                             1
      6                                                               Normal          3 (100%)                       3 (100%)                      1 (100%)
      7                                                               Normal              <NA>                           <NA>                          <NA>
      8                                                                Total                 1                              1                             1
      9                                                                  Low                 0                              0                      1 (100%)
      10                                                              Normal          1 (100%)                       1 (100%)                             0

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
      1                        High                            Total                 3                              3                             4                 10
      2                        <NA>                           Normal          3 (100%)                       3 (100%)                      4 (100%)          10 (100%)
      3                         Low                            Total                 3                              3                             1                  7
      4                        <NA>                           Normal          3 (100%)                       3 (100%)                      1 (100%)           7 (100%)
      5                      Normal                            Total                 1                              1                             1                  3
      6                        <NA>                              Low                 0                              0                      1 (100%)          1 (33.3%)
      7                        <NA>                           Normal          1 (100%)                       1 (100%)                             0          2 (66.7%)

---

    Code
      as.data.frame(tbl)
    Output
         Baseline  \nNCI-CTCAE Grade  \n    Post-baseline  \nNCI-CTCAE Grade Placebo  \nN = 86 Xanomeline High Dose  \nN = 84 Xanomeline Low Dose  \nN = 84 Overall  \nN = 254
      1                                                                 High              <NA>                           <NA>                          <NA>               <NA>
      2                                                                Total                 3                              3                             4                 10
      3                                                               Normal          3 (100%)                       3 (100%)                      4 (100%)          10 (100%)
      4                                                                  Low              <NA>                           <NA>                          <NA>               <NA>
      5                                                                Total                 3                              3                             1                  7
      6                                                               Normal          3 (100%)                       3 (100%)                      1 (100%)           7 (100%)
      7                                                               Normal              <NA>                           <NA>                          <NA>               <NA>
      8                                                                Total                 1                              1                             1                  3
      9                                                                  Low                 0                              0                      1 (100%)          1 (33.3%)
      10                                                              Normal          1 (100%)                       1 (100%)                             0          2 (66.7%)

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
                        Cohort Baseline  \nNCI-CTCAE Grade      Low     Normal
      1 All Participants, N=20                        High        0 10 (50.0%)
      2                   <NA>                         Low        0  7 (35.0%)
      3                   <NA>                      Normal 1 (5.0%)  2 (10.0%)

