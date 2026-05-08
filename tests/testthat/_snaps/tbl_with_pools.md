# tbl_with_pools() validates inputs correctly

    Code
      tbl_with_pools(data = list(A = 1), pools = standard_pools, by = "TRTA",
      .tbl_fun = tbl_summary)
    Condition
      Error in `tbl_with_pools()`:
      x `data` must be a data frame.
      i Provided data is of class <list>.

---

    Code
      tbl_with_pools(data = ADSL_subset, pools = standard_pools, by = "MISSING_VAR",
        .tbl_fun = tbl_summary)
    Condition
      Error in `tbl_with_pools()`:
      x The grouping variable "MISSING_VAR" must exist in `data`.

---

    Code
      tbl_with_pools(data = ADAE_subset, pools = standard_pools, by = "TRTA",
        denominator = data.frame(USUBJID = 1), .tbl_fun = tbl_summary)
    Condition
      Error in `tbl_with_pools()`:
      x The grouping variable "TRTA" must exist in `denominator`.

---

    Code
      tbl_with_pools(data = ADSL_subset, pools = c("Just a vector"), by = "TRTA",
      .tbl_fun = tbl_summary)
    Condition
      Error in `tbl_with_pools()`:
      x `pools` must be a fully named list.

---

    Code
      tbl_with_pools(data = ADSL_subset, pools = standard_pools, by = "TRTA",
        .tbl_fun = "not_a_function")
    Condition
      Error in `tbl_with_pools()`:
      x `.tbl_fun` must be a function (e.g., `gtsummary::tbl_summary`).

---

    Code
      tbl_with_pools(data = ADSL_subset, pools = standard_pools, by = "TRTA",
        denominator = c("I am", "not a data frame"), keep_original = FALSE, .tbl_fun = tbl_summary)
    Condition
      Error in `tbl_with_pools()`:
      x `denominator` must be a data frame or NULL.

# tbl_with_pools() works with standard functions like tbl_summary

    Code
      as.data.frame(tbl)
    Output
        Characteristic          Placebo  \nN = 8 Xanomeline High Dose  \nN = 11 Xanomeline Low Dose  \nN = 11  Any Xanomeline  \nN = 22    All Patients  \nN = 30
      1            Age 74<U+00A0>(64,<U+00A0>83)      61<U+00A0>(56,<U+00A0>77)     74<U+00A0>(68,<U+00A0>80) 71<U+00A0>(61,<U+00A0>79) 71<U+00A0>(61,<U+00A0>79)
      2            Sex                      <NA>                           <NA>                          <NA>                      <NA>                      <NA>
      3              F          4<U+00A0>(50.0%)               5<U+00A0>(45.5%)              3<U+00A0>(27.3%)          8<U+00A0>(36.4%)         12<U+00A0>(40.0%)
      4              M          4<U+00A0>(50.0%)               6<U+00A0>(54.5%)              8<U+00A0>(72.7%)         14<U+00A0>(63.6%)         18<U+00A0>(60.0%)

# tbl_with_pools() passes the denominator correctly for custom functions

    Code
      as.data.frame(tbl)
    Condition
      Warning in `do.call()`:
      unable to translate 'Body System or Organ Class  
      <U+00A0><U+00A0><U+00A0><U+00A0>Dictionary-Derived Term' to native encoding
    Output
         Body System or Organ Class  \n<U+00A0><U+00A0><U+00A0><U+00A0>Dictionary-Derived Term Placebo  \n(N = 8) Xanomeline High Dose  \n(N = 11) Xanomeline Low Dose  \n(N = 11) Any Xanomeline  \n(N = 22)
      1                           Total number of participants with at least one adverse event   3<U+00A0>(37.5%)                 2<U+00A0>(18.2%)                2<U+00A0>(18.2%)           4<U+00A0>(18.2%)
      2                                                         Overall total number of events                 11                                4                              15                         19
      3                                                                      CARDIAC DISORDERS               <NA>                             <NA>                            <NA>                       <NA>
      4                           Total number of participants with at least one adverse event   2<U+00A0>(25.0%)                                0                               0                       <NA>
      5                                                                 Total number of events                  2                                0                               0                       <NA>
      6                                                   ATRIOVENTRICULAR BLOCK SECOND DEGREE   1<U+00A0>(12.5%)                                0                               0                       <NA>
      7                                                               BUNDLE BRANCH BLOCK LEFT   1<U+00A0>(12.5%)                                0                               0                       <NA>
      8                                                             GASTROINTESTINAL DISORDERS               <NA>                             <NA>                            <NA>                       <NA>
      9                           Total number of participants with at least one adverse event   2<U+00A0>(25.0%)                                0                               0                       <NA>
      10                                                                Total number of events                  3                                0                               0                       <NA>
      11                                                                             DIARRHOEA   1<U+00A0>(12.5%)                                0                               0                       <NA>
      12                                                                         HIATUS HERNIA   1<U+00A0>(12.5%)                                0                               0                       <NA>
      13                                  GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS               <NA>                             <NA>                            <NA>                       <NA>
      14                          Total number of participants with at least one adverse event   1<U+00A0>(12.5%)                 2<U+00A0>(18.2%)                 1<U+00A0>(9.1%)           3<U+00A0>(13.6%)
      15                                                                Total number of events                  2                                4                               3                          7
      16                                                             APPLICATION SITE ERYTHEMA   1<U+00A0>(12.5%)                  1<U+00A0>(9.1%)                               0            1<U+00A0>(4.5%)
      17                                                             APPLICATION SITE PRURITUS   1<U+00A0>(12.5%)                 2<U+00A0>(18.2%)                 1<U+00A0>(9.1%)           3<U+00A0>(13.6%)
      18                                                             APPLICATION SITE VESICLES                  0                                0                 1<U+00A0>(9.1%)            1<U+00A0>(4.5%)
      19                                                                               FATIGUE                  0                  1<U+00A0>(9.1%)                               0            1<U+00A0>(4.5%)
      20                                                           INFECTIONS AND INFESTATIONS               <NA>                             <NA>                            <NA>                       <NA>
      21                          Total number of participants with at least one adverse event   1<U+00A0>(12.5%)                                0                 1<U+00A0>(9.1%)            1<U+00A0>(4.5%)
      22                                                                Total number of events                  1                                0                               1                          1
      23                                                                   LOCALISED INFECTION                  0                                0                 1<U+00A0>(9.1%)            1<U+00A0>(4.5%)
      24                                                     UPPER RESPIRATORY TRACT INFECTION   1<U+00A0>(12.5%)                                0                               0                       <NA>
      25                                       RESPIRATORY, THORACIC AND MEDIASTINAL DISORDERS               <NA>                             <NA>                            <NA>                       <NA>
      26                          Total number of participants with at least one adverse event                  0                                0                 1<U+00A0>(9.1%)            1<U+00A0>(4.5%)
      27                                                                Total number of events                  0                                0                               2                          2
      28                                                                      NASAL CONGESTION                  0                                0                 1<U+00A0>(9.1%)            1<U+00A0>(4.5%)
      29                                                                PHARYNGOLARYNGEAL PAIN                  0                                0                 1<U+00A0>(9.1%)            1<U+00A0>(4.5%)
      30                                                SKIN AND SUBCUTANEOUS TISSUE DISORDERS               <NA>                             <NA>                            <NA>                       <NA>
      31                          Total number of participants with at least one adverse event   1<U+00A0>(12.5%)                                0                2<U+00A0>(18.2%)            2<U+00A0>(9.1%)
      32                                                                Total number of events                  3                                0                               9                          9
      33                                                                              ERYTHEMA   1<U+00A0>(12.5%)                                0                2<U+00A0>(18.2%)            2<U+00A0>(9.1%)
      34                                                                              PRURITUS                  0                                0                 1<U+00A0>(9.1%)            1<U+00A0>(4.5%)
      35                                                                  PRURITUS GENERALISED                  0                                0                 1<U+00A0>(9.1%)            1<U+00A0>(4.5%)
         All Patients  \n(N = 30)
      1          7<U+00A0>(23.3%)
      2                        30
      3                      <NA>
      4           2<U+00A0>(6.7%)
      5                         2
      6           1<U+00A0>(3.3%)
      7           1<U+00A0>(3.3%)
      8                      <NA>
      9           2<U+00A0>(6.7%)
      10                        3
      11          1<U+00A0>(3.3%)
      12          1<U+00A0>(3.3%)
      13                     <NA>
      14         4<U+00A0>(13.3%)
      15                        9
      16          2<U+00A0>(6.7%)
      17         4<U+00A0>(13.3%)
      18          1<U+00A0>(3.3%)
      19          1<U+00A0>(3.3%)
      20                     <NA>
      21          2<U+00A0>(6.7%)
      22                        2
      23          1<U+00A0>(3.3%)
      24          1<U+00A0>(3.3%)
      25                     <NA>
      26          1<U+00A0>(3.3%)
      27                        2
      28          1<U+00A0>(3.3%)
      29          1<U+00A0>(3.3%)
      30                     <NA>
      31         3<U+00A0>(10.0%)
      32                       12
      33         3<U+00A0>(10.0%)
      34          1<U+00A0>(3.3%)
      35          1<U+00A0>(3.3%)

# tbl_with_pools() warns and skips empty pools properly

    Code
      tbl_with_pools(data = ADSL_subset, pools = list(`Ghost Arm` = "Fake Drug"), by = "TRTA",
      denominator = NULL, keep_original = FALSE, .tbl_fun = tbl_summary)
    Condition
      Warning:
      Pool "Ghost Arm" has 0 rows in the data. Skipping.
      Error in `tbl_with_pools()`:
      ! No tables were generated. Check your pool definitions and data.

# tbl_with_pools() skips pools with 0 events to prevent cards engine crash

    Code
      tbl_with_pools(data = ADAE_edge, pools = list(`Drug C Only` = "Drug C"), by = "TRTA",
      denominator = ADSL_edge, keep_original = FALSE, .tbl_fun = tbl_hierarchical_rate_and_count,
      variables = c(AEBODSYS, AEDECOD))
    Condition
      Warning:
      Pool "Drug C Only" has 0 rows in the data. Skipping.
      Error in `tbl_with_pools()`:
      ! No tables were generated. Check your pool definitions and data.

# tbl_with_pools() skips when denominator has 0 patients but data has >0

    Code
      tbl_with_pools(data = ADAE_extra_arm, pools = list(`Drug Z Pool` = "Drug Z"),
      by = "TRTA", denominator = ADSL_missing_arm, keep_original = FALSE, .tbl_fun = tbl_hierarchical_rate_and_count,
      variables = c(AEBODSYS, AEDECOD))
    Condition
      Warning:
      Pool "Drug Z Pool" has 0 patients in the denominator. Skipping.
      Error in `tbl_with_pools()`:
      ! No tables were generated. Check your pool definitions and data.

# tbl_with_pools() skips if an rlang::expr() evaluates to 0 rows

    Code
      tbl_with_pools(data = ADSL_expr, pools = impossible_pool, by = "TRTA",
        denominator = NULL, keep_original = FALSE, .tbl_fun = tbl_summary, include = AGE)
    Condition
      Warning:
      Pool "Impossible Pool" has 0 rows in the data. Skipping.
      Error in `tbl_with_pools()`:
      ! No tables were generated. Check your pool definitions and data.

