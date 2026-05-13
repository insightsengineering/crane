# tbl_hierarchical_rate_and_count() works

    Code
      as.data.frame(tbl)
    Output
              Body System or Organ Class  \n    MedDRA Preferred Term Placebo  \n(N = 86) Xanomeline High Dose  \n(N = 84) Xanomeline Low Dose  \n(N = 84) All Participants  \n(N = 254)
      1  Total number of participants with at least one adverse event            3 (3.5%)                         2 (2.4%)                        1 (1.2%)                      6 (2.4%)
      2                                Overall total number of events                   5                                4                               1                            10
      3                                    GASTROINTESTINAL DISORDERS                <NA>                             <NA>                            <NA>                          <NA>
      4  Total number of participants with at least one adverse event            3 (3.5%)                                0                               0                      3 (1.2%)
      5                                        Total number of events                   3                                0                               0                             3
      6                                                     DIARRHOEA            3 (3.5%)                                0                               0                      3 (1.2%)
      7          GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS                <NA>                             <NA>                            <NA>                          <NA>
      8  Total number of participants with at least one adverse event            1 (1.2%)                         2 (2.4%)                        1 (1.2%)                      4 (1.6%)
      9                                        Total number of events                   2                                4                               1                             7
      10                                    APPLICATION SITE ERYTHEMA            1 (1.2%)                         1 (1.2%)                        1 (1.2%)                      3 (1.2%)
      11                                    APPLICATION SITE PRURITUS            1 (1.2%)                         2 (2.4%)                               0                      3 (1.2%)
      12                                                      FATIGUE                   0                         1 (1.2%)                               0                      1 (0.4%)

---

    Code
      as.data.frame(tbl)
    Output
         Body System or Organ Class  \n    High Level Term  \n        Dictionary-Derived Term Placebo  \n(N = 86) Xanomeline High Dose  \n(N = 84) Xanomeline Low Dose  \n(N = 84) All Participants  \n(N = 254)
      1                          Total number of participants with at least one adverse event            3 (3.5%)                         2 (2.4%)                        1 (1.2%)                      6 (2.4%)
      2                                                        Overall total number of events                   5                                4                               1                            10
      3                                                            GASTROINTESTINAL DISORDERS                <NA>                             <NA>                            <NA>                          <NA>
      4                          Total number of participants with at least one adverse event            3 (3.5%)                                0                               0                      3 (1.2%)
      5                                                                Total number of events                   3                                0                               0                             3
      6                                                                              HLT_0148                <NA>                             <NA>                            <NA>                          <NA>
      7                          Total number of participants with at least one adverse event            3 (3.5%)                                0                               0                      3 (1.2%)
      8                                                                Total number of events                   3                                0                               0                             3
      9                                                                             DIARRHOEA            3 (3.5%)                                0                               0                      3 (1.2%)
      10                                 GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS                <NA>                             <NA>                            <NA>                          <NA>
      11                         Total number of participants with at least one adverse event            1 (1.2%)                         2 (2.4%)                        1 (1.2%)                      4 (1.6%)
      12                                                               Total number of events                   2                                4                               1                             7
      13                                                                             HLT_0043                <NA>                             <NA>                            <NA>                          <NA>
      14                         Total number of participants with at least one adverse event                   0                         1 (1.2%)                               0                      1 (0.4%)
      15                                                               Total number of events                   0                                1                               0                             1
      16                                                                              FATIGUE                   0                         1 (1.2%)                               0                      1 (0.4%)
      17                                                                             HLT_0317                <NA>                             <NA>                            <NA>                          <NA>
      18                         Total number of participants with at least one adverse event            1 (1.2%)                         2 (2.4%)                               0                      3 (1.2%)
      19                                                               Total number of events                   1                                2                               0                             3
      20                                                            APPLICATION SITE PRURITUS            1 (1.2%)                         2 (2.4%)                               0                      3 (1.2%)
      21                                                                             HLT_0617                <NA>                             <NA>                            <NA>                          <NA>
      22                         Total number of participants with at least one adverse event            1 (1.2%)                         1 (1.2%)                        1 (1.2%)                      3 (1.2%)
      23                                                               Total number of events                   1                                1                               1                             3
      24                                                            APPLICATION SITE ERYTHEMA            1 (1.2%)                         1 (1.2%)                        1 (1.2%)                      3 (1.2%)

# tbl_hierarchical_rate_and_count() emits zero-rows for unobserved factor levels

    Code
      as.data.frame(tbl)
    Output
                              AEBODSYS  \n    Dictionary-Derived Term Placebo  \n(N = 86) Xanomeline High Dose  \n(N = 84) Xanomeline Low Dose  \n(N = 84)
      1  Total number of participants with at least one adverse event            3 (3.5%)                         2 (2.4%)                        1 (1.2%)
      2                                Overall total number of events                   5                                4                               1
      3                                    GASTROINTESTINAL DISORDERS                <NA>                             <NA>                            <NA>
      4  Total number of participants with at least one adverse event            3 (3.5%)                                0                               0
      5                                        Total number of events                   3                                0                               0
      6                                                     DIARRHOEA            3 (3.5%)                                0                               0
      7          GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS                <NA>                             <NA>                            <NA>
      8  Total number of participants with at least one adverse event            1 (1.2%)                         2 (2.4%)                        1 (1.2%)
      9                                        Total number of events                   2                                4                               1
      10                                    APPLICATION SITE ERYTHEMA            1 (1.2%)                         1 (1.2%)                        1 (1.2%)
      11                                    APPLICATION SITE PRURITUS            1 (1.2%)                         2 (2.4%)                               0
      12                                                      FATIGUE                   0                         1 (1.2%)                               0
      13                                               UNOBSERVED SOC                <NA>                             <NA>                            <NA>
      14 Total number of participants with at least one adverse event                   0                                0                               0
      15                                       Total number of events                   0                                0                               0

# tbl_hierarchical_rate_and_count() handles 0-row data with factor levels

    Code
      as.data.frame(tbl)
    Output
                             AEBODSYS  \n    Dictionary-Derived Term Placebo  \nN = 86 Xanomeline High Dose  \nN = 84 Xanomeline Low Dose  \nN = 84
      1 Total number of participants with at least one adverse event                 0                              0                             0
      2                                                        SOC_A              <NA>                           <NA>                          <NA>
      3 Total number of participants with at least one adverse event                 0                              0                             0
      4                                       Total number of events                 0                              0                             0
      5                                                        SOC_B              <NA>                           <NA>                          <NA>
      6 Total number of participants with at least one adverse event                 0                              0                             0
      7                                       Total number of events                 0                              0                             0

# tbl_hierarchical_rate_and_count() works only with 2 or 3 variables

    Code
      tbl <- tbl_hierarchical_rate_and_count(ADAE_subset, denominator = cards::ADSL,
      by = TRTA, variables = c(AEBODSYS), sort = "descending")
    Condition
      Error in `tbl_hierarchical_rate_and_count()`:
      ! The `variables` argument must select 2 or 3 columns
      i For a single variable summary, use `gtsummary::hierarchical(variables="AEBODSYS")`

---

    Code
      tbl <- tbl_hierarchical_rate_and_count(ADAE_subset, denominator = cards::ADSL,
      by = TRTA, variables = c(SEX, AEBODSYS, AEDECOD, SAFFL), sort = "descending")
    Condition
      Error in `tbl_hierarchical_rate_and_count()`:
      ! The `variables` argument must select 2 or 3 columns
      i Columns select are typically `c(AEBODSYS, AEDECOD)` or `c(AEBODSYS, AEHLT, AEDECOD)`

