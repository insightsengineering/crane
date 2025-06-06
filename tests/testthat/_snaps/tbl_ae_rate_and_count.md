# tbl_hierarchical_rate_and_count() works

    Code
      as.data.frame(tbl)
    Output
         **Body System or Organ Class**  \n    **Dictionary-Derived Term** **Placebo**  \nN = 86 **Xanomeline High Dose**  \nN = 84 **Xanomeline Low Dose**  \nN = 84 **Overall**  \nN = 254
      1           Total number of patients with at least one adverse event              3 (3.5%)                           2 (2.4%)                          1 (1.2%)               6 (2.4%)
      2                                     Overall total number of events                     5                                  4                                 1                     10
      3                                         GASTROINTESTINAL DISORDERS                  <NA>                               <NA>                              <NA>                   <NA>
      4           Total number of patients with at least one adverse event              3 (3.5%)                                  0                                 0               3 (1.2%)
      5                                             Total number of events                     3                                  0                                 0                      3
      6                                                          DIARRHOEA              3 (3.5%)                                  0                                 0               3 (1.2%)
      7               GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS                  <NA>                               <NA>                              <NA>                   <NA>
      8           Total number of patients with at least one adverse event              1 (1.2%)                           2 (2.4%)                          1 (1.2%)               4 (1.6%)
      9                                             Total number of events                     2                                  4                                 1                      7
      10                                         APPLICATION SITE ERYTHEMA              1 (1.2%)                           1 (1.2%)                          1 (1.2%)               3 (1.2%)
      11                                         APPLICATION SITE PRURITUS              1 (1.2%)                           2 (2.4%)                                 0               3 (1.2%)
      12                                                           FATIGUE                     0                           1 (1.2%)                                 0               1 (0.4%)

---

    Code
      as.data.frame(tbl)
    Output
         **Body System or Organ Class**  \n    **High Level Term**  \n        **Dictionary-Derived Term** **Placebo**  \nN = 86 **Xanomeline High Dose**  \nN = 84 **Xanomeline Low Dose**  \nN = 84 **Overall**  \nN = 254
      1                                          Total number of patients with at least one adverse event              3 (3.5%)                           2 (2.4%)                          1 (1.2%)               6 (2.4%)
      2                                                                    Overall total number of events                     5                                  4                                 1                     10
      3                                                                        GASTROINTESTINAL DISORDERS                  <NA>                               <NA>                              <NA>                   <NA>
      4                                          Total number of patients with at least one adverse event              3 (3.5%)                                  0                                 0               3 (1.2%)
      5                                                                            Total number of events                     3                                  0                                 0                      3
      6                                                                                          HLT_0148                  <NA>                               <NA>                              <NA>                   <NA>
      7                                          Total number of patients with at least one adverse event              3 (3.5%)                                  0                                 0               3 (1.2%)
      8                                                                            Total number of events                     3                                  0                                 0                      3
      9                                                                                         DIARRHOEA              3 (3.5%)                                  0                                 0               3 (1.2%)
      10                                             GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS                  <NA>                               <NA>                              <NA>                   <NA>
      11                                         Total number of patients with at least one adverse event              1 (1.2%)                           2 (2.4%)                          1 (1.2%)               4 (1.6%)
      12                                                                           Total number of events                     2                                  4                                 1                      7
      13                                                                                         HLT_0043                  <NA>                               <NA>                              <NA>                   <NA>
      14                                         Total number of patients with at least one adverse event                     0                           1 (1.2%)                                 0               1 (0.4%)
      15                                                                           Total number of events                     0                                  1                                 0                      1
      16                                                                                          FATIGUE                     0                           1 (1.2%)                                 0               1 (0.4%)
      17                                                                                         HLT_0317                  <NA>                               <NA>                              <NA>                   <NA>
      18                                         Total number of patients with at least one adverse event              1 (1.2%)                           2 (2.4%)                                 0               3 (1.2%)
      19                                                                           Total number of events                     1                                  2                                 0                      3
      20                                                                        APPLICATION SITE PRURITUS              1 (1.2%)                           2 (2.4%)                                 0               3 (1.2%)
      21                                                                                         HLT_0617                  <NA>                               <NA>                              <NA>                   <NA>
      22                                         Total number of patients with at least one adverse event              1 (1.2%)                           1 (1.2%)                          1 (1.2%)               3 (1.2%)
      23                                                                           Total number of events                     1                                  1                                 1                      3
      24                                                                        APPLICATION SITE ERYTHEMA              1 (1.2%)                           1 (1.2%)                          1 (1.2%)               3 (1.2%)

