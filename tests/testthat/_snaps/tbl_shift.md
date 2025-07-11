# tbl_shift() works

    Code
      as.data.frame(tbl)
    Output
        Baseline  \nNCI-CTCAE Grade Post-baseline  \nNCI-CTCAE Grade
      1                           0                            Total
      2                        <NA>                                0
      3                        <NA>                                1
      4                        <NA>                                2
      5                           1                            Total
      6                        <NA>                                1
      7                           2                            Total
      8                        <NA>                                2
        Placebo  \nN = 86 Xanomeline High Dose  \nN = 72
      1                81                             70
      2        77 (95.1%)                     66 (94.3%)
      3          2 (2.5%)                       4 (5.7%)
      4          2 (2.5%)                              0
      5                 0                              0
      6                 0                              0
      7                 5                              2
      8          5 (100%)                       2 (100%)
        Xanomeline Low Dose  \nN = 96
      1                            94
      2                    91 (96.8%)
      3                      1 (1.1%)
      4                      2 (2.1%)
      5                             1
      6                      1 (100%)
      7                             1
      8                      1 (100%)

