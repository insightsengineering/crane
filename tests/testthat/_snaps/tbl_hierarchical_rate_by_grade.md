# tbl_hierarchical_rate_by_grade() works

    Code
      as.data.frame(tbl)[1:25, ]
    Output
         MedDRA System Organ Class  \n    MedDRA Preferred Term         Grade Placebo  \nN = 86 Xanomeline High Dose  \nN = 84 Xanomeline Low Dose  \nN = 84
      1                                  - Any adverse events - - Any Grade -        26 (30.2%)                     42 (50.0%)                    40 (47.6%)
      2                                                                     1        20 (23.3%)                     23 (27.4%)                    20 (23.8%)
      3                                                                     2          2 (2.3%)                      9 (10.7%)                    10 (11.9%)
      4                                                                     3          3 (3.5%)                     10 (11.9%)                      8 (9.5%)
      5                                                                     4          1 (1.2%)                              0                      2 (2.4%)
      6                                       CARDIAC DISORDERS                            <NA>                           <NA>                          <NA>
      7                                             - Overall - - Any Grade -          2 (2.3%)                       3 (3.6%)                             0
      8                                                                     1          1 (1.2%)                       1 (1.2%)                             0
      9                                                                     2                 0                       2 (2.4%)                             0
      10                                                                    4          1 (1.2%)                              0                             0
      11                   ATRIOVENTRICULAR BLOCK SECOND DEGREE - Any Grade -          2 (2.3%)                       3 (3.6%)                             0
      12                                                                    1          1 (1.2%)                       1 (1.2%)                             0
      13                                                                    2                 0                       2 (2.4%)                             0
      14                                                                    4          1 (1.2%)                              0                             0
      15                             GASTROINTESTINAL DISORDERS                            <NA>                           <NA>                          <NA>
      16                                            - Overall - - Any Grade -         9 (10.5%)                       4 (4.8%)                      5 (6.0%)
      17                                                                    1         9 (10.5%)                       2 (2.4%)                      5 (6.0%)
      18                                                                    2                 0                       2 (2.4%)                             0
      19                                              DIARRHOEA - Any Grade -         9 (10.5%)                       4 (4.8%)                      5 (6.0%)
      20                                                                    1         9 (10.5%)                       2 (2.4%)                      5 (6.0%)
      21                                                                    2                 0                       2 (2.4%)                             0
      22   GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS                            <NA>                           <NA>                          <NA>
      23                                            - Overall - - Any Grade -          8 (9.3%)                     25 (29.8%)                    24 (28.6%)
      24                                                                    1          7 (8.1%)                     12 (14.3%)                    12 (14.3%)
      25                                                                    2                 0                       4 (4.8%)                      4 (4.8%)

---

    Code
      as.data.frame(tbl)[1:25, ]
    Output
         MedDRA System Organ Class  \n    MedDRA Preferred Term         Grade Placebo  \nN = 86 Xanomeline High Dose  \nN = 84 Xanomeline Low Dose  \nN = 84
      1                                  - Any adverse events - - Any Grade -        26 (30.2%)                     42 (50.0%)                    40 (47.6%)
      2                                                             Grade 1-2        22 (25.6%)                     32 (38.1%)                    30 (35.7%)
      3                                                                     1        20 (23.3%)                     23 (27.4%)                    20 (23.8%)
      4                                                                     2          2 (2.3%)                      9 (10.7%)                    10 (11.9%)
      5                                                             Grade 3-4          4 (4.7%)                     10 (11.9%)                    10 (11.9%)
      6                                                                     3          3 (3.5%)                     10 (11.9%)                      8 (9.5%)
      7                                                                     4          1 (1.2%)                              0                      2 (2.4%)
      8                                       CARDIAC DISORDERS                            <NA>                           <NA>                          <NA>
      9                                             - Overall - - Any Grade -          2 (2.3%)                       3 (3.6%)                             0
      10                                                            Grade 1-2          1 (1.2%)                       3 (3.6%)                             0
      11                                                                    1          1 (1.2%)                       1 (1.2%)                             0
      12                                                                    2                 0                       2 (2.4%)                             0
      13                                                            Grade 3-4          1 (1.2%)                              0                             0
      14                                                                    4          1 (1.2%)                              0                             0
      15                   ATRIOVENTRICULAR BLOCK SECOND DEGREE - Any Grade -          2 (2.3%)                       3 (3.6%)                             0
      16                                                            Grade 1-2          1 (1.2%)                       3 (3.6%)                             0
      17                                                                    1          1 (1.2%)                       1 (1.2%)                             0
      18                                                                    2                 0                       2 (2.4%)                             0
      19                                                            Grade 3-4          1 (1.2%)                              0                             0
      20                                                                    4          1 (1.2%)                              0                             0
      21                             GASTROINTESTINAL DISORDERS                            <NA>                           <NA>                          <NA>
      22                                            - Overall - - Any Grade -         9 (10.5%)                       4 (4.8%)                      5 (6.0%)
      23                                                            Grade 1-2         9 (10.5%)                       4 (4.8%)                      5 (6.0%)
      24                                                                    1         9 (10.5%)                       2 (2.4%)                      5 (6.0%)
      25                                                                    2                 0                       2 (2.4%)                             0

---

    Code
      as.data.frame(tbl)[1, ]
    Output
        MedDRA System Organ Class  \n    MedDRA Preferred Term         Grade Placebo  \nN = 86 Xanomeline High Dose  \nN = 84 Xanomeline Low Dose  \nN = 84
      1                                 - Any adverse events - - Any Grade -  26,0/86, 30.233%               42,0/84, 50.000%              40,0/84, 47.619%

# tbl_hierarchical_rate_by_grade(include_overall) works

    Code
      as.data.frame(tbl)[1:25, ]
    Output
         MedDRA System Organ Class  \n    MedDRA Preferred Term         Grade Placebo  \nN = 86 Xanomeline High Dose  \nN = 84 Xanomeline Low Dose  \nN = 84
      1                                  - Any adverse events - - Any Grade -        26 (30.2%)                     42 (50.0%)                    40 (47.6%)
      2                                                             Grade 1-2        22 (25.6%)                     32 (38.1%)                    30 (35.7%)
      3                                                                     1        20 (23.3%)                     23 (27.4%)                    20 (23.8%)
      4                                                                     2          2 (2.3%)                      9 (10.7%)                    10 (11.9%)
      5                                                             Grade 3-4          4 (4.7%)                     10 (11.9%)                    10 (11.9%)
      6                                                                     3          3 (3.5%)                     10 (11.9%)                      8 (9.5%)
      7                                                                     4          1 (1.2%)                              0                      2 (2.4%)
      8                                       CARDIAC DISORDERS                            <NA>                           <NA>                          <NA>
      9                                             - Overall - - Any Grade -          2 (2.3%)                       3 (3.6%)                             0
      10                                                            Grade 1-2          1 (1.2%)                       3 (3.6%)                             0
      11                                                                    1          1 (1.2%)                       1 (1.2%)                             0
      12                                                                    2                 0                       2 (2.4%)                             0
      13                                                            Grade 3-4          1 (1.2%)                              0                             0
      14                                                                    4          1 (1.2%)                              0                             0
      15                   ATRIOVENTRICULAR BLOCK SECOND DEGREE - Any Grade -          2 (2.3%)                       3 (3.6%)                             0
      16                                                            Grade 1-2          1 (1.2%)                       3 (3.6%)                             0
      17                                                                    1          1 (1.2%)                       1 (1.2%)                             0
      18                                                                    2                 0                       2 (2.4%)                             0
      19                                                            Grade 3-4          1 (1.2%)                              0                             0
      20                                                                    4          1 (1.2%)                              0                             0
      21                             GASTROINTESTINAL DISORDERS                            <NA>                           <NA>                          <NA>
      22                                            - Overall - - Any Grade -         9 (10.5%)                       4 (4.8%)                      5 (6.0%)
      23                                                            Grade 1-2         9 (10.5%)                       4 (4.8%)                      5 (6.0%)
      24                                                                    1         9 (10.5%)                       2 (2.4%)                      5 (6.0%)
      25                                                                    2                 0                       2 (2.4%)                             0

---

    Code
      as.data.frame(tbl)[1:25, ]
    Output
         MedDRA System Organ Class  \n    MedDRA Preferred Term         Grade Placebo  \nN = 86 Xanomeline High Dose  \nN = 84 Xanomeline Low Dose  \nN = 84
      1                                       CARDIAC DISORDERS                            <NA>                           <NA>                          <NA>
      2                    ATRIOVENTRICULAR BLOCK SECOND DEGREE - Any Grade -          2 (2.3%)                       3 (3.6%)                             0
      3                                                             Grade 1-2          1 (1.2%)                       3 (3.6%)                             0
      4                                                                     1          1 (1.2%)                       1 (1.2%)                             0
      5                                                                     2                 0                       2 (2.4%)                             0
      6                                                             Grade 3-4          1 (1.2%)                              0                             0
      7                                                                     4          1 (1.2%)                              0                             0
      8                              GASTROINTESTINAL DISORDERS                            <NA>                           <NA>                          <NA>
      9                                               DIARRHOEA - Any Grade -         9 (10.5%)                       4 (4.8%)                      5 (6.0%)
      10                                                            Grade 1-2         9 (10.5%)                       4 (4.8%)                      5 (6.0%)
      11                                                                    1         9 (10.5%)                       2 (2.4%)                      5 (6.0%)
      12                                                                    2                 0                       2 (2.4%)                             0
      13   GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS                            <NA>                           <NA>                          <NA>
      14                              APPLICATION SITE ERYTHEMA - Any Grade -          3 (3.5%)                     15 (17.9%)                    12 (14.3%)
      15                                                            Grade 1-2          3 (3.5%)                     12 (14.3%)                      7 (8.3%)
      16                                                                    1          3 (3.5%)                      9 (10.7%)                      4 (4.8%)
      17                                                                    2                 0                       3 (3.6%)                      3 (3.6%)
      18                                                            Grade 3-4                 0                       3 (3.6%)                      5 (6.0%)
      19                                                                    3                 0                       3 (3.6%)                      3 (3.6%)
      20                                                                    4                 0                              0                      2 (2.4%)
      21                              APPLICATION SITE PRURITUS - Any Grade -          6 (7.0%)                     22 (26.2%)                    22 (26.2%)
      22                                                            Grade 1-2          5 (5.8%)                     15 (17.9%)                    17 (20.2%)
      23                                                                    1          5 (5.8%)                     10 (11.9%)                    13 (15.5%)
      24                                                                    2                 0                       5 (6.0%)                      4 (4.8%)
      25                                                            Grade 3-4          1 (1.2%)                       7 (8.3%)                      5 (6.0%)

# tbl_hierarchical_rate_by_grade() error messaging works

    Code
      tbl <- tbl_hierarchical_rate_by_grade(ADAE_subset, variables = c(AEBODSYS,
        AEDECOD, AETOXGR), denominator = ADSL, by = TRTA, label = label,
      grades_exclude = 4:5)
    Condition
      Error in `tbl_hierarchical_rate_by_grade()`:
      ! The `grades_exclude` argument must be class <character> or empty, not an integer vector.

---

    Code
      tbl <- tbl_hierarchical_rate_by_grade(ADAE_subset, variables = c(AEBODSYS,
        AEDECOD, AETOXGR), denominator = ADSL, by = TRTA, label = label,
      grade_groups = list("Grade 5" ~ "5"))
    Condition
      Error in `tbl_hierarchical_rate_by_grade()`:
      ! Grade groups must be specified via a named list where each list element is a character vector of the grades to include in the grade group and each name is the corresponding name of the grade group. For example, `"Grade 3-4" = c("3", "4")`.

---

    Code
      tbl <- tbl_hierarchical_rate_by_grade(ADAE_subset, variables = c(AEBODSYS,
        AEDECOD, AETOXGR), denominator = ADSL, by = TRTA, label = label,
      grade_groups = list(`Grade 3-4` = c("3", "4"), `Grade 4-5` = c("4", "5")))
    Condition
      Error in `tbl_hierarchical_rate_by_grade()`:
      ! Grade groups specified via `grade_groups` cannot overlap. Please ensure that each grade is included in at most one grade group.

---

    Code
      tbl <- tbl_hierarchical_rate_by_grade(ADAE_subset, variables = c(AEBODSYS,
        AEDECOD, AETOXGR), denominator = ADSL, by = TRTA, label = label,
      grades_exclude = as.character(c(1:3, 5:7)))
    Condition
      Error in `tbl_hierarchical_rate_by_grade()`:
      ! Grade(s) "6" and "7" supplied to `grades_exclude` are invalid. All grades specified via `grades_exclude` must be levels of "AETOXGR".

