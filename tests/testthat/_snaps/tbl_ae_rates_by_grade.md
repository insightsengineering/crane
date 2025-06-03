# tbl_ae_rates_by_grade() works

    Code
      as.data.frame(tbl)[1:25, ]
    Output
         **MedDRA System Organ Class**  \n    **MedDRA Preferred Term**    \n**Grade** **Placebo**  \nN = 86 **Xanomeline High Dose**  \nN = 84 **Xanomeline Low Dose**  \nN = 84
      1                                          - Any adverse events -  - Any Grade -            26 (30.2%)                         42 (50.0%)                        40 (47.6%)
      2                                                                              1              7 (8.1%)                         12 (14.3%)                          7 (8.3%)
      3                                                                              2            13 (15.1%)                         11 (13.1%)                        13 (15.5%)
      4                                                                              3              2 (2.3%)                           8 (9.5%)                          6 (7.1%)
      5                                                                              4              3 (3.5%)                         11 (13.1%)                        12 (14.3%)
      6                                                                              5              1 (1.2%)                                  0                          2 (2.4%)
      7            GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS                                 <NA>                               <NA>                              <NA>
      8                                                     - Overall -  - Any Grade -              8 (9.3%)                         25 (29.8%)                        24 (28.6%)
      9                                                                              1              2 (2.3%)                           3 (3.6%)                          4 (4.8%)
      10                                                                             2              5 (5.8%)                          9 (10.7%)                          8 (9.5%)
      11                                                                             3                     0                           5 (6.0%)                          4 (4.8%)
      12                                                                             4              1 (1.2%)                           8 (9.5%)                          6 (7.1%)
      13                                                                             5                     0                                  0                          2 (2.4%)
      14                                      APPLICATION SITE PRURITUS  - Any Grade -              6 (7.0%)                         22 (26.2%)                        22 (26.2%)
      15                                                                             1              2 (2.3%)                           1 (1.2%)                          7 (8.3%)
      16                                                                             2              3 (3.5%)                          9 (10.7%)                          6 (7.1%)
      17                                                                             3                     0                           8 (9.5%)                          4 (4.8%)
      18                                                                             4              1 (1.2%)                           4 (4.8%)                          4 (4.8%)
      19                                                                             5                     0                                  0                          1 (1.2%)
      20                                      APPLICATION SITE ERYTHEMA  - Any Grade -              3 (3.5%)                         15 (17.9%)                        12 (14.3%)
      21                                                                             1              1 (1.2%)                           7 (8.3%)                          2 (2.4%)
      22                                                                             2              2 (2.3%)                           2 (2.4%)                          2 (2.4%)
      23                                                                             3                     0                           1 (1.2%)                          3 (3.6%)
      24                                                                             4                     0                           5 (6.0%)                          3 (3.6%)
      25                                                                             5                     0                                  0                          2 (2.4%)

---

    Code
      as.data.frame(tbl)[1:25, ]
    Output
         **MedDRA System Organ Class**  \n    **MedDRA Preferred Term**    \n**Grade** **Placebo**  \nN = 86 **Xanomeline High Dose**  \nN = 84 **Xanomeline Low Dose**  \nN = 84
      1                                          - Any adverse events -  - Any Grade -            26 (30.2%)                         42 (50.0%)                        40 (47.6%)
      2                                                                      Grade 1-2            20 (23.3%)                         23 (27.4%)                        20 (23.8%)
      3                                                                              1              7 (8.1%)                         12 (14.3%)                          7 (8.3%)
      4                                                                              2            13 (15.1%)                         11 (13.1%)                        13 (15.5%)
      5                                                                      Grade 3-4              5 (5.8%)                         19 (22.6%)                        18 (21.4%)
      6                                                                              3              2 (2.3%)                           8 (9.5%)                          6 (7.1%)
      7                                                                              4              3 (3.5%)                         11 (13.1%)                        12 (14.3%)
      8                                                                        Grade 5              1 (1.2%)                                  0                          2 (2.4%)
      9                                                                              5              1 (1.2%)                                  0                          2 (2.4%)
      10           GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS                                 <NA>                               <NA>                              <NA>
      11                                                    - Overall -  - Any Grade -              8 (9.3%)                         25 (29.8%)                        24 (28.6%)
      12                                                                     Grade 1-2              7 (8.1%)                         12 (14.3%)                        12 (14.3%)
      13                                                                             1              2 (2.3%)                           3 (3.6%)                          4 (4.8%)
      14                                                                             2              5 (5.8%)                          9 (10.7%)                          8 (9.5%)
      15                                                                     Grade 3-4              1 (1.2%)                         13 (15.5%)                        10 (11.9%)
      16                                                                             3                     0                           5 (6.0%)                          4 (4.8%)
      17                                                                             4              1 (1.2%)                           8 (9.5%)                          6 (7.1%)
      18                                                                       Grade 5                     0                                  0                          2 (2.4%)
      19                                                                             5                     0                                  0                          2 (2.4%)
      20                                      APPLICATION SITE PRURITUS  - Any Grade -              6 (7.0%)                         22 (26.2%)                        22 (26.2%)
      21                                                                     Grade 1-2              5 (5.8%)                         10 (11.9%)                        13 (15.5%)
      22                                                                             1              2 (2.3%)                           1 (1.2%)                          7 (8.3%)
      23                                                                             2              3 (3.5%)                          9 (10.7%)                          6 (7.1%)
      24                                                                     Grade 3-4              1 (1.2%)                         12 (14.3%)                          8 (9.5%)
      25                                                                             3                     0                           8 (9.5%)                          4 (4.8%)

# tbl_ae_rates_by_grade(include_overall) works

    Code
      as.data.frame(tbl)[1:25, ]
    Output
         **MedDRA System Organ Class**  \n    **MedDRA Preferred Term**    \n**Grade** **Placebo**  \nN = 86 **Xanomeline High Dose**  \nN = 84 **Xanomeline Low Dose**  \nN = 84
      1                                          - Any adverse events -  - Any Grade -            26 (30.2%)                         42 (50.0%)                        40 (47.6%)
      2                                                                      Grade 1-2            20 (23.3%)                         23 (27.4%)                        20 (23.8%)
      3                                                                              1              7 (8.1%)                         12 (14.3%)                          7 (8.3%)
      4                                                                              2            13 (15.1%)                         11 (13.1%)                        13 (15.5%)
      5                                                                      Grade 3-4              5 (5.8%)                         19 (22.6%)                        18 (21.4%)
      6                                                                              3              2 (2.3%)                           8 (9.5%)                          6 (7.1%)
      7                                                                              4              3 (3.5%)                         11 (13.1%)                        12 (14.3%)
      8                                                                        Grade 5              1 (1.2%)                                  0                          2 (2.4%)
      9                                                                              5              1 (1.2%)                                  0                          2 (2.4%)
      10           GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS                                 <NA>                               <NA>                              <NA>
      11                                                    - Overall -  - Any Grade -              8 (9.3%)                         25 (29.8%)                        24 (28.6%)
      12                                                                     Grade 1-2              7 (8.1%)                         12 (14.3%)                        12 (14.3%)
      13                                                                             1              2 (2.3%)                           3 (3.6%)                          4 (4.8%)
      14                                                                             2              5 (5.8%)                          9 (10.7%)                          8 (9.5%)
      15                                                                     Grade 3-4              1 (1.2%)                         13 (15.5%)                        10 (11.9%)
      16                                                                             3                     0                           5 (6.0%)                          4 (4.8%)
      17                                                                             4              1 (1.2%)                           8 (9.5%)                          6 (7.1%)
      18                                                                       Grade 5                     0                                  0                          2 (2.4%)
      19                                                                             5                     0                                  0                          2 (2.4%)
      20                                      APPLICATION SITE PRURITUS  - Any Grade -              6 (7.0%)                         22 (26.2%)                        22 (26.2%)
      21                                                                     Grade 1-2              5 (5.8%)                         10 (11.9%)                        13 (15.5%)
      22                                                                             1              2 (2.3%)                           1 (1.2%)                          7 (8.3%)
      23                                                                             2              3 (3.5%)                          9 (10.7%)                          6 (7.1%)
      24                                                                     Grade 3-4              1 (1.2%)                         12 (14.3%)                          8 (9.5%)
      25                                                                             3                     0                           8 (9.5%)                          4 (4.8%)

---

    Code
      as.data.frame(tbl)[1:25, ]
    Output
         **MedDRA System Organ Class**  \n    **MedDRA Preferred Term**    \n**Grade** **Placebo**  \nN = 86 **Xanomeline High Dose**  \nN = 84 **Xanomeline Low Dose**  \nN = 84
      1                                          - Any adverse events -  - Any Grade -            26 (30.2%)                         42 (50.0%)                        40 (47.6%)
      2                                                                      Grade 1-2            20 (23.3%)                         23 (27.4%)                        20 (23.8%)
      3                                                                              1              7 (8.1%)                         12 (14.3%)                          7 (8.3%)
      4                                                                              2            13 (15.1%)                         11 (13.1%)                        13 (15.5%)
      5                                                                      Grade 3-4              5 (5.8%)                         19 (22.6%)                        18 (21.4%)
      6                                                                              3              2 (2.3%)                           8 (9.5%)                          6 (7.1%)
      7                                                                              4              3 (3.5%)                         11 (13.1%)                        12 (14.3%)
      8                                                                        Grade 5              1 (1.2%)                                  0                          2 (2.4%)
      9                                                                              5              1 (1.2%)                                  0                          2 (2.4%)
      10           GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS                                 <NA>                               <NA>                              <NA>
      11                                                    - Overall -  - Any Grade -              8 (9.3%)                         25 (29.8%)                        24 (28.6%)
      12                                                                     Grade 1-2              7 (8.1%)                         12 (14.3%)                        12 (14.3%)
      13                                                                             1              2 (2.3%)                           3 (3.6%)                          4 (4.8%)
      14                                                                             2              5 (5.8%)                          9 (10.7%)                          8 (9.5%)
      15                                                                     Grade 3-4              1 (1.2%)                         13 (15.5%)                        10 (11.9%)
      16                                                                             3                     0                           5 (6.0%)                          4 (4.8%)
      17                                                                             4              1 (1.2%)                           8 (9.5%)                          6 (7.1%)
      18                                                                       Grade 5                     0                                  0                          2 (2.4%)
      19                                                                             5                     0                                  0                          2 (2.4%)
      20                                      APPLICATION SITE PRURITUS  - Any Grade -              6 (7.0%)                         22 (26.2%)                        22 (26.2%)
      21                                                                     Grade 1-2              5 (5.8%)                         10 (11.9%)                        13 (15.5%)
      22                                                                             1              2 (2.3%)                           1 (1.2%)                          7 (8.3%)
      23                                                                             2              3 (3.5%)                          9 (10.7%)                          6 (7.1%)
      24                                                                     Grade 3-4              1 (1.2%)                         12 (14.3%)                          8 (9.5%)
      25                                                                             3                     0                           8 (9.5%)                          4 (4.8%)

# tbl_ae_rates_by_grade() error messaging works

    Code
      tbl <- tbl_ae_rates_by_grade(ADAE_subset, grade = AETOXGR, ae = AEDECOD, soc = AEBODSYS,
        denominator = ADSL, by = TRTA, label = label, grades_exclude = 4:5)
    Condition
      Error in `tbl_ae_rates_by_grade()`:
      ! The `grades_exclude` argument must be class <character> or empty, not an integer vector.

---

    Code
      tbl <- tbl_ae_rates_by_grade(ADAE_subset, grade = AETOXGR, ae = AEDECOD, soc = AEBODSYS,
        denominator = ADSL, by = TRTA, label = label, grade_groups = list(`Grade 5` = "5"))
    Condition
      Error in `tbl_ae_rates_by_grade()`:
      ! Each grade group must be specified via a <formula> where the left-hand side of the formula is a vector of grades and the right-hand side is the name of the grade group. For example, `c("3", "4") ~ "Grade 3-4"`

---

    Code
      tbl <- tbl_ae_rates_by_grade(ADAE_subset, grade = AETOXGR, ae = AEDECOD, soc = AEBODSYS,
        denominator = ADSL, by = TRTA, label = label, grade_groups = list(c("3", "4") ~
          "Grade 3-4", c("4", "5") ~ "Grade 4-5"))
    Condition
      Error in `tbl_ae_rates_by_grade()`:
      ! Grade groups specified via `grade_groups` cannot overlap. Please ensure that each grade is included in only one grade group.

---

    Code
      tbl <- tbl_ae_rates_by_grade(ADAE_subset, grade = AETOXGR, ae = AEDECOD, soc = AEBODSYS,
        denominator = ADSL, by = TRTA, label = label, grades_exclude = as.character(c(
          1:3, 5:7)))
    Condition
      Error in `tbl_ae_rates_by_grade()`:
      ! Grade(s) "6" and "7" supplied to `grades_exclude` are invalid. All grades specified via `grades_exclude` must be levels of "AETOXGR".

