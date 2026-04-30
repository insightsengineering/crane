# tbl_baseline_chg() works

    Code
      as.data.frame(tbl)[1:25, 1:5]
    Output
                   Visit       Value at Visit Change from Baseline     Value at Visit.1 Change from Baseline.1
      1         Baseline                 <NA>                 <NA>                 <NA>                   <NA>
      2                n                    7                 <NA>                    7                   <NA>
      3  Median (Q1, Q3) 141.0 (140.0, 142.0)                 <NA> 139.0 (138.0, 142.0)                   <NA>
      4           Week 2                 <NA>                 <NA>                 <NA>                   <NA>
      5                n                    7                    7                    6                      6
      6  Median (Q1, Q3) 141.0 (139.0, 142.0)     -1.0 (-2.0, 1.0) 140.5 (137.0, 141.0)        0.5 (-2.0, 2.0)
      7           Week 4                 <NA>                 <NA>                 <NA>                   <NA>
      8                n                    7                    7                    6                      6
      9  Median (Q1, Q3) 140.0 (137.0, 141.0)     -1.0 (-3.0, 0.0) 138.5 (138.0, 139.0)         0.0 (0.0, 2.0)
      10          Week 6                 <NA>                 <NA>                 <NA>                   <NA>
      11               n                    5                    5                    6                      6
      12 Median (Q1, Q3) 140.0 (139.0, 140.0)      0.0 (-2.0, 0.0) 139.0 (138.0, 141.0)        0.5 (-1.0, 2.0)
      13          Week 8                 <NA>                 <NA>                 <NA>                   <NA>
      14               n                    5                    5                    4                      4
      15 Median (Q1, Q3) 141.0 (139.0, 142.0)      1.0 (-1.0, 1.0) 139.5 (137.5, 140.5)       -0.5 (-2.5, 1.5)
      16         Week 12                 <NA>                 <NA>                 <NA>                   <NA>
      17               n                    5                    5                    4                      4
      18 Median (Q1, Q3) 141.0 (140.0, 142.0)       1.0 (0.0, 1.0) 140.0 (139.0, 142.5)         1.0 (0.5, 2.0)
      19         Week 16                 <NA>                 <NA>                 <NA>                   <NA>
      20               n                    5                    5                    4                      4
      21 Median (Q1, Q3) 141.0 (141.0, 141.0)       1.0 (0.0, 1.0) 141.0 (139.5, 144.5)         2.0 (1.0, 4.0)
      22         Week 20                 <NA>                 <NA>                 <NA>                   <NA>
      23               n                    5                    5                    4                      4
      24 Median (Q1, Q3) 142.0 (141.0, 143.0)       2.0 (1.0, 2.0) 137.5 (136.5, 141.0)       -1.0 (-2.5, 1.0)
      25         Week 24                 <NA>                 <NA>                 <NA>                   <NA>

# tbl_baseline_chg() works with no `by` variable

    Code
      as.data.frame(tbl)[1:25, ]
    Output
                   Visit       Value at Visit Change from Baseline
      1         Baseline                 <NA>                 <NA>
      2                n                   20                 <NA>
      3  Median (Q1, Q3) 140.0 (138.0, 142.0)                 <NA>
      4           Week 2                 <NA>                 <NA>
      5                n                   19                   19
      6  Median (Q1, Q3) 140.0 (138.0, 142.0)     -1.0 (-2.0, 2.0)
      7           Week 4                 <NA>                 <NA>
      8                n                   18                   18
      9  Median (Q1, Q3) 139.5 (138.0, 141.0)      0.0 (-2.0, 0.0)
      10          Week 6                 <NA>                 <NA>
      11               n                   15                   15
      12 Median (Q1, Q3) 139.0 (138.0, 141.0)      0.0 (-1.0, 2.0)
      13          Week 8                 <NA>                 <NA>
      14               n                   12                   12
      15 Median (Q1, Q3) 140.0 (139.0, 141.0)      1.0 (-2.0, 2.0)
      16         Week 12                 <NA>                 <NA>
      17               n                   11                   11
      18 Median (Q1, Q3) 140.0 (139.0, 142.0)       1.0 (0.0, 2.0)
      19         Week 16                 <NA>                 <NA>
      20               n                   11                   11
      21 Median (Q1, Q3) 141.0 (139.0, 142.0)       1.0 (0.0, 3.0)
      22         Week 20                 <NA>                 <NA>
      23               n                   11                   11
      24 Median (Q1, Q3) 139.0 (137.0, 143.0)      1.0 (-2.0, 2.0)
      25         Week 24                 <NA>                 <NA>

# tbl_baseline_chg() works with custom `statistic` argument

    Code
      as.data.frame(tbl)[1:15, 1:5]
    Output
                   Visit       Value at Visit Change from Baseline     Value at Visit.1 Change from Baseline.1
      1         Baseline                 <NA>                 <NA>                 <NA>                   <NA>
      2                n                    7                 <NA>                    7                   <NA>
      3        Mean (SD)          141.1 (1.5)                 <NA>          139.1 (2.2)                   <NA>
      4  Median (Q1, Q3) 141.0 (140.0, 142.0)                 <NA> 139.0 (138.0, 142.0)                   <NA>
      5           Week 2                 <NA>                 <NA>                 <NA>                   <NA>
      6                n                    7                    7                    6                      6
      7        Mean (SD)          140.6 (1.6)           -0.6 (1.9)          138.8 (3.3)              0.2 (3.5)
      8  Median (Q1, Q3) 141.0 (139.0, 142.0)     -1.0 (-2.0, 1.0) 140.5 (137.0, 141.0)        0.5 (-2.0, 2.0)
      9           Week 4                 <NA>                 <NA>                 <NA>                   <NA>
      10               n                    7                    7                    6                      6
      11       Mean (SD)          139.3 (2.8)           -1.9 (2.5)          139.8 (3.5)              1.2 (2.0)
      12 Median (Q1, Q3) 140.0 (137.0, 141.0)     -1.0 (-3.0, 0.0) 138.5 (138.0, 139.0)         0.0 (0.0, 2.0)
      13          Week 6                 <NA>                 <NA>                 <NA>                   <NA>
      14               n                    5                    5                    6                      6
      15       Mean (SD)          140.0 (1.9)           -0.4 (1.7)          139.7 (2.0)              1.0 (2.3)

# add_overall.tbl_baseline_chg() works

    Code
      as.data.frame(tbl)[1:25, c(1, 4:9)]
    Output
                   Visit       Value at Visit Change from Baseline     Value at Visit.1 Change from Baseline.1     Value at Visit.2 Change from Baseline.2
      1         Baseline                 <NA>                 <NA>                 <NA>                   <NA>                 <NA>                   <NA>
      2                n                    7                 <NA>                    6                   <NA>                   20                   <NA>
      3  Median (Q1, Q3) 139.0 (138.0, 142.0)                 <NA> 138.5 (137.0, 144.0)                   <NA> 140.0 (138.0, 142.0)                   <NA>
      4           Week 2                 <NA>                 <NA>                 <NA>                   <NA>                 <NA>                   <NA>
      5                n                    6                    6                    6                      6                   19                     19
      6  Median (Q1, Q3) 140.5 (137.0, 141.0)      0.5 (-2.0, 2.0) 139.5 (138.0, 142.0)       -0.5 (-2.0, 3.0) 140.0 (138.0, 142.0)       -1.0 (-2.0, 2.0)
      7           Week 4                 <NA>                 <NA>                 <NA>                   <NA>                 <NA>                   <NA>
      8                n                    6                    6                    5                      5                   18                     18
      9  Median (Q1, Q3) 138.5 (138.0, 139.0)       0.0 (0.0, 2.0) 140.0 (138.0, 140.0)        0.0 (-3.0, 1.0) 139.5 (138.0, 141.0)        0.0 (-2.0, 0.0)
      10          Week 6                 <NA>                 <NA>                 <NA>                   <NA>                 <NA>                   <NA>
      11               n                    6                    6                    4                      4                   15                     15
      12 Median (Q1, Q3) 139.0 (138.0, 141.0)      0.5 (-1.0, 2.0) 140.0 (138.5, 142.5)        2.5 (-2.5, 6.0) 139.0 (138.0, 141.0)        0.0 (-1.0, 2.0)
      13          Week 8                 <NA>                 <NA>                 <NA>                   <NA>                 <NA>                   <NA>
      14               n                    4                    4                    3                      3                   12                     12
      15 Median (Q1, Q3) 139.5 (137.5, 140.5)     -0.5 (-2.5, 1.5) 140.0 (136.0, 141.0)        2.0 (-4.0, 2.0) 140.0 (139.0, 141.0)        1.0 (-2.0, 2.0)
      16         Week 12                 <NA>                 <NA>                 <NA>                   <NA>                 <NA>                   <NA>
      17               n                    4                    4                    2                      2                   11                     11
      18 Median (Q1, Q3) 140.0 (139.0, 142.5)       1.0 (0.5, 2.0) 139.0 (139.0, 139.0)        0.0 (-5.0, 5.0) 140.0 (139.0, 142.0)         1.0 (0.0, 2.0)
      19         Week 16                 <NA>                 <NA>                 <NA>                   <NA>                 <NA>                   <NA>
      20               n                    4                    4                    2                      2                   11                     11
      21 Median (Q1, Q3) 141.0 (139.5, 144.5)       2.0 (1.0, 4.0) 139.5 (139.0, 140.0)        0.5 (-4.0, 5.0) 141.0 (139.0, 142.0)         1.0 (0.0, 3.0)
      22         Week 20                 <NA>                 <NA>                 <NA>                   <NA>                 <NA>                   <NA>
      23               n                    4                    4                    2                      2                   11                     11
      24 Median (Q1, Q3) 137.5 (136.5, 141.0)     -1.0 (-2.5, 1.0) 137.5 (137.0, 138.0)       -1.5 (-6.0, 3.0) 139.0 (137.0, 143.0)        1.0 (-2.0, 2.0)
      25         Week 24                 <NA>                 <NA>                 <NA>                   <NA>                 <NA>                   <NA>

# add_overall.tbl_baseline_chg() messaging

    Code
      tbl <- add_overall(tbl_baseline_chg(data = df, baseline_level = "Baseline", denominator = cards::ADSL))
    Message
      Original table was not stratified, and overall columns cannot be added.
      i Table has been returned unaltered.

---

    Code
      tbl <- add_overall(modify_table_body(tbl_baseline_chg(data = df, by = "TRTA", baseline_level = "Baseline", denominator = cards::ADSL), ~ dplyr::filter(.x, dplyr::row_number() %in% 1:5)))
    Message
      ! The structures of the original table and the overall table are not identical, and the resulting table may be malformed.

# tbl_baseline_chg() throws error when required arguments are missing

    Code
      tbl <- tbl_baseline_chg(data = test_data, baseline_level = "Baseline",
        denominator = cards::ADSL)
    Condition
      Error in `tbl_baseline_chg()`:
      ! Columns "USUBJID" and "AVISIT" do not uniquely identify the rows in `data`.
      i See row number 140.

# tbl_baseline_chg() messaging

    Code
      tbl <- tbl_baseline_chg(data = dplyr::mutate(df, TRTA = as.character(TRTA)), baseline_level = "Baseline", by = "TRTA", denominator = cards::ADSL)
    Message
      i Converting column "TRTA" to a factor.

# gather_ard() works on output table

    Code
      gather_ard(tbl)
    Output
      $tbl_baseline_chg
    Message
      {cards} data frame: 506 x 12
    Output
         group1 group1_level variable variable_level stat_name stat_label stat
      1    TRTA      Placebo     AVAL       Baseline    median     Median  141
      2    TRTA      Placebo     AVAL       Baseline       p25         Q1  140
      3    TRTA      Placebo     AVAL       Baseline       p75         Q3  142
      4    TRTA      Placebo     AVAL         Week 2    median     Median  141
      5    TRTA      Placebo     AVAL         Week 2       p25         Q1  139
      6    TRTA      Placebo     AVAL         Week 2       p75         Q3  142
      7    TRTA      Placebo     AVAL         Week 4    median     Median  140
      8    TRTA      Placebo     AVAL         Week 4       p25         Q1  137
      9    TRTA      Placebo     AVAL         Week 4       p75         Q3  141
      10   TRTA      Placebo     AVAL         Week 6    median     Median  140
    Message
      i 496 more rows
      i Use `print(n = ...)` to see more rows
      i 5 more variables: context, fmt_fun, warning, error, gts_column
    Output
      
      $add_overall
    Message
      {cards} data frame: 191 x 10
    Output
         variable variable_level context stat_name stat_label  stat
      1      AVAL       Baseline summary    median     Median   140
      2      AVAL       Baseline summary       p25         Q1   138
      3      AVAL       Baseline summary       p75         Q3   142
      4      AVAL         Week 2 summary    median     Median   140
      5      AVAL         Week 2 summary       p25         Q1   138
      6      AVAL         Week 2 summary       p75         Q3   142
      7      AVAL         Week 4 summary    median     Median 139.5
      8      AVAL         Week 4 summary       p25         Q1   138
      9      AVAL         Week 4 summary       p75         Q3   141
      10     AVAL         Week 6 summary    median     Median   139
    Message
      i 181 more rows
      i Use `print(n = ...)` to see more rows
      i 4 more variables: fmt_fun, warning, error, gts_column
    Output
      

