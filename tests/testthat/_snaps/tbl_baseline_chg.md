# tbl_baseline_chg() works

    Code
      as.data.frame(tbl)[1:25, 1:5]
    Output
             Visit  Value at Visit Change from Baseline Value at Visit.1 Change from Baseline.1
      1   Baseline            <NA>                 <NA>             <NA>                   <NA>
      2          n               7                 <NA>                7                   <NA>
      3  Mean (SD)   141.14 (1.46)                 <NA>    139.14 (2.19)                   <NA>
      4     Median          141.00                 <NA>           139.00                   <NA>
      5  Min - Max 140.00 - 144.00                 <NA>  136.00 - 142.00                   <NA>
      6     Week 2            <NA>                 <NA>             <NA>                   <NA>
      7          n               7                    7                6                      6
      8  Mean (SD)   140.57 (1.62)           -0.6 (1.9)    138.83 (3.25)              0.2 (3.5)
      9     Median          141.00                 -1.0           140.50                    0.5
      10 Min - Max 138.00 - 142.00           -3.0 - 2.0  133.00 - 141.00             -5.0 - 5.0
      11    Week 4            <NA>                 <NA>             <NA>                   <NA>
      12         n               7                    7                6                      6
      13 Mean (SD)   139.29 (2.81)         -1.86 (2.54)    139.83 (3.54)            1.17 (2.04)
      14    Median          140.00                -1.00           138.50                   0.00
      15 Min - Max 134.00 - 142.00         -7.00 - 0.00  138.00 - 147.00            0.00 - 5.00
      16    Week 6            <NA>                 <NA>             <NA>                   <NA>
      17         n               5                    5                6                      6
      18 Mean (SD)   140.00 (1.87)         -0.40 (1.67)    139.67 (1.97)            1.00 (2.28)
      19    Median          140.00                 0.00           139.00                   0.50
      20 Min - Max 138.00 - 143.00         -2.00 - 2.00  138.00 - 143.00           -1.00 - 5.00
      21    Week 8            <NA>                 <NA>             <NA>                   <NA>
      22         n               5                    5                4                      4
      23 Mean (SD)   140.60 (1.52)          0.20 (1.64)    139.00 (2.16)           -0.50 (2.38)
      24    Median          141.00                 1.00           139.50                  -0.50
      25 Min - Max 139.00 - 142.00         -2.00 - 2.00  136.00 - 141.00           -3.00 - 2.00

# tbl_baseline_chg() works with no `by` variable

    Code
      as.data.frame(tbl)[1:25, ]
    Output
             Visit  Value at Visit Change from Baseline
      1   Baseline            <NA>                 <NA>
      2          n              20                 <NA>
      3  Mean (SD)   139.95 (2.78)                 <NA>
      4     Median          140.00                 <NA>
      5  Min - Max 134.00 - 145.00                 <NA>
      6     Week 2            <NA>                 <NA>
      7          n              19                   19
      8  Mean (SD)   139.68 (2.43)           -0.2 (2.9)
      9     Median          140.00                 -1.0
      10 Min - Max 133.00 - 142.00           -5.0 - 5.0
      11    Week 4            <NA>                 <NA>
      12         n              18                   18
      13 Mean (SD)   139.50 (2.64)         -0.50 (2.85)
      14    Median          139.50                 0.00
      15 Min - Max 134.00 - 147.00         -7.00 - 5.00
      16    Week 6            <NA>                 <NA>
      17         n              15                   15
      18 Mean (SD)   140.00 (2.00)          0.73 (3.10)
      19    Median          139.00                 0.00
      20 Min - Max 138.00 - 144.00         -5.00 - 7.00
      21    Week 8            <NA>                 <NA>
      22         n              12                   12
      23 Mean (SD)   139.67 (2.02)         -0.08 (2.19)
      24    Median          140.00                 1.00
      25 Min - Max 136.00 - 142.00         -4.00 - 2.00

# add_overall.tbl_baseline_chg() works

    Code
      as.data.frame(tbl)[1:25, c(1, 4:9)]
    Output
             Visit  Value at Visit Change from Baseline Value at Visit.1 Change from Baseline.1 Value at Visit.2 Change from Baseline.2
      1   Baseline            <NA>                 <NA>             <NA>                   <NA>             <NA>                   <NA>
      2          n               7                 <NA>                6                   <NA>               20                   <NA>
      3  Mean (SD)   139.14 (2.19)                 <NA>    139.50 (4.23)                   <NA>    139.95 (2.78)                   <NA>
      4     Median          139.00                 <NA>           138.50                   <NA>           140.00                   <NA>
      5  Min - Max 136.00 - 142.00                 <NA>  134.00 - 145.00                   <NA>  134.00 - 145.00                   <NA>
      6     Week 2            <NA>                 <NA>             <NA>                   <NA>             <NA>                   <NA>
      7          n               6                    6                6                      6               19                     19
      8  Mean (SD)   138.83 (3.25)            0.2 (3.5)    139.50 (2.35)              0.0 (3.7)    139.68 (2.43)             -0.2 (2.9)
      9     Median          140.50                  0.5           139.50                   -0.5           140.00                   -1.0
      10 Min - Max 133.00 - 141.00           -5.0 - 5.0  136.00 - 142.00             -5.0 - 5.0  133.00 - 142.00             -5.0 - 5.0
      11    Week 4            <NA>                 <NA>             <NA>                   <NA>             <NA>                   <NA>
      12         n               6                    6                5                      5               18                     18
      13 Mean (SD)   139.83 (3.54)          1.17 (2.04)    139.40 (1.34)           -0.60 (3.51)    139.50 (2.64)           -0.50 (2.85)
      14    Median          138.50                 0.00           140.00                   0.00           139.50                   0.00
      15 Min - Max 138.00 - 147.00          0.00 - 5.00  138.00 - 141.00           -5.00 - 4.00  134.00 - 147.00           -7.00 - 5.00
      16    Week 6            <NA>                 <NA>             <NA>                   <NA>             <NA>                   <NA>
      17         n               6                    6                4                      4               15                     15
      18 Mean (SD)   139.67 (1.97)          1.00 (2.28)    140.50 (2.65)            1.75 (5.38)    140.00 (2.00)            0.73 (3.10)
      19    Median          139.00                 0.50           140.00                   2.50           139.00                   0.00
      20 Min - Max 138.00 - 143.00         -1.00 - 5.00  138.00 - 144.00           -5.00 - 7.00  138.00 - 144.00           -5.00 - 7.00
      21    Week 8            <NA>                 <NA>             <NA>                   <NA>             <NA>                   <NA>
      22         n               4                    4                3                      3               12                     12
      23 Mean (SD)   139.00 (2.16)         -0.50 (2.38)    139.00 (2.65)            0.00 (3.46)    139.67 (2.02)           -0.08 (2.19)
      24    Median          139.50                -0.50           140.00                   2.00           140.00                   1.00
      25 Min - Max 136.00 - 141.00         -3.00 - 2.00  136.00 - 141.00           -4.00 - 2.00  136.00 - 142.00           -4.00 - 2.00

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
      {cards} data frame: 620 x 12
    Output
         group1 group1_level variable variable_level stat_name stat_label    stat
      1    TRTA      Placebo     AVAL       Baseline      mean       Mean 141.143
      2    TRTA      Placebo     AVAL       Baseline        sd         SD   1.464
      3    TRTA      Placebo     AVAL       Baseline    median     Median     141
      4    TRTA      Placebo     AVAL       Baseline       min        Min     140
      5    TRTA      Placebo     AVAL       Baseline       max        Max     144
      6    TRTA      Placebo     AVAL         Week 2      mean       Mean 140.571
      7    TRTA      Placebo     AVAL         Week 2        sd         SD   1.618
      8    TRTA      Placebo     AVAL         Week 2    median     Median     141
      9    TRTA      Placebo     AVAL         Week 2       min        Min     138
      10   TRTA      Placebo     AVAL         Week 2       max        Max     142
    Message
      i 610 more rows
      i Use `print(n = ...)` to see more rows
      i 5 more variables: context, fmt_fun, warning, error, gts_column
    Output
      
      $add_overall
    Message
      {cards} data frame: 229 x 10
    Output
         variable variable_level context stat_name stat_label    stat
      1      AVAL       Baseline summary      mean       Mean  139.95
      2      AVAL       Baseline summary        sd         SD   2.781
      3      AVAL       Baseline summary    median     Median     140
      4      AVAL       Baseline summary       min        Min     134
      5      AVAL       Baseline summary       max        Max     145
      6      AVAL         Week 2 summary      mean       Mean 139.684
      7      AVAL         Week 2 summary        sd         SD   2.428
      8      AVAL         Week 2 summary    median     Median     140
      9      AVAL         Week 2 summary       min        Min     133
      10     AVAL         Week 2 summary       max        Max     142
    Message
      i 219 more rows
      i Use `print(n = ...)` to see more rows
      i 4 more variables: fmt_fun, warning, error, gts_column
    Output
      

