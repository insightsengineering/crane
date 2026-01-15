# tbl_baseline_chg() works

    Code
      as.data.frame(tbl)[1:25, 1:5]
    Output
             Visit Value at Visit Change from Baseline Value at Visit.1 Change from Baseline.1
      1   Baseline           <NA>                 <NA>             <NA>                   <NA>
      2          n              7                 <NA>                7                   <NA>
      3  Mean (SD)    141.1 (1.5)                 <NA>      139.1 (2.2)                   <NA>
      4     Median          141.0                 <NA>            139.0                   <NA>
      5  Min - Max      140 - 144                 <NA>        136 - 142                   <NA>
      6     Week 2           <NA>                 <NA>             <NA>                   <NA>
      7          n              7                    7                6                      6
      8  Mean (SD)    140.6 (1.6)           -0.6 (1.9)      138.8 (3.3)              0.2 (3.5)
      9     Median          141.0                 -1.0            140.5                    0.5
      10 Min - Max      138 - 142               -3 - 2        133 - 141                 -5 - 5
      11    Week 4           <NA>                 <NA>             <NA>                   <NA>
      12         n              7                    7                6                      6
      13 Mean (SD)    139.3 (2.8)           -1.9 (2.5)      139.8 (3.5)              1.2 (2.0)
      14    Median          140.0                 -1.0            138.5                    0.0
      15 Min - Max      134 - 142               -7 - 0        138 - 147                  0 - 5
      16    Week 6           <NA>                 <NA>             <NA>                   <NA>
      17         n              5                    5                6                      6
      18 Mean (SD)    140.0 (1.9)           -0.4 (1.7)      139.7 (2.0)              1.0 (2.3)
      19    Median          140.0                  0.0            139.0                    0.5
      20 Min - Max      138 - 143               -2 - 2        138 - 143                 -1 - 5
      21    Week 8           <NA>                 <NA>             <NA>                   <NA>
      22         n              5                    5                4                      4
      23 Mean (SD)    140.6 (1.5)            0.2 (1.6)      139.0 (2.2)             -0.5 (2.4)
      24    Median          141.0                  1.0            139.5                   -0.5
      25 Min - Max      139 - 142               -2 - 2        136 - 141                 -3 - 2

# tbl_baseline_chg() works with no `by` variable

    Code
      as.data.frame(tbl)[1:25, ]
    Output
             Visit Value at Visit Change from Baseline
      1   Baseline           <NA>                 <NA>
      2          n             20                 <NA>
      3  Mean (SD)    140.0 (2.8)                 <NA>
      4     Median          140.0                 <NA>
      5  Min - Max      134 - 145                 <NA>
      6     Week 2           <NA>                 <NA>
      7          n             19                   19
      8  Mean (SD)    139.7 (2.4)           -0.2 (2.9)
      9     Median          140.0                 -1.0
      10 Min - Max      133 - 142               -5 - 5
      11    Week 4           <NA>                 <NA>
      12         n             18                   18
      13 Mean (SD)    139.5 (2.6)           -0.5 (2.9)
      14    Median          139.5                  0.0
      15 Min - Max      134 - 147               -7 - 5
      16    Week 6           <NA>                 <NA>
      17         n             15                   15
      18 Mean (SD)    140.0 (2.0)            0.7 (3.1)
      19    Median          139.0                  0.0
      20 Min - Max      138 - 144               -5 - 7
      21    Week 8           <NA>                 <NA>
      22         n             12                   12
      23 Mean (SD)    139.7 (2.0)           -0.1 (2.2)
      24    Median          140.0                  1.0
      25 Min - Max      136 - 142               -4 - 2

# add_overall.tbl_baseline_chg() works

    Code
      as.data.frame(tbl)[1:25, c(1, 4:9)]
    Output
             Visit Value at Visit Change from Baseline Value at Visit.1 Change from Baseline.1 Value at Visit.2 Change from Baseline.2
      1   Baseline           <NA>                 <NA>             <NA>                   <NA>             <NA>                   <NA>
      2          n              7                 <NA>                6                   <NA>               20                   <NA>
      3  Mean (SD)    139.1 (2.2)                 <NA>      139.5 (4.2)                   <NA>      140.0 (2.8)                   <NA>
      4     Median          139.0                 <NA>            138.5                   <NA>            140.0                   <NA>
      5  Min - Max      136 - 142                 <NA>        134 - 145                   <NA>        134 - 145                   <NA>
      6     Week 2           <NA>                 <NA>             <NA>                   <NA>             <NA>                   <NA>
      7          n              6                    6                6                      6               19                     19
      8  Mean (SD)    138.8 (3.3)            0.2 (3.5)      139.5 (2.3)              0.0 (3.7)      139.7 (2.4)             -0.2 (2.9)
      9     Median          140.5                  0.5            139.5                   -0.5            140.0                   -1.0
      10 Min - Max      133 - 141               -5 - 5        136 - 142                 -5 - 5        133 - 142                 -5 - 5
      11    Week 4           <NA>                 <NA>             <NA>                   <NA>             <NA>                   <NA>
      12         n              6                    6                5                      5               18                     18
      13 Mean (SD)    139.8 (3.5)            1.2 (2.0)      139.4 (1.3)             -0.6 (3.5)      139.5 (2.6)             -0.5 (2.9)
      14    Median          138.5                  0.0            140.0                    0.0            139.5                    0.0
      15 Min - Max      138 - 147                0 - 5        138 - 141                 -5 - 4        134 - 147                 -7 - 5
      16    Week 6           <NA>                 <NA>             <NA>                   <NA>             <NA>                   <NA>
      17         n              6                    6                4                      4               15                     15
      18 Mean (SD)    139.7 (2.0)            1.0 (2.3)      140.5 (2.6)              1.8 (5.4)      140.0 (2.0)              0.7 (3.1)
      19    Median          139.0                  0.5            140.0                    2.5            139.0                    0.0
      20 Min - Max      138 - 143               -1 - 5        138 - 144                 -5 - 7        138 - 144                 -5 - 7
      21    Week 8           <NA>                 <NA>             <NA>                   <NA>             <NA>                   <NA>
      22         n              4                    4                3                      3               12                     12
      23 Mean (SD)    139.0 (2.2)           -0.5 (2.4)      139.0 (2.6)              0.0 (3.5)      139.7 (2.0)             -0.1 (2.2)
      24    Median          139.5                 -0.5            140.0                    2.0            140.0                    1.0
      25 Min - Max      136 - 141               -3 - 2        136 - 141                 -4 - 2        136 - 142                 -4 - 2

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
      

# tbl_baseline_chg(split_by = PARAM) works

    Code
      names(tbl)
    Output
      [1] "SODIUM" "K"     

---

    Code
      as.data.frame(tbl[[2]])[1:25, 1:5]
    Output
             Visit Value at Visit Change from Baseline Value at Visit.1 Change from Baseline.1
      1   Baseline           <NA>                 <NA>             <NA>                   <NA>
      2          n              7                 <NA>                7                   <NA>
      3  Mean (SD)    4.34 (0.40)                 <NA>      4.30 (0.18)                   <NA>
      4     Median           4.30                 <NA>             4.40                   <NA>
      5  Min - Max      3.7 - 4.9                 <NA>        4.0 - 4.5                   <NA>
      6     Week 2           <NA>                 <NA>             <NA>                   <NA>
      7          n              7                    7                6                      6
      8  Mean (SD)    4.47 (0.42)          0.13 (0.46)      4.35 (0.27)            0.07 (0.12)
      9     Median           4.50                -0.10             4.40                   0.05
      10 Min - Max      3.9 - 5.1           -0.4 - 1.0        3.9 - 4.6             -0.1 - 0.2
      11    Week 4           <NA>                 <NA>             <NA>                   <NA>
      12         n              7                    7                6                      6
      13 Mean (SD)    4.44 (0.38)          0.10 (0.37)      4.32 (0.46)           -0.02 (0.32)
      14    Median           4.60                 0.00             4.45                   0.05
      15 Min - Max      3.9 - 4.8           -0.4 - 0.7        3.4 - 4.6             -0.6 - 0.3
      16    Week 6           <NA>                 <NA>             <NA>                   <NA>
      17         n              5                    5                6                      6
      18 Mean (SD)    4.50 (0.35)          0.26 (0.26)      4.20 (0.23)           -0.13 (0.12)
      19    Median           4.60                 0.10             4.25                  -0.15
      20 Min - Max      4.0 - 4.8            0.1 - 0.7        3.8 - 4.4             -0.3 - 0.0
      21    Week 8           <NA>                 <NA>             <NA>                   <NA>
      22         n              5                    5                4                      4
      23 Mean (SD)    4.18 (0.23)         -0.06 (0.33)      4.18 (0.56)           -0.13 (0.41)
      24    Median           4.10                 0.00             4.20                  -0.20
      25 Min - Max      3.9 - 4.5           -0.4 - 0.3        3.5 - 4.8             -0.5 - 0.4

