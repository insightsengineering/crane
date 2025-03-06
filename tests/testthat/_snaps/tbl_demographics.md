# tbl_demographics() |> add_overall() works

    Code
      as.data.frame(add_overall(tbl_demographics(trial, by = trt, digits = list(
        grade = list(p = 1)), include = c(age, grade))))
    Output
         **Characteristic** **Overall**  \nN = 200 **Drug A**  \nN = 98
      1                 Age                   <NA>                 <NA>
      2                   n                    189                   91
      3           Mean (SD)                47 (14)              47 (15)
      4              Median                     47                   46
      5           Min - Max                 6 - 83               6 - 78
      6               Grade                   <NA>                 <NA>
      7                   n                    200                   98
      8                   I             68 (34.0%)           35 (35.7%)
      9                  II             68 (34.0%)           32 (32.7%)
      10                III             64 (32.0%)           31 (31.6%)
         **Drug B**  \nN = 102
      1                   <NA>
      2                     98
      3                47 (14)
      4                     48
      5                 9 - 83
      6                   <NA>
      7                    102
      8             33 (32.4%)
      9             36 (35.3%)
      10            33 (32.4%)

