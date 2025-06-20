# tbl_demographics() |> add_overall() works

    Code
      as.data.frame(add_overall(tbl_demographics(trial, by = trt, digits = list(grade = list(p = 1)), include = c(age, grade))))
    Output
                   All Participants \nN = 200 Drug A  \nN = 98 Drug B  \nN = 102
      1        Age                       <NA>             <NA>              <NA>
      2          n                        189               91                98
      3  Mean (SD)                    47 (14)          47 (15)           47 (14)
      4     Median                         47               46                48
      5  Min - Max                     6 - 83           6 - 78            9 - 83
      6      Grade                       <NA>             <NA>              <NA>
      7          n                        200               98               102
      8          I                 68 (34.0%)       35 (35.7%)        33 (32.4%)
      9         II                 68 (34.0%)       32 (32.7%)        36 (35.3%)
      10       III                 64 (32.0%)       31 (31.6%)        33 (32.4%)

