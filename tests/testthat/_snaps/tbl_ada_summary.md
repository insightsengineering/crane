# tbl_ada_summary() snapshot: two-arm with Total column

    Code
      as.data.frame(tbl_ada_summary(adat_two_arm, by = "TRT"))
    Output
                                                          Drug A  \n(N = 5) Drug B  \n(N = 5) All Patients  \n(N = 10)
      1                       Baseline Prevalence of NAbs              <NA>              <NA>                     <NA>
      2               Baseline evaluable patients for ADA              <NA>              <NA>                     <NA>
      3                                                 N                 0                 2                        2
      4                                                 Y                 5                 3                        8
      5   Patients with a positive ADA sample at baseline              <NA>              <NA>                     <NA>
      6                                                 N                 2                 5                        7
      7                                                 Y                 3                 0                        3
      8               Baseline evaluable patients for NAb              <NA>              <NA>                     <NA>
      9                                                 N                 0                 1                        1
      10                                                Y                 5                 4                        9
      11  Patients with a positive NAb sample at baseline              <NA>              <NA>                     <NA>
      12                                                N         3 (60.0%)          5 (100%)                8 (80.0%)
      13                                                Y         2 (40.0%)                 0                2 (20.0%)
      14 Patients with no positive NAb sample at baseline              <NA>              <NA>                     <NA>
      15                                                N                 2                 0                        2
      16                                                Y                 3                 5                        8
      17                                             <NA>              <NA>              <NA>                     <NA>
      18                                Incidence of NAbs              <NA>              <NA>                     <NA>
      19         Post-baseline evaluable patients for ADA              <NA>              <NA>                     <NA>
      20                                                N                 0                 2                        2
      21                                                Y                 5                 3                        8
      22                        Patients positive for ADA              <NA>              <NA>                     <NA>
      23                                                N                 2                 5                        7
      24                                                Y                 3                 0                        3
      25         Post-baseline evaluable patients for NAb              <NA>              <NA>                     <NA>
      26                                                N                 0                 1                        1
      27                                                Y                 5                 4                        9
      28                        Patients positive for NAb              <NA>              <NA>                     <NA>
      29                                                N         3 (60.0%)          5 (100%)                8 (80.0%)
      30                                                Y         2 (40.0%)                 0                2 (20.0%)
      31                        Patients negative for NAb              <NA>              <NA>                     <NA>
      32                                                N                 2                 0                        2
      33                                                Y                 3                 5                        8

# tbl_ada_summary() snapshot: unstratified

    Code
      as.data.frame(tbl_ada_summary(adat_two_arm, by = NULL))
    Output
                                                          Overall  \n(N = 10)
      1                       Baseline Prevalence of NAbs                <NA>
      2               Baseline evaluable patients for ADA                <NA>
      3                                                 N                   2
      4                                                 Y                   8
      5   Patients with a positive ADA sample at baseline                <NA>
      6                                                 N                   7
      7                                                 Y                   3
      8               Baseline evaluable patients for NAb                <NA>
      9                                                 N                   1
      10                                                Y                   9
      11  Patients with a positive NAb sample at baseline                <NA>
      12                                                N           8 (80.0%)
      13                                                Y           2 (20.0%)
      14 Patients with no positive NAb sample at baseline                <NA>
      15                                                N                   2
      16                                                Y                   8
      17                                             <NA>                <NA>
      18                                Incidence of NAbs                <NA>
      19         Post-baseline evaluable patients for ADA                <NA>
      20                                                N                   2
      21                                                Y                   8
      22                        Patients positive for ADA                <NA>
      23                                                N                   7
      24                                                Y                   3
      25         Post-baseline evaluable patients for NAb                <NA>
      26                                                N                   1
      27                                                Y                   9
      28                        Patients positive for NAb                <NA>
      29                                                N           8 (80.0%)
      30                                                Y           2 (20.0%)
      31                        Patients negative for NAb                <NA>
      32                                                N                   2
      33                                                Y                   8

