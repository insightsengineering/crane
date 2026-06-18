# tbl_survfit_times() works for stratified data

    Code
      as.data.frame(tbl)
    Output
                                  Time                          
      1              Placebo N at Risk         69.0         59.0
      2                 <NA>  Survival         0.84         0.77
      3                 <NA>    95% CI (0.77, 0.93) (0.68, 0.87)
      4 Xanomeline High Dose N at Risk         38.0         14.0
      5                 <NA>  Survival         0.53         0.24
      6                 <NA>    95% CI (0.43, 0.66) (0.16, 0.37)
      7  Xanomeline Low Dose N at Risk         42.0         20.0
      8                 <NA>  Survival         0.53         0.31
      9                 <NA>    95% CI (0.43, 0.66) (0.22, 0.44)

# tbl_survfit_times() works for unstratified data

    Code
      as.data.frame(tbl)
    Output
             Time                          
      1 N at Risk        149.0         93.0
      2  Survival         0.64         0.46
      3    90% CI (0.58, 0.70) (0.40, 0.53)

# tbl_survfit_times() adapts to user modifications (dropping columns)

    Code
      tbl_df
    Output
            Time     
      1 Survival 0.64

