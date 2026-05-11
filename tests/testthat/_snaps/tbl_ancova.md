# tbl_ancova() works with default settings

    Code
      as.data.frame(tbl)
    Output
                                 Characteristic Placebo Xanomeline High Dose Xanomeline Low Dose
      1                    Change from Baseline    <NA>                 <NA>                <NA>
      2                                       n       5                    4                   3
      3                           Adjusted Mean    0.60                -0.66               -0.47
      4            Difference in Adjusted Means                        -1.26               -1.07
      5 95% CI for Difference in Adjusted Means                  -4.23, 1.71         -4.35, 2.21
      6                                 p-value                       0.3563              0.4733

# tbl_ancova() works with denominator

    Code
      as.data.frame(tbl)
    Output
                                 Characteristic Placebo\n(N = 86) Xanomeline High Dose\n(N = 84) Xanomeline Low Dose\n(N = 84)
      1                    Change from Baseline              <NA>                           <NA>                          <NA>
      2                                       n                 5                              4                             3
      3                           Adjusted Mean              0.60                          -0.66                         -0.47
      4            Difference in Adjusted Means                                            -1.26                         -1.07
      5 95% CI for Difference in Adjusted Means                                      -4.23, 1.71                   -4.35, 2.21
      6                                 p-value                                           0.3563                        0.4733

# tbl_ancova() errors on invalid ref_group

    Code
      tbl_ancova(data = df_ancova, formula = CHG ~ TRTA + BASE, by = TRTA, ref_group = "NonexistentArm")
    Condition
      Error in `tbl_ancova()`:
      ! `ref_group` value "NonexistentArm" not found in column "TRTA".

# tbl_ancova() works with custom label

    Code
      as.data.frame(tbl)
    Output
                                 Characteristic Placebo Xanomeline High Dose Xanomeline Low Dose
      1                         Sodium (mmol/L)    <NA>                 <NA>                <NA>
      2                                       n       5                    4                   3
      3                           Adjusted Mean    0.60                -0.66               -0.47
      4            Difference in Adjusted Means                        -1.26               -1.07
      5 95% CI for Difference in Adjusted Means                  -4.23, 1.71         -4.35, 2.21
      6                                 p-value                       0.3563              0.4733

# tbl_ancova() errors on non-string label

    Code
      tbl_ancova(data = df_ancova, formula = CHG ~ TRTA + BASE, by = TRTA, ref_group = "Placebo",
      label = 123)
    Condition
      Error in `tbl_ancova()`:
      ! The `label` argument must be a string, not a number.

# tbl_ancova() works with Dunnett adjustment

    Code
      as.data.frame(tbl)
    Output
                                 Characteristic Placebo Xanomeline High Dose Xanomeline Low Dose
      1                    Change from Baseline    <NA>                 <NA>                <NA>
      2                                       n       5                    4                   3
      3                           Adjusted Mean    0.60                -0.66               -0.47
      4            Difference in Adjusted Means                        -1.26               -1.07
      5 95% CI for Difference in Adjusted Means                  -4.73, 2.22         -4.91, 2.77
      6                                 p-value                       0.5491              0.6839

# tbl_ancova() works without covariates

    Code
      as.data.frame(tbl)
    Output
                                 Characteristic Placebo Xanomeline High Dose Xanomeline Low Dose
      1                    Change from Baseline    <NA>                 <NA>                <NA>
      2                                       n       5                    4                   3
      3                           Adjusted Mean    0.20                -0.50                0.00
      4            Difference in Adjusted Means                        -0.70               -0.20
      5 95% CI for Difference in Adjusted Means                  -4.34, 2.94         -4.16, 3.76
      6                                 p-value                       0.6738              0.9116

