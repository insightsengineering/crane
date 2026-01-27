# modify_zero_recode() works

    Code
      as.data.frame(modify_zero_recode(tbl_summary(dplyr::mutate(gtsummary::trial, trt = factor(trt, levels = c("Drug A", "Drug B", "Drug C")), grade = factor(grade, levels = c("I", "II", "III",
        "IV"))), by = trt, include = c(response, grade), statistic = response ~ "{n} / {N} ({p}%)", missing = "no")))
    Output
        Characteristic Drug A  \nN = 98 Drug B  \nN = 102 Drug C  \nN = 0
      1 Tumor Response  28 / 95 (29.5%)   33 / 98 (33.7%)           0 / 0
      2          grade             <NA>              <NA>            <NA>
      3              I       35 (35.7%)        33 (32.4%)               0
      4             II       32 (32.7%)        36 (35.3%)               0
      5            III       31 (31.6%)        33 (32.4%)               0
      6             IV                0                 0               0

