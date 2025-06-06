# add_hierarchical_count_row() works

    Code
      dplyr::slice(as.data.frame(add_hierarchical_count_row(tbl_hierarchical(dplyr::slice(
        cards::ADAE, 1:10), by = "TRTA", variables = AEDECOD, denominator = dplyr::rename(
        cards::ADSL, TRTA = TRT01A), id = "USUBJID", overall_row = TRUE), .after = 1L),
      col_label = FALSE), 2)
    Output
                                 label stat_1 stat_2
      1 Overall total number of events      7      3

