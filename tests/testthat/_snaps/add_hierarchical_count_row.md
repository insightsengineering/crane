# add_hierarchical_count_row() works

    Code
      dplyr::slice(as.data.frame(tbl, col_label = FALSE), 2)
    Output
                                 label stat_1 stat_2
      1 Overall total number of events      7      3

---

    Code
      dplyr::slice(as.data.frame(add_hierarchical_count_row(add_overall(tbl0),
      .after = 1L), col_label = FALSE), 2)
    Output
                                 label stat_0 stat_1 stat_2
      1 Overall total number of events     10      7      3

