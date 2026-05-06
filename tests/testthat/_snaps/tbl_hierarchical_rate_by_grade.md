# tbl_hierarchical_rate_by_grade() error messaging works

    Code
      tbl <- tbl_hierarchical_rate_by_grade(ADAE_subset, variables = c(AEBODSYS,
        AEDECOD, AETOXGR), denominator = ADSL, by = TRTA, label = label,
      grades_exclude = 4:5)
    Condition
      Error in `tbl_hierarchical_rate_by_grade()`:
      ! `grades_exclude` must be a <character> vector or empty.

---

    Code
      tbl <- tbl_hierarchical_rate_by_grade(ADAE_subset, variables = c(AEBODSYS,
        AEDECOD, AETOXGR), denominator = ADSL, by = TRTA, label = label,
      grade_groups = list(`Grade 5` ~ "5"))
    Condition
      Error in `tbl_hierarchical_rate_by_grade()`:
      ! Grade groups must be specified via a named list where each list element is a character vector of the grades to include in the grade group and each name is the corresponding name of the grade group. For example, `"Grade 3-4" = c("3", "4")`.

---

    Code
      tbl <- tbl_hierarchical_rate_by_grade(ADAE_subset, variables = c(AEBODSYS,
        AEDECOD, AETOXGR), denominator = ADSL, by = TRTA, label = label,
      grade_groups = list(`Grade 3-4` = c("3", "4"), `Grade 4-5` = c("4", "5")))
    Condition
      Error in `tbl_hierarchical_rate_by_grade()`:
      ! Grade groups specified via `grade_groups` cannot overlap. Please ensure that each grade is included in at most one grade group.

---

    Code
      tbl <- tbl_hierarchical_rate_by_grade(ADAE_subset, variables = c(AEBODSYS,
        AEDECOD, AETOXGR), denominator = ADSL, by = TRTA, label = label,
      grades_exclude = as.character(c(1:3, 5:7)))
    Condition
      Error in `tbl_hierarchical_rate_by_grade()`:
      ! Grade(s) "6" and "7" supplied to `grades_exclude` are invalid. All grades specified via `grades_exclude` must be levels of "AETOXGR".

