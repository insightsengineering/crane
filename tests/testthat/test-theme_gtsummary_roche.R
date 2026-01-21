test_that("theme_gtsummary_roche() works", {
  # check structure of theme
  expect_snapshot(
    gtsummary::check_gtsummary_theme(theme_gtsummary_roche(set_theme = FALSE))
  )
})

test_that("theme_gtsummary_roche() adds relevant {flextable} directives", {
  # check user_added2+ are present
  cmds <-
    gtsummary::with_gtsummary_theme(
      theme_gtsummary_roche(),
      as_flex_table(gtsummary::tbl_summary(data.frame(1)), return_calls = TRUE)
    )

  added_cmds_roche_specific <- cmds[grepl("user_added[0-9]", names(cmds))]
  expect_snapshot(
    added_cmds_roche_specific
  )
})

test_that("theme_gtsummary_roche() no errors with a gt print", {
  expect_silent(
    gtsummary::with_gtsummary_theme(
      theme_gtsummary_roche(),
      tbl_summary(data.frame(1)) |> as_gt()
    )
  )
})

test_that("theme_gtsummary_roche() styles `tbl_hierarchical*()` results", {
  expect_silent(
    tbl <-
      with_gtsummary_theme(
        x = theme_gtsummary_roche(),
        cards::ADAE |>
          dplyr::slice(.by = "TRTA", 1L) |>
          tbl_hierarchical(
            variables = c(AESOC, AETERM),
            by = TRTA,
            denominator = cards::ADSL |> mutate(TRTA = ARM),
            id = USUBJID
          )
      )
  )

  # check no bold syntax in header
  expect_equal(
    tbl$table_styling$header$label,
    modify_header_rm_md(tbl)$table_styling$header$label
  )

  # check zero recode
  expect_equal(
    as.data.frame(tbl),
    modify_zero_recode(tbl) |> as.data.frame()
  )

  # check there is no footnote
  expect_equal(
    tbl |>
      as_gt() |>
      getElement("_footnotes") |>
      nrow(),
    0L
  )


  expect_silent(
    tbl <-
      with_gtsummary_theme(
        x = theme_gtsummary_roche(),
        cards::ADAE |>
          dplyr::slice(.by = "TRTA", 1L) |>
          tbl_hierarchical_count(
            variables = c(AESOC, AETERM, AESEV),
            by = TRTA
          )
      )
  )

  # check no bold syntax in header
  expect_equal(
    tbl$table_styling$header$label,
    modify_header_rm_md(tbl)$table_styling$header$label
  )

  # check there is no footnote
  expect_equal(
    tbl |>
      as_gt() |>
      getElement("_footnotes") |>
      nrow(),
    0L
  )
})

test_that("with assign_summary_type-arg:cat_threshold=0L each data type is always mapped as it is", {
  # it would be 4 if casted categorical instead of numeric for cyl
  expect_equal(nrow(gtsummary::tbl_summary(mtcars, include = cyl)$table_body), 1)
})

test_that("theme pre-conversion modifies header not to be bold", {
  tbl <- with_gtsummary_theme(
    x = theme_gtsummary_roche(),
    pharmaverseadam::adsl |>
      tbl_strata(
        strata = ETHNIC,
        ~ .x |>
          tbl_roche_summary(
            by = TRT01A,
            include = SEX
          )
      ) |>
      gtsummary::as_flex_table()
  )
  expect_true(all(!grepl(tbl$header$dataset[1, -1], pattern = "\\*")))
})
