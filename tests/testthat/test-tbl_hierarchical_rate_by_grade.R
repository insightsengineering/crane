skip_on_cran()

ADSL <- cards::ADSL
ADAE_subset <- cards::ADAE |>
  dplyr::filter(
    AESOC %in% unique(cards::ADAE$AESOC)[1:5],
    AETERM %in% unique(cards::ADAE$AETERM)[1:5]
  )

label <- list(
  AEBODSYS = "MedDRA System Organ Class",
  AEDECOD = "MedDRA Preferred Term",
  AETOXGR = "Grade"
)

grade_groups <- list(
  "Grade 1-2" = c("1", "2"),
  "Grade 3-4" = c("3", "4"),
  "Grade 5" = "5"
)

test_that("tbl_hierarchical_rate_by_grade() works", {
  withr::local_options(width = 400)

  # no grade groups
  expect_silent(
    tbl <-
      tbl_hierarchical_rate_by_grade(
        ADAE_subset,
        variables = c(AEBODSYS, AEDECOD, AETOXGR),
        denominator = ADSL,
        by = TRTA,
        label = label
      )
  )
  expect_snapshot(as.data.frame(tbl)[1:25, ])

  # with grade groups
  expect_silent(
    tbl <-
      tbl_hierarchical_rate_by_grade(
        ADAE_subset,
        variables = c(AEBODSYS, AEDECOD, AETOXGR),
        denominator = ADSL,
        by = TRTA,
        label = label,
        grade_groups = grade_groups
      )
  )
  expect_snapshot(as.data.frame(tbl)[1:25, ])

  # no by, no label
  expect_silent(
    tbl <-
      tbl_hierarchical_rate_by_grade(
        ADAE_subset,
        variables = c(AEBODSYS, AEDECOD, AETOXGR),
        denominator = ADSL
      )
  )

  # custom statistic/digits
  expect_silent(
    tbl <-
      tbl_hierarchical_rate_by_grade(
        ADAE_subset,
        variables = c(AEBODSYS, AEDECOD, AETOXGR),
        denominator = ADSL,
        by = TRTA,
        label = label,
        statistic = everything() ~ "{n}/{N}, {p}%",
        digits = everything() ~ list(n = label_style_number(digits = 1, decimal.mark = ","), p = 3)
      )
  )
  expect_snapshot(as.data.frame(tbl)[1, ])
})

test_that("tbl_hierarchical_rate_by_grade(include_overall) works", {
  withr::local_options(width = 350)

  # all overall sections added
  expect_silent(
    tbl <-
      tbl_hierarchical_rate_by_grade(
        ADAE_subset,
        variables = c(AEBODSYS, AEDECOD, AETOXGR),
        denominator = ADSL,
        by = TRTA,
        label = label,
        grade_groups = grade_groups,
        include_overall = everything()
      )
  )
  expect_snapshot(as.data.frame(tbl)[1:25, ])

  # all overall sections removed
  expect_silent(
    tbl <-
      tbl_hierarchical_rate_by_grade(
        ADAE_subset,
        variables = c(AEBODSYS, AEDECOD, AETOXGR),
        denominator = ADSL,
        by = TRTA,
        label = label,
        grade_groups = grade_groups,
        include_overall = NULL
      )
  )
  expect_snapshot(as.data.frame(tbl)[1:25, ])
})

test_that("tbl_hierarchical_rate_by_grade() works with add_overall()", {
  tbl <-
    tbl_hierarchical_rate_by_grade(
      ADAE_subset,
      variables = c(AEBODSYS, AEDECOD, AETOXGR),
      denominator = ADSL,
      by = TRTA,
      label = label,
      grade_groups = grade_groups
    )


  expect_silent(
    tbl <- tbl |> add_overall(last = TRUE)
  )

  # overall column is added with correct label
  expect_equal(
    tbl$table_styling$header |>
      dplyr::filter(column == "stat_0") |>
      dplyr::pull(label),
    "All Participants  \n(N = 254)"
  )
})

test_that("tbl_hierarchical_rate_by_grade(sort) works", {
  # default "alphanumeric" sort
  expect_silent(
    tbl <-
      tbl_hierarchical_rate_by_grade(
        ADAE_subset,
        variables = c(AEBODSYS, AEDECOD, AETOXGR),
        denominator = ADSL,
        by = TRTA,
        label = label,
        grade_groups = grade_groups
      )
  )
  # check order is correct
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "AEBODSYS", !startsWith(label, "Total")) |>
      dplyr::pull("label"),
    c(
      "- Any adverse events -", "CARDIAC DISORDERS", "GASTROINTESTINAL DISORDERS",
      "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS",
      "SKIN AND SUBCUTANEOUS TISSUE DISORDERS"
    )
  )

  # "descending" sort works
  expect_silent(
    tbl_desc <-
      tbl_hierarchical_rate_by_grade(
        ADAE_subset,
        variables = c(AEBODSYS, AEDECOD, AETOXGR),
        denominator = ADSL,
        by = TRTA,
        label = label,
        grade_groups = grade_groups,
        sort = "descending"
      )
  )
  expect_equal(nrow(tbl), nrow(tbl_desc))
  # check order is correct
  expect_equal(
    tbl_desc$table_body |>
      dplyr::filter(variable == "AEBODSYS", !startsWith(label, "Total")) |>
      dplyr::pull("label"),
    c(
      "- Any adverse events -", "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS",
      "SKIN AND SUBCUTANEOUS TISSUE DISORDERS", "GASTROINTESTINAL DISORDERS",
      "CARDIAC DISORDERS"
    )
  )
})

test_that("tbl_hierarchical_rate_by_grade(filter) works", {
  expect_silent(
    tbl <-
      tbl_hierarchical_rate_by_grade(
        ADAE_subset,
        variables = c(AEBODSYS, AEDECOD, AETOXGR),
        denominator = ADSL,
        by = TRTA,
        label = label,
        filter = sum(n) / sum(N) > 0.07,
        grade_groups = grade_groups
      ) |>
      add_overall()
  )

  # check that all AE rows have a percent larger than 7%
  expect_true(
    tbl$table_body |>
      dplyr::filter(str_detect(stat_0, "%") & variable == "AEDECOD") |>
      dplyr::pull(stat_0) |>
      str_extract("(?<=\\().*?(?=\\))") |> # extract the percent between the parentheses
      str_remove("%") %>%
      {
        as.numeric(.) > 7
      } |> # styler off
      all()
  )

  # if only the SOC overall section is kept by the filter the SOC is removed
  expect_silent(
    tbl <-
      tbl_hierarchical_rate_by_grade(
        ADAE_subset,
        variables = c(AEBODSYS, AEDECOD, AETOXGR),
        denominator = ADSL,
        by = TRTA,
        label = label,
        filter = sum(n) > 18,
        grade_groups = grade_groups
      ) |>
      add_overall()
  )
  expect_equal(nrow(tbl$table_body), 42)
})

test_that("tbl_hierarchical_rate_by_grade() works with non-factor grade variables", {
  ADAE_subset$AETOXGR <- as.character(ADAE_subset$AETOXGR)
  expect_message(
    tbl <-
      tbl_hierarchical_rate_by_grade(
        ADAE_subset,
        variables = c(AEBODSYS, AEDECOD, AETOXGR),
        denominator = ADSL,
        by = TRTA,
        label = label,
        grade_groups = grade_groups
      ),
    "`AETOXGR`:"
  )
})

test_that("tbl_hierarchical_rate_by_grade(grade_groups) works with some grades not in groups", {
  withr::local_options(width = 200)

  expect_silent(
    tbl <-
      tbl_hierarchical_rate_by_grade(
        ADAE_subset,
        variables = c(AEBODSYS, AEDECOD, AETOXGR),
        denominator = ADSL,
        by = TRTA,
        label = label,
        grade_groups = list("Grade 3-4" = c("3", "4"))
      )
  )
})

test_that("tbl_hierarchical_rate_by_grade(grades_exclude) works", {
  # no grades excluded
  tbl_no_excl <-
    tbl_hierarchical_rate_by_grade(
      ADAE_subset,
      variables = c(AEBODSYS, AEDECOD, AETOXGR),
      denominator = ADSL,
      by = TRTA,
      label = label,
      grade_groups = grade_groups
    )


  # one grade excluded
  expect_silent(
    tbl_excl <-
      tbl_hierarchical_rate_by_grade(
        ADAE_subset,
        variables = c(AEBODSYS, AEDECOD, AETOXGR),
        denominator = ADSL,
        by = TRTA,
        label = label,
        grade_groups = grade_groups,
        grades_exclude = "5"
      )
  )
  expect_identical(
    tbl_excl$table_body,
    tbl_no_excl$table_body |>
      dplyr::filter(label_grade != "5")
  )

  # all grades excluded
  expect_silent(
    tbl_excl <-
      tbl_hierarchical_rate_by_grade(
        ADAE_subset,
        variables = c(AEBODSYS, AEDECOD, AETOXGR),
        denominator = ADSL,
        by = TRTA,
        label = label,
        grade_groups = grade_groups,
        grades_exclude = as.character(1:5)
      )
  )
  expect_identical(
    tbl_excl$table_body,
    tbl_no_excl$table_body |>
      dplyr::filter(!label_grade %in% as.character(1:5))
  )
})

test_that("tbl_hierarchical_rate_by_grade(keep_zero_rows) works", {
  # remove zero rows
  tbl_no_keep <-
    tbl_hierarchical_rate_by_grade(
      ADAE_subset,
      variables = c(AEBODSYS, AEDECOD, AETOXGR),
      denominator = ADSL,
      by = TRTA,
      label = label,
      grade_groups = grade_groups
    )

  # keep zero rows
  expect_silent(
    tbl_keep <-
      tbl_hierarchical_rate_by_grade(
        ADAE_subset,
        variables = c(AEBODSYS, AEDECOD, AETOXGR),
        denominator = ADSL,
        by = TRTA,
        label = label,
        grade_groups = grade_groups,
        keep_zero_rows = TRUE
      )
  )

  expect_true(nrow(tbl_no_keep$table_body) < nrow(tbl_keep$table_body))
  expect_identical(
    tbl_no_keep$cards$tbl_hierarchical_rate_by_grade$tbl_hierarchical,
    tbl_keep$cards$tbl_hierarchical_rate_by_grade$tbl_hierarchical |>
      cards::filter_ard_hierarchical(sum(n) > 0)
  )
})

test_that("tbl_hierarchical_rate_by_grade() error messaging works", {
  expect_snapshot(
    tbl <-
      tbl_hierarchical_rate_by_grade(
        ADAE_subset,
        variables = c(AEBODSYS, AEDECOD, AETOXGR),
        denominator = ADSL,
        by = TRTA,
        label = label,
        grades_exclude = 4:5
      ),
    error = TRUE
  )

  expect_snapshot(
    tbl <-
      tbl_hierarchical_rate_by_grade(
        ADAE_subset,
        variables = c(AEBODSYS, AEDECOD, AETOXGR),
        denominator = ADSL,
        by = TRTA,
        label = label,
        grade_groups = list("Grade 5" ~ "5")
      ),
    error = TRUE
  )

  expect_snapshot(
    tbl <-
      tbl_hierarchical_rate_by_grade(
        ADAE_subset,
        variables = c(AEBODSYS, AEDECOD, AETOXGR),
        denominator = ADSL,
        by = TRTA,
        label = label,
        grade_groups = list("Grade 3-4" = c("3", "4"), "Grade 4-5" = c("4", "5"))
      ),
    error = TRUE
  )

  expect_snapshot(
    tbl <-
      tbl_hierarchical_rate_by_grade(
        ADAE_subset,
        variables = c(AEBODSYS, AEDECOD, AETOXGR),
        denominator = ADSL,
        by = TRTA,
        label = label,
        grades_exclude = as.character(c(1:3, 5:7))
      ),
    error = TRUE
  )
})
