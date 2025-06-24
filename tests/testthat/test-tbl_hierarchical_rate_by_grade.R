skip_on_cran()

ADSL <- cards::ADSL |> mutate(TRTA = ARM)
ADAE_subset <- cards::ADAE |>
  dplyr::filter(
    AESOC %in% unique(cards::ADAE$AESOC)[1:5],
    AETERM %in% unique(cards::ADAE$AETERM)[1:5]
  )

## Add AETOXGR variable to example dataset
set.seed(1)
ADAE_subset <- ADAE_subset |>
  dplyr::rowwise() |>
  mutate(
    AETOXGR = dplyr::case_when(
      AESEV == "MILD" ~ sample(1:2, 1),
      AESEV == "MODERATE" ~ sample(3:4, 1),
      AESEV == "SEVERE" ~ 5,
    ) |> factor(levels = 1:5)
  ) |>
  dplyr::ungroup()

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
  withr::local_options(width = 300)

  # no grade groups
  expect_silent(
    expect_message(
      tbl <-
        tbl_hierarchical_rate_by_grade(
          ADAE_subset,
          variables = c(AEBODSYS, AEDECOD, AETOXGR),
          denominator = ADSL,
          by = TRTA,
          label = label
        )
    )
  )
  expect_snapshot(as.data.frame(tbl)[1:25, ])

  # with grade groups
  expect_silent(
    expect_message(
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
  )
  expect_snapshot(as.data.frame(tbl)[1:25, ])

  # no by, no label
  expect_silent(
    expect_message(
      tbl <-
        tbl_hierarchical_rate_by_grade(
          ADAE_subset,
          variables = c(AEBODSYS, AEDECOD, AETOXGR),
          denominator = ADSL
        )
    )
  )
})

test_that("tbl_hierarchical_rate_by_grade(include_overall) works", {
  withr::local_options(width = 300)

  # all overall sections added
  expect_silent(
    expect_message(
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
  )
  expect_snapshot(as.data.frame(tbl)[1:25, ])

  # all overall sections removed
  expect_silent(
    expect_message(
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
  )
  expect_snapshot(as.data.frame(tbl)[1:25, ])
})

test_that("tbl_hierarchical_rate_by_grade() works with add_overall()", {
  expect_message(
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

  expect_silent(
    expect_message(
      tbl <- tbl |> add_overall(last = TRUE)
    )
  )

  # overall column is added with correct label
  expect_equal(
    tbl$table_styling$header |>
      dplyr::filter(column == "stat_0") |>
      dplyr::pull(label),
    "All Participants  \nN = 254"
  )
})

test_that("tbl_hierarchical_rate_by_grade(sort) works", {
  # default "descending" sort
  expect_silent(
    expect_message(
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
  )
  # check order is correct
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "AEBODSYS", !startsWith(label, "Total")) |>
      dplyr::pull("label"),
    c(
      "- Any adverse events -", "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS",
      "SKIN AND SUBCUTANEOUS TISSUE DISORDERS", "GASTROINTESTINAL DISORDERS",
      "CARDIAC DISORDERS"
    )
  )

  # "alphanumeric" sort works
  expect_silent(
    expect_message(
      tbl_alphanum <-
        tbl_hierarchical_rate_by_grade(
          ADAE_subset,
          variables = c(AEBODSYS, AEDECOD, AETOXGR),
          denominator = ADSL,
          by = TRTA,
          label = label,
          grade_groups = grade_groups,
          sort = "alphanumeric"
        )
    )
  )
  expect_equal(nrow(tbl), nrow(tbl_alphanum))
  # check order is correct
  expect_equal(
    tbl_alphanum$table_body |>
      dplyr::filter(variable == "AEBODSYS", !startsWith(label, "Total")) |>
      dplyr::pull("label"),
    c(
      "- Any adverse events -", "CARDIAC DISORDERS", "GASTROINTESTINAL DISORDERS",
      "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS",
      "SKIN AND SUBCUTANEOUS TISSUE DISORDERS"
    )
  )
})

test_that("tbl_hierarchical_rate_by_grade(filter) works", {
  expect_silent(
    expect_message(
      expect_message(
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
    )
  )

  # check that all rows have a percent larger than 7.5%
  expect_true(
    tbl$table_body |>
      dplyr::filter(str_detect(stat_0, "%") & variable == "AEDECOD") |>
      dplyr::pull(stat_0) |>
      str_extract("(?<=\\().*?(?=\\))") |> # extract the percent between the parentheses
      str_remove("%") %>%
      {
        as.numeric(.) > 7.5
      } |> # styler off
      all()
  )
})

test_that("tbl_hierarchical_rate_by_grade() works with non-factor `grade` variables", {
  ADAE_subset$AETOXGR <- as.character(ADAE_subset$AETOXGR)
  expect_message(
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
      '"AETOXGR" has been converted'
    )
  )
})

test_that("tbl_hierarchical_rate_by_grade(grade_groups) works with some grades not in groups", {
  withr::local_options(width = 200)

  expect_silent(
    expect_message(
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
  )
})

test_that("tbl_hierarchical_rate_by_grade(grades_exclude) works", {
  # no grades excluded
  expect_message(
    tbl_no_excl <-
      tbl_hierarchical_rate_by_grade(
        ADAE_subset,
        variables = c(AEBODSYS, AEDECOD, AETOXGR),
        denominator = ADSL,
        by = TRTA,
        label = label,
        grade_groups = grade_groups
      )
  )

  # one grade excluded
  expect_silent(
    expect_message(
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
  expect_message(
    tbl_no_keep <-
      tbl_hierarchical_rate_by_grade(
        ADAE_subset,
        variables = c(AEBODSYS, AEDECOD, AETOXGR),
        denominator = ADSL,
        by = TRTA,
        label = label,
        grade_groups = grade_groups
      )
  )

  # keep zero rows
  expect_silent(
    expect_message(
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
  )

  expect_true(nrow(tbl_no_keep$table_body) < nrow(tbl_keep$table_body))
  expect_identical(
    tbl_no_keep$cards$tbl_hierarchical_rate_by_grade$tbl_hierarchical,
    tbl_keep$cards$tbl_hierarchical_rate_by_grade$tbl_hierarchical |>
      filter_ard_hierarchical(sum(n) > 0)
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
