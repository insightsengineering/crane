skip_on_cran()

ADSL <- cards::ADSL |> mutate(TRTA = ARM)
ADAE_subset <- cards::ADAE |>
  dplyr::filter(
    AESOC %in% unique(cards::ADAE$AESOC)[1:5],
    AETERM %in% unique(cards::ADAE$AETERM)[1:10]
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

grade_groups <- list(
  c("1", "2") ~ "Grade 1-2",
  c("3", "4") ~ "Grade 3-4",
  "5" ~ "Grade 5"
)

test_that("tbl_ae_rates_by_grade() works", {
  withr::local_options(width = 200)

  # no grade groups
  expect_silent(
    expect_message(
      tbl <-
        tbl_ae_rates_by_grade(
          ADAE_subset,
          grade = AETOXGR,
          ae = AEDECOD,
          soc = AEBODSYS,
          denominator = ADSL,
          by = TRTA
        )
    )
  )
  expect_snapshot(as.data.frame(tbl))

  # with grade groups
  expect_silent(
    expect_message(
      tbl <-
        tbl_ae_rates_by_grade(
          ADAE_subset,
          grade = AETOXGR,
          ae = AEDECOD,
          soc = AEBODSYS,
          denominator = ADSL,
          by = TRTA,
          grade_groups = grade_groups
        )
    )
  )
  expect_snapshot(as.data.frame(tbl))

  # no by
  expect_silent(
    expect_message(
      tbl <-
        tbl_ae_rates_by_grade(
          ADAE_subset,
          grade = AETOXGR,
          ae = AEDECOD,
          soc = AEBODSYS,
          denominator = ADSL,
          label = NULL
        )
    )
  )
})

test_that("tbl_ae_rates_by_grade(include_overall) works", {
  withr::local_options(width = 200)

  # all overall sections added
  expect_silent(
    expect_message(
      tbl <-
        tbl_ae_rates_by_grade(
          ADAE_subset,
          grade = AETOXGR,
          ae = AEDECOD,
          soc = AEBODSYS,
          denominator = ADSL,
          by = TRTA,
          grade_groups = grade_groups,
          include_overall = everything()
        )
    )
  )
  expect_snapshot(as.data.frame(tbl))

  # all overall sections removed
  expect_silent(
    expect_message(
      tbl <-
        tbl_ae_rates_by_grade(
          ADAE_subset,
          grade = AETOXGR,
          ae = AEDECOD,
          soc = AEBODSYS,
          denominator = ADSL,
          by = TRTA,
          grade_groups = grade_groups,
          include_overall = everything()
        )
    )
  )
  expect_snapshot(as.data.frame(tbl))
})

test_that("tbl_ae_rates_by_grade(add_overall) works", {
  expect_silent(
    expect_message(
      expect_message(
        tbl <-
          tbl_ae_rates_by_grade(
            ADAE_subset,
            grade = AETOXGR,
            ae = AEDECOD,
            soc = AEBODSYS,
            denominator = ADSL,
            by = TRTA,
            grade_groups = grade_groups,
            add_overall = TRUE
          )
      )
    )
  )

  # overall column is added with correct label
  expect_equal(
    tbl$table_styling$header |>
      dplyr::filter(column == "stat_0") |>
      dplyr::pull(label),
    "**All Active Treatments**  \nN = 254"
  )
})

test_that("tbl_ae_rates_by_grade(sort) works", {
  # default "descending" sort
  expect_silent(
    expect_message(
      tbl <-
        tbl_ae_rates_by_grade(
          ADAE_subset,
          grade = AETOXGR,
          ae = AEDECOD,
          soc = AEBODSYS,
          denominator = ADSL,
          by = TRTA,
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
      "INFECTIONS AND INFESTATIONS", "CARDIAC DISORDERS"
    )
  )

  # "alphanumeric" sort works
  expect_silent(
    expect_message(
      tbl_alphanum <-
        tbl_ae_rates_by_grade(
          ADAE_subset,
          grade = AETOXGR,
          ae = AEDECOD,
          soc = AEBODSYS,
          denominator = ADSL,
          by = TRTA,
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
      "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS", "INFECTIONS AND INFESTATIONS",
      "SKIN AND SUBCUTANEOUS TISSUE DISORDERS"
    )
  )
})

test_that("tbl_ae_rates_by_grade(filter) works", {
  expect_silent(
    expect_message(
      expect_message(
        tbl <-
          tbl_ae_rates_by_grade(
            ADAE_subset,
            grade = AETOXGR,
            ae = AEDECOD,
            soc = AEBODSYS,
            denominator = ADSL,
            by = TRTA,
            filter = sum(n) / sum(N) > 0.07,
            add_overall = TRUE,
            grade_groups = grade_groups
          )
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

test_that("tbl_ae_rates_by_grade(grade_groups) works with some grades not in groups", {
  withr::local_options(width = 200)

  expect_silent(
    expect_message(
      tbl <-
        tbl_ae_rates_by_grade(
          ADAE_subset,
          grade = AETOXGR,
          ae = AEDECOD,
          soc = AEBODSYS,
          denominator = ADSL,
          by = TRTA,
          grade_groups = list(c("3", "4") ~ "Grade 3-4")
        )
    )
  )
  expect_snapshot(as.data.frame(tbl))
})

test_that("tbl_ae_rates_by_grade(grades_exclude) works", {
  # no grades excluded
  expect_message(
    tbl_no_excl <-
      tbl_ae_rates_by_grade(
        ADAE_subset,
        grade = AETOXGR,
        ae = AEDECOD,
        soc = AEBODSYS,
        denominator = ADSL,
        by = TRTA,
        grade_groups = grade_groups
      )
  )

  # one grade excluded
  expect_silent(
    expect_message(
      tbl_excl <-
        tbl_ae_rates_by_grade(
          ADAE_subset,
          grade = AETOXGR,
          ae = AEDECOD,
          soc = AEBODSYS,
          denominator = ADSL,
          by = TRTA,
          grade_groups = grade_groups,
          grades_exclude = "5"
        )
    )
  )
  expect_identical(
    tbl_excl$table_body,
    tbl_no_excl$table_body |>
      dplyr::filter(label != "5")
  )

  # all grades excluded
  expect_silent(
    tbl_excl <-
      tbl_ae_rates_by_grade(
        ADAE_subset,
        grade = AETOXGR,
        ae = AEDECOD,
        soc = AEBODSYS,
        denominator = ADSL,
        by = TRTA,
        grade_groups = grade_groups,
        grades_exclude = as.character(1:5)
      )
  )
  expect_identical(
    tbl_excl$table_body,
    tbl_no_excl$table_body |>
      dplyr::select(-tbl_id1) |>
      dplyr::filter(!label %in% as.character(1:5))
  )
})

test_that("tbl_ae_rates_by_grade() error messaging works", {
  expect_snapshot(
    tbl <-
      tbl_ae_rates_by_grade(
        ADAE_subset,
        grade = AETOXGR,
        ae = AEDECOD,
        soc = AEBODSYS,
        denominator = ADSL,
        by = TRTA,
        grades_exclude = 4:5
      ),
    error = TRUE
  )

  expect_snapshot(
    tbl <-
      tbl_ae_rates_by_grade(
        ADAE_subset,
        grade = AETOXGR,
        ae = AEDECOD,
        soc = AEBODSYS,
        denominator = ADSL,
        by = TRTA,
        grade_groups = list("Grade 5" = "5")
      ),
    error = TRUE
  )

  expect_snapshot(
    tbl <-
      tbl_ae_rates_by_grade(
        ADAE_subset,
        grade = AETOXGR,
        ae = AEDECOD,
        soc = AEBODSYS,
        denominator = ADSL,
        by = TRTA,
        grade_groups = list(c("3", "4") ~ "Grade 3-4", c("4", "5") ~ "Grade 4-5")
      ),
    error = TRUE
  )
})
