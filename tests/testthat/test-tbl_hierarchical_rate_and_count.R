skip_on_cran()

ADAE_subset <- cards::ADAE |>
  dplyr::slice(.by = AEDECOD, 1:3) |>
  dplyr::filter(AEBODSYS %in% unique(AEBODSYS)[1:2]) |>
  dplyr::slice(1:10)

test_that("tbl_hierarchical_rate_and_count() works", {
  withr::local_options(list(width = 220))
  expect_silent(
    tbl <-
      ADAE_subset |>
      tbl_hierarchical_rate_and_count(
        denominator = cards::ADSL,
        by = TRTA,
        variables = c(AEBODSYS, AEDECOD),
        label = AEDECOD ~ "MedDRA Preferred Term"
      ) |>
      add_overall(last = TRUE)
  )

  # check the first two labels are the overall rows
  expect_equal(
    tbl$table_body$label[1:2],
    c(
      "Total number of participants with at least one adverse event",
      "Overall total number of events"
    )
  )

  # check the header label is correct
  expect_equal(
    tbl$table_styling$header$label[6],
    "Body System or Organ Class  \n\U00A0\U00A0\U00A0\U00A0MedDRA Preferred Term"
  )

  # snapshot of the table
  expect_snapshot(as.data.frame(tbl))

  # testing with a high level term
  expect_silent(
    tbl <-
      ADAE_subset |>
      tbl_hierarchical_rate_and_count(
        denominator = cards::ADSL,
        by = TRTA,
        variables = c(AEBODSYS, AEHLT, AEDECOD)
      ) |>
      add_overall(last = TRUE)
  )
  # snapshot of the table
  expect_snapshot(as.data.frame(tbl))
})

test_that("tbl_hierarchical_rate_and_count(sort)", {
  expect_silent(
    tbl <-
      ADAE_subset |>
      tbl_hierarchical_rate_and_count(
        denominator = cards::ADSL,
        by = TRTA,
        variables = c(AEBODSYS, AEDECOD),
        sort = "descending"
      ) |>
      add_overall(last = TRUE)
  )

  # in this case, the 'GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS' should appear before 'GASTROINTESTINAL DISORDERS'
  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "AEBODSYS", !startsWith(label, "Total")) |>
      dplyr::pull("label"),
    c("GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS", "GASTROINTESTINAL DISORDERS")
  )
})

test_that("tbl_hierarchical_rate_and_count() digits styling defaults to gtsummary", {
  theme_gtsummary_roche()
  expect_equal(
    cards::ADAE |>
      dplyr::filter(AEBODSYS %in% unique(AEBODSYS)[1:2]) |>
      tbl_hierarchical_rate_and_count(
        denominator = cards::ADSL,
        by = TRTA,
        variables = c(AEBODSYS, AEDECOD)
      ) |>
      add_overall(last = TRUE) |> as.data.frame(),
    cards::ADAE |>
      dplyr::filter(AEBODSYS %in% unique(AEBODSYS)[1:2]) |>
      tbl_hierarchical_rate_and_count(
        denominator = cards::ADSL,
        by = TRTA,
        digits = everything() ~ list(
          n = label_style_number(),
          p = label_roche_percent(digits = 1)
        ),
        variables = c(AEBODSYS, AEDECOD)
      ) |>
      add_overall(last = TRUE) |> as.data.frame()
  )
})

test_that("tbl_hierarchical_rate_and_count() works only with 2 or 3 variables", {
  expect_snapshot(
    tbl <-
      ADAE_subset |>
      tbl_hierarchical_rate_and_count(
        denominator = cards::ADSL,
        by = TRTA,
        variables = c(AEBODSYS),
        sort = "descending"
      ),
    error = TRUE
  )

  expect_snapshot(
    tbl <-
      ADAE_subset |>
      tbl_hierarchical_rate_and_count(
        denominator = cards::ADSL,
        by = TRTA,
        variables = c(SEX, AEBODSYS, AEDECOD, SAFFL),
        sort = "descending"
      ),
    error = TRUE
  )
})
