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
          n = label_roche_number(),
          p = label_roche_percent(digits = 1)
        ),
        variables = c(AEBODSYS, AEDECOD)
      ) |>
      add_overall(last = TRUE) |> as.data.frame()
  )
})

test_that("tbl_hierarchical_rate_and_count() emits zero-rows for unobserved factor levels", {
  withr::local_options(list(width = 220))

  adae_factor <- ADAE_subset |>
    dplyr::mutate(
      AEBODSYS = factor(
        AEBODSYS,
        levels = c(unique(AEBODSYS), "UNOBSERVED SOC")
      )
    )

  tbl <- adae_factor |>
    tbl_hierarchical_rate_and_count(
      denominator = cards::ADSL,
      by = TRTA,
      variables = c(AEBODSYS, AEDECOD)
    )

  # the unobserved level appears in the table body
  expect_true("UNOBSERVED SOC" %in% tbl$table_body$label)

  # zero-rows have the correct structure: header (NA) + rate ("0") + count ("0")
  unobs_rows <- tbl$table_body |>
    dplyr::filter(.data$group1_level == "UNOBSERVED SOC")

  expect_equal(nrow(unobs_rows), 3L)
  expect_equal(unobs_rows$label[1], "UNOBSERVED SOC")
  expect_true(is.na(unobs_rows$stat_1[1]))
  expect_equal(unobs_rows$stat_1[2], "0")
  expect_equal(unobs_rows$stat_1[3], "0")

  # add_overall populates the overall column with "0" for zero-rows
  tbl_overall <- tbl |> add_overall(last = TRUE)
  unobs_overall <- tbl_overall$table_body |>
    dplyr::filter(.data$group1_level == "UNOBSERVED SOC")
  expect_equal(unobs_overall$stat_0[2], "0")

  expect_snapshot(as.data.frame(tbl))
})

test_that("tbl_hierarchical_rate_and_count() ignores non-factor variables", {
  # character AEBODSYS should not trigger zero-row injection
  tbl_char <- ADAE_subset |>
    dplyr::mutate(AEBODSYS = as.character(AEBODSYS)) |>
    tbl_hierarchical_rate_and_count(
      denominator = cards::ADSL,
      by = TRTA,
      variables = c(AEBODSYS, AEDECOD)
    )

  tbl_no_factor <- ADAE_subset |>
    tbl_hierarchical_rate_and_count(
      denominator = cards::ADSL,
      by = TRTA,
      variables = c(AEBODSYS, AEDECOD)
    )

  # both should produce identical output (no extra rows)
  expect_equal(nrow(tbl_char$table_body), nrow(tbl_no_factor$table_body))
})

test_that("tbl_hierarchical_rate_and_count() handles 0-row data with factor levels", {
  withr::local_options(list(width = 220))

  empty_adae <- ADAE_subset |>
    dplyr::filter(FALSE) |>
    dplyr::mutate(
      AEBODSYS = factor(character(0), levels = c("SOC_A", "SOC_B"))
    )

  tbl <- empty_adae |>
    tbl_hierarchical_rate_and_count(
      denominator = cards::ADSL,
      by = TRTA,
      variables = c(AEBODSYS, AEDECOD),
      label_overall_count = "remove"
    )

  # overall rate + 2 baskets * (header + rate + count) = 7 rows
  expect_equal(nrow(tbl$table_body), 7L)

  # all stat values are either "0" or NA (headers)
  stat_vals <- unlist(tbl$table_body[grep("^stat_", names(tbl$table_body))])
  expect_true(all(stat_vals %in% c("0", NA_character_)))

  # class is preserved

  expect_s3_class(tbl, "tbl_hierarchical_rate_and_count")

  expect_snapshot(as.data.frame(tbl))
})

test_that("tbl_hierarchical_rate_and_count() zero-rows work with by = NULL", {
  adae_factor <- ADAE_subset |>
    dplyr::mutate(
      AEBODSYS = factor(
        AEBODSYS,
        levels = c(unique(AEBODSYS), "EMPTY SOC")
      )
    )

  tbl <- adae_factor |>
    tbl_hierarchical_rate_and_count(
      denominator = cards::ADSL,
      variables = c(AEBODSYS, AEDECOD)
    )

  unobs_rows <- tbl$table_body |>
    dplyr::filter(.data$group1_level == "EMPTY SOC")

  expect_equal(nrow(unobs_rows), 3L)
  expect_equal(unobs_rows$stat_0[2], "0")
  expect_equal(unobs_rows$stat_0[3], "0")
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
