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
        denominator = cards::ADSL |> dplyr::rename(TRTA = TRT01A),
        by = TRTA,
        variables = c(AEBODSYS, AEDECOD)
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

  # snapshot of the table
  expect_snapshot(as.data.frame(tbl))

  # testing with a high level term
  expect_silent(
    tbl <-
      ADAE_subset |>
      tbl_hierarchical_rate_and_count(
        denominator = cards::ADSL |> dplyr::rename(TRTA = TRT01A),
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
        denominator = cards::ADSL |> dplyr::rename(TRTA = TRT01A),
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
