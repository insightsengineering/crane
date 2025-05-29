skip_on_cran()

ADAE_subset <- cards::ADAE |>
  dplyr::slice(.by = AEDECOD, 1:3) |>
  dplyr::filter(AEBODSYS %in% unique(AEBODSYS)[1:2]) |>
  dplyr::slice(1:10)

test_that("tbl_ae_rate_and_count() works", {
  withr::local_options(list(width = 220))
  expect_silent(
    tbl <-
      ADAE_subset |>
      tbl_ae_rate_and_count(
        denominator = cards::ADSL |> dplyr::rename(TRTA = TRT01A),
        by = TRTA,
        ae = AEDECOD,
        soc = AEBODSYS,
      ) |>
      add_overall(last = TRUE)
  )

  # check the first two labels are the overall rows
  expect_equal(
    tbl$table_body$label[1:2],
    c(
      "Total number of patients with at least one adverse event",
      "Overall total number of events"
    )
  )

  # snapshot of the table
  expect_snapshot(as.data.frame(tbl))
})

test_that("tbl_ae_rate_and_count(hlt)", {
  withr::local_options(list(width = 220))

  expect_silent(
    tbl <-
      ADAE_subset |>
      tbl_ae_rate_and_count(
        denominator = cards::ADSL |> dplyr::rename(TRTA = TRT01A),
        by = TRTA,
        ae = AEDECOD,
        soc = AEBODSYS,
        hlt = AEHLT
      ) |>
      add_overall(last = TRUE)
  )

  # snapshot of the table
  expect_snapshot(as.data.frame(tbl))
})

test_that("tbl_ae_rate_and_count(filter)", {
  expect_silent(
    tbl <-
      ADAE_subset |>
      tbl_ae_rate_and_count(
        denominator = cards::ADSL |> dplyr::rename(TRTA = TRT01A),
        by = TRTA,
        ae = AEDECOD,
        soc = AEBODSYS,
        filter = sum(n) / sum(N) > 0.01
      ) |>
      add_overall(last = TRUE)
  )

  # check that all rows have a percent larger than 1%
  expect_true(
    tbl$table_body |>
      dplyr::filter(str_detect(stat_0, "%") & variable == "AEDECOD") |>
      dplyr::pull(stat_0) |>
      str_extract("(?<=\\().*?(?=\\))") |> # extract the percent between the parantheses
      str_remove("%") %>%
      {
        as.numeric(.) > 1.0
      } |> # styler off
      all()
  )
})

# TODO: Update this after this issue is resolved https://github.com/ddsjoberg/gtsummary/issues/2237
# test_that("tbl_ae_rate_and_count(sort)", {
#   expect_silent(
#     tbl <-
#       ADAE_subset |>
#       tbl_ae_rate_and_count(
#         denominator = cards::ADSL |> dplyr::rename(TRTA = TRT01A),
#         by = TRTA,
#         ae = AEDECOD,
#         soc = AEBODSYS,
#         sort = "descending"
#       ) |>
#       add_overall(last = TRUE)
#   )
# })
