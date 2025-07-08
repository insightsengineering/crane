test_that("tbl_baseline_chg() works", {
  withr::local_options(list(width = 120))
  # loading lab data
  adlb <- syntheticadam::adlb
  adsl <- syntheticadam::adsl
  expect_silent(
    tbl <-
      tbl_baseline_chg(
        data = adlb,
        by = "TRT01A",
        denominator = adsl
      )
  )
  expect_snapshot(as.data.frame(tbl))

  # Vital Signs Data
  advs <- syntheticadam::advs
  expect_silent(
    tbl <-
      tbl_baseline_chg(
        data = advs,
        by = "TRT01A",
        test_code = "VSTESTCD",
        denominator = adsl,
        test_subset = "DIABP"
      )
  )
  expect_snapshot(as.data.frame(tbl))

  # ECG Data
  adeg <- syntheticadam::adeg
  expect_silent(
    tbl <-
      tbl_baseline_chg(
        data = adeg,
        by = "TRT01A",
        test_code = "EGTESTCD",
        denominator = adsl,
        test_subset = "EGHRMN"
      )
  )
  expect_snapshot(as.data.frame(tbl))
})

# test_that("add_overall.tbl_baseline_chg() works", {
#   withr::local_options(list(width = 180))
#   expect_snapshot(
#     tbl_baseline_chg(
#       data = adlb,
#       by = "TRT01A",
#       denominator = adsl
#     ) |>
#       add_overall(last = TRUE, col_label = "**All Participants**  \nN = {n}") |>
#       as.data.frame()
#   )
# })
