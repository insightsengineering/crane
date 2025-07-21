skip_if_not(is_pkg_installed("withr"))

test_that("tbl_roche_summary() works", {
  expect_silent(
    tbl <-
      gtsummary::trial |>
      tbl_roche_summary(
        by = trt,
        include = c(age, grade)
      )
  )
})

test_that("tbl_roche_summary() |> add_overall() works", {
  withr::local_options(list(width = 300))
  expect_equal(
    tbl_roche_summary(
      gtsummary::trial,
      by = trt,
      include = c(age, grade),
      digits = list(grade = list(p = 1)),
      nonmissing = "no"
    ) |>
      add_overall() |>
      as.data.frame(),
    gtsummary::tbl_summary(
      gtsummary::trial,
      by = trt,
      include = c(age, grade),
      type = list(age = "continuous2"),
      statistic = list(age = c("{mean} ({sd})", "{median}", "{min} - {max}")),
      digits = list(grade = list(p = 1)),
      missing = "no"
    ) |>
      add_overall(col_label = "All Participants  \n(N = {gtsummary::style_number(N)})") |>
      gtsummary::modify_header(label = "", all_stat_cols(FALSE) ~ "{level}  \n(N = {n})") |>
      modify_header_rm_md() |>
      as.data.frame()
  )

  expect_snapshot(
    tbl_roche_summary(
      gtsummary::trial,
      by = trt,
      digits = list(grade = list(p = 1)),
      include = c(age, grade),
      nonmissing = "always"
    ) |>
      add_overall() |>
      as.data.frame()
  )

  # addresses issue #41
  expect_equal(
    gtsummary::trial |>
      dplyr::mutate(grade = fct_expand(grade, "other")) |>
      tbl_roche_summary(
        by = trt,
        include = grade
      ) |>
      gtsummary::add_overall() |>
      as.data.frame(col_labels = FALSE) |>
      dplyr::pull("stat_0") |>
      dplyr::last(),
    "0"
  )
})

# addresses issue #60, when all NA column was tabulated it was returned as
#   "0 (NA%)" instead of "0"
test_that("tbl_roche_summary() recode counts for all NA column", {
  expect_equal(
    cards::ADSL |>
      dplyr::mutate(DTHCAT = ifelse(dplyr::row_number() == 1L, "ADVERSE EVENT", NA)) |>
      tbl_roche_summary(
        by = "ARM",
        include = "DTHCAT"
      ) |>
      as.data.frame(col_labels = FALSE) |>
      dplyr::pull(stat_3) |>
      dplyr::last(),
    "0"
  )
})
