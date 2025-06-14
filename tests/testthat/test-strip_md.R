test_that("strip_md_bold() works with character vector", {
  input_string <-
    c(
      "**Placebo**  \nN=45",
      "**High Dose**  \nN=54",
      "__Placebo__  \nN=45",
      "__High Dose__  \nN=54",
      "**High** **Dose**",
      "__High__ __Dose__",
      "**single",
      "__single"
    )

  expect_equal(
    strip_md_bold(input_string, type = "star"),
    c(
      "Placebo  \nN=45",
      "High Dose  \nN=54",
      "__Placebo__  \nN=45",
      "__High Dose__  \nN=54",
      "High Dose",
      "__High__ __Dose__",
      "**single",
      "__single"
    )
  )
  expect_equal(
    strip_md_bold(input_string, type = "underscore"),
    c(
      "**Placebo**  \nN=45",
      "**High Dose**  \nN=54",
      "Placebo  \nN=45",
      "High Dose  \nN=54",
      "**High** **Dose**",
      "High Dose",
      "**single",
      "__single"
    )
  )
  expect_equal(
    strip_md_bold(input_string, type = c("star", "underscore")),
    c(
      "Placebo  \nN=45",
      "High Dose  \nN=54",
      "Placebo  \nN=45",
      "High Dose  \nN=54",
      "High Dose",
      "High Dose",
      "**single",
      "__single"
    )
  )
})

test_that("strip_md_italic() works", {
  input_string <-
    c(
      "*Placebo*  \nN=45",
      "*High Dose*  \nN=54",
      "_Placebo_  \nN=45",
      "_High Dose_  \nN=54",
      "*High* *Dose*",
      "_High_ _Dose_",
      "*single",
      "_single"
    )

  expect_equal(
    strip_md_italic(input_string, type = "star"),
    c(
      "Placebo  \nN=45",
      "High Dose  \nN=54",
      "_Placebo_  \nN=45",
      "_High Dose_  \nN=54",
      "High Dose",
      "_High_ _Dose_",
      "*single",
      "_single"
    )
  )
  expect_equal(
    strip_md_italic(input_string, type = "underscore"),
    c(
      "*Placebo*  \nN=45",
      "*High Dose*  \nN=54",
      "Placebo  \nN=45",
      "High Dose  \nN=54",
      "*High* *Dose*",
      "High Dose",
      "*single",
      "_single"
    )
  )
  expect_equal(
    strip_md_italic(input_string, type = c("star", "underscore")),
    c(
      "Placebo  \nN=45",
      "High Dose  \nN=54",
      "Placebo  \nN=45",
      "High Dose  \nN=54",
      "High Dose",
      "High Dose",
      "*single",
      "_single"
    )
  )
})

test_that("strip_md_bold() works with gtsummary table", {
  expect_false(
    tbl_strata(
      trial,
      strata = "grade",
      ~ tbl_summary(.x, include = age)
    ) |>
      strip_md_bold() |>
      as.data.frame() |>
      names() |>
      str_detect(pattern = "*", fixed = TRUE) |>
      any()
  )

  expect_false(
    tbl_strata(
      trial,
      strata = "grade",
      ~ tbl_summary(.x, include = age),
      .header = "_{strata}_"
    ) |>
      strip_md_italic() |>
      as.data.frame() |>
      names() |>
      str_detect(pattern = "_", fixed = TRUE) |>
      any()
  )
})
