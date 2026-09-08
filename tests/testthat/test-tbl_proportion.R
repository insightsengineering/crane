skip_if_pkg_not_installed(c("cardx", "broom.helpers", "withr"))

# small reproducible dataset shared across tests
prop_data <- function() {
  set.seed(42)
  data.frame(
    arm = factor(
      rep(c("Placebo", "Low", "High"), each = 60),
      levels = c("Placebo", "Low", "High")
    ),
    rsp = c(
      sample(c(TRUE, FALSE), 60, TRUE, prob = c(0.2, 0.8)),
      sample(c(TRUE, FALSE), 60, TRUE, prob = c(0.4, 0.6)),
      sample(c(TRUE, FALSE), 60, TRUE, prob = c(0.5, 0.5))
    ),
    cat = factor(
      sample(c("CR", "PR", "SD", "PD", "NE"), 180, TRUE),
      levels = c("CR", "PR", "SD", "PD", "NE")
    ),
    strata = factor(sample(c("S1", "S2"), 180, TRUE))
  )
}

test_that("tbl_proportion() binary summary works", {
  withr::local_options(list(width = 200))
  tbl <- tbl_proportion(prop_data(), "rsp", "arm", value = TRUE)
  expect_s3_class(tbl, "tbl_proportion")
  expect_snapshot(as.data.frame(tbl))
})

test_that("tbl_proportion() multinomial summary preserves factor level order", {
  withr::local_options(list(width = 200))
  tbl <- tbl_proportion(
    prop_data(), "cat", "arm",
    label = list(CR = "Complete Response (CR)", PR = "Partial Response (PR)")
  )
  body <- tbl$table_body
  lab_rows <- body$label[body$row_type == "label"]
  expect_equal(
    lab_rows,
    c("Complete Response (CR)", "Partial Response (PR)", "SD", "PD", "NE")
  )
})

test_that("tbl_proportion() supports different CI methods and conf.level", {
  wald <- tbl_proportion(prop_data(), "rsp", "arm", value = TRUE, method = "wald")
  cp <- tbl_proportion(prop_data(), "rsp", "arm", value = TRUE, method = "clopper-pearson")
  # CI row label reflects the method
  expect_true(any(grepl("Wald", wald$table_body$label)))
  expect_true(any(grepl("Clopper-Pearson", cp$table_body$label)))
  # different methods give different CI strings
  expect_false(identical(
    wald$table_body$stat_1[2],
    cp$table_body$stat_1[2]
  ))
  # conf.level flows into the label
  ninety <- tbl_proportion(prop_data(), "rsp", "arm", value = TRUE, conf.level = 0.90)
  expect_true(any(grepl("^90% CI", ninety$table_body$label)))
})

test_that("tbl_proportion() works without a by variable", {
  tbl <- tbl_proportion(prop_data(), "rsp", by = NULL, value = TRUE)
  expect_s3_class(tbl, "tbl_proportion")
  expect_true("stat_0" %in% names(tbl$table_body) || "stat_1" %in% names(tbl$table_body))
})

test_that("add_proportion_difference() adds the comparison block", {
  withr::local_options(list(width = 200))
  tbl <- tbl_proportion(prop_data(), "rsp", "arm", value = TRUE) |>
    add_proportion_difference(reference = "Placebo", test = "chisq")
  labs <- tbl$table_body$label
  expect_true("Unstratified Analysis" %in% labs)
  expect_true("Difference in Response rate (%)" %in% labs)
  expect_true(any(grepl("Chi-Squared", labs)))
  expect_snapshot(as.data.frame(tbl))
})

test_that("add_proportion_difference() supports fisher and cmh", {
  fisher <- tbl_proportion(prop_data(), "rsp", "arm", value = TRUE) |>
    add_proportion_difference(reference = "Placebo", test = "fisher")
  expect_true(any(grepl("Fisher", fisher$table_body$label)))

  cmh <- tbl_proportion(prop_data(), "rsp", "arm", value = TRUE) |>
    add_proportion_difference(reference = "Placebo", strata = "strata")
  expect_true(any(grepl("Cochran-Mantel-Haenszel", cmh$table_body$label)))
  expect_true("Stratified Analysis" %in% cmh$table_body$label)
})

test_that("add_proportion_odds_ratio() adds an odds-ratio row", {
  tbl <- tbl_proportion(prop_data(), "rsp", "arm", value = TRUE) |>
    add_proportion_odds_ratio(reference = "Placebo")
  or_row <- tbl$table_body[grepl("Odds Ratio", tbl$table_body$label), ]
  expect_equal(nrow(or_row), 1L)
  # reference column is blank, comparison columns are populated
  expect_true(is.na(or_row$stat_1))
  expect_match(or_row$stat_2, "\\(")
})

test_that("odds ratio matches the 2x2 hand calculation", {
  d <- data.frame(
    arm = factor(c(rep("Ref", 100), rep("Trt", 100)), levels = c("Ref", "Trt")),
    rsp = c(rep(TRUE, 20), rep(FALSE, 80), rep(TRUE, 40), rep(FALSE, 60))
  )
  tbl <- tbl_proportion(d, "rsp", "arm", value = TRUE) |>
    add_proportion_odds_ratio(reference = "Ref")
  or_val <- tbl$table_body$stat_2[grepl("Odds Ratio", tbl$table_body$label)]
  manual <- (40 / 60) / (20 / 80) # 2.67
  expect_match(or_val, sprintf("^%.2f ", manual))
})

test_that("stratified odds ratio uses the CMH common odds ratio", {
  tbl <- tbl_proportion(prop_data(), "rsp", "arm", value = TRUE) |>
    add_proportion_odds_ratio(reference = "Placebo", strata = "strata")
  or_row <- tbl$table_body[grepl("Odds Ratio", tbl$table_body$label), ]
  expect_match(or_row$stat_2, "\\(")
})

test_that("informative errors are raised", {
  d <- prop_data()
  expect_snapshot(
    tbl_proportion(d, "rsp", "arm", value = TRUE, conf.level = 1.5),
    error = TRUE
  )
  expect_snapshot(
    tbl_proportion(d, "rsp", by = NULL, value = TRUE) |> add_proportion_difference(),
    error = TRUE
  )
  expect_snapshot(
    tbl_proportion(d, "rsp", "arm") |> add_proportion_difference(),
    error = TRUE
  )
  expect_snapshot(
    tbl_proportion(d, "rsp", "arm", value = TRUE) |> add_proportion_difference(test = "cmh"),
    error = TRUE
  )
  expect_snapshot(
    tbl_proportion(d, "rsp", "arm", value = TRUE) |> add_proportion_difference(reference = "Z"),
    error = TRUE
  )
})
