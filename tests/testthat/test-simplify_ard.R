adlb <- cards::ADLB |>
  dplyr::mutate(AVISIT = trimws(AVISIT)) |>
  dplyr::filter(
    PARAMCD %in% c("SODIUM", "K"),
    AVISIT %in% c("Baseline", "Week 2", "Week 4")
  )

tbl_param <- function(data) {
  gtsummary::tbl_strata(
    data,
    strata = PARAM,
    .tbl_fun = ~ tbl_baseline_chg(.x, baseline_level = "Baseline", by = "TRTA", denominator = cards::ADSL),
    .combine_with = "tbl_stack",
    .combine_args = list(group_header = NULL, quiet = TRUE)
  )
}

test_that("simplify_ard() works with a single table", {
  tbl <- tbl_roche_summary(cards::ADSL, by = ARM, include = c(AGE, SEX), nonmissing = "always") |>
    add_overall(last = TRUE)
  ard <- simplify_ard(tbl)

  expect_s3_class(ard, "card")
  # attributes and total N shared by `tbl_summary` and `add_overall` are kept once
  expect_equal(nrow(ard), nrow(dplyr::bind_rows(gtsummary::gather_ard(tbl))) - 5L)
  expect_true(all(is.na(ard$group1[ard$variable == "..ard_total_n.."])))
})

test_that("simplify_ard() adds strata as group columns", {
  tbl <- suppressMessages(tbl_param(adlb))
  ard <- simplify_ard(tbl)

  expect_equal(unique(ard$group2), "PARAM")
  expect_setequal(unlist(ard$group2_level), unique(adlb$PARAM))
  expect_equal(unique(stats::na.omit(ard$group1)), "TRTA")
})

test_that("simplify_ard() removes the ARD copies of split tables", {
  tbl <- suppressMessages(tbl_param(adlb))
  tbl_split <- gtsummary::tbl_split_by_rows(tbl, variable_level = ends_with("lbl"))

  expect_warning(ard_split <- simplify_ard(tbl_split), "repeated cop")
  expect_identical(ard_split, simplify_ard(tbl))
  expect_warning(
    expect_identical(simplify_ard(lapply(tbl_split, gtsummary::gather_ard)), ard_split),
    "repeated cop"
  )
})

test_that("simplify_ard() numbers nested strata innermost first", {
  tbl <- gtsummary::tbl_strata_nested_stack(
    dplyr::filter(adlb, AVISIT == "Week 2"),
    strata = c(PARAM, SEX),
    .tbl_fun = ~ tbl_roche_summary(.x, by = TRTA, include = AVAL),
    quiet = TRUE
  )
  ard <- simplify_ard(tbl)

  expect_equal(unique(ard$group2), "SEX")
  expect_equal(unique(ard$group3), "PARAM")
})

test_that("simplify_ard() flattens nested ARD lists", {
  tbl <- cards::ADAE |>
    dplyr::filter(AESOC %in% unique(AESOC)[1:2]) |>
    tbl_hierarchical_rate_and_count(
      variables = c(AESOC, AEDECOD), by = TRTA, denominator = cards::ADSL, id = USUBJID
    )
  ard <- simplify_ard(tbl)

  expect_s3_class(ard, "card")
  # subject and event counts share stat_name "n" and are told apart by context
  expect_setequal(unique(ard$context), c("hierarchical", "hierarchical_count", "tabulate"))
})

test_that("simplify_ard() errors on different values for the same statistic", {
  ard <- cards::ard_summary(cards::ADSL, variables = AGE)
  ard_other <- dplyr::mutate(ard, stat = lapply(stat, \(x) if (is.numeric(x)) x + 1 else x))

  expect_snapshot(simplify_ard(list(ard, ard_other)), error = TRUE)
})

test_that("simplify_ard(.deduplicate, .unlist) work", {
  ard <- cards::ard_summary(cards::ADSL, variables = AGE)

  expect_equal(nrow(simplify_ard(list(ard, ard), .deduplicate = FALSE)), 2L * nrow(ard))
  expect_s3_class(simplify_ard(ard, .unlist = TRUE), "card_unlisted")
})

test_that("simplify_ard() output works with compare_ard()", {
  ard <- simplify_ard(suppressMessages(tbl_param(adlb)))
  comparison <- cards::compare_ard(
    ard, ard,
    keys = c(cards::all_ard_groups(), cards::all_ard_variables(), "context", "stat_name")
  )

  expect_true(cards::is_ard_equal(comparison))
})

test_that("simplify_ard() returns an empty ARD for tables without ARD", {
  ard <- suppressMessages(simplify_ard(tbl_null_report()))

  expect_s3_class(ard, "card")
  expect_equal(nrow(ard), 0L)
})

test_that("simplify_ard() messaging", {
  expect_snapshot(simplify_ard("not a table"), error = TRUE)
  expect_snapshot(simplify_ard(list(), .unlist = "yes"), error = TRUE)
})
