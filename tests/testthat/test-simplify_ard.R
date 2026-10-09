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

  expect_message(ard_split <- simplify_ard(tbl_split), "repeated cop")
  expect_identical(ard_split, simplify_ard(tbl))
  expect_message(
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

test_that("simplify_ard() adds a `tbl_id1` group to stacks of the same statistics", {
  tbl_f <- tbl_roche_summary(dplyr::filter(cards::ADSL, SEX == "F"), by = ARM, include = AGE)
  tbl_m <- tbl_roche_summary(dplyr::filter(cards::ADSL, SEX == "M"), by = ARM, include = AGE)

  ard <- simplify_ard(gtsummary::tbl_stack(list(tbl_f, tbl_m), quiet = TRUE))
  expect_equal(unique(ard$group2), "tbl_id1")
  expect_setequal(unlist(ard$group2_level), c("1", "2"))

  ard <- simplify_ard(gtsummary::tbl_stack(list(tbl_f, tbl_m), tbl_ids = c("female", "male"), quiet = TRUE))
  expect_setequal(unlist(ard$group2_level), c("female", "male"))

  # different statistics on the same data do not need a `tbl_id1` group
  tbl_age <- tbl_roche_summary(cards::ADSL, by = ARM, include = AGE)
  tbl_sex <- tbl_roche_summary(cards::ADSL, by = ARM, include = SEX)
  ard <- simplify_ard(gtsummary::tbl_stack(list(tbl_age, tbl_sex), quiet = TRUE))
  expect_false("tbl_id1" %in% ard$group2)
})

test_that("simplify_ard() names nested `tbl_id` groups by level and aligns them", {
  adsl_f <- dplyr::filter(cards::ADSL, SEX == "F")
  adsl_m <- dplyr::filter(cards::ADSL, SEX == "M")
  stack_age <- function(data) {
    gtsummary::tbl_stack(
      list(
        tbl_roche_summary(dplyr::filter(data, AGE < 75), by = ARM, include = AGE),
        tbl_roche_summary(dplyr::filter(data, AGE >= 75), by = ARM, include = AGE)
      ),
      quiet = TRUE
    )
  }

  # inner stacks need `tbl_id1`, the outer stack `tbl_id2`
  ard <- simplify_ard(gtsummary::tbl_stack(list(stack_age(adsl_f), stack_age(adsl_m)), quiet = TRUE))
  expect_equal(unique(ard$group2), "tbl_id1")
  expect_equal(unique(ard$group3), "tbl_id2")

  # a stack id lands in the same column whether or not the pieces have groups
  stack_no_by <- gtsummary::tbl_stack(
    list(
      tbl_roche_summary(adsl_f, include = AGE),
      tbl_roche_summary(adsl_m, include = AGE)
    ),
    quiet = TRUE
  )
  ard <- simplify_ard(list(stack_age(cards::ADSL), stack_no_by))
  expect_equal(unique(stats::na.omit(ard$group1)), "ARM")
  expect_equal(unique(ard$group2), "tbl_id1")

  # an outer stack id keeps its column for pieces that are not stacked themselves
  tbl_all <- tbl_roche_summary(cards::ADSL, by = ARM, include = AGE)
  ard <- simplify_ard(
    gtsummary::tbl_stack(list(stack_age(adsl_f), stack_age(adsl_m), tbl_all), quiet = TRUE)
  )
  expect_equal(unique(stats::na.omit(ard$group2)), "tbl_id1")
  expect_equal(unique(ard$group3), "tbl_id2")
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

  expect_snapshot(simplify_ard(dplyr::bind_rows(ard, ard_other)), error = TRUE)
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
  expect_equal(nrow(simplify_ard(data.frame())), 0L)
})

test_that("simplify_ard() messaging", {
  expect_snapshot(simplify_ard("not a table"), error = TRUE)
  expect_snapshot(simplify_ard(cards::as_card(cards::ADSL[1:2, 1:3], check = FALSE)), error = TRUE)
  expect_snapshot(simplify_ard(list(), .unlist = "yes"), error = TRUE)
})
