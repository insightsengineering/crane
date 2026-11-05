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

# mean AGE by ARM, as a table stratified by `strata` and as an ARD built with cards only
adsl_age <- dplyr::filter(cards::ADSL, AGEGR1 %in% c("<65", ">80"))
tbl_mean_age <- ~ gtsummary::tbl_summary(
  .x,
  by = ARM, include = AGE, type = AGE ~ "continuous", statistic = AGE ~ "{mean}"
)
tbl_strata_mean_age <- function(data, strata) {
  gtsummary::tbl_strata(
    data,
    strata = {{ strata }},
    .tbl_fun = tbl_mean_age,
    .combine_with = "tbl_stack",
    .combine_args = list(quiet = TRUE)
  )
}
ard_mean_age <- function(data) {
  cards::ard_summary(data, by = ARM, variables = AGE, statistic = ~ cards::continuous_summary_fns("mean"))
}
summary_rows <- function(ard) dplyr::filter(ard, context == "summary", !is.na(group1))

# keys passed as names: cards 0.9.0 does not accept a selection of columns
expect_equal_to_cards <- function(ard, ard_cards) {
  keys <- names(dplyr::select(ard_cards, cards::all_ard_groups(), cards::all_ard_variables(), "context", "stat_name"))
  comparison <- cards::compare_ard(ard, ard_cards, keys = keys, columns = c("stat_label", "stat"))
  expect_true(cards::is_ard_equal(comparison))
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

test_that("simplify_ard() numbers strata as cards::ard_strata()", {
  # one call: the strata in the order of `strata`
  tbl <- gtsummary::tbl_strata_nested_stack(
    dplyr::filter(adlb, AVISIT == "Week 2"),
    strata = c(PARAM, SEX),
    .tbl_fun = ~ tbl_roche_summary(.x, by = TRTA, include = AVAL),
    quiet = TRUE
  )
  ard <- simplify_ard(tbl)
  expect_equal(unique(ard$group2), "PARAM")
  expect_equal(unique(ard$group3), "SEX")

  tbl <- gtsummary::tbl_strata_nested_stack(adsl_age, strata = c(SEX, AGEGR1), .tbl_fun = tbl_mean_age, quiet = TRUE)
  expect_equal_to_cards(
    summary_rows(simplify_ard(tbl)),
    cards::ard_strata(adsl_age, .strata = c(SEX, AGEGR1), .f = ard_mean_age)
  )

  # nested calls: the inner strata first, as with nested `ard_strata()` calls
  tbl <- gtsummary::tbl_strata(
    adsl_age,
    strata = SEX,
    .tbl_fun = ~ tbl_strata_mean_age(.x, AGEGR1),
    .combine_with = "tbl_stack",
    .combine_args = list(quiet = TRUE)
  )
  expect_equal_to_cards(
    summary_rows(simplify_ard(tbl)),
    cards::ard_strata(adsl_age, .strata = SEX, .f = \(df) cards::ard_strata(df, .strata = AGEGR1, .f = ard_mean_age))
  )
})

test_that("simplify_ard() matches the strata levels of `tbl_strata()` to their variables", {
  # AGEGR1 comes before SEX in the data, so `df_strata` has `strata_2` first
  ard <- simplify_ard(tbl_strata_mean_age(adsl_age, c(SEX, AGEGR1)))
  expect_equal(unique(ard$group2), "SEX")
  expect_setequal(unlist(ard$group2_level), c("F", "M"))
  expect_equal(unique(ard$group3), "AGEGR1")
  expect_setequal(unlist(ard$group3_level), c("<65", ">80"))
  expect_equal_to_cards(summary_rows(ard), cards::ard_strata(adsl_age, .strata = c(SEX, AGEGR1), .f = ard_mean_age))

  # three strata, none in the order of the data
  adsl_race <- dplyr::filter(cards::ADSL, RACE %in% c("WHITE", "BLACK OR AFRICAN AMERICAN"))
  expect_equal_to_cards(
    summary_rows(simplify_ard(tbl_strata_mean_age(adsl_race, c(RACE, SEX, AGEGR1)))),
    cards::ard_strata(adsl_race, .strata = c(RACE, SEX, AGEGR1), .f = ard_mean_age)
  )
})

test_that("simplify_ard() keeps the type of the strata levels", {
  adsl_fct <- dplyr::mutate(cards::ADSL, SEX = factor(SEX))
  ard <- summary_rows(simplify_ard(tbl_strata_mean_age(adsl_fct, SEX)))
  expect_s3_class(ard$group2_level[[1]], "factor")
  expect_equal(levels(ard$group2_level[[1]]), c("F", "M"))
  expect_equal_to_cards(ard, cards::ard_strata(adsl_fct, .strata = SEX, .f = ard_mean_age))

  ard <- summary_rows(simplify_ard(tbl_strata_mean_age(cards::ADSL, AGEGR1N)))
  expect_type(ard$group2_level[[1]], "double")
  expect_equal_to_cards(ard, cards::ard_strata(cards::ADSL, .strata = AGEGR1N, .f = ard_mean_age))
})

test_that("simplify_ard() reads factor levels with `,` and `=`", {
  # factor levels are not quoted in the names of the pieces
  adsl_comma <- dplyr::mutate(cards::ADSL, AGECAT = factor(ifelse(AGE < 70, "Young, <70", "Old, >=70")))
  ard <- simplify_ard(tbl_strata_mean_age(adsl_comma, AGECAT))
  expect_equal(unique(ard$group2), "AGECAT")
  expect_setequal(as.character(unlist(ard$group2_level)), c("Old, >=70", "Young, <70"))
  expect_equal_to_cards(summary_rows(ard), cards::ard_strata(adsl_comma, .strata = AGECAT, .f = ard_mean_age))

  # levels that read as two `VAR=level` pairs stay one stratum
  adsl_eq <- dplyr::mutate(cards::ADSL, AGECAT = factor(ifelse(AGE < 70, "Young, age=<70", "Old, age=>=70")))
  ard <- simplify_ard(tbl_strata_mean_age(adsl_eq, AGECAT))
  expect_equal(unique(ard$group2), "AGECAT")
  expect_false("group3" %in% names(ard))
})

test_that("simplify_ard() reads the strata from the names of the pieces when `df_strata` does not fit", {
  tbl <- tbl_strata_mean_age(adsl_age, SEX)
  tbl_other_levels <- tbl
  tbl_other_levels$df_strata$strata_1 <- c("x", "y")
  tbl_no_strata_1 <- tbl
  names(tbl_no_strata_1$df_strata)[names(tbl_no_strata_1$df_strata) == "strata_1"] <- "strata_2"

  for (x in list(tbl_other_levels, tbl_no_strata_1)) {
    ard <- simplify_ard(x)
    expect_equal(unique(ard$group2), "SEX")
    expect_setequal(unlist(ard$group2_level), c("F", "M"))
  }
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
  expect_false("group2" %in% names(ard))
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

test_that("simplify_ard(.deduplicate) works", {
  ard <- cards::ard_summary(cards::ADSL, variables = AGE)

  expect_equal(nrow(simplify_ard(list(ard, ard), .deduplicate = FALSE)), 2L * nrow(ard))
})

test_that("simplify_ard() output matches the same ARD built with cards", {
  adlb_wk2 <- dplyr::filter(adlb, AVISIT == "Week 2")
  ard <- gtsummary::tbl_strata(
    adlb_wk2,
    strata = PARAM,
    .tbl_fun = ~ tbl_roche_summary(.x, by = TRTA, include = AVAL, type = AVAL ~ "continuous2"),
    .combine_with = "tbl_stack",
    .combine_args = list(group_header = NULL, quiet = TRUE)
  ) |>
    suppressMessages() |>
    simplify_ard() |>
    dplyr::filter(context == "summary")
  ard_qc <- cards::ard_strata(
    adlb_wk2,
    .strata = PARAM,
    .f = \(df) {
      cards::ard_summary(
        df,
        by = TRTA,
        variables = AVAL,
        statistic = ~ cards::continuous_summary_fns(c("mean", "sd", "median", "min", "max"))
      )
    }
  )
  expect_equal_to_cards(ard, ard_qc)
})

test_that("simplify_ard() returns a `card_simplified` and does not simplify twice", {
  ard <- simplify_ard(suppressMessages(tbl_param(adlb)))
  expect_s3_class(ard, c("card_simplified", "card"))
  expect_identical(simplify_ard(ard), ard)
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
  expect_snapshot(simplify_ard(list(), .deduplicate = "yes"), error = TRUE)
})
