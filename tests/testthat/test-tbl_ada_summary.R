# Setup: minimal ADAT-like data frame with all required columns ----------------
make_adat <- function(arms = c("Drug A", "Drug B"), n_per_arm = 5) {
  n <- length(arms) * n_per_arm
  data.frame(
    USUBJID = paste0("S", seq_len(n)),
    TRT = rep(arms, each = n_per_arm),
    ADABLPFL = c(rep("Y", n - 2), "N", "N"),
    adaposbl = c(rep("Y", 3), rep("N", n - 3)),
    nabevalbase = c(rep("Y", n - 1), "N"),
    nabposbl = c(rep("Y", 2), rep("N", n - 2)),
    nabnegbl = c(rep("N", 2), rep("Y", n - 2)),
    ADPBLPFL = c(rep("Y", n - 2), "N", "N"),
    adastatpos = c(rep("Y", 3), rep("N", n - 3)),
    nabpbeval = c(rep("Y", n - 1), "N"),
    nabstatpos = c(rep("Y", 2), rep("N", n - 2)),
    nabstatneg = c(rep("N", 2), rep("Y", n - 2)),
    stringsAsFactors = FALSE
  )
}

adat_two_arm <- make_adat()
adat_one_arm <- make_adat(arms = "Drug A")
adat_three_arm <- make_adat(arms = c("Drug A", "Drug B", "Drug C"))


# --- 1. Input validation ------------------------------------------------------
test_that("tbl_ada_summary() errors on missing or invalid inputs", {
  # data is not a data frame
  expect_error(
    tbl_ada_summary(data = list(x = 1), by = "TRT"),
    regexp = "must be class.*data.frame"
  )

  # required columns missing
  expect_error(
    tbl_ada_summary(data = data.frame(TRT = "A"), by = "TRT"),
    regexp = "required columns are missing"
  )

  # pools not a fully named list
  expect_error(
    tbl_ada_summary(adat_two_arm, by = "TRT", pools = c("bad")),
    regexp = "pools.*must be a fully named list"
  )

  # total_label duplicates a pool name
  expect_error(
    tbl_ada_summary(
      adat_two_arm,
      by = "TRT",
      pools = list("All Patients" = c("Drug A", "Drug B")),
      total_label = "All Patients"
    ),
    regexp = "total_label.*must not duplicate"
  )

  # null_postbaseline is not a character vector
  expect_error(
    tbl_ada_summary(adat_two_arm, by = "TRT", null_postbaseline = 42L),
    regexp = "null_postbaseline.*must be a character vector"
  )

  # null_postbaseline references an unknown label
  expect_error(
    tbl_ada_summary(
      adat_two_arm,
      by = "TRT",
      null_postbaseline = "Nonexistent Pool"
    ),
    regexp = "null_postbaseline.*can only contain"
  )
})


# --- 2. Basic output ----------------------------------------------------------
test_that("tbl_ada_summary() returns correct class with two-arm data", {
  tbl <- tbl_ada_summary(adat_two_arm, by = "TRT")

  expect_s3_class(tbl, "tbl_ada_summary")
  expect_s3_class(tbl, "gtsummary")
})

test_that("tbl_ada_summary() works without a by variable (unstratified)", {
  expect_silent(
    tbl <- tbl_ada_summary(adat_two_arm, by = NULL)
  )
  expect_s3_class(tbl, "tbl_ada_summary")
})

test_that("tbl_ada_summary() works with a single-arm dataset", {
  expect_silent(
    tbl <- tbl_ada_summary(adat_one_arm, by = "TRT")
  )
  expect_s3_class(tbl, "tbl_ada_summary")
})


# --- 3. Computed columns (Total + pools) --------------------------------------
test_that("tbl_ada_summary() adds a Total column for multi-arm data", {
  tbl <- tbl_ada_summary(adat_two_arm, by = "TRT", total_label = "All Patients")

  header_labels <- tbl$table_styling$header$label
  expect_true(any(grepl("All Patients", header_labels, fixed = TRUE)))
})

test_that("tbl_ada_summary() adds custom pool columns alongside Total", {
  tbl <- tbl_ada_summary(
    adat_three_arm,
    by = "TRT",
    pools = list("Drugs A+B" = c("Drug A", "Drug B")),
    total_label = "All"
  )

  header_labels <- tbl$table_styling$header$label
  expect_true(any(grepl("Drugs A+B", header_labels, fixed = TRUE)))
  expect_true(any(grepl("All", header_labels, fixed = TRUE)))
})

test_that("tbl_ada_summary() keep_original = FALSE drops individual arm columns", {
  tbl <- tbl_ada_summary(
    adat_two_arm,
    by = "TRT",
    total_label = "All Patients",
    keep_original = FALSE
  )

  header_labels <- tbl$table_styling$header$label
  expect_false(any(grepl("Drug A", header_labels, fixed = TRUE)))
  expect_false(any(grepl("Drug B", header_labels, fixed = TRUE)))
  expect_true(any(grepl("All Patients", header_labels, fixed = TRUE)))
})


# --- 4. null_postbaseline blanking --------------------------------------------
test_that("tbl_ada_summary() blanks specified post-baseline computed columns", {
  tbl <- tbl_ada_summary(
    adat_two_arm,
    by = "TRT",
    total_label = "All Patients",
    null_postbaseline = "All Patients"
  )

  # Find the stat column(s) for All Patients
  header <- tbl$table_styling$header
  stat_cols <- header$column[
    grepl("^stat_", header$column) &
      grepl("All Patients", header$label, fixed = TRUE)
  ]

  # Post-baseline rows for that column should all be NA
  postbaseline_vars <- c("ADPBLPFL", "adastatpos", "nabpbeval", "nabstatpos", "nabstatneg")
  body <- tbl$table_body

  for (col in stat_cols) {
    postbaseline_vals <- body[[col]][body$variable %in% postbaseline_vars]
    expect_true(all(is.na(postbaseline_vals)))
  }
})

test_that("tbl_ada_summary() errors when null_postbaseline matches nothing", {
  expect_error(
    tbl_ada_summary(
      adat_two_arm,
      by = "TRT",
      null_postbaseline = "Unknown Pool"
    ),
    regexp = "null_postbaseline.*can only contain"
  )
})


# --- 5. Section headers -------------------------------------------------------
test_that("tbl_ada_summary() includes both section header rows in table body", {
  tbl <- tbl_ada_summary(
    adat_two_arm,
    by = "TRT",
    baseline_header = "Baseline Section",
    postbaseline_header = "Post-Baseline Section"
  )

  labels <- tbl$table_body$label
  expect_true("Baseline Section" %in% labels)
  expect_true("Post-Baseline Section" %in% labels)
})


# --- 6. inputs / call_list stored on object -----------------------------------
test_that("tbl_ada_summary() stores inputs and call_list", {
  tbl <- tbl_ada_summary(adat_two_arm, by = "TRT")

  expect_true(is.list(tbl$inputs))
  expect_true("tbl_ada_summary" %in% names(tbl$call_list))
})


# --- 7. Snapshot of rendered output -------------------------------------------
test_that("tbl_ada_summary() snapshot: two-arm with Total column", {
  withr::local_options(list(width = 200))
  expect_snapshot(
    as.data.frame(tbl_ada_summary(adat_two_arm, by = "TRT"))
  )
})

test_that("tbl_ada_summary() snapshot: unstratified", {
  withr::local_options(list(width = 200))
  expect_snapshot(
    as.data.frame(tbl_ada_summary(adat_two_arm, by = NULL))
  )
})
