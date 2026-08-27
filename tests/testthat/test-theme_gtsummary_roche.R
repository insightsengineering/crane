test_that("theme_gtsummary_roche() works", {
  # check structure of theme
  expect_snapshot(
    gtsummary::check_gtsummary_theme(theme_gtsummary_roche(set_theme = FALSE))
  )
})

test_that("theme_gtsummary_roche() adds relevant {flextable} directives", {
  # check user_added2+ are present
  cmds <-
    gtsummary::with_gtsummary_theme(
      theme_gtsummary_roche(),
      as_flex_table(gtsummary::tbl_summary(data.frame(1)), return_calls = TRUE)
    )

  added_cmds_roche_specific <- cmds[grepl("user_added[0-9]", names(cmds))]
  expect_snapshot(
    added_cmds_roche_specific
  )
})

test_that("theme_gtsummary_roche() no errors with a gt print", {
  expect_silent(
    gtsummary::with_gtsummary_theme(
      theme_gtsummary_roche(),
      tbl_summary(data.frame(1)) |> as_gt()
    )
  )
})

test_that("theme_gtsummary_roche() styles `tbl_hierarchical*()` results", {
  expect_silent(
    tbl <-
      with_gtsummary_theme(
        x = theme_gtsummary_roche(),
        cards::ADAE |>
          dplyr::slice(.by = "TRTA", 1L) |>
          tbl_hierarchical(
            variables = c(AESOC, AETERM),
            by = TRTA,
            denominator = cards::ADSL |> mutate(TRTA = ARM),
            id = USUBJID
          )
      )
  )

  # check no bold syntax in header
  expect_equal(
    tbl$table_styling$header$label,
    modify_header_rm_md(tbl)$table_styling$header$label
  )

  # check zero recode
  expect_equal(
    as.data.frame(tbl),
    modify_zero_recode(tbl) |> as.data.frame()
  )

  # check there is no footnote
  expect_equal(
    tbl |>
      as_gt() |>
      getElement("_footnotes") |>
      nrow(),
    0L
  )


  expect_silent(
    tbl <-
      with_gtsummary_theme(
        x = theme_gtsummary_roche(),
        cards::ADAE |>
          dplyr::slice(.by = "TRTA", 1L) |>
          tbl_hierarchical_count(
            variables = c(AESOC, AETERM, AESEV),
            by = TRTA
          )
      )
  )

  # check no bold syntax in header
  expect_equal(
    tbl$table_styling$header$label,
    modify_header_rm_md(tbl)$table_styling$header$label
  )

  # check there is no footnote
  expect_equal(
    tbl |>
      as_gt() |>
      getElement("_footnotes") |>
      nrow(),
    0L
  )
})

test_that("with assign_summary_type-arg:cat_threshold=0L each data type is always mapped as it is", {
  # it would be 4 if casted categorical instead of numeric for cyl
  expect_equal(nrow(gtsummary::tbl_summary(mtcars, include = cyl)$table_body), 1)
})

test_that("theme pre-conversion modifies header not to be bold and border only 0.5", {
  skip_if_not_installed("pharmaverseadam")
  tbl <- with_gtsummary_theme(
    x = theme_gtsummary_roche(),
    pharmaverseadam::adsl |>
      tbl_strata(
        strata = ETHNIC,
        ~ .x |>
          tbl_roche_summary(
            by = TRT01A,
            include = SEX
          )
      ) |>
      gtsummary::as_flex_table()
  )

  # check no bold syntax in header
  expect_true(all(!grepl(tbl$header$dataset[1, -1], pattern = "\\*")))

  # Column labels are framed with an outer border only (width 0.5), so the
  # outer edges carry the border while inner edges between header rows are 0.
  n_hdr <- nrow(tbl$header$dataset)
  bottom <- tbl$header$styles$cells$border.width.bottom$data
  top <- tbl$header$styles$cells$border.width.top$data
  expect_true(all(top[1, ] == 0.5)) # top of the block
  expect_true(all(bottom[n_hdr, ] == 0.5)) # bottom of the block
  if (n_hdr > 1) {
    expect_true(all(bottom[-n_hdr, ] == 0)) # no internal horizontal borders
    # The internal border must also be style "none": a width-0 solid border is
    # still written as a visible single line in docx (regression with spanners).
    style_bottom <- tbl$header$styles$cells$border.style.bottom$data
    expect_true(all(style_bottom[-n_hdr, ] == "none"))
  }
})

test_that("theme draws no internal horizontal line between spanner and column labels in docx", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  # xml2 is an indirect dependency of officer/flextable, so it is always
  # available whenever this test runs; guard anyway for clarity.
  skip_if_not_installed("xml2")

  tbl <- with_gtsummary_theme(
    x = theme_gtsummary_roche(),
    {
      t1 <- gtsummary::trial |> gtsummary::tbl_summary(by = trt, include = age)
      t2 <- gtsummary::trial |> gtsummary::tbl_summary(by = trt, include = grade)
      gtsummary::tbl_merge(
        list(t1, t2),
        tab_spanner = c("**Group A**", "**Group B**"),
        quiet = TRUE
      ) |>
        gtsummary::as_flex_table()
    }
  )

  f <- withr::local_tempfile(fileext = ".docx")
  flextable::save_as_docx(tbl, path = f)
  d <- withr::local_tempdir()
  utils::unzip(f, exdir = d)

  doc <- xml2::read_xml(file.path(d, "word", "document.xml"))
  ns <- xml2::xml_ns(doc)
  rows <- xml2::xml_find_all(doc, ".//w:tr", ns)

  # Locate the spanner row and the column-label row by their text content.
  row_text <- xml2::xml_text(rows)
  spanner <- rows[[which(grepl("Group A", row_text))[1]]]
  labels <- rows[[which(grepl("Characteristic", row_text))[1]]]

  # Border style of the first cell on a given side, via XPath.
  border_val <- function(row, side) {
    node <- xml2::xml_find_first(
      row,
      sprintf(".//w:tc[1]//w:tcBorders/w:%s", side),
      ns
    )
    if (inherits(node, "xml_missing")) NA_character_ else xml2::xml_attr(node, "val")
  }

  # No line between the spanner row and the column-label row.
  expect_identical(border_val(spanner, "bottom"), "none")
  expect_identical(border_val(labels, "top"), "none")
  # Outer frame of the header block is kept.
  expect_identical(border_val(spanner, "top"), "single")
  expect_identical(border_val(labels, "bottom"), "single")
})

test_that("theme pre-conversion protects stat columns with non-breaking spaces", {
  tbl <- with_gtsummary_theme(
    x = theme_gtsummary_roche(),
    gtsummary::trial |>
      gtsummary::tbl_summary(by = trt, include = age)
  )

  # Extract the pre_conversion function from the theme
  theme <- theme_gtsummary_roche(set_theme = FALSE)
  pre_conv_fn <- theme[["pkgwide-fun:pre_conversion"]]

  # Apply the pre-conversion processing manually to the table
  tbl_processed <- pre_conv_fn(tbl)

  # Get a value from a stat column
  stat_val <- tbl_processed$table_body$stat_1[1]

  # Check for the non-breaking space (decimal 160)
  char_codes <- utf8ToInt(stat_val)

  # Verify standard spaces are gone and NBSPs are present
  expect_true(160 %in% char_codes)
  expect_false(32 %in% char_codes)
})
