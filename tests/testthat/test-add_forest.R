skip_on_cran()
tbl <-
  trial |>
  dplyr::select(age, marker, grade, response) |>
  gtsummary::tbl_uvregression(
    y = response,
    method = glm,
    method.args = list(family = binomial),
    exponentiate = TRUE,
    hide_n = TRUE
  ) |>
  gtsummary::modify_column_merge(
    pattern = "{estimate} (95% CI {ci}; {p.value})",
    rows = !is.na(estimate)
  ) |>
  gtsummary::modify_header(estimate = "**Odds Ratio**") |>
  gtsummary::bold_labels()

tbl_sub <- trial |>
  tbl_roche_subgroups(
    subgroups = c("grade", "stage"),
    rsp = "response",
    by = "trt",
    ~ glm(response ~ trt, data = .x) |>
      gtsummary::tbl_regression(show_single_row = trt, exponentiate = TRUE)
  )
forest_l8 <- add_forest(tbl_sub, pagesize = "L8")
# the portrait page is too narrow for 2.5in: warns that the text columns wrap
forest_p10 <- suppressWarnings(add_forest(tbl_sub, pagesize = "P10"))

# citril::decorate_tlg() as plain flextable calls (crane does not depend on citril)
decorate <- function(x, pagesize = "L8", footnotes = "A footnote.") {
  size <- as.numeric(substring(pagesize, 2))
  out <- x |>
    flextable::add_header_lines("Subtitle") |>
    flextable::add_header_lines("Title") |>
    flextable::fontsize(size = size, part = "body") |>
    flextable::fontsize(size = size, part = "header") |>
    flextable::add_footer_lines(footnotes) |>
    flextable::padding(padding = 0, part = "footer") |>
    flextable::fontsize(size = size - 1, part = "footer") |>
    flextable::set_table_properties(
      layout = x$properties$layout, width = x$properties$width, opts_word = list(repeat_headers = TRUE)
    ) |>
    flextable::paginate(init = FALSE, hdr_ftr = TRUE)
  attr(out, "pagesize") <- pagesize
  out
}

# the pictures of the forest column: part, row, width, height and file
forest_pictures <- function(x) {
  j <- which(x$col_keys == "ggplot")
  rows <- lapply(c("header", "body"), function(part) {
    cells <- x[[part]]$content$data[, j]
    lapply(seq_along(cells), function(i) {
      d <- cells[[i]]
      img <- !vapply(d$img_data, is.null, logical(1))
      if (any(img)) {
        data.frame(part = part, row = i, width = d$width[img], height = d$height[img], path = unlist(d$img_data[img]))
      }
    })
  })
  do.call(rbind, unlist(rows, recursive = FALSE))
}

# word/document.xml of a docx holding `x` (a template with the "header" paragraph style, as citril's)
docx_xml <- function(x) {
  skip_if_not_installed("officer")
  skip_if_not_installed("xml2")
  f <- withr::local_tempfile(fileext = ".docx")
  officer::read_docx() |>
    officer::docx_set_paragraph_style(style_id = "Header", style_name = "header") |>
    flextable::body_add_flextable(x) |>
    print(target = f)
  dir <- withr::local_tempfile()
  utils::unzip(f, exdir = dir)
  xml2::read_xml(file.path(dir, "word", "document.xml"))
}

# how far each docx picture sticks out of its cell (in); cell widths come from w:gridCol
docx_overflow <- function(x) {
  doc <- docx_xml(x)
  ns <- xml2::xml_ns(doc)
  span <- function(tc) {
    gs <- xml2::xml_find_first(tc, "w:tcPr/w:gridSpan", ns)
    if (inherits(gs, "xml_missing")) 1L else as.integer(xml2::xml_attr(gs, "val"))
  }
  vapply(xml2::xml_find_all(doc, "//w:drawing", ns), function(dr) {
    cx <- as.numeric(xml2::xml_attr(xml2::xml_find_first(dr, ".//wp:extent", ns), "cx")) / 914400
    tc <- xml2::xml_find_first(dr, "ancestor::w:tc[1]", ns)
    grid <- xml2::xml_find_all(xml2::xml_find_first(tc, "ancestor::w:tbl[1]", ns), "w:tblGrid/w:gridCol", ns)
    grid <- as.numeric(xml2::xml_attr(grid, "w")) / 1440
    start <- 1L + sum(vapply(xml2::xml_find_all(tc, "preceding-sibling::w:tc", ns), span, integer(1)))
    cx - sum(grid[start:(start + span(tc) - 1L)])
  }, numeric(1))
}

test_that("add_forest(table_engine = 'flextable') works", {
  expect_warning(
    add_forest(tbl, table_engine = "flextable"),
    "Less than 2 spanning headers detected."
  )

  # the "gt" engine was removed in 0.4.0.9000, crane renders with flextable only (#271)
  expect_error(
    add_forest(tbl, table_engine = "gt"),
    class = "lifecycle_error_deprecated"
  )
  expect_error(
    add_forest(tbl, table_engine = "not_an_engine"),
    "flextable"
  )

  expect_error(
    forest_ft <- trial |>
      tbl_roche_subgroups(
        subgroups = c("grade", "stage"),
        rsp = "response",
        by = "trt",
        ~ glm(response ~ trt, data = .x) |>
          gtsummary::tbl_regression(
            show_single_row = trt,
            exponentiate = TRUE
          )
      ) |>
      add_forest(
        pvalue = starts_with("p.value"),
        table_engine = "flextable"
      ),
    NA
  )

  # default 5pt L/R padding would push the fixed-width image out of its cell (#270)
  gg <- which(forest_ft$col_keys == "ggplot")
  expect_identical(unique(forest_ft$body$styles$pars$padding.left$data[, gg]), 0)
  expect_identical(unique(forest_ft$body$styles$pars$padding.right$data[, gg]), 0)

  # shrunk font drops the paragraph mark's descent so consecutive plots abut
  expect_identical(unique(forest_ft$body$styles$text$font.size$data[, gg]), 1)
  expect_false(any(forest_ft$body$styles$text$font.size$data[, -gg] == 1))
})

test_that("add_forest handles extreme limits and character NA p-values safely", {
  # 1. SETUP: Create a basic model and table
  df_dummy <- data.frame(
    status = c(1, 0, 1, 0, 1, 1, 0, 0),
    age = c(50, 45, 60, 30, 40, 55, 35, 25)
  )

  tbl_base <- glm(status ~ age, data = df_dummy, family = binomial) |>
    tbl_regression(exponentiate = TRUE)

  # 2. CORRUPT THE DATA: Force the edge cases
  tbl_edge_cases <- tbl_base

  # Inject the bad data directly into the dataframe to bypass gtsummary's formatters
  tbl_edge_cases$table_body <- tbl_edge_cases$table_body |>
    dplyr::mutate(
      # Issue 1 Trigger: Extreme estimates > 1.0
      estimate = 564637495,
      conf.low = 0,
      conf.high = Inf,

      # Issue 2 Trigger: Overwrite the numeric p.value with the literal string "NA"
      p.value = NA
    )

  # gg_chunk() renders eagerly, so latent geom_vline warnings surface here

  # 3. TEST: Run add_forest
  # We expect absolutely no errors (Issue 2 fixed) and no warnings (Issue 1 fixed)
  # Ensure it works for flextable; gt was removed
  expect_no_error(
    expect_warning(
      out_flex <- tbl_edge_cases |> add_forest(table_engine = "flextable"),
      "Less than 2 spanning headers detected."
    )
  )

  expect_s3_class(out_flex, "flextable")
  expect_s3_class(out_flex, "tbl_forest")
  expect_identical(class(out_flex), c("tbl_forest", "flextable"))
})

test_that("add_forest() warns when {magick} is missing", {
  # rlang otherwise emits the once-per-session warning only on the first call
  withr::local_options(rlib_warning_verbosity = "verbose")

  # flextable needs magick to draw gg_chunk() images in PDF/PNG/SVG exports
  expect_snapshot(.warn_if_no_magick(installed = FALSE))
  expect_no_warning(.warn_if_no_magick(installed = TRUE))
})

test_that("add_forest(row_height, pagesize, total_width) draws compact rows that fit the page (#270)", {
  forest_ft <- forest_l8
  gg <- which(forest_ft$col_keys == "ggplot")
  n <- flextable::nrow_part(forest_ft, "body")

  # the table fills the L8 text width; the forest column keeps 2.5in
  expect_equal(sum(forest_ft$body$colwidths), 11.69 - (3.30 + 3.35) / 2.54)
  expect_equal(forest_ft$body$colwidths[[gg]], 2.5)
  expect_identical(forest_ft$properties$layout, "fixed")

  # exact rows; one merged forest cell whose picture is as tall as all rows
  expect_identical(unique(forest_ft$body$hrule), "exact")
  expect_equal(forest_ft$body$spans$columns[, gg], c(n, rep(0, n - 1)))
  expect_equal(forest_ft$body$content$data[[1, gg]]$height, sum(forest_ft$body$rowheights))

  # picture paragraphs take single spacing from the "header" style, not a bare w:line
  expect_identical(unique(forest_ft$body$styles$pars$word_style$data[, gg]), "header")
  expect_true(all(is.na(forest_ft$body$styles$pars$line_spacing$data[, gg])))

  # column labels end on the same line
  label_row <- flextable::nrow_part(forest_ft, "header")
  expect_identical(unique(forest_ft$header$styles$cells$vertical.align$data[label_row, ]), "bottom")

  # total_width instead of the page's text width; a page that is too narrow warns
  expect_warning(narrow <- add_forest(tbl_sub, total_width = 3), "needs")
  expect_equal(sum(narrow$body$colwidths), 3)
  # a non-positive row height errors
  expect_error(add_forest(tbl_sub, row_height = 0), "row_height")
})

test_that(".forest_page() maps page sizes to text width, height and font size (#270)", {
  expect_equal(.forest_page("P8")$width, 8.27 - (3.66 + 2.11) / 2.54)
  expect_equal(.forest_page("P8")$height, 11.69 - (3.55 + 3.30) / 2.54)
  expect_equal(.forest_page("L6")$height, 8.27 - (3.66 + 2.11) / 2.54)
  expect_equal(.forest_page("L6")$font_size, 6)
  expect_null(.forest_page(NULL))
  expect_null(.forest_page("A4"))

  # the page the table goes on: citril's, else the one of add_forest(), else A4 landscape
  page_height <- function(...) .forest_page_height(structure(list(), ...))
  expect_equal(page_height(pagesize = "P10", forest = list(pagesize = "L8")), 8.993, tolerance = 1e-3)
  expect_equal(page_height(forest = list(pagesize = "P8")), 8.993, tolerance = 1e-3)
  expect_equal(page_height(forest = list()), 5.998, tolerance = 1e-3)
  # its page size, for the width too: an unknown one of citril's counts as none
  pagesize <- function(...) .forest_pagesize(structure(list(), ...))
  expect_identical(pagesize(pagesize = "P10", forest = list(pagesize = "L8")), "P10")
  expect_identical(pagesize(pagesize = "A3", forest = list(pagesize = "L8")), "L8")
  expect_null(pagesize(forest = list()))
})

test_that("add_forest(pagesize, total_width) checks its arguments (#270)", {
  # citril page size codes, upper case only
  for (ok in c(list(NULL), as.list(.forest_pagesizes))) {
    expect_no_error(.check_pagesize(ok))
  }
  for (bad in list("l8", "L5", "L11", "A4", "", c("L8", "P8"), 8, NA, TRUE)) {
    expect_error(.check_pagesize(bad), "pagesize")
  }
  expect_error(.check_pagesize("l8"), "Did you mean \"L8\"")

  # add_forest() checks them before building the plots
  expect_error(add_forest(tbl, pagesize = "l8"), "pagesize")
  expect_error(add_forest(tbl, total_width = c(5, 6)), "total_width")
  expect_error(add_forest(tbl, total_width = -3), "total_width")
})

test_that("add_forest(pagesize) sets the page's font size (#270)", {
  gg <- which(forest_p10$col_keys == "ggplot")
  expect_identical(unique(as.vector(forest_p10$body$styles$text$font.size$data[, -gg])), 10)
  expect_identical(unique(as.vector(forest_p10$header$styles$text$font.size$data[, -gg])), 10)
  # the header labels are drawn at that size
  expect_identical(attr(forest_p10, "forest")$drawn$header$font_size, 10)
})

test_that("attr(x, 'forest') keeps what the plot is drawn from as plain data (#270)", {
  forest <- attr(forest_l8, "forest")

  # what citril reads to size the forest column
  expect_identical(forest$version, 1L)
  expect_identical(forest$col_key, "ggplot")
  expect_identical(forest$width, c(min = 1.5, natural = 2.5))

  # one plot per row of the gtsummary table, plus the axis row
  expect_identical(nrow(forest$data), nrow(tbl_sub$table_body))
  expect_identical(flextable::nrow_part(forest_l8, "body"), nrow(forest$data) + 1L)
  expect_identical(forest$pagesize, "L8")
  expect_identical(forest$header, list(left = "Drug A", right = "Drug B"))

  # no gtsummary or ggplot objects, so it stays small and survives saveRDS()
  classes <- function(obj) c(class(obj), if (is.list(obj)) unlist(lapply(obj, classes)))
  expect_false(any(c("gtsummary", "gg", "ggplot", "grob") %in% classes(forest)))
  f <- withr::local_tempfile(fileext = ".rds")
  saveRDS(forest, f)
  expect_identical(readRDS(f), forest)

  # the body rows the plots are drawn for, as plain dataset values (a hash would change across rlang versions)
  expect_identical(names(forest$rows), setdiff(names(forest_l8$body$dataset), "ggplot"))
  expect_identical(forest$rows$label_1[1:4], c("All Participants", "Grade", "I", "II"))
  expect_identical(forest$rows$estimate_3[1:3], c("1.04", NA, "1.19"))
})

test_that(".forest_plan() lays out the forest column for the table as it is (#270)", {
  x <- forest_l8
  forest <- attr(x, "forest")
  gg <- which(x$col_keys == "ggplot")
  plan <- .forest_plan(x, forest, page_height = 6)

  # what add_forest() drew; the same plan for the same table
  expect_identical(.forest_drawing(plan, forest), forest$drawn)
  expect_identical(.forest_plan(x, forest, page_height = 6), plan)
  expect_identical(plan$width, x$body$colwidths[[gg]])
  expect_identical(plan$heights, x$body$rowheights)
  expect_identical(plan$header_row, flextable::nrow_part(x, "header"))

  # body rows: row_height, the last (axis) row what the axis needs
  n <- length(plan$heights)
  expect_identical(unique(plan$heights[-n]), round(0.24 * 1440) / 1440)
  expect_gt(plan$heights[n], 0.24)

  # header labels at the column labels' font size, each with "Better" underneath
  expect_identical(plan$header$font_size, 8)
  expect_identical(plan$header$left, c("Drug A", "Better"))
  expect_equal(plan$header$height, 2 * 8 * 1.15 / 72)

  # the whole table counts: header rows plus body rows (no footer here)
  picture <- c(plan$header_row, gg, plan$header$height)
  header_in <- .forest_measuring(.forest_part_height(x, "header", picture))
  expect_equal(plan$table_height, header_in + sum(plan$heights))
  expect_true(plan$merged)
  expect_false(.forest_plan(x, forest, page_height = 2)$merged)
  # with 0.3in to spare, for the empty paragraph above the table in a Word document (0.26in in citril's)
  expect_true(.forest_plan(x, forest, page_height = plan$table_height + 0.31)$merged)
  expect_false(.forest_plan(x, forest, page_height = plan$table_height + 0.29)$merged)

  # more footnote rows, a taller table
  footnotes <- flextable::add_footer_lines(x, rep("A footnote.", 10))
  line_in <- 10 * footnotes$footer$styles$text$font.size$data[1, 1] * 1.15 / 72
  expect_gt(.forest_plan(footnotes, forest, page_height = 6)$table_height, plan$table_height + line_in)

  # a wider forest column: the same rows, a wider picture
  wider <- .forest_plan(flextable::width(x, j = "ggplot", width = 3), forest, page_height = 6)
  expect_identical(wider$width, 3)
  expect_identical(wider$heights, plan$heights)
})

test_that(".forest_part_height() estimates rows as Word lays them out (#270)", {
  x <- flextable::flextable(data.frame(a = "one two three")) |>
    flextable::fontsize(size = 10, part = "all") |>
    flextable::padding(padding = 0, part = "all") |>
    flextable::width(width = 0.45)
  line_in <- 10 * 1.15 / 72
  part_height <- function(...) .forest_measuring(.forest_part_height(...))

  # as tall as the text wrapped at the cell width: one word per line, and a word wider than the cell takes two
  expect_equal(part_height(x, "body"), 3 * line_in)
  expect_equal(part_height(flextable::width(x, width = 0.3), "body"), 4 * line_in)
  expect_equal(part_height(x, "header"), line_in)
  # padding counts; "exact" rows keep their height; "atleast" ones are no lower than it
  expect_equal(part_height(flextable::padding(x, padding.top = 6, part = "body"), "body"), 3 * line_in + 6 / 72)
  exact <- x |>
    flextable::height(height = 0.2, part = "body") |>
    flextable::hrule(rule = "exact", part = "body")
  expect_equal(part_height(exact, "body"), 0.2)
  atleast <- flextable::hrule(exact, rule = "atleast", part = "body")
  expect_equal(part_height(atleast, "body"), 3 * line_in)
  expect_equal(part_height(flextable::height(atleast, height = 1, part = "body"), "body"), 1)
  # a picture instead of the text
  expect_equal(part_height(x, "header", picture = c(1, 1, 1)), 1)
  expect_identical(part_height(x, "footer"), 0)

  # bold text is measured bold: a column as wide as the plain text wraps it when bold
  y <- flextable::flextable(data.frame(a = "estimate")) |>
    flextable::fontsize(size = 10, part = "all") |>
    flextable::padding(padding = 0, part = "all")
  family <- y$body$styles$text$font.family$data[[1, 1]]
  width <- function(bold) .forest_measuring(.forest_text_width("estimate", 10, bold, family))
  y <- flextable::width(y, width = width(FALSE) + 0.04)
  expect_equal(part_height(y, "body"), line_in)
  expect_equal(part_height(flextable::bold(y, part = "body"), "body"), 2 * line_in)
  # and add_forest() gives a column the width of its bold header words
  header <- flextable::set_header_labels(y, a = "estimate") |>
    flextable::compose(i = 1, j = 1, value = flextable::as_paragraph("x"), part = "body")
  min_width <- function(ft) .forest_measuring(.forest_min_widths(ft, "a", slack_in = 0))[["a"]]
  expect_equal(min_width(flextable::bold(header, bold = FALSE, part = "header")), width(FALSE))
  expect_equal(min_width(flextable::bold(header, part = "header")), width(TRUE))
})

test_that("a word wider than its cell breaks where it overflows, as in Word and browsers (#270)", {
  # every character 0.1in wide, the space too
  measure <- function(x) 0.1 * nchar(x)
  # between words: "one two" fits 0.75in
  expect_identical(.forest_line_count("one two three", 0.75, measure), 2L)
  # "0.91," and "1.19" in 0.25in lines: "0." "91" "," and "1." "19", not one line per 0.25in of each word
  expect_identical(.forest_line_count("0.91, 1.19", 0.25, measure), 5L)
  # the text goes on after the last part of a broken word: "abc" and "d e"
  expect_identical(.forest_line_count("abcd e", 0.35, measure), 2L)
  # each line of the text, an empty one too
  expect_identical(.forest_line_count("abcd e\n\nf", 0.35, measure), 4L)
})

test_that("text is measured in the wider of Arial and its cell's font as this system has it (#270)", {
  skip_if_not_installed("systemfonts")
  text <- "182\u00a0(22.5\u00a0%) of all"
  arial <- .forest_measuring(.forest_text_width(text, 10))
  x <- flextable::flextable(data.frame(a = text)) |>
    flextable::fontsize(size = 10, part = "all") |>
    flextable::padding(padding = 0, part = "all") |>
    flextable::width(width = 1.1 * arial + 0.03)
  part_height <- function(ft) .forest_measuring(.forest_part_height(ft, "body"))
  # the cell's font, as wide as Arial times `factor`
  font <- function(factor) {
    function(x, size_pt, family, bold = FALSE) {
      factor * graphics::strwidth(x, units = "inches", family = "sans", cex = size_pt / 12)
    }
  }

  # narrower than Arial: as wide as in Arial, which Word draws it in
  local_mocked_bindings(.forest_font_width = font(0.9))
  expect_equal(part_height(x), 10 * 1.15 / 72)
  # wider (e.g. a missing Arial's fallback): the row and the column follow the wider font
  local_mocked_bindings(.forest_font_width = font(1.2))
  expect_equal(part_height(x), 2 * 10 * 1.15 / 72)
  expect_equal(.forest_measuring(.forest_min_widths(x, "a", slack_in = 0))[["a"]], 1.2 * arial)
  # its longest word: words joined by non-breaking spaces are one
  word <- .forest_measuring(.forest_min_widths(x, "a", words = TRUE, slack_in = 0))[["a"]]
  expect_equal(word, 1.2 * .forest_measuring(.forest_text_width("182\u00a0(22.5\u00a0%)", 10)))
})

test_that("adjust_forest_widths() draws the pictures again for new column widths (#270)", {
  decorated <- decorate(forest_l8)
  n_col <- length(decorated$col_keys)

  # bug 1: citril::set_column_widths() narrows the forest column after decoration (too narrow for the labels)
  expect_warning(x <- adjust_forest_widths(flextable::width(decorated, width = 1)), "do not fit the 1in")
  pics <- forest_pictures(x)
  expect_identical(pics$part, c("header", "body"))
  expect_equal(pics$width, c(1, 1))
  expect_equal(pics$height[2], sum(x$body$rowheights))
  expect_true(all(file.exists(pics$path)))
  expect_true(all(docx_overflow(x) <= 0.005))

  # bug 2: citril 0.1.23 measures the column as empty and gives it 0.40in, too narrow for the header labels
  expect_warning(
    x <- adjust_forest_widths(flextable::width(decorated, j = "ggplot", width = 0.4)),
    "do not fit the 0.4in forest column"
  )
  expect_equal(forest_pictures(x)$width, c(0.4, 0.4))
  expect_true(all(docx_overflow(x) <= 0.005))

  # or set the widths here: shares of total_width
  x <- adjust_forest_widths(decorated, widths = c(2, rep(1, n_col - 2), 3), total_width = 8)
  expect_equal(sum(x$body$colwidths), 8)
  expect_equal(x$body$colwidths[[1]], 2 * x$body$colwidths[[2]])
  expect_equal(unique(forest_pictures(x)$width), x$body$colwidths[["ggplot"]])
  # one share for equal widths; a total alone keeps the shares
  expect_warning(equal <- adjust_forest_widths(decorated, widths = 1), "do not fit")
  expect_equal(unname(equal$body$colwidths), rep(sum(decorated$body$colwidths) / n_col, n_col))
  half <- adjust_forest_widths(decorated, total_width = sum(decorated$body$colwidths) / 2)
  expect_equal(half$body$colwidths, decorated$body$colwidths / 2)
})

test_that("adjust_forest_widths() measures the table with the fonts of its decoration (#270)", {
  withr::local_options(rlib_warning_verbosity = "verbose")
  # bug 3: made for L8, decorated for P10; the table is still as wide as the landscape page
  decorated <- decorate(forest_l8, pagesize = "P10")
  expect_warning(x <- adjust_forest_widths(decorated), "wider than the 6in text width of a \"P10\" page")
  gg <- which(x$col_keys == "ggplot")
  expect_identical(attr(x, "forest")$drawn$header$font_size, 10)

  # same widths and decoration, same rows and header, whatever add_forest() was told (some fonts warn at 10pt)
  w <- forest_p10$body$colwidths
  a <- suppressWarnings(adjust_forest_widths(x, widths = w, total_width = sum(w)))
  b <- adjust_forest_widths(decorate(forest_p10, pagesize = "P10"))
  expect_identical(a$body$rowheights, b$body$rowheights)
  expect_identical(attr(a, "forest")$drawn$header, attr(b, "forest")$drawn$header)

  # decoration resets the 1pt font of the picture cells: set again
  expect_identical(unique(decorated$body$styles$text$font.size$data[, gg]), 10)
  expect_identical(unique(x$body$styles$text$font.size$data[, gg]), 1)
  expect_identical(x$header$styles$text$font.size$data[[flextable::nrow_part(x, "header"), gg]], 1)

  # nothing else of the decoration changes, not even the keep_with_next of paginate()
  keep_with_next <- function(ft) {
    lapply(c("header", "body", "footer"), function(part) ft[[part]]$styles$pars$keep_with_next$data)
  }
  expect_identical(keep_with_next(x), keep_with_next(decorated))
  for (part in c("header", "footer")) {
    expect_identical(x[[part]]$dataset, decorated[[part]]$dataset)
    expect_identical(x[[part]]$spans, decorated[[part]]$spans)
  }
  expect_identical(x$body$dataset[, -gg], decorated$body$dataset[, -gg])
})

test_that("adjust_forest_widths() keeps the class and every attribute (#270)", {
  x <- decorate(forest_l8)
  attr(x, "custom") <- "kept"
  y <- adjust_forest_widths(x, widths = c(rep(1, length(x$col_keys) - 1), 2))
  expect_identical(class(y), c("tbl_forest", "flextable"))
  keep <- setdiff(names(attributes(x)), c("names", "forest"))
  expect_identical(attributes(y)[keep], attributes(x)[keep])
})

test_that("adjust_forest_widths() adds crane's HTML class and rules to the table's own (#270)", {
  # bug 4: decorate_tlg() calls set_table_properties(), which drops the HTML options
  decorated <- decorate(forest_l8)
  expect_null(decorated$properties$opts_html$extra_class)
  x <- adjust_forest_widths(decorated)
  html <- x$properties$opts_html
  # the plain class, for CSS of the user's, and one of this table only
  expect_match(html$extra_class, "^crane-forest crane-forest-[0-9a-f]{8}$")
  id <- strsplit(html$extra_class, " ")[[1]][2]
  expect_true(x$properties$opts_word$repeat_headers)

  # fixed layout: table-layout:auto (overrides citril's `table` rule), table width pinned, text breaks in its cell,
  # pictures at their drawn width
  width <- sprintf("%.3fin", sum(x$body$colwidths))
  pictures <- paste0(
    ".", id, " thead img[style*=\";width:180px;\"],.", id, " tbody img{display:block;margin:0 auto;",
    "width:2.500in !important;height:auto !important;max-width:none !important;}"
  )
  expect_identical(
    html$extra_css,
    paste0(
      ".", id, "{table-layout:auto;width:", width, ";min-width:", width, ";}",
      ".", id, " td,.", id, " th{overflow-wrap:anywhere;}",
      pictures
    )
  )
  f <- withr::local_tempfile(fileext = ".html")
  flextable::save_as_html(x, path = f)
  page <- paste(readLines(f, warn = FALSE), collapse = "\n")
  header <- regmatches(page, regexpr("<thead.*</thead>", page))
  expect_match(header, "<img style=\"vertical-align:baseline;width:180px;", fixed = TRUE)

  # added to citril's class and CSS, idempotent; a user rule for the plain class stays
  y <- flextable::set_table_properties(
    x,
    layout = "fixed", opts_html = list(
      extra_class = "citril", extra_css = "table{table-layout:fixed;width:9in;}.crane-forest td{color:red;}"
    )
  )
  y <- adjust_forest_widths(adjust_forest_widths(y))
  expect_identical(y$properties$opts_html$extra_class, paste("citril", html$extra_class))
  expect_identical(
    y$properties$opts_html$extra_css,
    paste0("table{table-layout:fixed;width:9in;}.crane-forest td{color:red;}", html$extra_css)
  )
  # every class of several, or none
  classes <- function(extra_class) {
    ft <- flextable::set_table_properties(x, layout = "fixed", opts_html = list(extra_class = extra_class))
    adjust_forest_widths(ft)$properties$opts_html$extra_class
  }
  expect_identical(classes(c("citril-tlg", "citril-fixed")), paste("citril-tlg citril-fixed", html$extra_class))
  expect_identical(classes(character(0)), html$extra_class)

  # another width, another class: both tables in one page keep their own rules
  z <- adjust_forest_widths(x, total_width = 8)
  z_id <- strsplit(z$properties$opts_html$extra_class, " ")[[1]][2]
  expect_false(identical(z_id, id))
  flextable::save_as_html(x, z, path = f)
  page <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_match(page, paste0("<table[^>]+", id))
  expect_match(page, paste0("<table[^>]+", z_id))
  expect_match(page, paste0(".", z_id, "{table-layout:auto;width:8.000in;min-width:8.000in;}"), fixed = TRUE)

  # autofit: no pinned width (it would widen the forest column), body text on one line, spanning title and footnote
  # rows do not widen the table
  a <- adjust_forest_widths(flextable::set_table_properties(x, layout = "autofit"))
  a_id <- strsplit(a$properties$opts_html$extra_class, " ")[[1]][2]
  n_col <- length(a$col_keys)
  expect_identical(
    a$properties$opts_html$extra_css,
    paste0(
      ".", a_id, "{table-layout:auto;width:auto;min-width:0;}",
      ".", a_id, " tbody td{white-space:nowrap;}",
      ".", a_id, " thead [colspan=\"", n_col, "\"],.", a_id, " tfoot [colspan=\"", n_col, "\"]{width:1px;}",
      gsub(id, a_id, pictures, fixed = TRUE)
    )
  )
  flextable::save_as_html(a, path = f)
  page <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_match(page, paste0("<thead>.*<th colspan=\"", n_col, "\".*</thead>"))
  expect_match(page, paste0("<tfoot>.*<td colspan=\"", n_col, "\".*</tfoot>"))
})

test_that("adjust_forest_widths() draws the pictures again only when something changed (#270)", {
  # decorated for the page add_forest() was told: the same pictures
  x <- adjust_forest_widths(decorate(forest_l8))
  expect_identical(forest_pictures(x)$path, forest_pictures(forest_l8)$path)
  expect_identical(forest_pictures(adjust_forest_widths(x))$path, forest_pictures(x)$path)

  # another forest column width, header font size or page: drawn again
  narrower <- adjust_forest_widths(flextable::width(x, j = "ggplot", width = 2))
  expect_equal(forest_pictures(narrower)$width, c(2, 2))
  larger <- adjust_forest_widths(flextable::fontsize(x, size = 9, part = "header"))
  expect_identical(attr(larger, "forest")$drawn$header$font_size, 9)
  expect_false(any(forest_pictures(larger)$path %in% forest_pictures(x)$path))
  expect_identical(larger$header$styles$text$font.size$data[[4, which(x$col_keys == "ggplot")]], 1)
  lower <- adjust_forest_widths(x, page_height = 2)
  expect_false(attr(lower, "forest")$drawn$merged)

  # attr(x, "forest")$drawn records what they were drawn from, plot data included
  expect_identical(attr(x, "forest")$drawn, attr(forest_l8, "forest")$drawn)
  expect_null(attr(x, "forest")$plan)
  edited <- x
  data <- attr(edited, "forest")$data
  i <- which(!is.na(data$estimate))[2]
  attr(edited, "forest")$data$estimate[i] <- data$conf_high[i]
  expect_false(any(forest_pictures(adjust_forest_widths(edited))$path %in% forest_pictures(x)$path))
})

test_that("adjust_forest_widths() draws the pictures of a table read back in a new session (#270)", {
  # fresh pictures (another width), so the shared fixture keeps its files
  x <- adjust_forest_widths(decorate(forest_l8), total_width = 9)
  f <- withr::local_tempfile(fileext = ".rds")
  saveRDS(x, f)
  # gg_chunk() keeps only the path of a tempfile(), which a new R session does not have
  file.remove(forest_pictures(x)$path)

  y <- adjust_forest_widths(readRDS(f))
  pics <- forest_pictures(y)
  expect_true(all(file.exists(pics$path)))
  expect_identical(pics[c("part", "row", "width", "height")], forest_pictures(x)[c("part", "row", "width", "height")])
})

test_that("the forest plot is one picture only when the whole table fits the page (#270)", {
  # a merge in the label column, to keep
  merged <- flextable::merge_at(forest_l8, i = 2:3, j = 1, part = "body")
  gg <- which(merged$col_keys == "ggplot")
  n <- flextable::nrow_part(merged, "body")

  # titles and many footnote rows push it over the page: one picture per row
  long <- adjust_forest_widths(decorate(merged, footnotes = rep("A footnote line.", 40)))
  expect_false(attr(long, "forest")$drawn$merged)
  expect_equal(long$body$spans$columns[, gg], rep(1, n))
  pics <- forest_pictures(long)
  expect_equal(pics$height[pics$part == "body"], long$body$rowheights)
  expect_true(all(docx_overflow(long) <= 0.005))
  # only the forest column is unmerged: merge_none() would clear every merge of the body
  expect_equal(long$body$spans$columns[2:3, 1], c(2, 0))

  # and one picture again when it fits
  back <- adjust_forest_widths(long, page_height = 50)
  expect_equal(back$body$spans$columns[, gg], c(n, rep(0, n - 1)))
  expect_equal(back$body$spans$columns[2:3, 1], c(2, 0))
  expect_identical(sum(forest_pictures(back)$part == "body"), 1L)

  # without a page: A4 landscape (6.00in); 11 rows of 0.6in would fit a portrait page, not that one
  tall <- forest_l8
  attr(tall, "forest")[c("pagesize", "row_height")] <- list(NULL, 0.6)
  tall <- adjust_forest_widths(tall)
  expect_lt(.forest_plan(tall, attr(tall, "forest"), page_height = 6)$table_height + 0.3, .forest_page("P8")$height)
  expect_false(attr(tall, "forest")$drawn$merged)
  expect_identical(nrow(forest_pictures(tall)), flextable::nrow_part(tall, "body") + 1L)
})

test_that("add_forest() and adjust_forest_widths() give the active device back (#270)", {
  grDevices::pdf(NULL)
  first <- grDevices::dev.cur()
  grDevices::pdf(NULL)
  middle <- grDevices::dev.cur()
  grDevices::pdf(NULL)
  last <- grDevices::dev.cur()
  withr::defer(for (d in c(last, middle, first)) grDevices::dev.off(d))
  grDevices::dev.set(middle)
  devices <- grDevices::dev.list()

  # drawing closes png devices, which makes another one active
  suppressWarnings(add_forest(tbl))
  expect_identical(grDevices::dev.cur(), middle)
  adjust_forest_widths(forest_l8, total_width = 8)
  expect_identical(grDevices::dev.cur(), middle)
  expect_identical(grDevices::dev.list(), devices)

  # text is measured on one device, closed afterwards
  expect_identical(.forest_measuring(length(grDevices::dev.list())), length(devices) + 1L)
  expect_identical(grDevices::dev.list(), devices)
  expect_identical(grDevices::dev.cur(), middle)
})

test_that("adjust_forest_widths() checks its arguments (#270)", {
  expect_error(adjust_forest_widths(flextable::flextable(head(mtcars))), "add_forest")
  expect_error(adjust_forest_widths(forest_l8, widths = c(1, 2)), "one value per column")
  expect_error(adjust_forest_widths(forest_l8, widths = -1), "widths")
  expect_error(adjust_forest_widths(forest_l8, total_width = 0), "total_width")
  expect_error(adjust_forest_widths(forest_l8, page_height = "A4"), "page_height")

  # the plots cannot be matched to the rows when body rows were added or removed after add_forest()
  expect_error(adjust_forest_widths(flextable::delete_rows(forest_l8, i = 1, part = "body")), "body rows")
})

test_that("missing values, infinite and other widths give errors naming the argument (#270)", {
  n_col <- length(forest_l8$col_keys)
  # cli errors, not base R's "missing value where TRUE/FALSE needed"
  for (bad in list(NA, NA_real_, Inf, -1, 0, "8", c(8, 9), NULL)) {
    if (!is.null(bad)) {
      expect_error(adjust_forest_widths(forest_l8, total_width = bad), "total_width", class = "rlang_error")
      expect_error(adjust_forest_widths(forest_l8, page_height = bad), "page_height", class = "rlang_error")
      expect_error(add_forest(tbl_sub, total_width = bad), "total_width", class = "rlang_error")
    }
    expect_error(add_forest(tbl_sub, row_height = bad), "row_height", class = "rlang_error")
  }
  for (bad in list(c(1, NA, rep(1, n_col - 2)), c(Inf, rep(1, n_col - 1)), rep(0, n_col), NA, "1", rep(1, n_col + 1))) {
    expect_error(adjust_forest_widths(forest_l8, widths = bad), "widths", class = "rlang_error")
  }
  expect_snapshot(error = TRUE, {
    adjust_forest_widths(forest_l8, widths = c(1, NA, rep(1, n_col - 2)))
    adjust_forest_widths(forest_l8, total_width = NA_real_)
    adjust_forest_widths(forest_l8, page_height = Inf)
    add_forest(tbl_sub, row_height = NA)
  })

  # a table without its forest column, or with a body row merged across it
  per_row <- adjust_forest_widths(forest_l8, page_height = 1)
  expect_snapshot(error = TRUE, {
    adjust_forest_widths(flextable::delete_columns(forest_l8, j = "ggplot"))
    adjust_forest_widths(flextable::merge_at(per_row, i = 2, j = (n_col - 1):n_col, part = "body"))
  })
})

test_that("adjust_forest_widths(widths, total_width) makes the layout fixed, as citril::set_column_widths() (#270)", {
  n_col <- length(forest_l8$col_keys)
  auto <- flextable::set_table_properties(
    forest_l8,
    layout = "autofit", align = "left", word_title = "Forest plot", word_description = "Subgroups",
    opts_word = list(repeat_headers = FALSE), opts_pdf = list(tabcolsep = 4)
  )
  # no widths: the layout is kept
  expect_identical(adjust_forest_widths(auto)$properties$layout, "autofit")

  x <- adjust_forest_widths(auto, widths = c(3, rep(1, n_col - 2), 3), total_width = 9)
  expect_identical(x$properties$layout, "fixed")
  expect_identical(adjust_forest_widths(auto, total_width = 8)$properties$layout, "fixed")
  # every other table property is passed back as it is
  keep <- c("width", "align", "opts_word", "opts_pdf", "word_title", "word_description")
  expect_identical(x$properties[keep], auto$properties[keep])

  # so Word keeps the widths the pictures are drawn for
  doc <- docx_xml(x)
  ns <- xml2::xml_ns(doc)
  expect_identical(xml2::xml_attr(xml2::xml_find_first(doc, "//w:tblLayout", ns), "type"), "fixed")
  grid <- as.numeric(xml2::xml_attr(xml2::xml_find_all(doc, "//w:tblGrid/w:gridCol", ns), "w")) / 1440
  expect_equal(grid, unname(x$body$colwidths), tolerance = 1e-3)
  expect_true(all(docx_overflow(x) <= 0.005))
})

test_that("adjust_forest_widths() stops when the body rows are not the ones drawn (#270)", {
  x <- forest_l8
  # plain text columns, as flextable::add_body_row() needs them (the label column is a glue string): same keys
  for (k in names(x$body$dataset)) {
    if (inherits(x$body$dataset[[k]], "glue")) x$body$dataset[[k]] <- as.character(x$body$dataset[[k]])
  }
  expect_no_error(adjust_forest_widths(x))
  values <- as.list(stats::setNames(c("Added row", rep("", length(x$col_keys) - 1)), x$col_keys))
  add_on_top <- function(ft) {
    flextable::add_body_row(ft, top = TRUE, values = values, colwidths = rep(1, length(ft$col_keys)))
  }

  # a row deleted and one added on top: as many rows as drawn, but each CI above the deleted row one row off
  edited <- add_on_top(flextable::delete_rows(x, i = 3, part = "body"))
  expect_identical(flextable::nrow_part(edited, "body"), flextable::nrow_part(x, "body"))
  expect_snapshot(adjust_forest_widths(edited), error = TRUE)
  # a deleted row (the first changed row is the one after it), rows moved, a row added at the bottom
  expect_snapshot(adjust_forest_widths(flextable::delete_rows(x, i = 3, part = "body")), error = TRUE)
  expect_error(
    adjust_forest_widths(flextable::delete_rows(x, i = flextable::nrow_part(x, "body"), part = "body")),
    "has 10 body rows, the plot was drawn for 11"
  )
  # the last data row deleted: the axis row, without a label, is the first that differs
  last <- flextable::delete_rows(x, i = flextable::nrow_part(x, "body") - 1, part = "body")
  expect_error(adjust_forest_widths(last), "Body row 10 is not the row the plot was drawn for.", fixed = TRUE)
  moved <- x
  moved$body$dataset <- moved$body$dataset[c(1, 3, 2, 4:nrow(x$body$dataset)), ]
  expect_error(adjust_forest_widths(moved), "Body row 2")

  # title and footnote rows, other text and deleted columns are fine: the rows are the same
  same <- x |>
    flextable::add_header_lines("A title") |>
    flextable::add_footer_lines("A footnote.") |>
    flextable::compose(i = 1, j = 1, value = flextable::as_paragraph("Everyone")) |>
    flextable::delete_columns(j = 2)
  expect_no_error(y <- adjust_forest_widths(same))
  expect_equal(forest_pictures(y)$width, rep(y$body$colwidths[["ggplot"]], 2))
})

test_that("a text label put in place of the header picture is kept, and readable (#270)", {
  gg <- which(forest_l8$col_keys == "ggplot")
  label_row <- flextable::nrow_part(forest_l8, "header")
  x <- flextable::set_header_labels(forest_l8, ggplot = "Odds ratio (95% CI)")
  # the label replaced the picture in a cell at crane's 1pt
  expect_identical(x$header$styles$text$font.size$data[[label_row, gg]], 1)

  y <- adjust_forest_widths(x, total_width = 8)
  expect_identical(y$header$content$data[[label_row, gg]]$txt, "Odds ratio (95% CI)")
  expect_identical(unique(forest_pictures(y)$part), "body")
  expect_null(attr(y, "forest")$drawn$header)
  # at the font size of the other column labels of its row
  expect_identical(
    y$header$styles$text$font.size$data[[label_row, gg]], y$header$styles$text$font.size$data[[label_row, 1]]
  )
  expect_identical(adjust_forest_widths(y), y)

  # also after decoration, with title rows on top; the pictures fit in Word
  d <- adjust_forest_widths(decorate(x, pagesize = "P10"), widths = 1, total_width = 6)
  row <- flextable::nrow_part(d, "header")
  expect_identical(d$header$content$data[[row, gg]]$txt, "Odds ratio (95% CI)")
  expect_identical(d$header$styles$text$font.size$data[[row, gg]], 10)
  expect_true(all(docx_overflow(d) <= 0.005))
  # with single line spacing instead of the picture's none, so flextable can draw the table as an image
  expect_identical(d$header$styles$pars$line_spacing$data[[row, gg]], 1)
  skip_if_not_installed("magick")
  expect_no_error(flextable::save_as_image(d, path = withr::local_tempfile(fileext = ".png")))
})

test_that("the header picture is drawn again where it is: titles above it, header rows below it (#270)", {
  n_col <- length(forest_l8$col_keys)
  gg <- which(forest_l8$col_keys == "ggplot")
  # a header row added below the column labels keeps its text and gets no picture
  x <- flextable::add_header_row(
    forest_l8,
    top = FALSE, values = c("(units)", rep("", n_col - 1)), colwidths = rep(1, n_col)
  )
  y <- adjust_forest_widths(decorate(x), total_width = 8)
  n_header <- flextable::nrow_part(y, "header")
  pics <- forest_pictures(y)
  header <- pics[pics$part == "header", ]
  expect_identical(header$row, n_header - 1L)
  expect_equal(header$width, y$body$colwidths[["ggplot"]])
  expect_identical(y$header$content$data[[n_header, 1]]$txt, "(units)")
  expect_identical(y$header$styles$text$font.size$data[[n_header - 1L, gg]], 1)
  expect_true(all(docx_overflow(y) <= 0.005))
  # the added row copied the picture cell's style: it gets single line spacing (flextable draws it as an image)
  expect_identical(y$header$styles$pars$line_spacing$data[[n_header, gg]], 1)

  # a picture whose file is gone (a table read back in a new session) is still a picture: drawn again in place
  file.remove(pics$path)
  z <- adjust_forest_widths(y)
  expect_identical(forest_pictures(z)[c("part", "row", "width", "height")], pics[c("part", "row", "width", "height")])
  expect_true(all(file.exists(forest_pictures(z)$path)))
})

test_that("the header labels warn when they do not fit their column, when drawn (#270)", {
  decorated <- decorate(forest_l8)
  # 1.3in still fits "Drug A Better" and "Drug B Better": no warning, even where warnings are errors
  withr::with_options(
    list(warn = 2),
    x <- adjust_forest_widths(flextable::width(decorated, j = "ggplot", width = 1.3))
  )
  expect_true(attr(x, "forest")$drawn$header$fits)

  # too narrow: cut off, with a warning giving the width they need (it depends on the fonts of the system)
  expect_snapshot(
    y <- adjust_forest_widths(flextable::width(decorated, j = "ggplot", width = 0.6)),
    transform = function(lines) gsub("at least [0-9.]+in", "at least <width>in", lines)
  )
  expect_false(attr(y, "forest")$drawn$header$fits)
  # not again when nothing is drawn
  expect_no_warning(adjust_forest_widths(y))

  # longer labels need more room
  long <- forest_l8
  attr(long, "forest")$header <- list(left = "Xanomeline High Dose", right = "Xanomeline Low Dose")
  expect_warning(adjust_forest_widths(long, widths = 1), "do not fit")
  expect_no_warning(adjust_forest_widths(long, widths = c(rep(1, length(long$col_keys) - 1), 3)))
})

test_that("the header labels fit when they stay on the picture, either side of the line at 1 (#270)", {
  forest <- attr(forest_l8, "forest")
  fits <- function(labels, width, limits = forest$limits, pt = 8) .forest_label_layout(labels, limits, width, pt)$fits

  # the width they need (to 0.01in): they fit in it, and not in a narrower one
  need <- .forest_header_need(forest$header, forest$limits, 8)
  expect_true(fits(forest$header, need))
  expect_false(fits(forest$header, need - 0.02))
  expect_true(fits(forest$header, 2.5))

  # a CI up to 150 moves the line at 1 left: at 2.5in the left label would cross it
  wide_ci <- c(0.2, 150)
  labels <- list(left = "Xanomeline High Dose", right = "Placebo")
  layout <- .forest_label_layout(labels, wide_ci, 2.5, 10)
  expect_false(layout$fits)
  # it crosses the line at 1, but stays on the picture
  expect_true(layout$crosses)
  expect_false(layout$cut_off)
  need <- .forest_header_need(labels, wide_ci, 10)
  expect_gt(need, 2.5)
  expect_true(fits(labels, need, wide_ci, 10))

  # label blocks may use the picture's outer margins: labels slightly wider than their side still fit on one line
  drugs <- list(left = "Drug A", right = "Drug B")
  label_in <- max(.forest_label_width(c("Drug A", "Drug B"), 10))
  xr <- log10(forest$limits) + c(-0.05, 0.05) * diff(log10(forest$limits))
  side <- min(-xr[1] / diff(xr), 1 + xr[1] / diff(xr)) # the narrower side, a share of the panel
  width <- (label_in - 0.02 + 0.05) / side + 10 / 72 # the labels 0.02in wider than the room without the margin
  layout <- .forest_label_layout(drugs, forest$limits, width, 10)
  expect_identical(layout$left, c("Drug A", "Better"))
  expect_identical(layout$right, c("Drug B", "Better"))
  expect_true(layout$fits)
})

test_that("on a narrow page, the text columns keep their longest words and the forest column gives way (#270)", {
  # the table's font here 20% wider than Arial (as DejaVu Sans, a missing Arial's fallback, is)
  local_mocked_bindings(.forest_font_width = function(x, size_pt, family, bold = FALSE) {
    1.2 * graphics::strwidth(x, units = "inches", family = "sans", font = if (bold) 2L else 1L, cex = size_pt / 12)
  })
  x <- forest_p10
  forest <- attr(x, "forest")
  forest$header <- list(left = "Xanomeline High Dose", right = "Xanomeline Low Dose") # too long for 1.5in at 10pt
  label_row <- flextable::nrow_part(x, "header")
  keys <- setdiff(x$col_keys, "ggplot")
  longest <- function(...) {
    .forest_measuring(.forest_min_widths(x, keys, words = TRUE, slack_in = .forest_line_safety, ...))
  }
  word <- longest()
  arial <- longest(system_fonts = FALSE)
  # the widths for a page `table_width` wide, the header labels needing a forest column `label_w` wide
  fit <- function(table_width, label_w) {
    local_mocked_bindings(.forest_header_need = function(...) label_w)
    .forest_fit_widths(x, table_width, forest, label_row)$body$colwidths
  }

  # room for the longest words next to the labels' width: the forest column gets it, text wraps between words
  expect_warning(w <- fit(sum(word) + 1.95, label_w = 1.9), "their text wraps")
  expect_equal(w[["ggplot"]], 1.9)
  expect_true(all(w[keys] >= word - 1e-9))
  expect_equal(sum(w), sum(word) + 1.95)
  # labels wider than the natural width, or no room for them: the minimum width; text keeps its longest words
  for (label_w in c(2.6, 1.9)) {
    expect_warning(w <- fit(sum(word) + 1.55, label_w = label_w), "their text wraps")
    expect_equal(w[["ggplot"]], 1.5)
    expect_true(all(w[keys] >= word - 1e-9))
  }
  # room for the longest words only as Word draws them, in Arial: Word breaks none
  expect_warning(w <- fit(sum(arial) + 1.55, label_w = 1.9), "their text wraps")
  expect_true(all(w[keys] >= arial - 1e-9))
  expect_true(any(w[keys] < word))
  # not even those fit: all text columns are narrowed in proportion, and words break
  expect_warning(w <- fit(sum(arial) + 1.3, label_w = 1.9), "words too long for their column break")
  expect_equal(w[["ggplot"]], 1.5)
  expect_equal(sum(w), sum(arial) + 1.3)
  expect_true(any(w[keys] < arial))
})

test_that("an unknown attr(x, 'pagesize') falls back to the page of add_forest(), with a warning (#270)", {
  withr::local_options(rlib_warning_verbosity = "verbose")
  x <- decorate(forest_l8)
  attr(x, "pagesize") <- "A3"
  expect_snapshot(height <- .forest_page_height(x))
  expect_equal(height, .forest_page("L8")$height)
  # without a page of add_forest(): A4 landscape
  y <- decorate(add_forest(tbl_sub, total_width = 9))
  attr(y, "pagesize") <- 8
  expect_warning(expect_equal(.forest_page_height(y), 5.998, tolerance = 1e-3), "not a page size")
  # adjust_forest_widths() goes on; with page_height, the attribute is not used
  expect_warning(adjust_forest_widths(x), "A3")
  expect_no_warning(adjust_forest_widths(x, page_height = 6))

  # once per session and value
  withr::local_options(rlib_warning_verbosity = "default")
  attr(x, "pagesize") <- basename(tempfile("P"))
  expect_warning(.forest_page_height(x), attr(x, "pagesize"))
  expect_no_warning(.forest_page_height(x))
})

test_that("a total_width wider than the page is kept, with a warning (#270)", {
  expect_snapshot(x <- add_forest(tbl_sub, pagesize = "P8", total_width = 9.07))
  expect_equal(sum(x$body$colwidths), 9.07)

  # adjust_forest_widths(): for the page of attr(x, "pagesize")
  d <- decorate(forest_l8)
  expect_warning(y <- adjust_forest_widths(d, total_width = 10), "wider than the 9.07in text width")
  expect_equal(sum(y$body$colwidths), 10)
  expect_no_warning(adjust_forest_widths(d, total_width = 9.07))
  # else for the page of add_forest(pagesize); a table without a page does not warn
  expect_warning(adjust_forest_widths(forest_l8, total_width = 10), "text width of a \"L8\" page")
  no_page <- forest_l8
  attr(no_page, "forest")$pagesize <- NULL
  expect_no_warning(adjust_forest_widths(no_page, total_width = 10))

  # a fixed-layout table wider than its page warns without total_width too (once per session and width)
  withr::local_options(rlib_warning_verbosity = "verbose")
  expect_warning(adjust_forest_widths(y), "Set narrower widths")
  expect_no_warning(adjust_forest_widths(flextable::set_table_properties(y, layout = "autofit")))

  # an autofit table warns when its text needs more than the page (Word would wrap it, the exact rows cut it off)
  auto <- flextable::set_table_properties(forest_l8, layout = "autofit")
  expect_no_warning(adjust_forest_widths(decorate(auto)))
  expect_warning(
    adjust_forest_widths(decorate(auto, pagesize = "P10")),
    "needs [0-9.]+in, wider than the 6in text width of a \"P10\" page"
  )
})

test_that("the forest column keeps no padding around its pictures (#270)", {
  # padding for every cell, e.g. by the user: the pictures are as wide as the column and as tall as the rows
  x <- adjust_forest_widths(flextable::padding(decorate(forest_l8), padding = 3, part = "all"))
  gg <- which(x$col_keys == "ggplot")
  row <- flextable::nrow_part(x, "header")
  for (side in c("left", "right", "top", "bottom")) {
    expect_identical(unique(x$body$styles$pars[[paste0("padding.", side)]]$data[, gg]), 0)
  }
  expect_identical(x$header$styles$pars$padding.left$data[[row, gg]], 0)
  expect_identical(x$header$styles$pars$padding.right$data[[row, gg]], 0)
  # the other cells keep theirs
  expect_identical(unique(x$body$styles$pars$padding.left$data[, 1]), 3)
  expect_identical(x$header$styles$pars$padding.left$data[[1, gg]], 3)
})

test_that("a footnote mark next to the header picture is kept, readable, when the picture is drawn again (#270)", {
  gg <- which(forest_l8$col_keys == "ggplot")
  row <- flextable::nrow_part(forest_l8, "header")
  x <- flextable::footnote(
    forest_l8,
    i = row, j = "ggplot", part = "header", ref_symbols = "a",
    value = flextable::as_paragraph("Odds ratio, log scale.")
  )
  y <- adjust_forest_widths(decorate(x), total_width = 8)
  row <- flextable::nrow_part(y, "header")
  cell <- y$header$content$data[[row, gg]]
  expect_identical(cell$txt, c(NA, "a"))
  expect_identical(cell$vertical.align[[2]], "superscript")
  expect_equal(cell$width[[1]], y$body$colwidths[["ggplot"]])
  expect_true(file.exists(cell$img_data[[1]]))
  expect_identical(nrow(forest_pictures(y)), 2L)
  # at the column labels' font size (the cell keeps 1pt), on its own line under the picture, single-spaced
  expect_identical(cell$font.size[[2]], 8)
  expect_identical(y$header$styles$text$font.size$data[[row, gg]], 1)
  expect_identical(y$header$styles$pars$line_spacing$data[[row, gg]], 1)
  expect_identical(adjust_forest_widths(y), y)
  size <- xml2::xml_attr(xml2::xml_find_all(docx_xml(y), "//w:tc[.//w:drawing]//w:r[w:t = 'a']/w:rPr/w:sz"), "val")
  expect_identical(unique(size), "16")
  skip_if_not_installed("magick")
  expect_no_error(flextable::save_as_image(y, path = withr::local_tempfile(fileext = ".png")))
})

test_that("text in body cells of the forest column is dropped, with a warning: the plots fill them (#270)", {
  gg <- which(forest_l8$col_keys == "ggplot")
  mark <- function(ft, i) {
    flextable::footnote(
      ft,
      i = i, j = "ggplot", part = "body", ref_symbols = "b", value = flextable::as_paragraph("CI not estimable.")
    )
  }
  # one merged picture: a mark below the first row is covered by the merge, so the pictures are drawn again
  x <- mark(forest_l8, 3)
  expect_snapshot(y <- adjust_forest_widths(x))
  expect_identical(.forest_body_text(y, gg), integer(0))
  expect_identical(nrow(forest_pictures(y)), 2L)
  # one picture per row: the row would grow in HTML and its CI move off its text; its mark is cut off in Word
  per_row <- adjust_forest_widths(forest_l8, page_height = 1)
  expect_warning(z <- adjust_forest_widths(mark(per_row, 2:3), page_height = 1), "body rows 2 and 3 is dropped")
  expect_identical(z$body$content$data[[2, gg]]$txt, NA_character_)
  expect_identical(nrow(forest_pictures(z)), flextable::nrow_part(z, "body") + 1L)
  # the footnote itself is kept
  expect_identical(z$footer$dataset, mark(per_row, 2:3)$footer$dataset)
})

test_that("text and plots are measured on one device per call (#270)", {
  opened <- 0
  local_mocked_bindings(.forest_open_device = function() {
    opened <<- opened + 1
    grDevices::pdf(NULL)
    grDevices::dev.cur()
  })
  x <- add_forest(tbl_sub, pagesize = "L8")
  expect_identical(opened, 1)
  # also when the header labels do not fit, and the warning gives the width they need
  narrow <- quote(expect_warning(adjust_forest_widths(flextable::width(x, j = "ggplot", width = 0.8)), "need"))
  for (call in list(quote(adjust_forest_widths(x, total_width = 8)), quote(adjust_forest_widths(x)), narrow)) {
    opened <- 0
    eval(call)
    expect_identical(opened, 1)
  }

  # nested measuring reuses the device, also after drawing made another one active
  grDevices::pdf(NULL)
  other <- grDevices::dev.cur()
  opened <- 0
  .forest_measuring({
    .forest_text_width("Drug A", 8)
    grDevices::dev.set(other) # as gg_chunk() closing its png device does
    .forest_axis_height(.plot_centered_axis(c(0.2, 2), 1, .get_default_forest_sizes()))
    expect_false(grDevices::dev.cur() == other)
  })
  expect_identical(opened, 1)
  expect_identical(grDevices::dev.cur(), other)
  grDevices::dev.off(other)

  # a measuring helper on its own opens its own device, not Rplots.pdf or the active one
  withr::local_dir(withr::local_tempdir())
  devices <- grDevices::dev.list()
  opened <- 0
  width <- .forest_text_width("Xanomeline High Dose", 8)
  expect_false(file.exists("Rplots.pdf"))
  grDevices::png(withr::local_tempfile(fileext = ".png"), res = 300)
  png_device <- grDevices::dev.cur()
  expect_identical(.forest_text_width("Xanomeline High Dose", 8), width)
  expect_identical(grDevices::dev.cur(), png_device)
  grDevices::dev.off(png_device)
  expect_identical(grDevices::dev.list(), devices)
  expect_identical(opened, 2)
})

test_that("a no-op adjust_forest_widths() returns the table as it is, without measuring the axis (#270)", {
  x <- adjust_forest_widths(decorate(forest_l8))
  # the axis row's height depends on the plot data only: measured once, by add_forest()
  expect_gt(attr(x, "forest")$axis_height, 0.24)
  expect_identical(attr(x, "forest")$axis_height, attr(forest_l8, "forest")$axis_height)
  local_mocked_bindings(.forest_axis_height = function(...) stop("The axis was measured again."))
  expect_identical(adjust_forest_widths(x), x)
  expect_no_error(adjust_forest_widths(x, total_width = 8))
})

test_that("text in several encodings is measured in a locale other than UTF-8 (#270)", {
  withr::local_locale(c(LC_CTYPE = "C"))
  x <- tbl_sub
  # a label read from a UTF-8 file in a C locale (bytes not marked UTF-8)
  x$table_body$label_1[x$table_body$label_1 == "T1"] <- rawToChar(as.raw(c(0xe2, 0x89, 0xa5, 0x31)))
  expect_no_error(y <- adjust_forest_widths(add_forest(x, pagesize = "L8"), total_width = 8))
  expect_equal(forest_pictures(y)$width, rep(y$body$colwidths[["ggplot"]], 2))
  # measured on the device: systemfonts would read its bytes as escapes
  skip_if_not_installed("systemfonts")
  label <- rawToChar(as.raw(c(0xe2, 0x89, 0xa5, 0x31)))
  expect_true(is.na(.forest_font_width(label, 8, "Arial")))
  expect_identical(.forest_text_width(label, 8, family = "Arial"), .forest_text_width(label, 8))
  expect_false(is.na(.forest_font_width("T1", 8, "Arial")))
})
