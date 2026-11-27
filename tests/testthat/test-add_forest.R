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
    add_forest(tbl),
    "Less than 2 spanning headers detected."
  )

  # table_engine is deprecated: crane renders with flextable only (#271)
  expect_warning(
    lifecycle::expect_deprecated(add_forest(tbl, table_engine = "flextable")),
    "Less than 2 spanning headers detected."
  )
  # the "gt" engine was removed in 0.4.1, and other values are no engine either
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
      add_forest(pvalue = starts_with("p.value")),
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
      out_flex <- tbl_edge_cases |> add_forest(),
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

  # the table fills the L8 text width, in the page's font size; the forest column keeps 2.5in
  expect_equal(sum(forest_ft$body$colwidths), 11.69 - (3.30 + 3.35) / 2.54)
  expect_equal(forest_ft$body$colwidths[[gg]], 2.5)
  expect_identical(forest_ft$properties$layout, "fixed")
  expect_identical(unique(as.vector(forest_p10$body$styles$text$font.size$data[, -gg])), 10)

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
})

test_that("add_forest() and adjust_forest_widths() check their arguments (#270)", {
  expect_error(add_forest(tbl, pagesize = "l8"), "Did you mean \"L8\"")
  expect_error(add_forest(tbl_sub, total_width = NA), "total_width")
  expect_error(add_forest(tbl_sub, row_height = 0), "row_height")
  # too narrow for the forest column's minimum plus the other columns' padding (widths would be negative)
  expect_error(add_forest(tbl_sub, total_width = 1), "total_width. must be at least")
  expect_error(adjust_forest_widths(flextable::flextable(head(mtcars))), "add_forest")
  expect_snapshot(error = TRUE, {
    adjust_forest_widths(forest_l8, widths = c(1, 2))
    adjust_forest_widths(forest_l8, total_width = NA_real_)
    adjust_forest_widths(forest_l8, page_height = Inf)
  })
})

test_that("attr(x, 'forest') keeps what the plot is drawn from as plain data (#270)", {
  forest <- attr(forest_l8, "forest")

  # what citril reads to size the forest column
  expect_identical(forest$version, 1L)
  expect_identical(forest$col_key, "ggplot")
  expect_identical(forest$width, c(min = 1.5, natural = 2.5))
  expect_identical(nrow(forest$data), nrow(tbl_sub$table_body))

  # no gtsummary or ggplot objects, so it stays small and survives saveRDS()
  classes <- function(obj) c(class(obj), if (is.list(obj)) unlist(lapply(obj, classes)))
  expect_false(any(c("gtsummary", "gg", "ggplot", "grob") %in% classes(forest)))
  f <- withr::local_tempfile(fileext = ".rds")
  saveRDS(forest, f)
  expect_identical(readRDS(f), forest)
})

test_that("adjust_forest_widths() keeps the class and every attribute (#270)", {
  x <- decorate(forest_l8)
  attr(x, "custom") <- "kept"
  y <- adjust_forest_widths(x, widths = c(rep(1, length(x$col_keys) - 1), 2))
  expect_identical(class(y), c("tbl_forest", "flextable"))
  keep <- setdiff(names(attributes(x)), c("names", "forest"))
  expect_identical(attributes(y)[keep], attributes(x)[keep])
})

test_that("adjust_forest_widths() draws the pictures again for new column widths (#270)", {
  decorated <- decorate(forest_l8)
  n_col <- length(decorated$col_keys)

  # bug 1: citril::set_column_widths() narrows the forest column after decoration (too narrow for the labels)
  expect_warning(x <- adjust_forest_widths(flextable::width(decorated, width = 1)), "do not fit the 1in")
  expect_equal(forest_pictures(x)$width, c(1, 1))
  expect_true(all(docx_overflow(x) <= 0.005))

  # bug 2: citril 0.1.23 measures the column as empty and gives it 0.40in
  expect_warning(x <- adjust_forest_widths(flextable::width(decorated, j = "ggplot", width = 0.4)), "do not fit")
  expect_equal(forest_pictures(x)$width, c(0.4, 0.4))

  # or set the widths here: shares of total_width, in a fixed layout as citril::set_column_widths() gives
  x <- adjust_forest_widths(decorated, widths = c(2, rep(1, n_col - 2), 3), total_width = 8)
  expect_equal(sum(x$body$colwidths), 8)
  expect_equal(x$body$colwidths[[1]], 2 * x$body$colwidths[[2]])
  expect_equal(unique(forest_pictures(x)$width), x$body$colwidths[["ggplot"]])
  auto <- flextable::set_table_properties(decorated, layout = "autofit")
  expect_identical(adjust_forest_widths(auto, total_width = 8)$properties$layout, "fixed")
})

test_that("adjust_forest_widths() measures the table with the fonts of its decoration (#270)", {
  withr::local_options(rlib_warning_verbosity = "verbose")
  # bug 3: made for L8, decorated for P10; the table is still as wide as the landscape page
  decorated <- decorate(forest_l8, pagesize = "P10")
  expect_warning(x <- adjust_forest_widths(decorated), "wider than the 6in text width of a \"P10\" page")
  gg <- which(x$col_keys == "ggplot")
  expect_identical(attr(x, "forest")$drawn$header$font_size, 10)

  # same widths and decoration, same rows, whatever add_forest() was told
  w <- forest_p10$body$colwidths
  a <- suppressWarnings(adjust_forest_widths(x, widths = w, total_width = sum(w)))
  b <- adjust_forest_widths(decorate(forest_p10, pagesize = "P10"))
  expect_identical(a$body$rowheights, b$body$rowheights)

  # decoration resets the 1pt font of the picture cells: set again, leaving the rest of the decoration as it is,
  # including the keep_with_next of paginate()
  expect_identical(unique(x$body$styles$text$font.size$data[, gg]), 1)
  keep_with_next <- function(ft) {
    lapply(c("header", "body", "footer"), function(part) ft[[part]]$styles$pars$keep_with_next$data)
  }
  expect_identical(keep_with_next(x), keep_with_next(decorated))
  expect_identical(x$footer$dataset, decorated$footer$dataset)
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

  # table-layout:auto (overrides citril's `table` rule), table width pinned, pictures at their drawn width; the
  # header picture is found by the width flextable writes in its style attribute
  width <- sprintf("%.3fin", sum(x$body$colwidths))
  expect_match(html$extra_css, paste0(".", id, "{table-layout:auto;width:", width, ";min-width:", width), fixed = TRUE)
  expect_match(html$extra_css, paste0(".", id, " thead img[style*=\";width:180px;\"]"), fixed = TRUE)
  expect_match(html$extra_css, "width:2.500in !important", fixed = TRUE)
  f <- withr::local_tempfile(fileext = ".html")
  flextable::save_as_html(x, path = f)
  page <- paste(readLines(f, warn = FALSE), collapse = "\n")
  header <- regmatches(page, regexpr("<thead.*</thead>", page))
  expect_match(header, "<img[^>]*style=\"[^\"]*;width:180px;")

  # added to citril's class and CSS, the same however often it runs
  citril_css <- "table{table-layout:fixed;width:9in;}"
  y <- flextable::set_table_properties(
    x,
    layout = "fixed", opts_html = list(extra_class = "citril", extra_css = citril_css)
  )
  y <- adjust_forest_widths(adjust_forest_widths(y))
  expect_identical(y$properties$opts_html$extra_class, paste("citril", html$extra_class))
  expect_identical(y$properties$opts_html$extra_css, paste0(citril_css, html$extra_css))

  # another width, another class: two forest tables on one page keep their own rules
  z <- adjust_forest_widths(x, total_width = 8)
  expect_false(identical(z$properties$opts_html$extra_class, html$extra_class))
})

test_that("adjust_forest_widths() draws the pictures again only when something changed (#270)", {
  # decorated for the page add_forest() was told: the same pictures, and nothing to do the next time
  x <- adjust_forest_widths(decorate(forest_l8))
  expect_identical(forest_pictures(x)$path, forest_pictures(forest_l8)$path)
  expect_identical(adjust_forest_widths(x), x)

  # another forest column width, header font size or page: drawn again
  narrower <- adjust_forest_widths(flextable::width(x, j = "ggplot", width = 2))
  expect_equal(forest_pictures(narrower)$width, c(2, 2))
  larger <- adjust_forest_widths(flextable::fontsize(x, size = 9, part = "header"))
  expect_identical(attr(larger, "forest")$drawn$header$font_size, 9)
  expect_false(attr(adjust_forest_widths(x, page_height = 2), "forest")$drawn$merged)
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
})

test_that("the page is citril's, else the one of add_forest(), else A4 landscape (#270)", {
  withr::local_options(rlib_warning_verbosity = "verbose")
  expect_equal(.forest_page_height(decorate(forest_l8, pagesize = "P10")), .forest_page("P10")$height)
  expect_equal(.forest_page_height(forest_l8), .forest_page("L8")$height)
  no_page <- forest_l8
  attr(no_page, "forest")$pagesize <- NULL
  expect_equal(.forest_page_height(no_page), 5.998, tolerance = 1e-3)

  # a page size crane does not know warns, and the page of add_forest() is used
  unknown <- decorate(forest_l8)
  attr(unknown, "pagesize") <- "A3"
  expect_warning(height <- .forest_page_height(unknown), "A3")
  expect_equal(height, .forest_page("L8")$height)
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

  # text is measured on one device per call
  opened <- 0
  local_mocked_bindings(.forest_open_device = function() {
    opened <<- opened + 1
    grDevices::pdf(NULL)
    grDevices::dev.cur()
  })
  adjust_forest_widths(forest_l8, total_width = 7)
  expect_identical(opened, 1)
  expect_identical(grDevices::dev.list(), devices)
})

test_that("adjust_forest_widths() stops when the body rows are not the ones drawn (#270)", {
  # the plots are matched to the rows by position: after a moved or deleted row, CIs would sit next to other rows
  moved <- forest_l8
  moved$body$dataset <- moved$body$dataset[c(1, 3, 2, 4:nrow(moved$body$dataset)), ]
  expect_error(adjust_forest_widths(moved), "Body row 2")
  expect_snapshot(adjust_forest_widths(flextable::delete_rows(forest_l8, i = 3, part = "body")), error = TRUE)

  # title and footnote rows and other text edits are fine
  same <- forest_l8 |>
    flextable::add_header_lines("A title") |>
    flextable::add_footer_lines("A footnote.") |>
    flextable::compose(i = 1, j = 1, value = flextable::as_paragraph("Everyone"))
  expect_no_error(adjust_forest_widths(same))
})

test_that("the header labels warn when they do not fit their column, when drawn (#270)", {
  decorated <- decorate(forest_l8)
  # 1.3in still fits "Drug A Better" and "Drug B Better": no warning about them (only crane's own warnings are
  # checked: ggplot2's annotation_logticks() warns on drawing under R-devel)
  expect_no_warning(
    x <- adjust_forest_widths(flextable::width(decorated, j = "ggplot", width = 1.3)),
    message = "do not fit"
  )
  expect_true(attr(x, "forest")$drawn$header$fits)

  # too narrow: a warning giving the width they need
  expect_warning(
    y <- adjust_forest_widths(flextable::width(decorated, j = "ggplot", width = 0.6)),
    "do not fit the 0.6in forest column"
  )
  # not again when nothing is drawn
  expect_no_warning(adjust_forest_widths(y))
})

test_that("text put in the forest column: a header label is kept, body text is dropped (#270)", {
  gg <- which(forest_l8$col_keys == "ggplot")
  label_row <- flextable::nrow_part(forest_l8, "header")

  # a text label in place of the header picture is kept, at the font size of the other column labels
  labelled <- flextable::set_header_labels(forest_l8, ggplot = "Odds ratio (95% CI)")
  x <- adjust_forest_widths(labelled, total_width = 8)
  expect_identical(x$header$content$data[[label_row, gg]]$txt, "Odds ratio (95% CI)")
  fonts <- x$header$styles$text$font.size$data
  expect_identical(fonts[[label_row, gg]], fonts[[label_row, 1]])

  # text in a body cell (e.g. a footnote mark) is dropped, with a warning: the plots fill these cells
  marked <- flextable::footnote(
    forest_l8,
    i = 3, j = "ggplot", part = "body", ref_symbols = "b", value = flextable::as_paragraph("CI not estimable.")
  )
  expect_warning(y <- adjust_forest_widths(marked), "body row 3 is dropped")
  expect_identical(.forest_body_text(y, gg), integer(0))
})
