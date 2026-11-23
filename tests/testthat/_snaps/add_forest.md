# add_forest() warns when {magick} is missing

    Code
      .warn_if_no_magick(installed = FALSE)
    Condition
      Warning:
      Install magick to render the forest plot in PDF/PNG/SVG exports.
      i Without it the forest plot column is left blank in those formats. Word, RTF, and HTML output are unaffected.
      This warning is displayed once per session.

# missing values, infinite and other widths give errors naming the argument (#270)

    Code
      adjust_forest_widths(forest_l8, widths = c(1, NA, rep(1, n_col - 2)))
    Condition
      Error in `adjust_forest_widths()`:
      ! `widths` must be positive column shares, one value per column (8) or a single value.
      x You supplied 8 values: 1, NA, 1, 1, 1, 1, 1, and 1.
    Code
      adjust_forest_widths(forest_l8, total_width = NA_real_)
    Condition
      Error in `adjust_forest_widths()`:
      ! `total_width` must be a single positive number of inches.
      x You supplied NA.
    Code
      adjust_forest_widths(forest_l8, page_height = Inf)
    Condition
      Error in `adjust_forest_widths()`:
      ! `page_height` must be a single positive number of inches.
      x You supplied Inf.
    Code
      add_forest(tbl_sub, row_height = NA)
    Condition
      Error in `add_forest()`:
      ! `row_height` must be a single positive number of inches.
      x You supplied `NA`.

---

    Code
      adjust_forest_widths(flextable::delete_columns(forest_l8, j = "ggplot"))
    Condition
      Error in `adjust_forest_widths()`:
      ! The forest plot column "ggplot" is missing from `x`.
    Code
      adjust_forest_widths(flextable::merge_at(per_row, i = 2, j = (n_col - 1):n_col,
      part = "body"))
    Condition
      Error in `adjust_forest_widths()`:
      ! Body row 2 is merged across the forest plot column; unmerge it first.

# adjust_forest_widths() stops when the body rows are not the ones drawn (#270)

    Code
      adjust_forest_widths(edited)
    Condition
      Error in `adjust_forest_widths()`:
      ! The body rows of `x` are not the rows its forest plot was drawn for.
      x Body row 1 ("Added row") is not the row the plot was drawn for.
      i Add, remove or reorder rows in the gtsummary table, before `add_forest()`.

---

    Code
      adjust_forest_widths(flextable::delete_rows(x, i = 3, part = "body"))
    Condition
      Error in `adjust_forest_widths()`:
      ! The body rows of `x` are not the rows its forest plot was drawn for.
      x Body row 3 ("II") is not the row the plot was drawn for.
      x `x` has 10 body rows, the plot was drawn for 11.
      i Add, remove or reorder rows in the gtsummary table, before `add_forest()`.

# the header labels warn when they do not fit their column, when drawn (#270)

    Code
      y <- adjust_forest_widths(flextable::width(decorated, j = "ggplot", width = 0.6))
    Condition
      Warning:
      The forest plot header labels do not fit the 0.6in forest column: they are cut off at its edge and cross the line at 1.
      i At 8pt they need a column of at least <width>in (`attr(x, "forest")$width["min"]` is 1.5in): make the column wider, or the font size of the column labels smaller.

# an unknown attr(x, 'pagesize') falls back to the page of add_forest(), with a warning (#270)

    Code
      height <- .forest_page_height(x)
    Condition
      Warning:
      `attr(x, "pagesize")` is "A3", not a page size crane knows ("L6" to "L10", "P6" to "P10").
      i The forest plot is fitted to a 6in high page: the page size of `add_forest()`, else A4 landscape. Use `page_height` to give the page's text height.
      This warning is displayed once per session.

# a total_width wider than the page is kept, with a warning (#270)

    Code
      x <- add_forest(tbl_sub, pagesize = "P8", total_width = 9.07)
    Condition
      Warning:
      The table is 9.07in wide, wider than the 6in text width of a "P8" page.
      i `total_width` is kept, so the table runs into the page margins.

# text in body cells of the forest column is dropped, with a warning: the plots fill them (#270)

    Code
      y <- adjust_forest_widths(x)
    Condition
      Warning:
      The text in the forest plot column of body row 3 is dropped: the plots fill these cells.
      i Put footnote marks and other text of a row in another column, e.g. that of its estimate.

