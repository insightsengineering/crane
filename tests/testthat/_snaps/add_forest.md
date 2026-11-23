# add_forest() warns when {magick} is missing

    Code
      .warn_if_no_magick(installed = FALSE)
    Condition
      Warning:
      Install magick to render the forest plot in PDF/PNG/SVG exports.
      i Without it the forest plot column is left blank in those formats. Word, RTF, and HTML output are unaffected.
      This warning is displayed once per session.

# add_forest() and adjust_forest_widths() check their arguments (#270)

    Code
      adjust_forest_widths(forest_l8, widths = c(1, 2))
    Condition
      Error in `adjust_forest_widths()`:
      ! `widths` must be positive column shares, one value per column (8) or a single value.
      x You supplied 2 values: 1 and 2.
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

# adjust_forest_widths() stops when the body rows are not the ones drawn (#270)

    Code
      adjust_forest_widths(flextable::delete_rows(forest_l8, i = 3, part = "body"))
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

# text put in the forest column: a header label is kept, body text is dropped (#270)

    Code
      y <- adjust_forest_widths(marked)
    Condition
      Warning:
      The text in the forest plot column of body row 3 is dropped: the plots fill these cells.
      i Put footnote marks and other text of a row in another column, e.g. that of its estimate.

