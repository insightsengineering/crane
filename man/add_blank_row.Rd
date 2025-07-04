% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_blank_row.R
\name{add_blank_row}
\alias{add_blank_row}
\title{Add Blank Row}
\usage{
add_blank_row(x)
}
\arguments{
\item{x}{(\code{gtsummary})\cr
a 'gtsummary' table. The table must include a column named \code{'variable'}
in \code{x$table_body}.}
}
\value{
updated 'gtsummary' table
}
\description{
Add a blank row below each variable group.
A blank row will not be added to the bottom of the table.

\emph{NOTE}: For HTML flextable output (which includes the RStudio IDE Viewer),
the blank rows do not render. But they will appear when the
table is rendered to Word.
}
\examples{
trial |>
  tbl_roche_summary(
    by = trt,
    include = c(age, marker, grade),
    nonmissing = "always"
  ) |>
  add_blank_row()
}
