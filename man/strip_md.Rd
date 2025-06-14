% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/strip_md.R
\name{strip_md}
\alias{strip_md}
\alias{strip_md_bold}
\alias{strip_md_italic}
\title{Strip Markdown Bold or Italic}
\usage{
strip_md_bold(x, type = "star")

strip_md_italic(x, type = "star")
}
\arguments{
\item{x}{(\code{gtsummary}/\code{character})\cr
A 'gtsummary' table or character vector.
When a 'gtsummary' table is passed, the column headers and spanning headers
are processed.}

\item{type}{(\code{character})\cr
Specified which type of markdown syntax to strip.
Must be one or both of \code{c("star", "underscore")}}
}
\value{
character vector or gtsummary table
}
\description{
Remove markdown syntax that bold or italicizes headers and spanning
headers from gtsummary tables.
}
\examples{
# Example 1 ----------------------------------
gtsummary::trial |>
  tbl_demographics(by = trt, include = age) |>
  strip_md_bold()

# Example 2 ----------------------------------
strip_md_bold(c("**Placebo**  \nN=45", "**High Dose**  \nN=54"))
}
