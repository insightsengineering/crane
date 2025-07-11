% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tbl_hierarchical_rate_by_grade.R
\name{tbl_hierarchical_rate_by_grade}
\alias{tbl_hierarchical_rate_by_grade}
\alias{add_overall.tbl_hierarchical_rate_by_grade}
\title{AE Rates by Highest Toxicity Grade}
\usage{
tbl_hierarchical_rate_by_grade(
  data,
  variables,
  denominator,
  by = NULL,
  id = "USUBJID",
  include_overall = everything(),
  statistic = everything() ~ "{n} ({p}\%)",
  label = NULL,
  digits = NULL,
  sort = "alphanumeric",
  filter = NULL,
  grade_groups = list(),
  grades_exclude = NULL,
  keep_zero_rows = FALSE
)

\method{add_overall}{tbl_hierarchical_rate_by_grade}(
  x,
  last = FALSE,
  col_label = "**Overall**  \\nN = {style_number(N)}",
  statistic = NULL,
  digits = NULL,
  ...
)
}
\arguments{
\item{data}{(\code{data.frame})\cr
a data frame.}

\item{variables}{(\code{\link[dplyr:dplyr_tidy_select]{tidy-select}})\cr
A character vector or tidy-selector of 3 columns in \code{data} specifying a system organ class variable,
an adverse event terms variable, and a toxicity grade level variable, respectively.}

\item{denominator}{(\code{data.frame}, \code{integer})\cr
used to define the denominator and enhance the output.
The argument is required for \code{tbl_hierarchical()} and optional for \code{tbl_hierarchical_count()}.
The \code{denominator} argument must be specified when \code{id} is used to calculate event rates.}

\item{by}{(\code{\link[dplyr:dplyr_tidy_select]{tidy-select}})\cr
a single column from \code{data}. Summary statistics will be stratified by this variable.
Default is \code{NULL}.}

\item{id}{(\code{\link[dplyr:dplyr_tidy_select]{tidy-select}})\cr
argument used to subset \code{data} to identify rows in \code{data} to calculate
event rates in \code{tbl_hierarchical()}.}

\item{include_overall}{(\code{\link[dplyr:dplyr_tidy_select]{tidy-select}})\cr
Variables from \code{variables} for which an overall section at that hierarchy level should be computed.
An overall section at the SOC variable level will have label \code{"- Any adverse events -"}. An overall section at
the AE term variable level will have label \code{"- Overall -"}. If the grade level variable is included it has no
effect. The default is \code{everything()}.}

\item{statistic}{(\code{\link[gtsummary:syntax]{formula-list-selector}})\cr
used to specify the summary statistics to display for all variables in \code{tbl_hierarchical()}.
The default is \code{everything() ~ "{n} ({p})"}.}

\item{label}{(\code{\link[gtsummary:syntax]{formula-list-selector}})\cr
used to override default labels in hierarchical table, e.g. \code{list(AESOC = "System Organ Class")}.
The default for each variable is the column label attribute, \code{attr(., 'label')}.
If no label has been set, the column name is used.}

\item{digits}{(\code{\link[gtsummary:syntax]{formula-list-selector}})\cr
specifies how summary statistics are rounded. Values may be either integer(s) or function(s). If not specified,
default formatting is assigned via \code{label_style_number()} for statistics \code{n} and \code{N}, and
\code{label_style_percent(digits=1)} for statistic \code{p}.}

\item{sort}{(\code{string})\cr
type of sorting to perform. Value must be one of:
\itemize{
\item \code{"alphanumeric"} - at each hierarchy level of the table, rows are ordered alphanumerically (i.e. A to Z)
by label text.
\item \code{"descending"} - at each hierarchy level of the table, count sums are calculated for each row and rows are
sorted in descending order by sum. If \code{sort = "descending"}, the \code{n} statistic is used to calculate row sums if
included in \code{statistic} for all variables, otherwise \code{p} is used. If neither \code{n} nor \code{p} are present in \code{x} for
all variables, an error will occur.
}

Defaults to \code{"descending"}.}

\item{filter}{(\code{expression})\cr
An expression that is used to filter rows of the table. Filter will be applied to the second variable (adverse
event terms) specified via \code{variables}. See the Details section below for more information.}

\item{grade_groups}{(\verb{named list})\cr
A named list of grade groups for which rates should be calculated. Grade groups must be mutually exclusive, i.e.
each grade cannot be assigned to more than one grade group. Each grade group must be specified in the list as a
character vector of the grades included in the grade group, named with the corresponding name of the grade group,
e.g. \code{"Grade 1-2" = c("1", "2")}.}

\item{grades_exclude}{(\code{character})\cr
A vector of grades to omit individual rows for when printing the table. These grades will still be used when
computing overall totals and grade group totals. For example, to avoid duplication, if a grade group is defined as
\code{"Grade 5" = "5"}, the individual rows corresponding to grade 5 can be excluded by setting \code{grades_exclude = "5"}.}

\item{keep_zero_rows}{(\code{logical})\cr
Whether rows containing zero rates across all columns should be kept. If \code{FALSE}, this filter will be applied
prior to any filters specified via the \code{filter} argument which may still remove these rows. Defaults to \code{FALSE}.}

\item{x}{(\code{tbl_hierarchical_rate_by_grade})\cr
A gtsummary table of class \code{'tbl_hierarchical_rate_by_grade'}.}

\item{last}{(scalar \code{logical})\cr
Logical indicator to display overall column last in table.
Default is \code{FALSE}, which will display overall column first.}

\item{col_label}{(\code{string})\cr
String indicating the column label. Default is \code{"**Overall**  \nN = {style_number(N)}"}}

\item{...}{These dots are for future extensions and must be empty.}
}
\value{
a gtsummary table of class \code{"tbl_hierarchical_rate_by_grade"}.
}
\description{
A wrapper function for \code{\link[gtsummary:tbl_hierarchical]{gtsummary::tbl_hierarchical()}} to calculate rates of highest toxicity grades with the options
to add rows for grade groups and additional summary sections at each variable level.

Only the highest grade level recorded for each subject will be analyzed. Prior to running the function, ensure that
the toxicity grade variable (\code{grade}) is a factor variable, with factor levels ordered lowest to highest.

Grades will appear in rows in the order of the factor levels given, with each grade group appearing prior to the
first level in its group.
}
\details{
When using the \code{filter} argument, the filter will be applied to the second variable from \code{variables}, i.e. the
adverse event terms variable. If an AE does not meet the filtering criteria, the AE overall row as well as all grade
and grade group rows within an AE section will be excluded from the table. Filtering out AEs does not exclude the
records corresponding to these filtered out rows from being included in rate calculations for overall sections. If
all AEs for a given SOC have been filtered out, the SOC will be excluded from the table. If all AEs are filtered out
and the SOC variable is included in \code{include_overall} the \verb{- Any adverse events -} section will still be kept.

See \code{\link[gtsummary:filter_hierarchical]{gtsummary::filter_hierarchical()}} for more details and examples.
}
\examples{
theme_gtsummary_roche()
ADSL <- cards::ADSL
ADAE_subset <- cards::ADAE |>
  dplyr::filter(
    AESOC \%in\% unique(cards::ADAE$AESOC)[1:5],
    AETERM \%in\% unique(cards::ADAE$AETERM)[1:10]
  )

grade_groups <- list(
  "Grade 1-2" = c("1", "2"),
  "Grade 3-4" = c("3", "4"),
  "Grade 5" = "5"
)

# Example 1 ----------------------------------
tbl_hierarchical_rate_by_grade(
  ADAE_subset,
  variables = c(AEBODSYS, AEDECOD, AETOXGR),
  denominator = ADSL,
  by = TRTA,
  label = list(
    AEBODSYS = "MedDRA System Organ Class",
    AEDECOD = "MedDRA Preferred Term",
    AETOXGR = "Grade"
  ),
  grade_groups = grade_groups,
  grades_exclude = "5"
)

# Example 2 ----------------------------------
# Filter: Keep AEs with an overall prevalence of greater than 10\%
tbl_hierarchical_rate_by_grade(
  ADAE_subset,
  variables = c(AEBODSYS, AEDECOD, AETOXGR),
  denominator = ADSL,
  by = TRTA,
  grade_groups = list("Grades 1-2" = c("1", "2"), "Grades 3-5" = c("3", "4", "5")),
  filter = sum(n) / sum(N) > 0.10
) |>
  add_overall(last = TRUE)
}
