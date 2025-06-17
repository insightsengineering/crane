#' Remove Markdown Syntax from Header
#'
#' Remove markdown syntax (e.g. double star for bold, underscore for italic, etc)
#' from the headers and spanning headers of a gtsummary table.
#'
#' @param x (`gtsummary`)\cr
#'   A gtsummary table
#' @param md (`character`)\cr
#'   Must be one or more of `'bold'` and `'italic'`. Default is `'bold'`.
#' @param type (`character`)\cr
#'   Must be one or more of `'star'` and `'underscore'`. Default is `'star'`.
#'
#' @returns gtsummary table
#' @export
#'
#' @examples
#' tbl_demographics(
#'   data = cards::ADSL,
#'   include = AGE,
#'   by = ARM
#' ) |>
#'   modify_header_rm_md()
modify_header_rm_md <- function(x, md = "bold", type = "star") {
  # check inputs ---------------------------------------------------------------
  set_cli_abort_call()
  check_class(x, "gtsummary")
  md <- arg_match(md, multiple = TRUE, values = c("bold", "italic"))
  type <- arg_match(type, multiple = TRUE, values = c("star", "underscore"))

  # process inputs -------------------------------------------------------------
  # styler: off
  patterns <- character(0L)
  if (md == "bold" && type == "star") patterns <- c(patterns, "\\*\\*(.*?)\\*\\*")
  if (md == "bold" && type == "underscore") patterns <- c(patterns, "__(.*?)__")
  if (md == "italic" && type == "star") patterns <- c(patterns, "\\*(.*?)\\*")
  if (md == "italic" && type == "underscore") patterns <- c(patterns, "_(.*?)_")
  # styler: on

  # remove markdown syntax and return table ------------------------------------
  .strip_md(x, patterns)
}

.strip_md <- function(x, patterns) {
  # cycle over the patterns and make the replacements --------------------------
  for (pattern in patterns) {
    x$table_styling$header$label <-
      x$table_styling$header$label |>
      str_replace_all(pattern = pattern, replacement = "\\1")
    x$table_styling$spanning_header$spanning_header <-
      x$table_styling$spanning_header$spanning_header |>
      str_replace_all(pattern = pattern, replacement = "\\1")
  }

  # return table ---------------------------------------------------------------
  x
}
