#' Strip Markdown Bold or Italic
#'
#' Remove markdown syntax that bold or italicizes headers and spanning
#' headers from gtsummary tables.
#'
#' @param x (`gtsummary`/`character`)\cr
#'   A 'gtsummary' table or character vector.
#'   When a 'gtsummary' table is passed, the column headers and spanning headers
#'   are processed.
#' @param type (`character`)\cr
#'   Specified which type of markdown syntax to strip.
#'   Must be one or both of `c("star", "underscore")`
#'
#' @returns character vector or gtsummary table
#' @name strip_md
#'
#' @examples
#' # Example 1 ----------------------------------
#' gtsummary::trial |>
#'   tbl_demographics(by = trt, include = age) |>
#'   strip_md_bold()
#'
#' # Example 2 ----------------------------------
#' strip_md_bold(c("**Placebo**  \nN=45", "**High Dose**  \nN=54"))
NULL

#' @rdname strip_md
#' @export
strip_md_bold <- function(x, type = "star") {
  # check inputs ---------------------------------------------------------------
  set_cli_abort_call()
  check_class(x, c("character", "gtsummary"))
  type <- arg_match(type, multiple = TRUE, values = c("star", "underscore"))

  # process gtsummary tables ---------------------------------------------------
  if (inherits(x, "gtsummary")) {
    x$table_styling$header$label <-
      x$table_styling$header$label |>
      strip_md_bold(type = type)
    x$table_styling$spanning_header$spanning_header <-
      x$table_styling$spanning_header$spanning_header |>
      strip_md_bold(type = type)
    return(x)
  }

  # remove markdown syntax -----------------------------------------------------
  if ("star" %in% type) {
    x <- str_replace_all(x, "\\*\\*(.*?)\\*\\*", "\\1")
  }
  if ("underscore" %in% type) {
    x <- str_replace_all(x, "__(.*?)__", "\\1")
  }

  # return stripped vector -----------------------------------------------------
  x
}

#' @rdname strip_md
#' @export
strip_md_italic <- function(x, type = "star") {
  # check inputs ---------------------------------------------------------------
  set_cli_abort_call()
  check_class(x, c("character", "gtsummary"))
  type <- arg_match(type, multiple = TRUE, values = c("star", "underscore"))

  # process gtsummary tables ---------------------------------------------------
  if (inherits(x, "gtsummary")) {
    x$table_styling$header$label <-
      x$table_styling$header$label |>
      strip_md_italic(type = type)
    x$table_styling$spanning_header$spanning_header <-
      x$table_styling$spanning_header$spanning_header |>
      strip_md_italic(type = type)
    return(x)
  }

  # remove markdown syntax -----------------------------------------------------
  if ("star" %in% type) {
    x <- str_replace_all(x, "\\*(.*?)\\*", "\\1")
  }
  if ("underscore" %in% type) {
    x <- str_replace_all(x, "_(.*?)_", "\\1")
  }

  # return stripped vector -----------------------------------------------------
  x
}
