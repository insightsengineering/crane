#' Strip Markdown Bold or Italic
#'
#' @param x (`character`)\cr
#'   A character vector.
#' @param type (`character`)\cr
#'   Specified which type of markdown syntax to strip.
#'   Must be one or both of `c("star", "underscore")`
#'
#' @returns character vector
#' @name strip_md
#'
#' @examples
#' strip_md_bold(c("**Placebo**  \nN=45", "**High Dose**  \nN=54"))
NULL

#' @rdname strip_md
#' @export
strip_md_bold <- function(x, type = "star") {
  # check inputs ---------------------------------------------------------------
  set_cli_abort_call()
  check_class(x, "character")
  type <- arg_match(type, multiple = TRUE, values = c("star", "underscore"))

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
  check_class(x, "character")
  type <- arg_match(type, multiple = TRUE, values = c("star", "underscore"))

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
