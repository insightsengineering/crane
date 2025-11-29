# styler: off
# nocov start


assert_proportion_value <- function(x, include_boundaries = FALSE) {
  checkmate::assert_number(x, lower = 0, upper = 1)
  checkmate::assert_flag(include_boundaries)
  if (isFALSE(include_boundaries)) {
    checkmate::assert_true(x > 0)
    checkmate::assert_true(x < 1)
  }
}

check_list_of_variables <- function(x) {
  x <- Filter(Negate(is.null), x)
  res <- checkmate::check_list(x,
    names = "named", min.len = 1,
    any.missing = FALSE, types = "character"
  )
  if (isTRUE(res)) {
    res <- checkmate::check_character(unlist(x), min.chars = 1)
  }
  res
}

assert_list_of_variables <- function(x, .var.name = checkmate::vname(x), add = NULL) {
  if (missing(x)) {
    stop(sprintf(
      "argument \"%s\" is missing, with no default",
      .var.name
    ))
  }
  res <- check_list_of_variables(x)
  checkmate::makeAssertion(x, res, .var.name, add)
}

check_df_with_variables <- function(df, variables, na_level = NULL) {
  checkmate::assert_data_frame(df)
  assert_list_of_variables(variables)
  err_flag <- all(unlist(variables) %in% colnames(df))
  checkmate::assert_flag(err_flag)
  if (isFALSE(err_flag)) {
    vars <- setdiff(unlist(variables), colnames(df))
    return(paste(
      deparse(substitute(df)), "does not contain all specified variables as column names. Missing from data frame:",
      paste(vars, collapse = ", ")
    ))
  }
  if (!is.null(na_level)) {
    checkmate::assert_string(na_level)
    res <- unlist(lapply(
      as.list(df)[unlist(variables)],
      function(x) any(x == na_level)
    ))
    if (any(res)) {
      return(paste0(
        deparse(substitute(df)), " contains explicit na_level (",
        na_level, ") in the following columns: ", paste0(unlist(variables)[res],
          collapse = ", "
        )
      ))
    }
  }
  return(TRUE)
}

assert_df_with_variables <- function(df, variables, na_level = NULL, .var.name = checkmate::vname(df),
                                     add = NULL) {
  if (missing(df)) {
    stop(sprintf(
      "argument \"%s\" is missing, with no default",
      .var.name
    ))
  }
  res <- check_df_with_variables(df, variables, na_level)
  checkmate::makeAssertion(df, res, .var.name, add)
}

check_valid_factor <- function(x, min.levels = 1, max.levels = NULL, null.ok = TRUE,
                               any.missing = TRUE, n.levels = NULL, len = NULL) {
  checkmate::assert_int(min.levels, lower = 1)
  res <- checkmate::check_factor(x,
    min.levels = min.levels,
    null.ok = null.ok, max.levels = max.levels, any.missing = any.missing,
    n.levels = n.levels
  )
  if (isTRUE(res)) {
    res <- checkmate::check_character(levels(x), min.chars = 1)
  }
  return(res)
}

assert_valid_factor <- function(x, min.levels = 1, max.levels = NULL, null.ok = TRUE,
                                any.missing = TRUE, n.levels = NULL, len = NULL, .var.name = checkmate::vname(x),
                                add = NULL) {
  if (missing(x)) {
    stop(sprintf(
      "argument \"%s\" is missing, with no default",
      .var.name
    ))
  }
  res <- check_valid_factor(
    x, min.levels, max.levels, null.ok,
    any.missing, n.levels, len
  )
  checkmate::makeAssertion(x, res, .var.name, add)
}

# nocov end
# styler: on
