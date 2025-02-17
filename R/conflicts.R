crane_conflicts <- function(only = NULL) {
  envs <- grep("^package:", search(), value = TRUE)
  envs <- set_names(envs)

  if (!is.null(only)) {
    only <- union(only, core)
    envs <- envs[names(envs) %in% paste0("package:", only)]
  }

  objs <- invert(lapply(envs, ls_env))

  conflicts <- keep(objs, ~ length(.x) > 1)

  tidy_names <- paste0("package:", crane_packages())
  conflicts <- keep(conflicts, function(pkg) any(pkg %in% tidy_names))

  conflict_funs <- imap(conflicts, confirm_conflict)
  conflict_funs <- compact(conflict_funs)

  class(conflict_funs) <- "crane_conflicts"
  conflict_funs
}

crane_conflict_message <- function(x) {
  header <- cli::rule(
    left = cli::style_bold("Conflicts")
  )

  pkgs <- x %>% map(~ gsub("^package:", "", .))
  others <- pkgs %>% map(`[`, -1)
  other_calls <- map2_chr(
    others, names(others),
    ~ paste0(cli::col_blue(.x), "::", .y, "()", collapse = ", ")
  )

  winner <- pkgs %>% map_chr(\(x) x[1])
  funs <- format(paste0(
    cli::col_blue(winner), "::", cli::col_green(paste0(names(x), "()"))
  ))
  bullets <- paste0(
    cli::col_red(cli::symbol$cross), " ", funs, " masks ", other_calls,
    collapse = "\n"
  )

  conflicted <- paste0(
    cli::col_cyan(cli::symbol$info), " ",
    "Use the ",
    cli::format_inline("{.href [conflicted package](http://conflicted.r-lib.org/)}"),
    " to force all conflicts to become errors"
  )

  paste0(
    header, "\n",
    bullets, "\n",
    conflicted
  )
}

#' @export
print.crane_conflicts <- function(x, ..., startup = FALSE) {
  if (rlang::is_empty(x)) {
    return(invisible(x))
  }
  cli::cat_line(crane_conflict_message(x))
  invisible(x)
}

crane_packages <- function(include_self = TRUE) {
  raw <- utils::packageDescription("crane")$Imports
  imports <- strsplit(raw, ",")[[1]]
  parsed <- gsub("^\\s+|\\s+$", "", imports)
  names <- vapply(strsplit(parsed, "\\s+"), "[[", 1, FUN.VALUE = character(1))

  if (include_self) {
    names <- c(names, "crane")
  }

  names
}

confirm_conflict <- function(packages, name) {
  # Only look at functions
  objs <- packages %>%
    map(~ get(name, pos = .)) |>
    keep(is.function)

  if (length(objs) <= 1) {
    return()
  }

  # Remove identical functions
  objs <- objs[!duplicated(objs)]
  packages <- packages[!duplicated(packages)]
  if (length(objs) == 1) {
    return()
  }

  packages
}

ls_env <- function(env) {
  x <- ls(pos = env)

  # intersect, setdiff, setequal, union come from generics
  if (env %in% c("package:dplyr", "package:lubridate")) {
    x <- setdiff(x, c("intersect", "setdiff", "setequal", "union"))
  }

  if (env == "package:lubridate") {
    x <- setdiff(x, c(
      "as.difftime", # lubridate makes into an S4 generic
      "date" # matches base behaviour
    ))
  }

  x
}
