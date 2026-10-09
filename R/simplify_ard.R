#' Simplify the ARD of a table
#'
#' @description
#' Flattens the Analysis Results Data (ARD) of a table into a single ARD.
#'
#' Stacked and stratified tables (e.g. from [gtsummary::tbl_strata()] or
#' [gtsummary::tbl_strata_nested_stack()]) return one ARD per stratum from
#' [gtsummary::gather_ard()], with the stratum recorded only in the list names.
#' `simplify_ard()` adds each stratum as a group column, following the
#' convention of [cards::ard_strata()]: existing groups keep their numbers and
#' the strata take the next ones, innermost stratum first.
#'
#' Split tables ([gtsummary::tbl_split_by_rows()],
#' [gtsummary::tbl_split_by_columns()]) carry a full copy of the ARD in every
#' piece. These copies are removed.
#'
#' The result can be compared with an independently derived ARD using
#' [cards::compare_ard()].
#'
#' @param x (`gtsummary`, `tbl_split` or `list`)\cr
#'   a table, a list of split tables, or a (nested) list of ARDs as returned by
#'   [gtsummary::gather_ard()].
#' @param .deduplicate (scalar `logical`)\cr
#'   whether to remove duplicated statistics. Repeated copies of a whole ARD
#'   (e.g. from split tables) are removed with a warning. Rows shared by several
#'   ARDs (e.g. variable attributes repeated by `add_overall()`) are removed
#'   silently. Default is `TRUE`.
#' @param .unlist (scalar `logical`)\cr
#'   whether to return the ARD with atomic columns, see
#'   [cards::unlist_ard_columns()]. Useful with comparison tools that do not
#'   handle list columns. Default is `FALSE`.
#'
#' @returns A `card` object, or a `card_unlisted` data frame when `.unlist = TRUE`.
#'   The same statistic (same groups, variables, `context` and `stat_name`) with
#'   different values in two places of the table is an error.
#'
#' @details
#' Statistics are identified by their groups, variables, `context` and
#' `stat_name`. `context` is needed for tables that report two statistics with
#' the same name for the same rows, e.g. subject and event counts in
#' [tbl_hierarchical_rate_and_count()]. Pass the same keys to
#' [cards::compare_ard()]:
#'
#' ```r
#' cards::compare_ard(
#'   x, y,
#'   keys = c(cards::all_ard_groups(), cards::all_ard_variables(), "context", "stat_name")
#' )
#' ```
#'
#' @examples
#' adlb <- cards::ADLB |>
#'   dplyr::mutate(AVISIT = trimws(AVISIT)) |>
#'   dplyr::filter(
#'     PARAMCD %in% c("SODIUM", "K"),
#'     AVISIT %in% c("Baseline", "Week 2", "Week 4")
#'   )
#'
#' # one table per parameter, split into one page per parameter
#' tbl <- gtsummary::tbl_strata(
#'   adlb,
#'   strata = PARAM,
#'   .tbl_fun = ~ tbl_baseline_chg(
#'     .x,
#'     baseline_level = "Baseline",
#'     by = "TRTA",
#'     denominator = cards::ADSL
#'   ),
#'   .combine_with = "tbl_stack",
#'   .combine_args = list(group_header = NULL, quiet = TRUE)
#' ) |>
#'   gtsummary::tbl_split_by_rows(variable_level = ends_with("lbl"))
#'
#' # PARAM becomes group2, next to the treatment in group1
#' ard <- simplify_ard(tbl)
#' ard
#'
#' # compare with an ARD derived independently
#' cards::compare_ard(
#'   ard, ard,
#'   keys = c(cards::all_ard_groups(), cards::all_ard_variables(), "context", "stat_name")
#' ) |>
#'   cards::is_ard_equal()
#'
#' # atomic columns, e.g. for diffdf
#' simplify_ard(tbl, .unlist = TRUE)
#' @export
simplify_ard <- function(x, .deduplicate = TRUE, .unlist = FALSE) {
  set_cli_abort_call()
  check_not_missing(x)
  check_scalar_logical(.deduplicate)
  check_scalar_logical(.unlist)
  if (!is.list(x)) {
    cli::cli_abort(
      "The {.arg x} argument must be a {.cls gtsummary} table, a list of tables or a list of ARDs,
       not {.obj_type_friendly {x}}.",
      call = get_cli_abort_call()
    )
  }

  ards <- .collect_ards(x)
  if (isTRUE(.deduplicate)) {
    ards <- .drop_ard_copies(ards)
  }
  ard <- .bind_ards(ards) |>
    .check_ard_duplicates(deduplicate = .deduplicate)

  if (isTRUE(.unlist)) {
    return(cards::unlist_ard_columns(ard))
  }
  ard
}

#' Collect the ARDs of a table
#'
#' Walks a table, a list of tables or a nested list of ARDs and returns a flat
#' list of ARDs. Strata of stacked tables are added as group columns.
#'
#' @param x (`gtsummary`, `list`, `data.frame` or `NULL`)\cr
#'   object to walk.
#'
#' @returns A list of `card` objects.
#' @keywords internal
#' @noRd
.collect_ards <- function(x) {
  if (is.data.frame(x)) {
    return(list(cards::as_card(x, check = FALSE)))
  }
  if (inherits(x, c("tbl_stack", "tbl_merge"))) {
    return(.collect_stacked_ards(x[["tbls"]], strata = .table_strata(x)))
  }
  if (inherits(x, "gtsummary")) {
    return(.collect_ards(gtsummary::gather_ard(x)))
  }
  if (is.list(x)) {
    # a named list of ARDs as returned by `gather_ard()` on a stratified table
    return(.collect_stacked_ards(x, strata = .parse_strata_ids(names(x))))
  }
  list()
}

#' Collect the ARDs of stacked elements and add their strata
#'
#' @param elements (`list`)\cr
#'   tables or ARDs of a stacked table, or any list to flatten.
#' @param strata (`data.frame` or `NULL`)\cr
#'   one row per element, one column per stratum (outermost first), as
#'   returned by `.parse_strata_ids()`.
#'
#' @returns A list of `card` objects.
#' @keywords internal
#' @noRd
.collect_stacked_ards <- function(elements, strata = NULL) {
  ards_by_element <- lapply(unname(elements), .collect_ards)

  if (!is.null(strata)) {
    # the same group number for every element, so a stratum always lands in one column
    first_group <- max(c(0L, unlist(lapply(ards_by_element, \(ards) map_int(ards, .max_group_n))))) + 1L
    strata_vars <- rev(names(strata)) # innermost stratum gets the lowest number
    ards_by_element <- Map(
      \(ards, i) {
        levels <- rev(as.list(strata[i, , drop = TRUE]))
        lapply(ards, .add_strata_groups, variables = strata_vars, levels = levels, first_group = first_group)
      },
      ards_by_element,
      seq_along(ards_by_element)
    )
  }

  do.call(c, ards_by_element) %||% list()
}

#' Strata of a stacked table
#'
#' Strata are read from the names of `x$tbls` (`VAR="level"` pairs, as set by
#' `tbl_strata()` and `tbl_strata_nested_stack()`). `tbl_strata()` also keeps
#' the unformatted levels in `x$df_strata`, which are used when available.
#'
#' @param x (`tbl_stack` or `tbl_merge`)\cr
#'   stacked or merged table.
#'
#' @returns A data frame (one row per element of `x$tbls`) or `NULL`.
#' @keywords internal
#' @noRd
.table_strata <- function(x) {
  strata <- .parse_strata_ids(names(x[["tbls"]]))
  df_strata <- x[["df_strata"]]
  if (is.null(strata) || is.null(df_strata) || nrow(df_strata) != nrow(strata)) {
    return(strata)
  }

  levels <- df_strata[grepl("^strata_[0-9]+$", names(df_strata))]
  if (ncol(levels) != ncol(strata)) {
    return(strata)
  }
  levels <- lapply(levels, as.character)
  names(levels) <- names(strata)
  as.data.frame(levels, stringsAsFactors = FALSE, optional = TRUE)
}

#' Parse strata ids
#'
#' @param ids (`character` or `NULL`)\cr
#'   ids like `PARAM="Albumin",SEX="F"`.
#'
#' @returns A data frame with one column per stratum (outermost first) and one
#'   row per id, or `NULL` when the ids are not all strata ids with the same
#'   variables.
#' @keywords internal
#' @noRd
.parse_strata_ids <- function(ids) {
  pair_pattern <- '([^=,"]+)=("(?:[^"\\\\]|\\\\.)*"|[^,"]*)'
  id_pattern <- paste0("^", pair_pattern, "(?:,", pair_pattern, ")*$")
  if (is_empty(ids) || !all(grepl(id_pattern, ids, perl = TRUE))) {
    return(NULL)
  }

  pairs <- regmatches(ids, gregexpr(pair_pattern, ids, perl = TRUE))
  variables <- lapply(pairs, \(x) sub("=.*$", "", x))
  if (!all(map_lgl(variables, identical, variables[[1]]))) {
    return(NULL)
  }

  levels <- lapply(pairs, \(x) {
    value <- sub("^[^=]*=", "", x)
    quoted <- startsWith(value, '"')
    value[quoted] <- gsub("\\\\(.)", "\\1", substr(value[quoted], 2L, nchar(value[quoted]) - 1L))
    value
  })
  strata <- as.data.frame(do.call(rbind, levels), stringsAsFactors = FALSE)
  names(strata) <- variables[[1]]
  strata
}

#' Add strata as group columns
#'
#' @param ard (`card`)\cr
#'   ARD of one stratum.
#' @param variables,levels (`character`, `list`)\cr
#'   strata variables and their levels, innermost first.
#' @param first_group (`integer`)\cr
#'   group number of the innermost stratum.
#'
#' @returns The ARD with `group<n>` and `group<n>_level` columns added.
#' @keywords internal
#' @noRd
.add_strata_groups <- function(ard, variables, levels, first_group) {
  for (i in seq_along(variables)) {
    group <- paste0("group", first_group + i - 1L)
    ard[[group]] <- rep_len(variables[[i]], nrow(ard))
    ard[[paste0(group, "_level")]] <- rep_len(list(levels[[i]]), nrow(ard))
  }
  ard
}

#' Highest group number of an ARD
#'
#' @param ard (`card`)\cr
#'   an ARD.
#'
#' @returns An integer, `0L` when the ARD has no group columns.
#' @keywords internal
#' @noRd
.max_group_n <- function(ard) {
  groups <- grep("^group[0-9]+$", names(ard), value = TRUE)
  max(c(0L, as.integer(sub("^group", "", groups))))
}

#' Drop repeated copies of whole ARDs
#'
#' @param ards (`list`)\cr
#'   list of `card` objects.
#'
#' @returns The list without repeated copies.
#' @keywords internal
#' @noRd
.drop_ard_copies <- function(ards) {
  is_copy <- duplicated(map_chr(ards, rlang::hash))
  if (any(is_copy)) {
    cli::cli_warn(c(
      "Removed {sum(is_copy)} repeated cop{?y/ies} of an ARD.",
      i = "Split tables, e.g. from {.fun gtsummary::tbl_split_by_rows}, carry a full copy of the ARD in every piece."
    ))
  }
  ards[!is_copy]
}

#' Bind ARDs into one
#'
#' @param ards (`list`)\cr
#'   list of `card` objects.
#'
#' @returns A `card` object, with zero rows when `ards` is empty.
#' @keywords internal
#' @noRd
.bind_ards <- function(ards) {
  if (is_empty(ards)) {
    ard <- dplyr::tibble(
      variable = character(), context = character(), stat_name = character(),
      stat_label = character(), stat = list(), fmt_fun = list(),
      warning = list(), error = list()
    )
  } else {
    ard <- dplyr::bind_rows(lapply(ards, dplyr::as_tibble))
  }
  cards::as_card(ard, check = FALSE) |>
    cards::tidy_ard_column_order()
}

#' Remove duplicated statistics and stop on conflicting ones
#'
#' Statistics are identified by their groups, variables, `context` and
#' `stat_name`.
#'
#' @param ard (`card`)\cr
#'   an ARD.
#' @param deduplicate (scalar `logical`)\cr
#'   whether to remove rows with the same keys and the same statistic.
#'
#' @returns The ARD.
#' @keywords internal
#' @noRd
.check_ard_duplicates <- function(ard, deduplicate) {
  if (nrow(ard) == 0L) {
    return(ard)
  }

  key_cols <- names(dplyr::select(
    ard,
    cards::all_ard_groups(), cards::all_ard_variables(), dplyr::any_of("context"), "stat_name"
  ))
  keys <- do.call(
    mapply,
    c(list(FUN = \(...) rlang::hash(list(...)), SIMPLIFY = TRUE, USE.NAMES = FALSE), as.list(ard[key_cols]))
  )
  key_values <- paste(keys, map_chr(ard$stat, rlang::hash))

  if (isTRUE(deduplicate)) {
    keep <- !duplicated(key_values)
    ard <- ard[keep, ]
    keys <- keys[keep]
    key_values <- key_values[keep]
  }

  # same keys with different values
  unique_values <- !duplicated(key_values)
  conflicts <- keys %in% keys[unique_values][duplicated(keys[unique_values])]
  if (any(conflicts)) {
    first <- ard[which(conflicts)[1], key_cols] |>
      cards::unlist_ard_columns()
    first <- paste(names(first), unlist(lapply(first, as.character)), sep = " = ", collapse = ", ")
    cli::cli_abort(
      c(
        "The ARD has different values for the same statistic in {sum(conflicts)} rows.",
        i = "First one: {first}."
      ),
      call = get_cli_abort_call()
    )
  }

  ard
}
