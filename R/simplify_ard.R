#' Simplify the ARD of a table
#'
#' @description
#' Flattens the Analysis Results Data (ARD) of a table into a single ARD.
#'
#' Stacked and stratified tables (e.g. from [gtsummary::tbl_strata()] or
#' [gtsummary::tbl_strata_nested_stack()]) return one ARD per stratum from
#' [gtsummary::gather_ard()], with the stratum recorded only in the list names.
#' `simplify_ard()` adds each stratum as a group column, following the
#' convention of [cards::ard_strata()]: the groups of the table keep their
#' numbers and the strata take the numbers after the highest one, innermost
#' stratum first, so a stratum lands in the same column everywhere in the table.
#'
#' Stacked tables without strata whose pieces report the same statistics (e.g.
#' [gtsummary::tbl_stack()] of the same variables on two subsets) are told
#' apart by a `tbl_id1` group (`tbl_id2` for the next level of stacking), with
#' the `tbl_ids` of the stack, or the position of the piece, as level.
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
#'   (e.g. from split tables) are removed with a message. Rows shared by several
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

  leaves <- .collect_ards(x)
  if (isTRUE(.deduplicate)) {
    leaves <- .drop_ard_copies(leaves)
  }
  ard <- .add_strata_columns(leaves) |>
    .bind_ards() |>
    .check_ard_duplicates(deduplicate = .deduplicate)

  if (isTRUE(.unlist)) {
    return(cards::unlist_ard_columns(ard))
  }
  ard
}

#' Collect the ARDs of a table
#'
#' Walks a table, a list of tables or a nested list of ARDs and returns a flat
#' list of leaves: each ARD with the strata it belongs to. The strata are
#' turned into group columns at the end, by `.add_strata_columns()`, so they
#' get the same column numbers across the whole table.
#'
#' @param x (`gtsummary`, `list`, `data.frame` or `NULL`)\cr
#'   object to walk.
#'
#' @returns A list of leaves, each `list(ard, variables, levels)` with the
#'   strata innermost first.
#' @keywords internal
#' @noRd
.collect_ards <- function(x) {
  if (is.data.frame(x)) {
    # placeholder used by tables without results, e.g. `data.frame()`
    if (ncol(x) == 0L) {
      return(list())
    }
    missing_cols <- setdiff(c("variable", "stat_name", "stat"), names(x))
    if (!is_empty(missing_cols)) {
      cli::cli_abort(
        c(
          "The {.arg x} argument contains a data frame that is not an ARD.",
          i = "It has no {.val {missing_cols}} column{?s}.",
          i = "Data cast with {.code cards::as_card(check = FALSE)} is not an ARD."
        ),
        call = get_cli_abort_call()
      )
    }
    return(list(list(ard = cards::as_card(x, check = FALSE), variables = character(), levels = list())))
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

#' Collect the ARDs of stacked elements and record their strata
#'
#' @param elements (`list`)\cr
#'   tables or ARDs of a stacked table, or any list to flatten.
#' @param strata (`data.frame` or `NULL`)\cr
#'   one row per element, one column per stratum (outermost first), as
#'   returned by `.parse_strata_ids()`.
#'
#' @returns A list of leaves (see `.collect_ards()`). Elements without strata
#'   that report the same statistic with different values are told apart by a
#'   `tbl_id<n>` stratum (the element name, or its position).
#' @keywords internal
#' @noRd
.collect_stacked_ards <- function(elements, strata = NULL) {
  leaves_by_element <- lapply(unname(elements), .collect_ards)

  if (is.null(strata) && length(leaves_by_element) > 1L && .ards_conflict(do.call(c, leaves_by_element))) {
    ids <- names(elements)
    if (is_empty(ids) || !all(nzchar(ids)) || anyDuplicated(ids)) {
      ids <- as.character(seq_along(elements))
    }
    strata <- stats::setNames(
      data.frame(ids, stringsAsFactors = FALSE),
      .next_tbl_id(do.call(c, leaves_by_element))
    )
  }

  if (!is.null(strata)) {
    strata_vars <- rev(names(strata)) # innermost stratum first
    leaves_by_element <- Map(
      \(leaves, i) {
        levels <- rev(as.list(strata[i, , drop = FALSE]))
        lapply(leaves, \(leaf) {
          leaf$variables <- c(leaf$variables, strata_vars)
          leaf$levels <- c(leaf$levels, unname(levels))
          leaf
        })
      },
      leaves_by_element,
      seq_along(leaves_by_element)
    )
  }

  do.call(c, leaves_by_element) %||% list()
}

#' Name of the next `tbl_id` stratum
#'
#' @param leaves (`list`)\cr
#'   leaves of the stacked elements (see `.collect_ards()`).
#'
#' @returns `"tbl_id1"`, or the next number when inner stacks already use one,
#'   as `gtsummary::tbl_stack()` does for its `tbl_id` columns.
#' @keywords internal
#' @noRd
.next_tbl_id <- function(leaves) {
  used <- grep("^tbl_id[0-9]+$", unlist(lapply(leaves, `[[`, "variables")), value = TRUE)
  paste0("tbl_id", max(c(0L, as.integer(sub("^tbl_id", "", used)))) + 1L)
}

#' Turn the strata of leaves into group columns
#'
#' Each stratum variable gets one column for the whole table, after the highest
#' group of all ARDs, innermost first. Rows outside a stratum leave its column
#' empty, so a stratum is in the same column for every row.
#'
#' @param leaves (`list`)\cr
#'   leaves (see `.collect_ards()`).
#'
#' @returns A list of `card` objects.
#' @keywords internal
#' @noRd
.add_strata_columns <- function(leaves) {
  first_group <- max(c(0L, map_int(leaves, \(leaf) .max_group_n(leaf$ard)))) + 1L
  strata_order <- .strata_order(lapply(leaves, `[[`, "variables"))
  lapply(leaves, \(leaf) {
    .add_strata_groups(
      leaf$ard, leaf$variables, leaf$levels,
      groups = first_group + match(leaf$variables, strata_order) - 1L
    )
  })
}

#' Order of the strata variables across leaves
#'
#' Merges the strata of all leaves (each innermost first) into one order that
#' keeps every inner stratum before its outer ones.
#'
#' @param variables (`list`)\cr
#'   strata variables of each leaf, innermost first.
#'
#' @returns A character vector, innermost first.
#' @keywords internal
#' @noRd
.strata_order <- function(variables) {
  order <- character()
  for (vars in variables) {
    for (k in seq_along(vars)) {
      if (vars[k] %in% order) next
      inner <- intersect(vars[seq_len(k - 1L)], order)
      outer <- intersect(vars[-seq_len(k)], order)
      after <- if (!is_empty(inner)) {
        max(match(inner, order))
      } else if (!is_empty(outer)) {
        min(match(outer, order)) - 1L
      } else {
        length(order)
      }
      order <- append(order, vars[k], after = after)
    }
  }
  order
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
#' @param groups (`integer`)\cr
#'   group number of each stratum.
#'
#' @returns The ARD with `group<n>` and `group<n>_level` columns added.
#' @keywords internal
#' @noRd
.add_strata_groups <- function(ard, variables, levels, groups) {
  for (i in seq_along(variables)) {
    group <- paste0("group", groups[i])
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
#' @param leaves (`list`)\cr
#'   leaves (see `.collect_ards()`); a copy has the same ARD and strata.
#'
#' @returns The list without repeated copies.
#' @keywords internal
#' @noRd
.drop_ard_copies <- function(leaves) {
  is_copy <- duplicated(map_chr(leaves, rlang::hash))
  if (any(is_copy)) {
    cli::cli_inform(c(
      "Removed {sum(is_copy)} repeated cop{?y/ies} of an ARD.",
      i = "Split tables, e.g. from {.fun gtsummary::tbl_split_by_rows}, carry a full copy of the ARD in every piece."
    ))
  }
  leaves[!is_copy]
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

  key_cols <- .ard_key_cols(ard)
  keys <- .ard_key_hashes(ard)
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

#' Columns identifying a statistic
#'
#' @param ard (`card`)\cr
#'   an ARD.
#'
#' @returns Names of the group, variable, `context` and `stat_name` columns.
#' @keywords internal
#' @noRd
.ard_key_cols <- function(ard) {
  names(dplyr::select(
    ard,
    cards::all_ard_groups(), cards::all_ard_variables(), dplyr::any_of("context"), "stat_name"
  ))
}

#' One hash per statistic key
#'
#' @param ard (`card`)\cr
#'   an ARD.
#'
#' @returns A character vector, one hash per row.
#' @keywords internal
#' @noRd
.ard_key_hashes <- function(ard) {
  if (nrow(ard) == 0L) {
    return(character())
  }
  do.call(
    mapply,
    c(list(FUN = \(...) rlang::hash(list(...)), SIMPLIFY = TRUE, USE.NAMES = FALSE), as.list(ard[.ard_key_cols(ard)]))
  )
}

#' Whether ARDs report the same statistic with different values
#'
#' @param leaves (`list`)\cr
#'   leaves (see `.collect_ards()`), compared with their strata.
#'
#' @returns A scalar logical.
#' @keywords internal
#' @noRd
.ards_conflict <- function(leaves) {
  if (is_empty(leaves)) {
    return(FALSE)
  }
  ard <- .bind_ards(.add_strata_columns(leaves))
  keys <- .ard_key_hashes(ard)
  key_values <- unique(paste(keys, map_chr(ard$stat, rlang::hash)))
  anyDuplicated(sub(" .*$", "", key_values)) > 0L
}
