#' AE Rates by Highest Toxicity Grade
#'
#' @description
#'
#' A wrapper function for [gtsummary::tbl_hierarchical()] to calculate rates of highest toxicity grades with the options
#' to add rows for grade groups and additional summary sections at each variable level.
#'
#' Only the highest grade level recorded for each subject will be analyzed. Prior to running the function, ensure that
#' the toxicity grade variable (`grade`) is a factor variable, with factor levels ordered lowest to highest.
#'
#' Grades will appear in rows in the order of the factor levels given, with each grade group appearing prior to the
#' first level in its group.
#'
#' @inheritParams gtsummary::tbl_hierarchical
#' @inheritParams gtsummary::sort_hierarchical
#' @inheritParams gtsummary::add_overall.tbl_hierarchical
#' @param variables ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   A character vector or tidy-selector of 3 columns in `data` specifying a system organ class variable,
#'   an adverse event terms variable, and a toxicity grade level variable, respectively.
#' @param include_overall ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Variables from `variables` for which an overall section at that hierarchy level should be computed.
#'   An overall section at the SOC variable level will have label `"- Any adverse events -"`. An overall section at
#'   the AE term variable level will have label `"- Overall -"`. If the grade level variable is included it has no
#'   effect. The default is `everything()`.
#' @param filter (`expression`)\cr
#'   An expression that is used to filter rows of the table. Filter will be applied to the second variable (adverse
#'   event terms) specified via `variables`. See the Details section below for more information.
#' @param grade_groups (`named list`)\cr
#'   A named list of grade groups for which rates should be calculated. Grade groups must be mutually exclusive, i.e.
#'   each grade cannot be assigned to more than one grade group. Each grade group must be specified in the list as a
#'   character vector of the grades included in the grade group, named with the corresponding name of the grade group,
#'   e.g. `"Grade 1-2" = c("1", "2")`.
#' @param grades_exclude (`character`)\cr
#'   A vector of grades to omit individual rows for when printing the table. These grades will still be used when
#'   computing overall totals and grade group totals. For example, to avoid duplication, if a grade group is defined as
#'   `"Grade 5" = "5"`, the individual rows corresponding to grade 5 can be excluded by setting `grades_exclude = "5"`.
#' @param keep_zero_rows (`logical`)\cr
#'   Whether rows containing zero rates across all columns should be kept. If `FALSE`, this filter will be applied
#'   prior to any filters specified via the `filter` argument which may still remove these rows. Defaults to `FALSE`.
#' @param x (`tbl_hierarchical_rate_by_grade`)\cr
#'   A gtsummary table of class `'tbl_hierarchical_rate_by_grade'`.
#'
#' @details
#' When using the `filter` argument, the filter will be applied to the second variable from `variables`, i.e. the
#' adverse event terms variable. If an AE does not meet the filtering criteria, the AE overall row as well as all grade
#' and grade group rows within an AE section will be excluded from the table. Filtering out AEs does not exclude the
#' records corresponding to these filtered out rows from being included in rate calculations for overall sections. If
#' all AEs for a given SOC have been filtered out, the SOC will be excluded from the table. If all AEs are filtered out
#' and the SOC variable is included in `include_overall` the `- Any adverse events -` section will still be kept.
#'
#' See [gtsummary::filter_hierarchical()] for more details and examples.
#'
#' @returns a gtsummary table of class `"tbl_hierarchical_rate_by_grade"`.
#' @name tbl_hierarchical_rate_by_grade
#'
#' @examples
#' ADSL <- cards::ADSL
#' ADAE_subset <- cards::ADAE |>
#'   dplyr::filter(
#'     AESOC %in% unique(cards::ADAE$AESOC)[1:5],
#'     AETERM %in% unique(cards::ADAE$AETERM)[1:10]
#'   )
#'
#' grade_groups <- list(
#'   "Grade 1-2" = c("1", "2"),
#'   "Grade 3-4" = c("3", "4"),
#'   "Grade 5" = "5"
#' )
#'
#' # Example 1 ----------------------------------
#' tbl_hierarchical_rate_by_grade(
#'   ADAE_subset,
#'   variables = c(AEBODSYS, AEDECOD, AETOXGR),
#'   denominator = ADSL,
#'   by = TRTA,
#'   label = list(
#'     AEBODSYS = "MedDRA System Organ Class",
#'     AEDECOD = "MedDRA Preferred Term",
#'     AETOXGR = "Grade"
#'   ),
#'   grade_groups = grade_groups,
#'   grades_exclude = "5"
#' )
#'
#' # Example 2 ----------------------------------
#' # Filter: Keep AEs with an overall prevalence of greater than 10%
#' tbl_hierarchical_rate_by_grade(
#'   ADAE_subset,
#'   variables = c(AEBODSYS, AEDECOD, AETOXGR),
#'   denominator = ADSL,
#'   by = TRTA,
#'   grade_groups = list("Grades 1-2" = c("1", "2"), "Grades 3-5" = c("3", "4", "5")),
#'   filter = sum(n) / sum(N) > 0.10
#' ) |>
#'   add_overall(last = TRUE)
NULL

#' @export
#' @rdname tbl_hierarchical_rate_by_grade
tbl_hierarchical_rate_by_grade <- function(data,
                                           variables,
                                           denominator,
                                           by = NULL,
                                           id = "USUBJID",
                                           include_overall = everything(),
                                           statistic = everything() ~ "{n} ({p}%)",
                                           label = NULL,
                                           digits = NULL,
                                           sort = "descending",
                                           filter = NULL,
                                           grade_groups = list(),
                                           grades_exclude = NULL,
                                           keep_zero_rows = FALSE) {
  # check inputs ---------------------------------------------------------------
  set_cli_abort_call()
  check_not_missing(data)
  check_not_missing(variables)
  check_not_missing(denominator)
  check_class(grades_exclude, "character", allow_empty = TRUE)
  cards::process_selectors(data,
    variables = {{ variables }},
    by = {{ by }},
    id = {{ id }}
  )
  cards::process_selectors(data[variables], include_overall = {{ include_overall }})
  check_length(variables, 3)
  filter <- enquo(filter)

  # save function inputs
  tbl_hierarchical_rate_by_grade_inputs <- as.list(environment())
  soc <- variables[1]
  ae <- variables[2]
  grade <- variables[3]

  if (!is_empty(grade_groups) && !(is_named(grade_groups) && all(sapply(grade_groups, is_character)))) {
    cli::cli_abort(
      paste(
        "Grade groups must be specified via a named list where each list element is a character vector of the",
        "grades to include in the grade group and each name is the corresponding name of the grade group.",
        'For example, {.code "Grade 3-4" = c("3", "4")}.'
      )
    )
  }
  if (!is_empty(grade_groups)) {
    if (length(unique(unlist(grade_groups))) < length(unlist(grade_groups))) {
      cli::cli_abort(
        paste(
          "Grade groups specified via {.arg grade_groups} cannot overlap.",
          "Please ensure that each grade is included in at most one grade group."
        )
      )
    }
  }
  if (!is.factor(data[[grade]])) {
    label_grade <- attr(data[[grade]], "label")
    if (!is_empty(grade_groups) && length(unlist(grade_groups)) >= length(unique(data[[grade]]))) {
      # if all grades are in a grade group, use the defined grade group order to order factor levels
      data[[grade]] <- factor(data[[grade]], levels = unlist(grade_groups))
    } else {
      # otherwise, use the default order and append any expected (grouped) grades that do not appear
      data[[grade]] <- factor(data[[grade]])
      if (!is_empty(setdiff(unlist(grade_groups), levels(data[[grade]])))) {
        levels(data[[grade]]) <- union(levels(data[[grade]]), unlist(grade_groups))
      }
    }
    attr(data[[grade]], "label") <- label_grade
    vec <- cli::cli_vec(
      levels(data[[grade]]),
      style = list("vec-sep" = " < ", "vec-sep2" = " < ", "vec-last" = " < ", "vec-trunc" = 3)
    )
    cli::cli_inform("{.var {grade}}: {.val {vec}}")
  }
  if (!is_empty(grades_exclude) & !is_empty(setdiff(grades_exclude, levels(data[[grade]])))) {
    not_a_grade <- setdiff(grades_exclude, levels(data[[grade]]))
    cli::cli_abort(
      paste(
        "Grade(s) {.val {not_a_grade}} supplied to {.arg grades_exclude} {?is/are} invalid.",
        "All grades specified via {.arg grades_exclude} must be levels of {.val {grade}}."
      )
    )
  }

  # fill in unspecified statistics/labels/digits
  cards::process_formula_selectors(data[variables], statistic = statistic, digits = digits, label = label)
  cards::fill_formula_selectors(
    data[variables],
    statistic = eval(formals(gtsummary::tbl_hierarchical)[["statistic"]])
  )
  cards::fill_formula_selectors(
    data[variables],
    label = lapply(variables, \(x) attr(data[variables][[x]], "label") %||% x) |> stats::setNames(variables)
  )
  digits <-
    gtsummary::assign_summary_digits(
      data = data,
      statistic = statistic,
      type = rep_named(names(statistic), list("categorical")),
      digits = digits
    )
  digits <- lapply(digits, FUN = \(x) x[intersect(names(x), c("n", "N", "p"))])

  # get levels of grade variable
  lvls <- levels(data[[grade]])

  # add overall sections -------------------------------------------------------
  # overall section at SOC level (- Any adverse events -)
  if (soc %in% include_overall) {
    data <-
      dplyr::bind_rows(
        data |> dplyr::mutate(across(all_of(c(soc, ae)), ~ factor("- Any adverse events -"))),
        data
      )
  }

  # overall section for AE term level (- Overall -)
  if (ae %in% include_overall) {
    data <- data |>
      dplyr::filter(.data[[soc]] != "- Any adverse events -") |>
      dplyr::mutate(!!ae := "- Overall -") |>
      dplyr::bind_rows(data)
  }

  # ungrouped grades hierarchical summary --------------------------------------
  if (!is_empty(setdiff(lvls, grades_exclude))) {
    ard_ungrouped <- .ard_rate_by_grade_ordered(data, variables, by, id, denominator)

    # remove grades not part of any grade groups - these will be duplicated in ard_grouped
    if (!is_empty(grade_groups) && !is_empty(setdiff(lvls, unlist(grade_groups)))) {
      ard_ungrouped <- ard_ungrouped |>
        dplyr::filter(!.data[["variable_level"]] %in% setdiff(lvls, unlist(grade_groups)))
    }
  }

  # grouped grades hierarchical summary ----------------------------------------
  if (!is_empty(grade_groups)) {
    # generate grade groups ARD
    ard_grouped <- .ard_rate_by_grade_ordered(data, variables, by, id, denominator, grade_groups) |>
      suppressMessages()
    ard_args <- attr(ard_grouped, "args") # needed for sorting/filtering

    if (is_empty(setdiff(lvls, grades_exclude))) {
      # if all individual grades excluded, only grade groups included in final ARD
      ard_final <- ard_grouped
    } else {
      # if both grouped and ungrouped grade rows exist, combine the two ARDs
      ard_final <- cards::bind_ard(list(ard_ungrouped, ard_grouped), .quiet = TRUE)
    }
  } else {
    # if no grade groups, only individual grades included in final ARD
    ard_final <- ard_ungrouped
    ard_args <- attr(ard_ungrouped, "args") # needed for sorting/filtering
  }

  # format the final ARD -------------------------------------------------------
  # if `keep_zero_rows` is FALSE, filter all-zero rows out
  if (!keep_zero_rows) {
    n <- NULL # fix warning for undefined variable
    ard_final <- ard_final |> cards::filter_ard_hierarchical(filter = sum(n) > 0)
  }

  # apply filtering
  if (!quo_is_null(filter)) {
    ard_final <- ard_final |>
      cards::filter_ard_hierarchical(filter = !!filter, var = all_of(ae))
  }

  # apply digits
  ard_final <-
    ard_final |>
    dplyr::rows_update(
      imap(
        digits,
        ~ enframe(.x, "stat_name", "fmt_fun") |>
          dplyr::mutate(variable = .y)
      ) |>
        dplyr::bind_rows(),
      by = c("variable", "stat_name"),
      unmatched = "ignore"
    ) |>
    cards::apply_fmt_fun()

  # generate table from the final ARD ------------------------------------------
  tbl_final <-
    gtsummary::tbl_ard_hierarchical(
      cards = ard_final,
      variables = all_of(variables),
      by = all_of(by),
      statistic = statistic,
      label = label
    )

  # apply sorting
  tbl_final <- tbl_final |> gtsummary::sort_hierarchical(sort)

  # format the final table -----------------------------------------------------
  # arrange grade rows by level, with all groups prior to their first level
  tbl_final <- tbl_final |>
    gtsummary::modify_table_body(
      function(table_body) {
        table_body |>
          # save original row indices to keep outer hierarchy sections in the same order
          dplyr::mutate(idx = dplyr::row_number()) |>
          dplyr::group_by(dplyr::pick(cards::all_ard_groups(), "variable")) |>
          dplyr::group_split() |>
          map(function(dat) {
            # only rearrange at the grade/grade group level
            if (dat$variable[1] != grade) {
              dat
            } else {
              dat$idx_grade <- NA # initialize variable to track new order of grade row indices
              if (any(unlist(dat$label) %in% lvls)) {
                # if individual grade levels present, arrange them in their original order (unsorted)
                dat$idx_grade[is.na(dat$idx_grade) & !unlist(dat$label) %in% names(grade_groups)] <-
                  sapply(
                    dat$label[is.na(dat$idx_grade) & !unlist(dat$label) %in% names(grade_groups)],
                    \(x) which(lvls == unlist(x))
                  )
              }
              if (any(unlist(dat$label) %in% names(grade_groups))) {
                # if grade groups present, arrange so each grade group row
                # appears directly above the first grade in the group
                dat$idx_grade[unlist(dat$label) %in% names(grade_groups)] <-
                  sapply(
                    dat$label[unlist(dat$label) %in% names(grade_groups)],
                    \(x) min(which(lvls %in% grade_groups[[unlist(x)]])) - 0.5
                  )
              }
              # arrange rows at grade level using new order, soc/ae levels using their original (sorted) order
              dat |>
                dplyr::arrange(.data$idx_grade, .data$idx) |>
                # save new ordering
                dplyr::mutate(idx = sort(dat$idx)) |>
                dplyr::select(-"idx_grade")
            }
          }) |>
          dplyr::bind_rows() |>
          # apply new ordering
          dplyr::arrange(.data$idx) |>
          dplyr::select(-"idx")
      }
    )

  tbl_final <- tbl_final |>
    gtsummary::modify_table_body(
      \(table_body) {
        table_body |>
          # remove duplicate Any AE label row
          dplyr::filter(!(.data$label == "- Any adverse events -" & .data$variable != soc)) |>
          # remove soc summary rows if all sub-rows filtered out
          dplyr::filter(!(.data$variable == soc & dplyr::lead(.data$variable) %in% c(soc, NA))) |>
          # remove rows for grades in grades_exclude
          dplyr::filter(!.data$label %in% grades_exclude) |>
          dplyr::rowwise() |>
          # add label_grade column to display grade labels
          dplyr::mutate(
            label_grade = dplyr::case_when(
              .data$variable == grade ~ label,
              .data$variable == ae | label == "- Any adverse events -" ~ "- Any Grade -",
              .default = ""
            ),
            .after = "label"
          ) |>
          # remove grade levels from label column
          dplyr::mutate(label = if (.data$variable == grade) "" else label) |>
          # remove statistics from summary rows if not an overall row
          dplyr::mutate(
            across(
              gtsummary::all_stat_cols(),
              ~ if (
                .data$variable %in% c(ae, "..ard_hierarchical_overall..") |
                  .data$label_grade %in% c(lvls, names(grade_groups)) |
                  .data$label == "- Any adverse events -"
              ) {
                .
              } else {
                NA
              }
            )
          ) |>
          dplyr::ungroup()
      }
    ) |>
    # show label_grade column
    gtsummary::modify_column_unhide("label_grade") |>
    gtsummary::modify_column_alignment("label_grade", align = "left") |>
    # remove default footnote
    gtsummary::remove_footnote_header(columns = everything()) |>
    # convert "0 (0.0%)" to "0"
    gtsummary::modify_post_fmt_fun(
      fmt_fun = ~ ifelse(. %in% c("0 (0.0%)", "0 (NA%)"), "0", .),
      columns = gtsummary::all_stat_cols()
    ) |>
    # update header label
    gtsummary::modify_header(
      label ~ paste0(label[[soc]], "  \n", paste0(rep("\U00A0", 4L), collapse = ""), label[[ae]]),
      label_grade ~ label[[grade]],
      gtsummary::all_stat_cols() ~ "{level}  \nN = {n}"
    )

  # indent grade level labels within grade groups
  if (!is_empty(grade_groups)) {
    tbl_final <- tbl_final |>
      gtsummary::modify_indent(
        columns = "label_grade", rows = .data$variable == grade & .data$label_grade %in% unlist(grade_groups),
        indent = 4L
      )
  }

  # return final table ---------------------------------------------------------
  tbl_final$call_list <- list(tbl_hierarchical_rate_by_grade = match.call())
  tbl_final$cards <- list(
    tbl_hierarchical_rate_by_grade = list(tbl_hierarchical = tbl_final$cards$tbl_ard_hierarchical)
  )
  tbl_final$inputs <- tbl_hierarchical_rate_by_grade_inputs

  tbl_final |>
    structure(class = c("tbl_hierarchical_rate_by_grade", "gtsummary"))
}

# function to generate the hierarchical ARD(s) by highest grade
.ard_rate_by_grade_ordered <- function(data,
                                       variables,
                                       by,
                                       id,
                                       denominator,
                                       grade_groups = NULL) {
  # get name of grade variable
  grade <- dplyr::last(variables)

  if (!is_empty(grade_groups)) {
    # if any grade groups present, replace grades with their grade groups
    data[[grade]] <- do.call(fct_collapse, args = c(list(f = data[[grade]]), grade_groups))
  }

  # move grade variable to `by` to get rates by highest grade
  cards_ord <- cards::ard_stack_hierarchical(
    data = data,
    variables = all_of(setdiff(variables, grade)),
    by = all_of(c(by, grade)),
    id = all_of(id),
    denominator = denominator,
    include = all_of(dplyr::nth(variables, -2)),
    total_n = (is_empty(by) && length(variables) == 1)
  ) |>
    suppressMessages()

  # retain original input args
  attr(cards_ord, "args") <- list(by = by, variables = variables, include = variables)

  # update structure to match results for the soc/ae variables
  which_var <- which(names(cards_ord) == "variable")
  which_h <- which(names(cards_ord) == paste0("group", length(by) + 1))
  names(cards_ord) <- names(cards_ord)[
    c(0:(which_h - 1), which_var + 0:1, which_h:(which_var - 1), (which_var + 2):length(names(cards_ord)))
  ]

  # bind `cards_ord` to results for the soc/ae variables
  variables <- setdiff(variables, grade)
  if (!is_empty(by)) {
    # remove summary rows that will be duplicated in the following ARD
    cards_ord <- cards_ord |>
      dplyr::filter(.data$group1 == by[1] | .data$context == "total_n")
  }

  # get remaining rates for soc/ae variables
  cards <- cards::ard_stack_hierarchical(
    data = data,
    variables = all_of(variables),
    by = any_of(by),
    id = all_of(id),
    denominator = denominator,
    total_n = is_empty(by)
  ) |>
    suppressMessages()

  # bind ARDs for ordered and non-ordered results and return
  cards::bind_ard(cards_ord, cards, .quiet = TRUE)
}

#' @rdname tbl_hierarchical_rate_by_grade
#' @export
add_overall.tbl_hierarchical_rate_by_grade <- asNamespace("gtsummary")[["add_overall.tbl_hierarchical"]]
