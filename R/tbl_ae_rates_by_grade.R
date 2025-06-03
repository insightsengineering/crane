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
#' @param grade ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   A single column name with the toxicity grade levels.
#' @param ae ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   A single column name with the adverse event terms.
#' @param soc ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   A single column name with the system organ class.
#' @param include_overall ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Variables from `c(soc, ae)` for which an overall section at that hierarchy level should be computed.
#'   An overall section at the `soc` variable level will have label `"- Any adverse events -"`. An overall section at
#'   the `ae` variable level will have label `"- Overall -"`. The default is `c(soc, ae)`.
#' @param filter (`expression`)\cr
#'   An expression that is used to filter rows of the table. See the Details section below for more information.
#' @param grade_groups (`list`)\cr
#'   A list of formulas each corresponding to a grade group for which rates should be calculated. Grade groups must be
#'   mutually exclusive, i.e. each grade cannot be assigned to more than one grade group. To specify each grade group,
#'   create a formula with a vector of grades included on the left-hand side of the formula and the name of the grade
#'   on the right-hand side, e.g. `c("1", "2") ~ "Grade 1-2"`.
#' @param grades_exclude (`character`)\cr
#'   A vector of grades to omit individual rows for when printing the table. These grades will still be used when
#'   computing overall totals and grade group totals. For example, to avoid duplication, if a grade group is defined as
#'   `"5" ~ "Grade 5"`, the individual rows corresponding to grade 5 can be excluded by setting `grades_exclude = "5"`.
#'
#' @details
#' When using the `filter` argument, all grade and grade group rows that meet the given filtering criteria will be kept.
#' If a grade group row meets the filter criteria but the individual grade rows within the group do not, the grade group
#' row will still be kept. If all rows in a given table hierarchy section have been filtered out, the summary row of
#' that section will also be excluded. For example, if `soc = AESOC` and no grade or grade group rows in the section
#' corresponding to `AESOC = "CARDIAC DISORDERS"` meet the filtering criteria, the `"CARDIAC DISORDERS"` summary row and
#' all subsequent rows in this section will be removed from the table. Filtering out rows does not exclude the records
#' corresponding to these rows from being included in rate calculations for overall sections.
#'
#' See the `filter` argument of [gtsummary::filter_hierarchical()] for more details and examples.
#'
#' @returns a gtsummary table of class `"tbl_hierarchical"`.
#' @name tbl_ae_rates_by_grade
#'
#' @examples
#' ADSL <- cards::ADSL |> mutate(TRTA = ARM)
#' ADAE_subset <- cards::ADAE |>
#'   dplyr::filter(
#'     AESOC %in% unique(cards::ADAE$AESOC)[1:5],
#'     AETERM %in% unique(cards::ADAE$AETERM)[1:10]
#'   )
#'
#' ## Add AETOXGR variable to example dataset
#' set.seed(1)
#' ADAE_subset <- ADAE_subset |>
#'   dplyr::rowwise() |>
#'   mutate(
#'     AETOXGR = dplyr::case_when(
#'       AESEV == "MILD" ~ sample(1:2, 1),
#'       AESEV == "MODERATE" ~ sample(3:4, 1),
#'       AESEV == "SEVERE" ~ 5,
#'     ) |> factor(levels = 1:5)
#'   ) |>
#'   dplyr::ungroup()
#'
#' # Example 1 ----------------------------------
#' grade_groups <- list(
#'   c("1", "2") ~ "Grade 1-2",
#'   c("3", "4") ~ "Grade 3-4",
#'   "5" ~ "Grade 5"
#' )
#'
#' tbl_ae_rates_by_grade(
#'   ADAE_subset,
#'   grade = AETOXGR,
#'   ae = AEDECOD,
#'   soc = AEBODSYS,
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
NULL

#' @export
#' @rdname tbl_ae_rates_by_grade
tbl_ae_rates_by_grade <- function(data,
                                  grade,
                                  ae,
                                  soc,
                                  denominator,
                                  by = NULL,
                                  id = "USUBJID",
                                  include_overall = all_of(c(soc, ae)),
                                  statistic = everything() ~ "{n} ({p}%)",
                                  label = NULL,
                                  digits = NULL,
                                  sort = "descending",
                                  filter = NULL,
                                  grade_groups = list(),
                                  grades_exclude = NULL) {
  # check inputs ---------------------------------------------------------------
  set_cli_abort_call()
  check_not_missing(data)
  check_not_missing(grade)
  check_not_missing(ae)
  check_not_missing(soc)
  check_not_missing(denominator)
  check_class(grades_exclude, "character", allow_empty = TRUE)
  cards::process_selectors(data,
    grade = {{ grade }},
    ae = {{ ae }},
    soc = {{ soc }},
    by = {{ by }},
    id = {{ id }}
  )
  cards::process_selectors(data[c(soc, ae, grade)], include_overall = {{ include_overall }})
  filter <- enquo(filter)

  # save function inputs
  tbl_ae_rates_by_grade_inputs <- as.list(environment())

  variables <- c(soc, ae, grade)
  gp_nms <- NULL

  if (!all(sapply(grade_groups, is_formula))) {
    cli::cli_abort(
      paste(
        "Each grade group must be specified via a {.cls formula} where the left-hand side of the formula is a vector",
        "of grades and the right-hand side is the name of the grade group.",
        'For example, {.code c("3", "4") ~ "Grade 3-4"}'
      )
    )
  }
  if (!is_empty(grade_groups)) {
    gp_nms <- sapply(seq_along(grade_groups), \(x) grade_groups[[x]][[3]]) # get names of grade groups
    gps <- lapply(seq_along(grade_groups), \(x) (grade_groups[[x]][[2]] |> eval())) |> stats::setNames(gp_nms)

    if (length(unique(unlist(gps))) < length(unlist(gps))) {
      cli::cli_abort(
        paste(
          "Grade groups specified via {.arg grade_groups} cannot overlap.",
          "Please ensure that each grade is included in only one grade group."
        )
      )
    }
  }
  if (!is_empty(grades_exclude) & !is_empty(setdiff(grades_exclude, unique(data[[grade]])))) {
    not_a_grade <- setdiff(grades_exclude, unique(data[[grade]]))
    cli::cli_abort(
      paste(
        "Grade(s) {.val {not_a_grade}} supplied to {.arg grades_exclude} {?is/are} invalid.",
        "All grades specified via {.arg grades_exclude} must be levels of {.val {grade}}."
      )
    )
  }
  if (!is.factor(data[[grade]])) {
    data[[grade]] <- factor(data[[grade]], ordered = TRUE)
    vec <- cli::cli_vec(
      levels(data[[grade]]),
      style = list("vec-sep" = " < ", "vec-sep2" = " < ", "vec-last" = " < ", "vec-trunc" = 3)
    )
    cli::cli_inform(
      paste(
        "The {.arg grade} variable {.val {grade}} has been converted to an ordered {.cls factor}",
        "with levels: {.val {vec}}"
      )
    )
  }

  lvls <- levels(data[[grade]]) # get levels of grade variable
  if (!is.ordered(data[[grade]])) data[[grade]] <- factor(data[[grade]], levels = lvls, ordered = TRUE)
  data_ungrouped <- data

  # extract default labels
  if (is.null(label)) {
    label <- lapply(variables, \(x) attr(data[[x]], "label") %||% x) |> stats::setNames(variables)
  }

  # ungrouped grades overall sections-------------------------------------------
  # overall section at SOC level (- Any adverse events -)
  if (soc %in% include_overall) {
    data_ungrouped <-
      dplyr::bind_rows(
        data_ungrouped |>
          dplyr::mutate(across(all_of(c(soc, ae)), ~ factor("- Any adverse events -"))),
        data_ungrouped
      )
  }

  # overall section for AE variable (- Overall -)
  if (ae %in% include_overall) {
    data_ungrouped <- data_ungrouped |>
      dplyr::filter(.data[[soc]] != "- Any adverse events -") |>
      dplyr::mutate(!!ae := "- Overall -") |>
      dplyr::bind_rows(data_ungrouped)
  }

  # ungrouped grades hierarchical summary --------------------------------------
  if (!is_empty(setdiff(lvls, grades_exclude))) {
    tbl_ungrouped <-
      gtsummary::tbl_hierarchical(
        data = data_ungrouped,
        variables = all_of(variables),
        id = all_of(id),
        denominator = denominator,
        by = all_of(by),
        statistic = {{ statistic }},
        label = label,
        digits = {{ digits }}
      )
  }

  # grouped grades hierarchical summary ----------------------------------------
  if (!is_empty(grade_groups)) {
    data_grouped <- data_ungrouped

    # replace grades with their grade groups
    data_grouped[[grade]] <-
      do.call(
        dplyr::case_match,
        args = c(list(.x = data_grouped[[grade]]), grade_groups)
      ) |>
      factor(levels = gp_nms, ordered = TRUE)

    tbl_grouped <-
      gtsummary::tbl_hierarchical(
        data = data_grouped,
        variables = all_of(variables),
        id = all_of(id),
        denominator = denominator,
        by = all_of(by),
        statistic = {{ statistic }},
        label = label,
        digits = {{ digits }}
      ) |>
      suppressMessages()

    if (is_empty(setdiff(lvls, grades_exclude))) {
      # if all individual grades excluded, only grade groups included in final table
      tbl_final <- tbl_grouped
    } else {
      # if both grouped and ungrouped grade rows exist, combine the two tables
      tbl_list <- list(tbl_ungrouped, tbl_grouped)
      tbl_final <- gtsummary::tbl_stack(tbl_list)

      # set class to tbl_hierarchical
      class(tbl_final) <- c("tbl_hierarchical", "gtsummary")

      # get original inputs
      tbl_final$inputs <- tbl_ae_rates_by_grade_inputs

      # build tbl$cards, needed for sorting/filtering
      tbl_final$cards <- list(
        tbl_hierarchical = dplyr::bind_rows(lapply(tbl_list, \(x) x$cards$tbl_hierarchical)) |>
          dplyr::distinct(dplyr::pick(-"fmt_fn"), .keep_all = TRUE)
      )
    }
  } else {
    # if no grade groups, only individual grades included in final table
    tbl_final <- tbl_ungrouped
  }

  # format the final table -----------------------------------------------------

  # apply sorting/filtering, if specified
  tbl_final <- tbl_final |> gtsummary::sort_hierarchical(sort)
  if (!quo_is_null(filter)) tbl_final <- tbl_final |> gtsummary::filter_hierarchical(filter = !!filter)

  # arrange grade rows by level, with all groups prior to their first level
  tbl_final <- tbl_final |>
    gtsummary::modify_table_body(
      function(table_body) {
        table_body |>
          dplyr::mutate(idx = dplyr::row_number()) |>
          dplyr::group_by(dplyr::pick(cards::all_ard_groups(), "variable")) |>
          dplyr::group_split() |>
          map(function(dat) {
            if (dat$variable[1] != grade) {
              dat
            } else {
              dat$idx_lvl <- NA
              if (any(unlist(dat$label) %in% lvls)) {
                dat$idx_lvl[is.na(dat$idx_lvl) & !unlist(dat$label) %in% gp_nms] <-
                  sapply(
                    dat$label[is.na(dat$idx_lvl) & !unlist(dat$label) %in% gp_nms],
                    \(x) which(lvls == unlist(x))
                  )
              }
              if (any(unlist(dat$label) %in% gp_nms)) {
                dat$idx_lvl[unlist(dat$label) %in% gp_nms] <-
                  sapply(
                    dat$label[unlist(dat$label) %in% gp_nms],
                    \(x) min(which(lvls %in% gps[[unlist(x)]])) - 0.5
                  )
              }
              dat |>
                dplyr::arrange(.data$idx_lvl, .data$idx) |>
                dplyr::mutate(idx = sort(dat$idx)) |>
                dplyr::select(-"idx_lvl")
            }
          }) |>
          dplyr::bind_rows() |>
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
              all_stat_cols(),
              ~ if (
                .data$variable %in% c(ae, "..ard_hierarchical_overall..") |
                  .data$label_grade %in% c(lvls, gp_nms) |
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
      fmt_fun = ~ ifelse(. == "0 (0.0%)", "0", .),
      columns = all_stat_cols()
    ) |>
    # update header label
    gtsummary::modify_header(
      label = paste0(
        "**", label[[soc]], "**  \n",
        paste0(rep("\U00A0", 4L), collapse = ""),
        "**", label[[ae]], "**"
      ),
      label_grade = paste0("\U00A0  \n**", label[[grade]], "**")
    )

  # further indent grade level labels within grade groups
  if (!is_empty(grade_groups)) {
    tbl_final <- tbl_final |>
      gtsummary::modify_indent(
        columns = "label_grade", rows = .data$variable == grade & .data$label_grade %in% unlist(gps), indent = 4L
      )
  }

  # return final table ---------------------------------------------------------
  tbl_final$call_list <- list(tbl_ae_rates_by_grade = match.call())

  tbl_final
}
