#' Hierarchial Table with Grade Groups
#'
#' @description
#'
#' A wrapper function for [gtsummary::tbl_hierarchical()] to calculate rates of toxicity grades with the option to add
#' rows for grade groups, with additional (optional) summary sections at each variable level.
#'
#' To analyze only the highest grade level recorded for each subject, ensure that the toxicity grade variable is an
#' ordered factor variable, with factor levels ordered lowest to highest.
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
#'   Variables from `c(grade, ae, soc)` for which an overall section at that hierarchy level should be computed.
#'   An overall section at the `soc` variable level will have label `"- Any adverse events -"`. An overall section at
#'   the `ae` variable level will have label `"- Overall -"`. An overall row at the `grade` variable level will have
#'   label `"- Any Grade -"`. The default is `everything()`.
#' @param filter (`expression`)\cr
#'   An expression that is used to filter rows of the table. See [gtsummary::filter_hierarchical()] for details.
#' @param add_overall (`logical`)\cr
#'   Whether to add a column with overall summary statistics to the tables. Column will be displayed last, with a
#'   default label of `"**All Active Treatments**  \nN = {style_number(N)}"`. Defaults to `FALSE`.
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
#' @returns a gtsummary table
#' @name tbl_hierarchical_groups
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
#'   ungroup()
#'
#' # Example 1 ----------------------------------------------------
#'
#' ## Order grade variable to analyze *highest* grades per subject
#' ADAE_subset$AETOXGR <- factor(ADAE_subset$AETOXGR, ordered = TRUE)
#'
#' grade_groups <- list(
#'   c("1", "2") ~ "Grade 1-2",
#'   c("3", "4") ~ "Grade 3-4",
#'   "5" ~ "Grade 5"
#' )
#'
#' tbl_hierarchical_groups(
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
#' @rdname tbl_hierarchical_groups
tbl_hierarchical_groups <- function(data,
                                    grade,
                                    ae,
                                    soc,
                                    denominator,
                                    by = NULL,
                                    id = "USUBJID",
                                    include_overall = everything(),
                                    statistic = everything() ~ "{n} ({p}%)",
                                    overall_row = FALSE,
                                    label = NULL,
                                    digits = NULL,
                                    sort = "descending",
                                    filter = NULL,
                                    add_overall = FALSE,
                                    grade_groups = list(),
                                    grades_exclude = NULL) {
  # check inputs ---------------------------------------------------------------
  set_cli_abort_call()
  check_not_missing(data)
  check_not_missing(grade)
  check_not_missing(ae)
  check_not_missing(soc)
  check_not_missing(denominator)
  check_list_elements(grade_groups, \(x) is_formula(x))
  cards::process_selectors(data, grade = {{ grade }}, ae = {{ ae }}, soc = {{ soc }}, by = {{ by }}, id = {{ id }})
  variables <- c(soc, ae, grade)
  cards::process_selectors(data[variables], include_overall = {{ include_overall }})

  if (!is.character(grades_exclude)) {
    cli::cli_abort(
      "Grades to exclude from the table specified via {.arg grades_exclude} must be of class {.cls character}."
    )
  }
  if (!all(sapply(grade_groups, is_formula))) {
    cli::cli_abort(
      paste(
        "Each grade group must be specified via a formula where the left-hand side of the formula is a vector",
        "of grades and the right-hand side is the name of the grade group."
      )
    )
  }
  if (length(unique(unlist(grade_groups))) < length(unlist(grade_groups))) {
    cli::cli_abort(
      paste(
        "Grade groups specified via {.arg grade_groups} cannot overlap.",
        "Please ensure that each grade is included in only one grade group."
      )
    )
  }

  filter <- enquo(filter)
  data_list <- list(data)
  lvls <- levels(data[[grade]]) # get levels of grade variable

  # extract default labels
  if (is.null(label)) {
    label <- lapply(variables, \(x) attr(data[[x]], "label") %||% x) |> setNames(variables)
  }

  # ungrouped grades overall sections-------------------------------------------
  # overall section at SOC level (- Any adverse events -)
  if (soc %in% include_overall) {
    data_overall <- data
    data_overall[c(soc, ae)] <- factor("- Any adverse events -")
    data_list <- c(data_list, list(data_overall))
  }

  # overall section for AE variable (- Overall -)
  if (ae %in% include_overall) {
    data_list[[1]] <- data_list[[1]] |>
      mutate(!!ae := "- Overall -") |>
      dplyr::bind_rows(data_list[[1]])
  }

  # ungrouped grades hierarchical summary --------------------------------------
  tbl_list <- lapply(
    rev(seq_along(data_list)),
    function(i) {
      tbl <- tbl_hierarchical(
        data = data_list[[i]],
        variables = variables,
        id = id,
        denominator = denominator,
        by = by,
        statistic = {{ statistic }},
        overall_row = if (i == 1) overall_row else FALSE,
        label = label,
        digits = {{ digits }}
      )

      if (add_overall) {
        tbl <- tbl |> add_overall(col_label = "**All Active Treatments**  \nN = {style_number(N)}", last = TRUE)
      }

      tbl
    }
  )

  # grouped grades hierarchical summary ----------------------------------------
  if (length(grade_groups) > 0) {
    gp_nms <- sapply(seq_along(grade_groups), \(x) grade_groups[[x]][[3]])
    gps <- lapply(seq_along(grade_groups), \(x) (grade_groups[[x]][[2]] |> eval())) |> setNames(gp_nms)

    tbl_list <- c(
      tbl_list,
      lapply(
        rev(seq_along(data_list)),
        function(i) {
          data_i <- data_list[[i]]

          # replace grades with their grade groups
          data_i[[grade]] <-
            do.call(
              dplyr::case_match,
              args = c(list(.x = data_i[[grade]]), grade_groups)
            ) |>
            factor(levels = gp_nms, ordered = is.ordered(data[[grade]]))

          tbl <- tbl_hierarchical(
            data = data_i,
            variables = variables,
            id = id,
            denominator = denominator,
            by = by,
            statistic = {{ statistic }},
            label = label,
            digits = {{ digits }}
          ) |>
            suppressMessages()

          if (add_overall) {
            tbl <- tbl |>
              add_overall(col_label = "**All Active Treatments**  \nN = {style_number(N)}", last = TRUE) |>
              suppressMessages()
          }

          tbl
        }
      )
    )

  }

  # Build and format table -----------------------------------------------------
  tbl <- tbl_stack(tbl_list)
  class(tbl) <- class(tbl_list[[1]])
  tbl$cards <- list(tbl_hierarchical = dplyr::bind_rows(lapply(tbl_list, \(x) x$cards$tbl_hierarchical)))
  tbl$inputs <- tbl_list[[1]]$inputs
  tbl <- tbl |> sort_hierarchical(sort)
  tbl$cards$tbl_hierarchical <- tbl$cards$tbl_hierarchical |>
    dplyr::distinct(dplyr::pick(-.data$fmt_fn), .keep_all = TRUE)
  if (!quo_is_null(filter)) tbl <- tbl |> filter_hierarchical({{ filter }})

  # arrange inner variable by level, with all groups prior to their first level
  tbl <- tbl |>
    modify_table_body(
      function(table_body) {
        table_body |>
          mutate(idx = dplyr::row_number()) |>
          dplyr::group_by(dplyr::pick(all_ard_groups(), "variable")) |>
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
                mutate(idx = sort(dat$idx)) |>
                select(-"idx_lvl")
            }
          }) |>
          dplyr::bind_rows() |>
          dplyr::arrange(.data$idx) |>
          select(-"idx")
      }
    )

  # add overall rows for inner variable (- Any Grade -)
  if (grade %in% include_overall) {
    tbl <- tbl |>
      modify_table_body(
        function(table_body) {
          table_body |>
            mutate(idx = dplyr::row_number()) |>
            dplyr::group_by(dplyr::pick(all_ard_groups())) |>
            dplyr::group_split() |>
            map(
              function(x) {
                if (any(x$variable == ae)) {
                  dplyr::bind_rows(
                    x[1, ] |> mutate(across(all_stat_cols(), ~ NA)),
                    x[1, ] |> mutate(variable = grade, label = "- Any Grade -", idx = .data$idx + 0.5),
                    x[-1, ]
                  ) |>
                    dplyr::filter(!.data$label %in% grades_exclude)
                } else {
                  x
                }
              }
            ) |>
            dplyr::bind_rows() |>
            dplyr::arrange(.data$idx) |>
            select(-"idx")
        }
      )
  }

  tbl <- tbl |>
    modify_table_body(
      \(table_body) {
        table_body |>
          # remove duplicated Any AE label row
          dplyr::filter(!(.data$label == "- Any adverse events -" & .data$variable != soc)) |>
          dplyr::rowwise() |>
          # remove statistics from summary rows for outer variables
          mutate(
            across(all_stat_cols(), ~ if (.data$variable %in% c(grade, "..ard_hierarchical_overall..")) . else NA)
          )
      }
    ) |>
    # remove default footnote
    gtsummary::remove_footnote_header(columns = everything())

  tbl
}
