#' AE Analysis of Grade Groups
#'
#' @export
#'
#' @examples
#' ADSL <- random.cdisc.data::cadsl
#' ADAE <- random.cdisc.data::cadae
#'
#' grade_groups <- list(
#'   c("1", "2") ~ "Grade 1-2",
#'   c("3", "4") ~ "Grade 3-4",
#'   "5" ~ "Grade 5"
#' )
#'
#' tbl_hierarchical_groups(
#'   ADAE |> mutate(AETOXGR = factor(AETOXGR, ordered = TRUE)),
#'   variables = c(AEBODSYS, AEDECOD, AETOXGR),
#'   level_groups = grade_groups,
#'   levels_exclude = "5",
#'   id = USUBJID,
#'   denominator = ADSL,
#'   by = ACTARM,
#'   labels = list(
#'     AEBODSYS = "MedDRA System Organ Class",
#'     AEDECOD = "MedDRA Preferred Term",
#'     AETOXGR = "Grade"
#'   )
#' )
tbl_hierarchical_groups <- function(data,
                                    variables,
                                    id,
                                    denominator,
                                    by = NULL,
                                    include = dplyr::last(variables),
                                    include_overall = everything(),
                                    statistic = everything() ~ "{n} ({p}%)",
                                    labels = NULL,
                                    digits = NULL,
                                    sort = "alphanumeric",
                                    filter = NULL,
                                    level_groups = list(),
                                    levels_exclude = NULL) {
  # check inputs ---------------------------------------------------------------
  set_cli_abort_call()
  cards::process_selectors(data, variables = {{ variables }}, by = {{ by }}, id = {{ id }})
  cards::process_selectors(data[variables], include = {{ include }}, include_overall = {{ include_overall }})

  filter <- enquo(filter)
  var <- dplyr::last(variables)
  ord <- is.ordered(data[[var]])
  lvls <- levels(data[[var]])
  include_all <- union(include, variables)

  if (is.null(labels)) {
    labels <- lapply(variables, \(x) attr(data[[x]], "label") %||% x) |> setNames(variables)
  }

  # ungrouped overall summary -------
  data_list <- list(data)

  if (variables[1] %in% include_overall) {
    data_overall <- data
    data_overall[head(variables, -1)] <- "- Any adverse events -"
    data_list <- c(data_list, list(data_overall))
  }

  for (v in setdiff(include_overall, variables[c(1, length(variables))])) {
    data_list[[1]] <- data_list[[1]] |>
      mutate(!!v := "- Overall -") |>
      dplyr::bind_rows(data_list[[1]])
  }

  # ungrouped data -----
  tbl_list <- lapply(
    rev(seq_along(data_list)),
    function(i) {
      tbl_hierarchical(
        data_list[[i]],
        variables,
        id,
        denominator,
        by,
        include_all,
        {{ statistic }},
        overall_row = FALSE,
        labels,
        {{ digits }}
      )
    }
  )

  # grouped data -------
  if (length(level_groups) > 0) {
    ## groups mutually exclusive?
    ## at least 2 vars

    gp_nms <- sapply(seq_along(level_groups), \(x) level_groups[[x]][[3]])
    gps <- lapply(seq_along(level_groups), \(x) (level_groups[[x]][[2]] |> eval())) |> setNames(gp_nms)

    tbl_list <- c(
      tbl_list,
      lapply(
        rev(seq_along(data_list)),
        function(i) {
          data <- data_list[[i]]
          data[[var]] <-
            do.call(
              dplyr::case_match,
              args = c(list(.x = data[[var]]), level_groups)
            ) |>
            factor(levels = gp_nms, ordered = ord)

          tbl_hierarchical(
            data,
            variables,
            id,
            denominator,
            by,
            include_all,
            {{ statistic }},
            overall_row = FALSE,
            labels,
            {{ digits }}
          ) |>
            suppressMessages()
        }
      )
    )

    tbl <- tbl_stack(tbl_list)
    class(tbl) <- class(tbl_list[[1]])
    tbl$cards <- list(tbl_hierarchical = dplyr::bind_rows(sapply(tbl_list, \(x) x$cards)))
    tbl <- tbl |> sort_hierarchical(sort)
    tbl$cards$tbl_hierarchical <- tbl$cards$tbl_hierarchical |>
      dplyr::distinct(dplyr::pick(-.data$fmt_fn), .keep_all = TRUE)
    if (!quo_is_null(filter)) tbl <- tbl |> filter_hierarchical({{ filter }})

    # arrange with groups prior to their first level
    tbl <- tbl |>
      modify_table_body(
        function(tb) {
          tb |>
            mutate(idx = dplyr::row_number()) |>
            dplyr::group_by(dplyr::pick(all_ard_groups(), "variable")) |>
            dplyr::group_split() |>
            map(function(dat) {
              if (dat$variable[1] != var) {
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
                  dplyr::arrange("idx_lvl", "idx") |>
                  mutate(idx = seq(min(.data$idx), max(.data$idx))) |>
                  select(-"idx_lvl")
              }
            }) |>
            dplyr::bind_rows() |>
            dplyr::arrange("idx") |>
            select(-"idx")
        }
      )
  }

  tbl <- tbl |>
    # Add "- Any Grade -" rows
    modify_table_body(
      function(tb) {
        tb |>
          mutate(idx = dplyr::row_number()) |>
          dplyr::group_by(dplyr::pick(all_ard_groups())) |>
          dplyr::group_split() |>
          map(
            function(x) {
              if (any(x$variable == variables[length(variables) - 1])) {
                dplyr::bind_rows(
                  x[1, ] |>
                    mutate(across(all_stat_cols(), ~ if (.data$variable %in% include) . else NA)),
                  x[1, ] |>
                    mutate(variable = var, label = "- Any Grade -", idx = .data$idx + 0.5),
                  x[-1, ]
                ) |>
                  dplyr::filter(!.data$label %in% levels_exclude)
              } else {
                x
              }
            }
          ) |>
          dplyr::bind_rows() |>
          dplyr::filter(!(.data$label == "- Any adverse events -" & .data$idx == 2)) |>
          dplyr::arrange("idx") |>
          select(-"idx")
      }
    ) |>
    # Remove statistics from rows for variables not in include
    modify_table_body(
      \(x) {
        x |>
          dplyr::rowwise() |>
          mutate(across(all_stat_cols(), ~ if (.data$variable %in% include) . else NA))
      }
    )

  tbl
}
