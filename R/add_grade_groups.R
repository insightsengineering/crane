#' AE Analysis of Grade Groups
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
                                    level_groups = list(),
                                    levels_exclude = NULL,
                                    id,
                                    denominator,
                                    by = NULL,
                                    include = last(variables),
                                    include_overall = everything(),
                                    statistic = everything() ~ "{n} ({p}%)",
                                    labels = NULL,
                                    digits = NULL) {
  # check inputs ---------------------------------------------------------------
  set_cli_abort_call()
  cards::process_selectors(data, variables = {{ variables }}, by = {{ by }}, id = {{ id }})
  cards::process_selectors(data[variables], include_overall = {{ include_overall }})

  var <- last(variables)
  ord <- is.ordered(data[[var]])
  by_cols <- if (!is.null(by)) c("group1", "group1_level") else NULL
  lvls <- levels(data[[var]])#droplevels(data_no_gps[[var]]))

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
      bind_rows(data_list[[1]])
  }

  # ungrouped data -----
  ard_list <- lapply(
    seq_along(data_list),
    function(i) {
      data_no_gps <- data_list[[i]] |>
        # drop levels from levels_exclude
        filter(!.data[[var]] %in% levels_exclude)

      tbl <-
        tbl_hierarchical(
          data_no_gps,
          variables,
          id,
          denominator,
          by,
          {{ include }},
          {{ statistic }},
          overall_row = FALSE,
          labels,
          {{ digits }}
        )

      ard <- gather_ard(tbl)[[1]]

      if (i > 1) {
        ard |>
          filter(variable == var) |>
          select(-all_ard_groups("levels"), by_cols, group2_level)
      } else {
        ard
      }
    }
  )

  # grouped data -------
  if (length(level_groups) > 0) {

    ## groups mutually exclusive?
    ## at least 2 vars

    ovrl_gr <- var %in% include_overall
    gp_nms <- sapply(seq_along(level_groups), \(x) level_groups[[x]][[3]])
    gps <- lapply(seq_along(level_groups), \(x) (level_groups[[x]][[2]] |> eval())) |> setNames(gp_nms)

    ard_list <- c(
      ard_list,
      lapply(
        seq_along(if (ovrl_gr) rep(data_list, 2) else data_list),
        function(i) {
          which_data <- if (ovrl_gr) rep(seq_along(data_list), 2)[i] else i
          data <- data_list[[which_data]]

          data[[var]] <- if (i > length(data_list)) {
            rep("- Any Grade -", nrow(data)) |>
              factor(levels = "- Any Grade -", ordered = ord)
          } else {
            do.call(
              dplyr::case_match,
              args = c(list(.x = data[[var]]), level_groups)
            ) |>
            factor(levels = gp_nms, ordered = ord)
          }

          tbl <-
            tbl_hierarchical(
              data,
              variables,
              id,
              denominator,
              by,
              {{ include }},
              {{ statistic }},
              overall_row = FALSE,
              labels,
              {{ digits }}
            ) |>
            suppressMessages()

          ard <- gather_ard(tbl)[[1]]

          if (which_data > 1) {
            ard |>
              filter(variable == var) |>
              select(-all_ard_groups("levels"), by_cols, group2_level)
          } else {
            ard
          }
        }
      )
    )

    ard <- bind_ard(
      ard_list,
      .quiet = TRUE
    ) |>
      sort_ard_hierarchical("alphanumeric")

    # arrange with groups prior to their first level
    ard <- ard |>
      mutate(idx = dplyr::row_number()) |>
      group_by(pick(all_ard_groups(), "variable", -by_cols)) |>
      group_split() |>
      map(function(dat) {
        if (dat$variable[1] != var) {
          dat
        } else {
          dat$idx_lvl <- NA
          dat$idx_lvl[unlist(dat$variable_level) == "- Any Grade -"] <- 0
          if (any(unlist(dat$variable_level) %in% lvls)) {
            dat$idx_lvl[is.na(dat$idx_lvl) & !unlist(dat$variable_level) %in% gp_nms] <-
              sapply(
                dat$variable_level[is.na(dat$idx_lvl) & !unlist(dat$variable_level) %in% gp_nms],
                \(x) which(lvls == unlist(x))
              )
          }
          if (any(unlist(dat$variable_level) %in% gp_nms)) {
            dat$idx_lvl[unlist(dat$variable_level) %in% gp_nms] <-
              sapply(
                dat$variable_level[unlist(dat$variable_level) %in% gp_nms],
                \(x) min(which(lvls %in% gps[[unlist(x)]])) - 0.5
              )
          }
          dat |>
            arrange(idx_lvl, idx) |>
            mutate(idx = seq(min(idx), max(idx))) |>
            select(-idx_lvl)
        }
      }) |>
      bind_rows() |>
      arrange(idx) |>
      select(-idx) |>
      as_card()

    tbl <- tbl_ard_hierarchical(
      ard,
      variables = variables,
      by = by,
      include = {{ include }},
      statistic = {{ statistic }},
      label = labels
    )
  }

  tbl
}
