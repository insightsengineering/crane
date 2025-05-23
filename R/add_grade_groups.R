#' AE Analysis of Grade Groups
#' @export
#'
#' @examples
#' grade_groups <- list(
#'   c("1", "2") ~ "Grade 1-2",
#'   c("3", "4") ~ "Grade 3-4",
#'   "5" ~ "Grade 5"
#' )
#'
#' tbl_hierarchical_groups(
#'   adae, #|> mutate(AETOXGR = factor(AETOXGR, ordered = TRUE)),
#'   variables = c(AEBODSYS, AEDECOD, AETOXGR),
#'   level_groups = grade_groups,
#'   id = USUBJID,
#'   denominator = adsl,
#'   by = ACTARM
#' )
tbl_hierarchical_groups <- function(data,
                                    variables,
                                    level_groups = list(),
                                    levels_exclude = NULL,
                                    id,
                                    denominator,
                                    by = NULL,
                                    include = everything(),
                                    statistic = everything() ~ "{n} ({p}%)",
                                    overall_row = FALSE,
                                    labels = NULL) {
  # check inputs ---------------------------------------------------------------
  set_cli_abort_call()
  cards::process_selectors(data, variables = {{variables}}, by = {{ by }}, id = {{ id }})

  var <- dplyr::last(variables)

  if (is.null(labels)) {
    labels <- lapply(data[variables], \(x) attr(x, "label"))
  }

  # ungrouped data -----
  data_no_gps <- data |>
    # drop levels from levels_exclude
    filter(!var %in% levels_exclude)

  # browser()

  ard <-
    ard_stack_hierarchical(
      data = data_no_gps,
      variables = variables,
      by = by,
      id = id,
      denominator = denominator,
      include = {{ include }},
      over_variables = overall_row
    )

  # grouped data -------
  if (length(level_groups) > 0) {
    data_gps <- data

    ## groups must be mutually exclusive?

    gp_nms <- sapply(seq_along(level_groups), \(x) level_groups[[x]][[3]])
    gps <- lapply(seq_along(level_groups), \(x) (level_groups[[x]][[2]] |> eval())) |> setNames(gp_nms)
    ord <- is.ordered(data_gps[[var]])
    by_cols <- if (!is.null(by)) c("group1", "group1_level") else NULL

    data_gps[[var]] <-
      do.call(
        dplyr::case_match,
        args = c(list(.x = data_gps[[var]]), level_groups)
      ) |>
      factor(levels = gp_nms, ordered = ord)

    ard_gps <-
      ard_stack_hierarchical(
        data = data_gps,
        variables = variables,
        by = by,
        id = id,
        denominator = denominator,
        include = {{ include }},
        over_variables = overall_row
      )

    ard <- bind_ard(
      ard,
      ard_gps,
      .quiet = TRUE
    ) |>
      sort_ard_hierarchical("alphanumeric")

    lvls <- ard$variable_level[!unlist(ard$variable_level) %in% gp_nms] |> unlist() |> as.character() |> unique()

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
          dat$idx_lvl[!unlist(dat$variable_level) %in% gp_nms] <-
            sapply(dat$variable_level[!unlist(dat$variable_level) %in% gp_nms], \(x) which(lvls == unlist(x)))
          dat$idx_lvl[unlist(dat$variable_level) %in% gp_nms] <-
            sapply(dat$variable_level[unlist(dat$variable_level) %in% gp_nms], \(x) min(which(lvls %in% gps[[unlist(x)]])) - 0.5)
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
  }

  tbl <- tbl_ard_hierarchical(
    ard,
    variables = variables,
    by = by,
    include = {{ include }},
    statistic = {{ statistic }},
    label = labels
  )

  tbl
}
