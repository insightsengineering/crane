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
#'   adae,
#'   variables = AETOXGR,
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

    ## groups must be mutually exclusive

    gps <- sapply(seq_along(level_groups), \(x) level_groups[[x]][[3]])
    ord <- is.ordered(data_gps[[var]])

    data_gps[[var]] <-
      do.call(
        dplyr::case_match,
        args = c(list(.x = data_gps[[var]]), level_groups)
      ) |>
      factor(levels = gps, ordered = ord)

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

    browser()
    # arrange

    ard |>
      group_by(pick(all_ard_groups(), "variable")) |>
      group_keys()

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
