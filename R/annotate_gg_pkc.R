#' Annotate PK Plot with Summary Table
#'
#' @param gg_plt (`ggplot2`)\cr
#'   The PK plot.
#' @param data (`data.frame`)\cr
#'   The raw or summarized dataset used to generate the table.
#' @param time_var,analyte_var,group ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Optional. If `NULL` (default), the function automatically extracts these
#'   from the `gg_plt` mapping.
#' @param summary_stats (`character`)\cr
#'   A vector of statistics to include. Defaults to `c("n", "mean", "sd")`.
#' @param text_size (`numeric`)\cr
#'   The font size for the table text. Defaults to `3`.
#' @param rel_height_plot (`numeric`)\cr
#'   Relative height of the plot vs the table. Defaults to `0.75`.
#' @param ... Additional args passed to `df2gg_2()`.
#'
#' @export
annotate_gg_pkc <- function(data,
                            gg_plt,
                            time_var = NULL,
                            analyte_var = NULL,
                            group = NULL,
                            summary_stats = c("n", "mean", "sd"),
                            text_size = 3.5,
                            rel_height_plot = 0.75,
                            ...) {
  # 1. Helper & Variable Extraction---------------------------------------------
  get_var <- function(mapping_quo) {
    if (is.null(mapping_quo) || !rlang::is_quosure(mapping_quo)) {
      return(NULL)
    }
    expr <- rlang::quo_get_expr(mapping_quo)
    if (rlang::is_symbol(expr)) {
      return(as.character(expr))
    }
    if (rlang::is_call(expr, "[[") && identical(expr[[2]], quote(.data))) {
      return(rlang::eval_bare(expr[[3]]))
    }
    return(NULL)
  }

  if (is.null(time_var)) time_var <- get_var(gg_plt$mapping$x)
  if (is.null(analyte_var)) analyte_var <- get_var(gg_plt$mapping$y)
  if (is.null(group)) group <- get_var(gg_plt$mapping$colour)

  if (any(vapply(list(time_var, analyte_var, group), is.null, logical(1)))) {
    cli::cli_abort(
      paste0(
        "Missing variables. Specify {.arg time_var}, ",
        "{.arg analyte_var}, and {.arg group}."
      )
    )
  }

  # 2. Stat Setup---------------------------------------------------------------
  stat_syntax <- c(
    "n" = "{N_nonmiss}",
    "mean" = "{mean}",
    "sd" = "{sd}",
    "median" = "{median}",
    "iqr" = "{p25}, {p75}"
  )
  stat_labels <- c(
    "n" = "n",
    "mean" = "Mean",
    "sd" = "SD",
    "median" = "Median",
    "iqr" = "IQR"
  )

  summary_stats <- match.arg(
    summary_stats,
    choices = names(stat_syntax),
    several.ok = TRUE
  )
  gts_stat <- stat_syntax[summary_stats]
  gts_stat_labels <- stat_labels[summary_stats]

  # 3. Table Generation---------------------------------------------------------
  gts_tbl <- data |>
    dplyr::mutate(..time_factor.. = as.factor(.data[[time_var]])) |>
    gtsummary::tbl_strata(
      strata = dplyr::all_of(group),
      .combine_with = "tbl_stack",
      .tbl_fun = ~ .x |>
        gtsummary::tbl_summary(
          by = "..time_factor..",
          include = dplyr::all_of(analyte_var),
          type = list(dplyr::all_of(analyte_var) ~ "continuous2"),
          statistic = list(dplyr::all_of(analyte_var) ~ gts_stat),
          missing = "no",
          label = list(dplyr::all_of(analyte_var) ~ " ")
        ) |>
        gtsummary::add_stat_label(
          label = dplyr::all_of(analyte_var) ~ gts_stat_labels
        ) |>
        gtsummary::modify_header(gtsummary::all_stat_cols() ~ "{level}")
    ) |>
    gtsummary::modify_header(label = "remove")

  # 4. Cleanup with Positional Fix----------------------------------------------
  formatted_df <- as.data.frame(
    gtsummary::as_tibble(gts_tbl, col_labels = TRUE)
  )
  formatted_df[is.na(formatted_df)] <- ""
  names(formatted_df) <- gsub("\\*", "", names(formatted_df))

  strata_col_name <- names(formatted_df)[1]

  formatted_df <- formatted_df |>
    dplyr::mutate(
      Group = paste0(.data[[strata_col_name]], .data[["remove"]])
    ) |>
    dplyr::select(-remove)

  # 5. Format Labels using plotmath---------------------------------------------
  raw_labels <- as.character(formatted_df[[1]])
  is_stat <- trimws(raw_labels) %in% c("n", "Mean", "SD", "Median", "IQR")

  pk_y_labels <- parse(text = ifelse(
    is_stat,
    paste0('~~~~"', trimws(raw_labels), '"'),
    paste0('bold("', trimws(raw_labels), '")')
  ))

  # 6. Table Plotting-----------------------------------------------------------
  gg_table <- df2gg_aligned(
    df = formatted_df,
    gg_plt = gg_plt,
    show_xaxis = FALSE,
    type = "PK",
    y_labels = pk_y_labels,
    ...
  )

  gg_table
}
