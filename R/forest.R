#' Create a Custom Forest Plot
#'
#' Generates a forest plot using {ggplot2} from a data frame containing
#' estimates, confidence intervals, and sample sizes. This function is designed
#' to be a component of a combined table/plot output (e.g., used by [g_forest()]).
#'
#' @param data (`data.frame`)\cr
#'   A data frame (tibble) containing the plot data. It must include
#'   columns: `group` (for y-axis labels), `estimate`, `ci_lower`,
#'   `ci_upper`, and `n` (for point size).
#' @param xlim (`numeric(2)`)\cr
#'   A numeric vector of length 2 specifying the limits of the x-axis
#'   (e.g., `c(0.1, 10)`).
#' @param logx (`logical(1)`)\cr
#'   A logical value indicating whether the x-axis should be log-transformed
#'   (i.e., using [scale_x_log10()]). The default is `TRUE`, which is typical
#'   for effect measures like Odds Ratios or Hazard Ratios.
#' @param vline (`numeric(1)`)\cr
#'   A numeric value specifying the x-intercept for the vertical
#'   reference line (line of no effect). The default is `1`.
#'
#' @return A 'ggplot' object representing the forest plot.
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Assuming 'forest_data' is structured correctly:
#' forest_data <- data.frame(
#'   group = c("A vs B", "C vs D"),
#'   estimate = c(0.5, 2.0),
#'   ci_lower = c(0.2, 1.5),
#'   ci_upper = c(0.9, 3.5),
#'   n = c(100, 250)
#' )
#'
#' create_forest_plot(forest_data)
#' create_forest_plot(forest_data, xlim = c(0.05, 50), vline = 1)
#' }
create_forest_plot <- function(data,
                               groups = "",
                               xlim = c(0.1, 10),
                               logx = TRUE,
                               vline = 1) {
  forest_header <- paste0(groups, "\nBetter")
  # Calculate y positions (reverse order for top-to-bottom display)
  data <- data %>%
    mutate(y_pos = rev(dplyr::row_number()))

  # Apply log transformation if needed
  if (logx) {
    #   data <- data %>%
    #     mutate(
    #       estimate_log = log(estimate),
    #       ci_lower_log = log(ci_lower),
    #       ci_upper_log = log(ci_upper)
    #     )
    #   x_aesthetic_vars <- aes(x = estimate_log, xend = ci_lower_log, yend = ci_upper_log)
    #   x_scale <- scale_x_log10(limits = xlim, expand = c(0.01, 0))
    # } else {
    data <- data %>%
      mutate(
        estimate_log = estimate,
        ci_lower_log = ci_lower,
        ci_upper_log = ci_upper
      )
    x_aesthetic_vars <- aes(x = estimate, xend = ci_lower_log, yend = ci_upper_log)
    x_scale <- scale_x_log10(limits = xlim, expand = c(0.01, 0))
  }

  # Create plot
  ggplot(data) +
    # Background rectangle
    annotate("rect",
      xmin = xlim[1], xmax = xlim[2], ymin = 0.5, ymax = nrow(data) + 0.5,
      fill = "grey92", alpha = 0.5
    ) +
    # CI lines with arrows
    geom_errorbar(aes(xmin = ci_lower_log, xmax = ci_upper_log, y = y_pos),
      height = 0.2, color = "black", orientation = "y"
    ) +
    # Points
    geom_point(aes(x = estimate_log, y = y_pos, size = n),
      color = "#343cff", shape = 19
    ) +
    # Reference line
    geom_vline(xintercept = vline, linewidth = 1) +
    # Forest header text
    annotate("text",
      x = mean(c(xlim[1], vline)), y = nrow(data) + 1.25,
      label = forest_header[1], size = 3.5
    ) +
    annotate("text",
      x = mean(c(vline, xlim[2])), y = nrow(data) + 1.25,
      label = forest_header[2], size = 3.5
    ) +
    # Scales and theme
    x_scale +
    scale_y_continuous(
      limits = c(0, nrow(data) + 2.5), breaks = data$y_pos,
      labels = rep("", length(data$estimate)), expand = c(0, 0)
    ) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      legend.position = "none",
      plot.margin = margin(0.1, 0.1, 0.05, 0, "npc")
    )
}

#' Extract Data for Forest Plot from gtsummary Table
#'
#' Converts the table body (`tbl$table_body`) of a 'gtsummary' object
#' into a data frame suitable for plotting with [create_forest_plot()].
#' It selects and renames the necessary columns for the plot.
#'
#' @param tbl (`gtsummary`)\cr
#'   A 'gtsummary' object (e.g., from [tbl_regression()] or [tbl_uvregression()]).
#'
#' @return A data frame (tibble) with the columns:
#'   `group` (from `term`), `estimate`, `ci_lower` (from `conf.low`),
#'   `ci_upper` (from `conf.high`), and `n` (from `N_obs`).
#' @keywords internal
extract_plot_data <- function(tbl) {
  ret <- tbl$table_body %>%
    select(
      # group = termc("Drug A", "Drug B"),
      estimate = starts_with("estimate"),
      ci_lower = starts_with("conf.low"),
      ci_upper = starts_with("conf.high"),
      n = starts_with("N_obs", ignore.case = FALSE)
    )
  return(ret)
}

#' Create a Combined gtsummary Table and Forest Plot
#'
#' This is the main wrapper function that takes a 'gtsummary' object,
#' converts it to a 'ggplot' table, extracts the necessary data, creates
#' a forest plot, and combines the two plots side-by-side using `+`.
#' This likely relies on the {patchwork} package for plot combination.
#'
#' @param tbl (`gtsummary`)\cr
#'   A 'gtsummary' object (e.g., from [tbl_regression()]).
#'
#' @return A combined 'ggplot' object (likely a 'patchwork' object)
#'   showing the table on the left and the forest plot on the right.
#'
#' @seealso [extract_plot_data()], [create_forest_plot()]
#'
#' @export
#' @examples
#' tbl <-
#'   trial %>%
#'   tbl_roche_subgroups(
#'     rsp = "response",
#'     by = "trt",
#'     subgroups = c("grade", "stage"),
#'     ~ glm(response ~ trt, data = .x) %>%
#'       gtsummary::tbl_regression(
#'         show_single_row = trt,
#'         exponentiate = TRUE,
#'         tidy_fun = broom.helpers::tidy_parameters
#'       )
#'   )
#'
#' g_forest(tbl, groups = levels(factor(trial$trt)))
g_forest <- function(tbl, groups) {
  # todo need to make sure tbl does not have wrapped rows
  table_plot <- as_ggplot(tbl)
  # table_plot <- wrap_table(tbl, space = "fixed")
  forest_data <- extract_plot_data(tbl)
  forest_plot <- create_forest_plot(forest_data, groups)
  table_plot + forest_plot + plot_layout(widths = c(3, 1))
}


# extract_tbl_from_gtsummry <- function(tbl) {
#   ret <- tbl$table_body %>%
#     mutate(or = round(estimate, 2)) %>%
#     select(
#       `label` = label,
#       `Total n` = N,
#       `Odds ratio` = or,
#       `95% CI` = ci
#     ) %>%
#     as_tibble()
#   print(ret)
# }
