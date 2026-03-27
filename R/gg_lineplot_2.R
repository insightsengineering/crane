#' @title Generate Summary Statistic Function for ggplot2
#'
#' @description
#' A function factory that produces a summary closure compatible with 
#' `ggplot2::stat_summary(fun.data = ...)`. It computes a central tendency 
#' (mean or median) alongside a specified measure of dispersion.
#'
#' @param stat (`string`)\cr
#'   Primary statistic to calculate (`"mean"` or `"median"`).
#' @param variability (`string`)\cr
#'   Measure of variability (`"sd"`, `"se"`, `"ci"`, `"iqr"`, or `"none"`).
#' @param conf_level (`numeric`)\cr
#'   Confidence level for `"ci"` (default `0.95`).
#'
#' @return A `function` that takes a numeric vector and returns a `data.frame` 
#'   with `y`, `ymin`, and `ymax`.
#'
#' @examples
#' # Generate a closure for Mean and 90% CI
#' my_summary_fun <- gg_get_summary_stats(
#'   stat = "mean", 
#'   variability = "ci", 
#'   conf_level = 0.90
#' )
#' 
#' # Apply to a vector
#' my_summary_fun(rnorm(100, mean = 10, sd = 2))
#'
#' @export
gg_get_summary_stats <- function(
  stat = c("mean", "median"), 
  variability = c("sd", "se", "ci", "iqr", "none"),
  conf_level = 0.95
) {
  
  stat <- match.arg(stat)
  variability <- match.arg(variability)
  
  # Return the mathematical closure that ggplot2 will evaluate during rendering
  function(val) {
    val <- stats::na.omit(val)
    n <- length(val)
    
    # Return NAs to prevent ggplot2 from drawing artifact geometries on empty subsets
    if (n == 0) {
      return(data.frame(y = NA_real_, ymin = NA_real_, ymax = NA_real_))
    }
    
    if (stat == "median") {
      y_val <- stats::median(val)
      
      if (variability == "iqr") {
        ymin_val <- stats::quantile(val, 0.25)
        ymax_val <- stats::quantile(val, 0.75)
      } else {
        ymin_val <- y_val
        ymax_val <- y_val
      }
      
    } else if (stat == "mean") {
      y_val <- mean(val)
      se <- stats::sd(val) / sqrt(n)
      
      # Determine the error margin based on the requested dispersion metric
      err <- switch(
        variability,
        "sd" = stats::sd(val),
        "se" = se,
        "ci" = stats::qt((1 + conf_level) / 2, df = max(1, n - 1)) * se,
        "none" = 0,
        0
      )
      
      ymin_val <- y_val - err
      ymax_val <- y_val + err
    }
    
    data.frame(y = y_val, ymin = ymin_val, ymax = ymax_val)
  }
}


#' @title Generate a Summary Line Plot from Raw Data
#'
#' @description Calculates summary statistics inline using `ggplot2::stat_summary()`,
#'   generating a line plot directly from raw data. Supports configurable central 
#'   tendencies and dispersion metrics.
#'
#' @param data (`data.frame`)\cr
#'   The raw data frame (e.g., ADaM dataset).
#' @param x ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Column name for the x-axis timepoints (e.g., `AVISIT`).
#' @param y ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Column name for the continuous variable to summarize (e.g., `AVAL`).
#' @param group ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Optional column name for the grouping/treatment variable.
#' @param stat (`string`)\cr
#'   Primary summary statistic: `"mean"` or `"median"`. Default is `"mean"`.
#' @param variability (`string`)\cr
#'   Variability measure: `"sd"`, `"se"`, `"ci"`, `"iqr"`, or `"none"`. 
#'   Default is `"ci"`.
#' @param conf_level (`numeric`)\cr
#'   Confidence level for error bars when `variability = "ci"` (default: `0.95`).
#'
#' @return A `ggplot` object of class `crane_gg_line`.
#'
#' @export
#' @importFrom rlang .data
gg_lineplot <- function(data,
                        x,
                        y,
                        group = NULL,
                        stat = c("mean", "median"),
                        variability = c("ci", "sd", "se", "iqr", "none"),
                        conf_level = 0.95) {
  
  # 1. Argument Matching and Validation
  stat <- match.arg(stat)
  variability <- match.arg(variability)
  
  # Prevent mathematically invalid combinations
  if (stat == "mean" && variability == "iqr") {
    cli::cli_abort("Invalid combination: Cannot plot IQR around a mean.")
  } else if (stat == "median" && variability %in% c("sd", "se", "ci")) {
    cli::cli_abort(
      "Invalid combination of stat ({.val {stat}}) and variability ({.val {variability}})."
    )
  }

  cards::process_selectors(
    data,
    x = {{ x }},
    y = {{ y }},
    group = {{ group }}
  )

  # 2. Data Preprocessing
  # Expand grid ensures missing timepoints in specific groups are explicitly 
  # populated with NAs. This prevents ggplot from drawing misleading lines 
  # that bridge across missing visits.
  if (length(group) > 0) {
    df_plot <- tidyr::expand(data, .data[[group]], .data[[x]]) |>
      dplyr::left_join(data, by = c(group, x))
  } else {
    df_plot <- tidyr::expand(data, .data[[x]]) |>
      dplyr::left_join(data, by = x)
  }

  pd <- ggplot2::position_dodge(width = 0.4)

  # 3. Build Plot: Use clean aes() mapping so gg_varname_extraction succeeds
  if (length(group) > 0) {
    p <- ggplot2::ggplot(
      data = df_plot,
      ggplot2::aes(
        x = .data[[x]],
        y = .data[[y]],
        color = .data[[group]],
        shape = .data[[group]],
        linetype = .data[[group]],
        group = .data[[group]]
      )
    )
  } else {
    p <- ggplot2::ggplot(
      data = df_plot,
      ggplot2::aes(
        x = .data[[x]],
        y = .data[[y]]
      )
    )
  }

  p <- p + 
    ggplot2::stat_summary(
      fun = stat, 
      geom = "point", 
      position = pd, 
      size = 2, 
      na.rm = TRUE
    )
    
  if (length(group) > 0) {
    p <- p + ggplot2::stat_summary(
      fun = stat, 
      geom = "line", 
      position = pd, 
      na.rm = TRUE
    )
  }

  # 4. Add Variability Layer conditionally to avoid drawing degenerate lines
  if (variability != "none") {
    p <- p + ggplot2::stat_summary(
      fun.data = gg_get_summary_stats(
        stat = stat, 
        variability = variability, 
        conf_level = conf_level
      ),
      geom = "errorbar",
      width = 0.45,
      position = pd,
      na.rm = TRUE
    )
  }

  # 5. Theming
  p <- p +
    ggplot2::scale_y_continuous(labels = scales::label_comma()) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title.position = "top",
      legend.title.align = 0.5,
      legend.background = ggplot2::element_rect(fill = "white", color = "black", linewidth = 0.5),
      plot.title = ggplot2::element_text(face = "bold")
    )
    
  # Dynamically label the legends to overwrite the ugly ".data[[group]]" default
  if (length(group) > 0) {
    p <- p + ggplot2::labs(color = group, linetype = group, shape = group)
  }

  class(p) <- c("crane_gg_line", class(p))

  return(p)
}