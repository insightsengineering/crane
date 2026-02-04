source("R/add_forest_utils.R")

#' Add a Forest Plot
#'
#' @param x a gtsummary table
#'
#' @return a gt table
#'
#' @examples
#' tbl <-
#'   trial |>
#'   tbl_roche_subgroups(
#'     rsp = "response",
#'     by = "trt",
#'     subgroups = c("grade", "stage"),
#'     ~ glm(response ~ trt, data = .x) |>
#'       gtsummary::tbl_regression(
#'         show_single_row = trt,
#'         exponentiate = TRUE
#'       )
#'   )
#'
#' # Using gt
#' tbl |> add_forest(pvalue = starts_with("p.value"))
#'
#' # Using flextable
#' tbl |> add_forest(pvalue = starts_with("p.value"), table_engine = "flextable")
#'
#' @export
add_forest <- function(x,
                       estimate = starts_with("estimate"),
                       conf.low = starts_with("conf.low"), conf.high = starts_with("conf.high"),
                       pvalue = NULL,
                       after = starts_with("p.value"),
                       table_engine = c("gt", "flextable")) {
  set_cli_abort_call()
  check_not_missing(x)
  check_not_missing(estimate)
  check_not_missing(conf.low)
  check_not_missing(analysis_variable)
  check_not_missing(change_variable)
  cards::process_selectors(
    x$table_body,
    estimate = {{ estimate }}, conf.low = {{ conf.low }}, conf.high = {{ conf.high }},
    pvalue = {{ pvalue }}, after = {{ after }}
  )
  check_scalar(estimate)
  check_scalar(conf.low)
  check_scalar(conf.high)
  check_scalar(pvalue, allow_empty = TRUE)
  check_scalar(after)

  # 1. SETUP DEFAULTS ----------------------------------------------------------
  # Define two sets of sizes: "Huge" for GT (HTML) and "Standard" for Flextable (Word/PPT)
  table_engine <- arg_match(table_engine, error_call = get_cli_abort_call())
  header_spaces <- 20

  sizes <- .get_default_forest_sizes(table_engine = table_engine)

  # 2. DATA PREP ---------------------------------------------------------------
  global_limits <- c(min(c(x$table_body[[conf.low]], 0.2), na.rm = TRUE),
                     max(x$table_body[[conf.high]], na.rm = TRUE))
  mean_estimate <- mean(x$table_body[[estimate]], na.rm = TRUE)
  global_margins <- margin(t = 0, r = 5, b = 0, l = 5, unit = "pt")

  # 3. GENERATE PLOTS ----------------------------------------------------------
  lst_ggplots <-
    seq_len(nrow(x$table_body)) |>
    lapply(
      function(i) {

        # Handle missing estimates by creating an empty plot with just reference lines
        if (is.na(x$table_body[[estimate]][i]) ||
            is.na(x$table_body[[conf.low]][i]) ||
            is.na(x$table_body[[conf.high]][i])) {

          # Create an empty plot with just the reference lines
          out <- ggplot2::ggplot() +
            ggplot2::geom_vline(xintercept = mean_estimate, linetype = "dashed", linewidth = sizes$line_ref) +
            ggplot2::geom_vline(xintercept = 1, linewidth = sizes$line_ref) +
            ggplot2::geom_vline(xintercept = 0.2, linewidth = sizes$line_ref) +
            ggplot2::scale_x_log10(limits = global_limits) +
            ggplot2::theme_void() +
            ggplot2::theme(
              axis.title = element_blank(), axis.line = element_blank(),
              axis.ticks = element_blank(), axis.text = element_blank(),
              plot.margin = global_margins
            )
          return(out)
        }

        # Extract the row data
        dt <- x$table_body[i, ]

        # Dots sizes
        pvalue_size_i <- ifelse(
          length(pvalue) > 0 && !is.null(dt[[pvalue]]) && !is.na(dt[[pvalue]]),
          sizes$dot_max * dt[[pvalue]],
          sizes$dot_base
        )

        # Create the forest plot for this row
        out <- dt |>
          dplyr::mutate(y = 1L) |>
          ggplot2::ggplot(
            ggplot2::aes(
              y = .data$y,
              x = .data[[estimate]],
              xmin = .data[[conf.low]],
              xmax = .data[[conf.high]]
            )
          ) +
          ggplot2::geom_errorbar(height = 0, size = sizes$errorbar_size) +
          ggplot2::geom_vline(xintercept = mean_estimate, linetype = "dashed", linewidth = sizes$line_ref) +
          ggplot2::geom_vline(xintercept = 1, linewidth = sizes$line_ref) +
          ggplot2::geom_vline(xintercept = 0.2, linewidth = sizes$line_ref) +
          ggplot2::geom_point(size = pvalue_size_i, shape = 21, fill = "white", stroke = sizes$stroke) +
          ggplot2::scale_x_log10(limits = global_limits) + # Ensure log scale matches axis!
          ggplot2::theme_void() +
          ggplot2::theme(
            axis.title = element_blank(),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            plot.margin = global_margins
          )

        out
      }
    )

  # 4. CREATE AXIS & APPEND ----------------------------------------------------
  # Create the axis plot (using the function from the previous step)
  p_axis <- .plot_centered_axis(limits = global_limits, mean_estimate = mean_estimate, sizes = sizes)

  # Append it to your list of plots
  lst_ggplots_final <- c(lst_ggplots, list(p_axis))

  # Extract the Spanning Headers from gtsummary metadata
  header_text <- .determine_ggplot_header(x, header_spaces, table_engine)
  if (table_engine == "gt") {
    out <- x |>
      gtsummary::modify_table_body(~ .x |> dplyr::add_row() |> dplyr::mutate(ggplot = NA, .after = .data[[after]])) |>
      gtsummary::modify_footnote(gtsummary::everything() ~ NA) |>
      gtsummary::modify_header(ggplot = header_text) |>
      gtsummary::as_gt() |>
      gt::text_transform(
        locations = gt::cells_body(columns = .data$ggplot),
        fn = function(x) {
          suppressMessages( # avoid `height` was translated to `width`. message
            lst_ggplots_final |> gt::ggplot_image(height = gt::px(28), aspect_ratio = 8)
          )
        }
      ) |>
      gt::cols_width(ggplot ~ gt::px(250)) |>
      gt::tab_options(
        data_row.padding = gt::px(0),
        table_body.hlines.style = "none",
        table_body.vlines.style = "none"
      ) |>
      gt::opt_css("
        .gt_table img { display: block; vertical-align: bottom; margin: 0 auto; }
      ") |>
      gt::tab_style(style = gt::cell_text(whitespace = "nowrap"), locations = gt::cells_body())

  } else if (table_engine == "flextable") {
    out <- x |>
      gtsummary::modify_table_body(~ .x |> dplyr::add_row() |> dplyr::mutate(ggplot = NA, .after = .data[[after]])) |>
      gtsummary::modify_footnote(gtsummary::everything() ~ NA) |>
      gtsummary::modify_header(ggplot = header_text) |>
      gtsummary::as_flex_table() |>
      flextable::mk_par(
        j = "ggplot",
        value = flextable::as_paragraph(
          suppressMessages( # avoid `height` was translated to `width`. message
            flextable::gg_chunk(value = lst_ggplots_final, height = 0.4, width = 2.5)
          )
        )
      ) |>
      flextable::line_spacing(space = 0.8, part = "body") |>
      flextable::line_spacing(j = "ggplot", space = 0, part = "body") |>
      flextable::valign(valign = "center", part = "body") |>
      flextable::align(j = "ggplot", align = "center", part = "header") |>
      flextable::width(j = "ggplot", width = 2.5) |>
      flextable::padding(padding.top = 0, part = "body") |>
      flextable::padding(padding.bottom = 7, part = "body") |>
      flextable::padding(j = "ggplot", padding.bottom = 0, part = "body") |>
      flextable::valign(valign = "bottom", part = "body")
  }

  out
}
