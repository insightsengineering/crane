#' Create an ADA summary table
#'
#' Creates baseline prevalence and post-baseline incidence sections for
#' ADA/NAb data. For multi-arm data, original treatment columns are retained,
#' optional custom pooled columns are added, and a Total column is always added.
#'
#' @param data A data frame containing ADA variables.
#' @param by A single treatment/grouping variable, or `NULL` for an
#'   unstratified table.
#' @param pools An optional named list of custom pooled columns. For example,
#'   `list("Total Dose" = c("High Dose", "Low Dose"))`.
#' @param total_label Display label for the Total column.
#' @param keep_original Whether original treatment columns are retained when
#'   calculated columns are added.
#' @param null_postbaseline Character vector of computed-column labels to blank
#'   in the post-baseline section. Values must be the Total label or a custom
#'   pool name. Use `NULL` to retain all post-baseline values.
#' @param baseline_header Header for the baseline section.
#' @param postbaseline_header Header for the post-baseline section.
#' @param baseline_label Optional named list of baseline variable labels.
#' @param postbaseline_label Optional named list of post-baseline variable
#'   labels.
#'
#' @return A `tbl_ada_summary` and `gtsummary` object.
#' @export
tbl_ada_summary <- function(
    data,
    by = NULL,
    pools = NULL,
    total_label = "All Patients",
    keep_original = TRUE,
    null_postbaseline = NULL,
    baseline_header = "Baseline Prevalence of NAbs",
    postbaseline_header = "Incidence of NAbs",
    baseline_label = list(
      ADABLPFL = "Baseline evaluable patients for ADA",
      adaposbl = "Patients with a positive ADA sample at baseline",
      nabevalbase = "Baseline evaluable patients for NAb",
      nabposbl = "Patients with a positive NAb sample at baseline",
      nabnegbl = "Patients with no positive NAb sample at baseline"
    ),
    postbaseline_label = list(
      ADPBLPFL = "Post-baseline evaluable patients for ADA",
      adastatpos = "Patients positive for ADA",
      nabpbeval = "Post-baseline evaluable patients for NAb",
      nabstatpos = "Patients positive for NAb",
      nabstatneg = "Patients negative for NAb"
    )) {
  set_cli_abort_call()
  check_not_missing(data)
  check_data_frame(data)
  check_string(total_label)
  check_scalar_logical(keep_original)
  check_string(baseline_header)
  check_string(postbaseline_header)

  cards::process_selectors(
    data,
    by = {{ by }}
  )

  check_scalar(by, allow_empty = TRUE)

  baseline_variables <- c(
    "ADABLPFL", "adaposbl", "nabevalbase", "nabposbl", "nabnegbl"
  )

  postbaseline_variables <- c(
    "ADPBLPFL", "adastatpos", "nabpbeval", "nabstatpos", "nabstatneg"
  )

  required_variables <- unique(c(
    by,
    baseline_variables,
    postbaseline_variables
  ))

  missing_variables <- setdiff(required_variables, names(data))

  if (length(missing_variables) > 0L) {
    cli::cli_abort(
      "The following required columns are missing from {.arg data}: {.val {missing_variables}}.",
      call = get_cli_abort_call()
    )
  }

  if (!is.null(pools) &&
      (!is.list(pools) || is.null(names(pools)) || any(names(pools) == ""))) {
    cli::cli_abort(
      "{.arg pools} must be a fully named list or {.code NULL}.",
      call = get_cli_abort_call()
    )
  }

  if (!is.null(pools) && total_label %in% names(pools)) {
    cli::cli_abort(
      "{.arg total_label} must not duplicate a custom pool name.",
      call = get_cli_abort_call()
    )
  }

  if (!is.null(null_postbaseline) && !is.character(null_postbaseline)) {
    cli::cli_abort(
      "{.arg null_postbaseline} must be a character vector or {.code NULL}.",
      call = get_cli_abort_call()
    )
  }

  computed_column_labels <- c(names(pools), total_label)

  unknown_null_columns <- setdiff(
    null_postbaseline,
    computed_column_labels
  )

  if (length(unknown_null_columns) > 0L) {
    cli::cli_abort(
      c(
        "{.arg null_postbaseline} can only contain custom pool names or the Total label.",
        i = "Unknown label(s): {.val {unknown_null_columns}}."
      ),
      call = get_cli_abort_call()
    )
  }

  tbl_ada_summary_inputs <- as.list(environment())

  # For multi-arm data, always add the Total column and optionally add
  # study-specific pooled columns, such as "Total Dose".
  use_computed_columns <- length(by) == 1L &&
    dplyr::n_distinct(data[[by]], na.rm = TRUE) > 1L

  pool_definitions <- c(
    pools,
    stats::setNames(list("all"), total_label)
  )

  build_section <- function(
      include,
      statistic,
      label,
      section_header,
      indent) {
    tbl <- if (use_computed_columns) {
      tbl_with_pools(
        data = data,
        pools = pool_definitions,
        by = by,
        keep_original = keep_original,
        .tbl_fun = tbl_roche_summary,
        include = dplyr::all_of(include),
        statistic = statistic,
        label = label,
        nonmissing = "no"
      )
    } else {
      tbl_roche_summary(
        data = data,
        by = dplyr::any_of(by),
        include = dplyr::all_of(include),
        statistic = statistic,
        label = label,
        nonmissing = "no"
      )
    }

    tbl <- gtsummary::modify_table_body(
      tbl,
      ~ tibble::add_row(
        .x,
        variable = "header",
        var_label = section_header,
        row_type = "label",
        label = section_header,
        .before = 1
      )
    )

    for (variable in names(indent)) {
      tbl <- rlang::inject(
        gtsummary::modify_indent(
          tbl,
          columns = "label",
          rows = .data$variable == !!variable,
          indent = !!indent[[variable]]
        )
      )
    }

    tbl
  }

  tbl_baseline <- build_section(
    include = baseline_variables,
    statistic = list(
      ADABLPFL = "{n}",
      adaposbl = "{n}",
      nabevalbase = "{n}",
      nabposbl = "{n} ({p}%)",
      nabnegbl = "{n}"
    ),
    label = baseline_label,
    section_header = baseline_header,
    indent = c(
      ADABLPFL = 4L,
      adaposbl = 4L,
      nabevalbase = 4L,
      nabposbl = 7L,
      nabnegbl = 7L
    )
  )

  tbl_postbaseline <- build_section(
    include = postbaseline_variables,
    statistic = list(
      ADPBLPFL = "{n}",
      adastatpos = "{n}",
      nabpbeval = "{n}",
      nabstatpos = "{n} ({p}%)",
      nabstatneg = "{n}"
    ),
    label = postbaseline_label,
    section_header = postbaseline_header,
    indent = c(
      ADPBLPFL = 4L,
      adastatpos = 4L,
      nabpbeval = 4L,
      nabstatpos = 7L,
      nabstatneg = 7L
    )
  )

  # Blank selected computed columns in the post-baseline section.
  if (use_computed_columns && length(null_postbaseline) > 0L) {
    stat_columns <- names(
      dplyr::select(
        tbl_postbaseline$table_body,
        gtsummary::all_stat_cols()
      )
    )

    header <- tbl_postbaseline$table_styling$header

    columns_to_blank <- header$column[
      header$column %in% stat_columns &
        header$modify_stat_level %in% null_postbaseline
    ]

    if (length(columns_to_blank) == 0L) {
      cli::cli_abort(
        c(
          "No post-baseline statistic columns matched {.arg null_postbaseline}.",
          i = "Requested label(s): {.val {null_postbaseline}}."
        ),
        call = get_cli_abort_call()
      )
    }

    tbl_postbaseline <- gtsummary::modify_table_body(
      tbl_postbaseline,
      ~ dplyr::mutate(
        .x,
        dplyr::across(
          dplyr::all_of(columns_to_blank),
          ~ NA_character_
        )
      )
    )
  }

  tbl <- gtsummary::tbl_stack(
    list(tbl_baseline, tbl_postbaseline),
    quiet = TRUE
  )

  # Insert a blank row between the two sections.
  tbl <- add_blank_rows(
    tbl,
    row_numbers = nrow(tbl_baseline$table_body)
  )

  tbl$inputs <- tbl_ada_summary_inputs
  tbl$call_list <- list(tbl_ada_summary = match.call())

  structure(
    tbl,
    class = c("tbl_ada_summary", class(tbl))
  )
}
