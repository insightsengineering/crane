# =============================================================================
# Script: gtsummary_to_reporter_clinical
#
# Packages referenced:
# - gtsummary: table building and styling
# - reporter: RTF/TXT report generation
# - tidyverse: dplyr/tidyr helpers used in processing
#
# Function arguments (gtsummary_to_reporter_clinical):
# - gts_obj: gtsummary object or plain data.frame to export
# - file_path: output path (extension determines RTF/TXT names)
# - max_table_width: max width (inches/cm) for all columns combined
# - min_col_width: minimum width for any column
# - column_widths: manual widths (e.g., "10|5|5") or numeric vector
# - column_labels: override column header labels (named vector or data.frame)
# - spanning_headers: spanning header specs (data.frame or list)
# - report_orientation: "landscape" or "portrait"
# - report_paper_size: "letter", "legal", "a4", or numeric c(width, height)
# - report_units: "inches" or "cm"
# - report_margins: NULL or named vector/list (top/right/bottom/left)
# - report_font_size: base font size for table
# - indent_unit: spaces per indent unit for label column
# - output_types: c("RTF","TXT") output selection
# - save_rds: save processed data and ARD objects as RDS
# - rds_dir: output directory for RDS (uses file_path directory if empty)
# - rows_per_page: rows per page (auto if NULL)
# - debug_indent: print indent debug details
# - debug_spanning: print spanning header debug details
# - group_columns: optional grouping columns to hide and blank_after
# - group_blank_after: if TRUE, apply reporter blank_after by group column
#
# Environment variables (auto-used if defined):
# - title1-9: report title lines
# - footnote1-9: report footnotes
# - progname: footer program name (e.g., "ctr/t-bin-eff.r")
#
# Example:
# title1 <- "Table 14.1 Summary of Demographics"
# footnote1 <- "MMRM = Mixed Model for Repeated Measures"
# progname <- "ctr/t-bin-eff.r"
# gtsummary_to_reporter_clinical(
#   gts_obj = my_gts_table,
#   file_path = "Clinical_Trial_Output.rtf",
#   column_widths = "10|5|5",
#   column_labels = c(label = "Visit / Statistic", stat_1 = "PBO", stat_2 = "TRT"),
#   spanning_headers = data.frame(from = 2, to = 3, label = "Treatment Group")
# )
# =============================================================================

#' Export `gtsummary` tables to clinical-style `reporter` outputs
#'
#' Convert a `gtsummary` object (or a plain `data.frame`) into a `reporter`
#' table and write RTF and/or TXT outputs. The function is designed for
#' clinical reporting workflows and supports column labels, spanning headers,
#' pagination, indentation handling, and optional export of intermediate data.
#'
#' Environment variables are automatically consumed when available:
#' `title1`-`title9`, `footnote1`-`footnote9`, and `progname`.
#'
#' @param gts_obj A `gtsummary` object (with `table_body`/`table_styling`) or
#'   a plain `data.frame` to export.
#' @param file_path Output path. The extension is ignored and output files are
#'   written according to `output_types`.
#' @param max_table_width Maximum total table width. If `NULL`, derived from
#'   `report_paper_size`, `report_orientation`, `report_units`, and
#'   `report_margins`.
#' @param min_col_width Minimum width allowed for any column.
#' @param column_widths Optional manual column widths, either numeric vector or
#'   a `"|"`-delimited string (e.g. `"10|5|5"`).
#' @param column_labels Optional column header overrides, as a named vector/list
#'   or a data frame with `column` and `label`.
#' @param spanning_headers Optional spanning header definitions, as a data frame
#'   or list with fields `from`, `to`, and `label`.
#' @param report_orientation Page orientation (`"landscape"` or `"portrait"`).
#' @param report_paper_size Paper size (`"letter"`, `"legal"`, `"a4"`, `"rd4"`,
#'   `"none"`, or numeric length-2 vector).
#' @param report_units Units for dimensions (`"inches"` or `"cm"`).
#' @param report_margins Optional margins as named vector/list (`top`, `right`,
#'   `bottom`, `left`) or numeric length-4 vector.
#' @param report_font_size Base report font size.
#' @param indent_unit Number of spaces per indent level in `label`.
#' @param output_types Output types to write; supported values are `"RTF"` and
#'   `"TXT"`.
#' @param save_rds Logical; when `TRUE`, save processed output data and ARD
#'   object (if available) as `.rds` files.
#' @param rds_dir Reserved argument for backward compatibility.
#' @param rows_per_page Optional maximum number of table rows per page. If
#'   `NULL`, estimated from page geometry and `report_font_size`.
#' @param debug_indent Logical; print indentation diagnostics.
#' @param debug_spanning Logical; print spanning-header diagnostics.
#' @param group_columns Optional grouping columns to hide and apply
#'   `blank_after`.
#' @param group_blank_after Logical; whether to apply `blank_after` based on
#'   grouping columns.
#'
#' @return A character vector containing generated output file paths.
#'
#' @examplesIf requireNamespace("reporter", quietly = TRUE)
#' gts_tbl <- gtsummary::trial |>
#'   dplyr::select(trt, age, grade) |>
#'   gtsummary::tbl_summary(by = trt) |>
#'   gtsummary::add_p()
#'
#' out <- gtsummary_to_reporter_clinical(
#'   gts_obj = gts_tbl,
#'   file_path = tempfile("clinical_report_", fileext = ".rtf"),
#'   output_types = "TXT",
#'   save_rds = FALSE
#' )
#'
#' out
#'
#' @export


gtsummary_to_reporter_clinical <- function(gts_obj, file_path = "Clinical_Report.rtf",
                                           max_table_width = NULL, min_col_width = 0.6,
                                           column_widths = NULL,
                                           column_labels = NULL,
                                           spanning_headers = NULL,
                                           report_orientation = "landscape",
                                           report_paper_size = "letter",
                                           report_units = "inches",
                                           report_margins = NULL,
                                           report_font_size = 9,
                                           indent_unit = 1,
                                           output_types = c("RTF", "TXT"),
                                           save_rds = TRUE,
                                           rds_dir = "rds",
                                           rows_per_page = NULL,
                                           debug_indent = FALSE,
                                           debug_spanning = FALSE,
                                           group_columns = NULL,
                                           group_blank_after = TRUE) {

  strip_md_bold <- function(x) {
    if (is.null(x)) return(x)
    x_chr <- as.character(x)
    x_chr <- gsub("\\*\\*(.*?)\\*\\*", "\\1", x_chr, perl = TRUE)
    x_chr <- gsub("__([^_]+)__", "\\1", x_chr, perl = TRUE)
    x_chr
  }

  # A. Extract data and styling metadata
  has_table_body <- is.data.frame(gts_obj) && "table_body" %in% names(gts_obj)
  is_plain_df <- is.data.frame(gts_obj) && !has_table_body
  if (is_plain_df) {
    raw_body <- gts_obj
    styling <- NULL
  } else {
    raw_body <- gts_obj$table_body
    styling <- gts_obj$table_styling
  }

  collect_env_lines <- function(prefix, max_n = 9L, env = parent.frame()) {
    vals <- character(0)
    for (i in seq_len(max_n)) {
      nm <- paste0(prefix, i)
      if (exists(nm, envir = env, inherits = TRUE)) {
        val <- get(nm, envir = env, inherits = TRUE)
        if (!is.null(val) && length(val) > 0) {
          vals <- c(vals, as.character(val))
        }
      }
    }
    vals <- vals[!is.na(vals) & nzchar(vals)]
    vals
  }

  title <- collect_env_lines("title", max_n = 9L, env = parent.frame())
  footnotes_vec <- collect_env_lines("footnote", max_n = 9L, env = parent.frame())
  title <- strip_md_bold(title)
  footnotes_vec <- strip_md_bold(footnotes_vec)


  resolve_rows <- function(row_spec, data) {
    if (is.null(row_spec)) return(integer(0))

    if (is.list(row_spec) && !rlang::is_quosure(row_spec)) {
      return(unique(unlist(lapply(row_spec, resolve_rows, data = data))))
    }

    if (is.logical(row_spec) && length(row_spec) == nrow(data)) {
      return(which(row_spec))
    }

    if (is.numeric(row_spec)) return(row_spec)

    if (rlang::is_quosure(row_spec) || rlang::is_expression(row_spec) ||
        rlang::is_call(row_spec) || rlang::is_symbol(row_spec)) {
      res <- rlang::eval_tidy(row_spec, data = data)
      if (is.logical(res)) return(which(res))
      if (is.numeric(res)) return(res)
    }

    integer(0)
  }

  indent_map <- tibble::tibble(row_id = integer(), indent = integer())
  indent_tbl <- if (!is.null(styling)) styling$indent else NULL
  if (!is.null(indent_tbl) && nrow(indent_tbl) > 0) {
    indent_vals <- NULL
    if ("n_spaces" %in% names(indent_tbl)) {
      indent_vals <- indent_tbl$n_spaces
    } else if ("indentation" %in% names(indent_tbl)) {
      indent_vals <- indent_tbl$indentation
    } else if ("indent" %in% names(indent_tbl)) {
      indent_vals <- indent_tbl$indent
    }
    if (is.null(indent_vals)) {
      indent_vals <- rep(0L, nrow(indent_tbl))
    }

    indent_vec <- rep(NA_integer_, nrow(raw_body))
    for (i in seq_len(nrow(indent_tbl))) {
      rows_spec <- indent_tbl$rows[[i]]
      indent_val <- indent_vals[[i]]
      row_ids <- resolve_rows(rows_spec, raw_body)
      if (is.null(indent_val) || is.na(indent_val)) indent_val <- 0L
      if (length(row_ids) > 0) {
        indent_vec[row_ids] <- as.integer(indent_val)
      }
    }

    indent_map <- tibble::tibble(
      row_id = which(!is.na(indent_vec)),
      indent = indent_vec[!is.na(indent_vec)]
    )
  }

  has_label_rows <- "row_type" %in% names(raw_body) &&
    any(raw_body$row_type == "label", na.rm = TRUE)


  get_col_label <- function(x, default) {
    lbl <- attr(x, "label", exact = TRUE)
    if (is.null(lbl) || !nzchar(as.character(lbl))) return(default)
    as.character(lbl)
  }

  if (!is.null(styling) && !is.null(styling$header)) {
    header_df <- styling$header %>%
      mutate(
        column = as.character(column),
        label  = strip_md_bold(as.character(label)),
        hide   = as.logical(hide)
      )
  } else {
    col_labels <- vapply(
      raw_body,
      get_col_label,
      character(1),
      default = ""
    )
    col_labels[col_labels == ""] <- names(raw_body)[col_labels == ""]
    header_df <- tibble::tibble(
      column = names(raw_body),
      label = strip_md_bold(unname(col_labels)),
      hide = FALSE
    )
  }

  visible_cols <- header_df %>%
    dplyr::filter(!hide) %>%
    dplyr::distinct(column) %>%
    dplyr::pull(column)

  visible_cols <- intersect(visible_cols, names(raw_body))

  pvalue_cols <- names(raw_body)[grepl("^p\\.value", names(raw_body))]

  if (is_plain_df) {
    processed_df <- raw_body
  } else {
    group_cols_raw <- grep("^group\\d+_level$", names(raw_body), value = TRUE)
    selected_cols <- unique(c(
      "label", "row_type", "variable",
      styling$header$column[!styling$header$hide],
      group_cols_raw
    ))
    processed_df <- raw_body %>%
      select(any_of(selected_cols)) %>%
      mutate(row_id = dplyr::row_number()) %>%
      dplyr::left_join(indent_map, by = "row_id") %>%
      mutate(
        label = if (nrow(indent_map) > 0) sub("^\\s+", "", label) else label,
        label = ifelse(!is.na(indent), paste0(strrep(" ", indent * indent_unit), label), label)
      )
  }

  if (isFALSE(group_blank_after)) {
    group_cols_to_hide <- character(0)
  } else if (!is.null(group_columns)) {
    group_cols_to_hide <- intersect(as.character(group_columns), names(processed_df))
  } else {
    group_cols_to_hide <- grep("^group\\d+_level$", names(processed_df), value = TRUE)
  }

  if (isTRUE(debug_indent) && !is.null(indent_tbl) && nrow(indent_tbl) > 0) {
    debug_df <- raw_body %>%
      mutate(
        row_id = dplyr::row_number(),
        leading_spaces = ifelse(
          is.na(label),
          NA_integer_,
          nchar(label) - nchar(sub("^\\s+", "", label))
        )
      ) %>%
      select(row_id, row_type, variable, label, leading_spaces) %>%
      dplyr::left_join(indent_map, by = "row_id")

    message("---- INDENT DEBUG (first 20 rows) ----")
    print(utils::head(debug_df, 20))
    message("---- STYLING$INDENT ----")
    print(indent_tbl)
    if (!is.null(indent_tbl) && nrow(indent_tbl) > 0) {
      rows_to_text <- function(x) {
        if (is.list(x) && !rlang::is_quosure(x)) return("list")
        if (rlang::is_quosure(x)) return(rlang::quo_text(x))
        if (rlang::is_expression(x) || rlang::is_call(x) || rlang::is_symbol(x) || rlang::is_language(x)) {
          return(rlang::expr_text(x))
        }
        if (is.logical(x)) return("logical")
        if (is.numeric(x)) return("numeric")
        class(x)[1]
      }

      indent_vals_dbg <- indent_vals
      if (is.null(indent_vals_dbg)) indent_vals_dbg <- rep(0L, nrow(indent_tbl))

      rule_df <- purrr::map2_dfr(
        indent_tbl$rows,
        indent_vals_dbg,
        function(rows_spec, indent_val) {
          row_ids <- resolve_rows(rows_spec, raw_body)
          sample_labels <- raw_body$label[row_ids]
          sample_labels <- sample_labels[!is.na(sample_labels)]
          sample_labels <- utils::head(sample_labels, 3)
          tibble::tibble(
            indent = as.integer(indent_val),
            rows_expr = rows_to_text(rows_spec),
            match_n = length(row_ids),
            example = paste(sample_labels, collapse = " | ")
          )
        }
      )

      message("---- INDENT RULES (matches) ----")
      print(rule_df)
    }
  }

  if (!is_plain_df && length(pvalue_cols) > 0) {
    if ("variable" %in% names(processed_df)) {
      processed_df <- processed_df %>%
        dplyr::group_by(variable) %>%
        mutate(
          .has_p = dplyr::if_any(any_of(pvalue_cols), ~ !is.na(.x) & .x != ""),
          .keep_p = if (any(.has_p)) dplyr::row_number() == min(which(.has_p)) else FALSE
        ) %>%
        dplyr::ungroup() %>%
        mutate(across(any_of(pvalue_cols), ~ ifelse(.keep_p, .x, NA)))
    } else {
      processed_df <- processed_df %>%
        mutate(
          .has_p = dplyr::if_any(any_of(pvalue_cols), ~ !is.na(.x) & .x != ""),
          .keep_p = if (any(.has_p)) dplyr::row_number() == min(which(.has_p)) else FALSE
        ) %>%
        mutate(across(any_of(pvalue_cols), ~ ifelse(.keep_p, .x, NA)))
    }
  }

  if (!is_plain_df && length(pvalue_cols) > 0) {
    processed_df <- processed_df %>%
      mutate(
        across(
          any_of(pvalue_cols),
          ~ {
            x_num <- if (is.numeric(.x)) .x else suppressWarnings(as.numeric(.x))
            out <- ifelse(
              is.na(x_num),
              "",
              formatC(x_num, format = "f", digits = 4)
            )
            out
          }
        )
      )
  }

  if (!is_plain_df) {
    processed_df <- processed_df %>%
      select(-row_id, -indent, -row_type, -variable, -any_of(c(".keep_p", ".has_p")))
  }

  if (isTRUE(save_rds)) {
    rds_full_dir <- dirname(file_path)
    if (is.null(rds_full_dir) || rds_full_dir == "." || !nzchar(rds_full_dir)) {
      rds_full_dir <- getwd()
    }
    if (!dir.exists(rds_full_dir)) {
      dir.create(rds_full_dir, recursive = TRUE)
    }
  }

  missing_cols <- setdiff(
    header_df %>% dplyr::filter(!hide) %>% dplyr::pull(column),
    names(processed_df)
  )
  if (length(missing_cols) > 0) {
    message(
      paste(missing_cols, collapse = ", "))
  }

  calc_col_width <- function(x, min_width = 0.8, max_width = 3.5, chars_per_in = 8) {
    chars <- suppressWarnings(max(nchar(as.character(x)), na.rm = TRUE))
    if (!is.finite(chars)) chars <- min_width * chars_per_in
    width <- chars / chars_per_in
    pmin(max_width, pmax(min_width, width))
  }

  parse_column_widths <- function(widths, n_cols) {
    if (is.null(widths)) return(NULL)

    if (is.character(widths) && length(widths) == 1L) {
      parts <- unlist(strsplit(widths, "\\|", fixed = FALSE))
      widths <- suppressWarnings(as.numeric(trimws(parts)))
    }

    if (!is.numeric(widths)) return(NULL)
    widths <- widths[is.finite(widths)]
    if (length(widths) == 0) return(NULL)

    if (length(widths) < n_cols) {
      widths <- c(widths, rep(tail(widths, 1), n_cols - length(widths)))
    } else if (length(widths) > n_cols) {
      widths <- widths[seq_len(n_cols)]
    }

    widths
  }

  adjust_col_widths <- function(widths, max_total, min_width) {
    widths[!is.finite(widths)] <- min_width
    if (length(widths) == 0) return(widths)

    if (length(widths) * min_width > max_total) {
      min_width <- max_total / length(widths)
    }

    widths <- pmax(widths, min_width)
    total <- sum(widths, na.rm = TRUE)
    if (total <= max_total) return(widths)

    extra <- pmax(widths - min_width, 0)
    extra_total <- sum(extra, na.rm = TRUE)
    if (extra_total <= 0) {
      return(widths * (max_total / total))
    }

    scale <- (max_total - length(widths) * min_width) / extra_total
    scale <- max(0, min(1, scale))
    widths <- min_width + extra * scale

    if (sum(widths, na.rm = TRUE) > max_total) {
      widths <- widths * (max_total / sum(widths, na.rm = TRUE))
    }

    widths
  }

  get_paper_dims <- function(paper_size, units) {
    if (is.numeric(paper_size) && length(paper_size) == 2) return(as.numeric(paper_size))
    size_key <- tolower(as.character(paper_size))
    dims <- switch(
      size_key,
      "letter" = c(8.5, 11),
      "legal" = c(8.5, 14),
      "a4" = c(8.27, 11.69),
      "rd4" = c(8.27, 11.69),
      "none" = c(Inf, Inf),
      c(8.5, 11)
    )
    if (tolower(units) == "cm") dims <- dims * 2.54
    dims
  }

  get_default_margins <- function(units) {
    if (tolower(units) == "cm") {
      return(c(top = 1.27, right = 2.54, bottom = 1.27, left = 2.54))
    }
    c(top = 0.5, right = 1, bottom = 0.5, left = 1)
  }

  normalize_margins <- function(margins, units) {
    if (is.null(margins)) return(get_default_margins(units))
    if (is.numeric(margins) && length(margins) == 4) {
      if (is.null(names(margins))) {
        names(margins) <- c("top", "right", "bottom", "left")
      }
      return(margins)
    }
    if (is.list(margins)) {
      out <- get_default_margins(units)
      nm <- intersect(names(margins), names(out))
      out[nm] <- unlist(margins[nm])
      return(out)
    }
    get_default_margins(units)
  }

  compute_max_table_width <- function(paper_size, orientation, units, margins) {
    dims <- get_paper_dims(paper_size, units)
    if (tolower(orientation) == "landscape") dims <- rev(dims)
    margins <- normalize_margins(margins, units)
    max(0, dims[1] - margins["left"] - margins["right"])
  }

  compute_max_table_height <- function(paper_size, orientation, units, margins) {
    dims <- get_paper_dims(paper_size, units)
    if (tolower(orientation) == "landscape") dims <- rev(dims)
    margins <- normalize_margins(margins, units)
    max(0, dims[2] - margins["top"] - margins["bottom"])
  }

  estimate_rows_per_page <- function(row_count, font_size, paper_size, orientation, units, margins) {
    height <- compute_max_table_height(paper_size, orientation, units, margins)
    if (!is.finite(height) || height <= 0 || row_count <= 0) return(NULL)

    line_height <- (as.numeric(font_size) * 1.2) / 72
    if (tolower(units) == "cm") line_height <- line_height * 2.54

    reserve_lines <- 4
    header_lines <- 2
    available_lines <- floor((height / line_height) - reserve_lines - header_lines)
    if (!is.finite(available_lines) || available_lines <= 0) return(NULL)
    min(row_count, available_lines)
  }

  format_iso8601_utc <- function(x) {
    paste0(format(x, tz = "UTC", "%Y-%m-%dT%H:%M:%S"), " (UTC+0)")
  }

  normalize_column_labels <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.character(x) || is.list(x)) {
      if (is.null(names(x))) return(NULL)
      return(as.list(x))
    }
    if (is.data.frame(x)) {
      names(x) <- tolower(names(x))
      if (all(c("column", "label") %in% names(x))) {
        out <- as.list(x$label)
        names(out) <- x$column
        return(out)
      }
    }
    NULL
  }

  label_overrides <- normalize_column_labels(column_labels)
  if (!is.null(label_overrides)) {
    label_overrides <- lapply(label_overrides, strip_md_bold)
  }

  header_label_map <- header_df %>%
    dplyr::filter(!hide) %>%
    select(column, label)
  if (!is.null(label_overrides)) {
    header_label_map$label <- ifelse(
      header_label_map$column %in% names(label_overrides),
      unlist(label_overrides[header_label_map$column]),
      header_label_map$label
    )
  }
  header_label_map <- dplyr::bind_rows(
    tibble::tibble(column = "label", label = "Characteristic"),
    header_label_map
  )
  if (!is.null(label_overrides) && "label" %in% names(label_overrides)) {
    header_label_map$label[header_label_map$column == "label"] <- label_overrides[["label"]]
  }

  width_cols <- setdiff(names(processed_df), group_cols_to_hide)
  if (!"label" %in% width_cols && "label" %in% names(processed_df)) {
    width_cols <- c("label", width_cols)
  }
  col_widths <- vapply(processed_df[width_cols], calc_col_width, numeric(1))
  label_widths <- setNames(
    vapply(header_label_map$label, calc_col_width, numeric(1)),
    header_label_map$column
  )
  col_widths[names(label_widths)] <- pmax(col_widths[names(label_widths)], label_widths)
  if (is.null(max_table_width)) {
    max_table_width <- compute_max_table_width(
      paper_size = report_paper_size,
      orientation = report_orientation,
      units = report_units,
      margins = report_margins
    )
  }

  manual_widths <- parse_column_widths(column_widths, n_cols = length(col_widths))
  if (!is.null(manual_widths)) {
    col_widths <- manual_widths
    names(col_widths) <- width_cols
    if (is.finite(max_table_width)) {
      col_widths <- adjust_col_widths(col_widths, max_total = max_table_width, min_width = min_col_width)
    }
  } else {
    if ("label" %in% names(col_widths) && is.finite(max_table_width)) {
      label_target <- col_widths[["label"]]
      other_cols <- setdiff(names(col_widths), "label")
      min_other_total <- length(other_cols) * min_col_width
      max_label_width <- max_table_width - min_other_total
      label_width <- min(label_target, max_label_width)
      label_width <- max(label_width, min_col_width)

      other_widths <- col_widths[other_cols]
      other_widths[!is.finite(other_widths)] <- min_col_width
      other_widths <- pmax(other_widths, min_col_width)

      total_other <- sum(other_widths)
      remaining <- max_table_width - label_width
      if (total_other > remaining && total_other > 0) {
        other_widths <- other_widths * (remaining / total_other)
      }

      col_widths <- c(label = label_width, other_widths)
      col_widths <- col_widths[names(processed_df)]
    } else {
      col_widths <- adjust_col_widths(col_widths, max_total = max_table_width, min_width = min_col_width)
    }
  }

  col_map <- header_df %>% dplyr::filter(!hide)

  cols_to_define <- setdiff(names(processed_df), c("label", group_cols_to_hide))
  ordered_cols <- names(processed_df)
  center_cols <- setdiff(cols_to_define, "label")

  normalize_spanning_headers <- function(x) {
    if (is.null(x)) return(NULL)

    if (is.data.frame(x)) {
      df <- x
      names(df) <- tolower(names(df))
      if (all(c("from", "to", "label") %in% names(df))) {
        return(df[, c("from", "to", "label"), drop = FALSE])
      }
    }

    if (is.list(x)) {
      if (!is.null(names(x)) && all(c("from", "to", "label") %in% names(x))) {
        return(data.frame(from = x$from, to = x$to, label = x$label, stringsAsFactors = FALSE))
      }

      if (length(x) > 0 && all(vapply(x, is.list, logical(1)))) {
        rows <- lapply(x, function(item) {
          if (all(c("from", "to", "label") %in% names(item))) {
            data.frame(from = item$from, to = item$to, label = item$label, stringsAsFactors = FALSE)
          } else {
            NULL
          }
        })
        rows <- rows[!vapply(rows, is.null, logical(1))]
        if (length(rows) > 0) return(do.call(rbind, rows))
      }
    }

    NULL
  }

  # E. Handle Spanning Headers
  span_df <- if (!is.null(styling)) styling$spanning_header else NULL
  span_use <- if (!is.null(spanning_headers)) normalize_spanning_headers(spanning_headers) else span_df
  spanning_header_fn <- tryCatch(getFromNamespace("spanning_header", "reporter"), error = function(e) NULL)

  resolve_span_col <- function(x, cols) {
    if (is.null(x)) return(NULL)
    if (is.numeric(x) && length(x) == 1) {
      idx <- as.integer(x)
      if (idx >= 1 && idx <= length(cols)) return(cols[idx])
      return(NULL)
    }
    if (is.character(x) && length(x) == 1 && x %in% cols) return(x)
    NULL
  }

  resolve_span_index <- function(x, cols) {
    if (is.null(x)) return(NULL)
    if (is.numeric(x) && length(x) == 1) {
      idx <- as.integer(x)
      if (idx >= 1 && idx <= length(cols)) return(idx)
      return(NULL)
    }
    if (is.character(x) && length(x) == 1) {
      idx <- match(x, cols)
      if (!is.na(idx)) return(idx)
    }
    NULL
  }

  apply_spanning_rows <- function(df) {
    if (is.null(span_use) || nrow(span_use) == 0) return(df)
    if (!is.null(spanning_header_fn)) return(df)

    span_rows <- vector("list", nrow(span_use))
    for (i in seq_len(nrow(span_use))) {
      from_idx <- resolve_span_index(span_use$from[i], ordered_cols)
      to_idx <- resolve_span_index(span_use$to[i], ordered_cols)
      label_val <- strip_md_bold(span_use$label[i])
      if (is.null(from_idx) || is.null(to_idx) || is.null(label_val)) next
      if (from_idx > to_idx) {
        tmp <- from_idx
        from_idx <- to_idx
        to_idx <- tmp
      }
      mid_idx <- floor((from_idx + to_idx) / 2)
      row_vals <- rep("", length(ordered_cols))
      row_vals[mid_idx] <- as.character(label_val)
      span_rows[[i]] <- as.list(row_vals)
    }

    span_rows <- span_rows[!vapply(span_rows, is.null, logical(1))]
    if (length(span_rows) == 0) return(df)
    span_df_rows <- do.call(rbind.data.frame, c(span_rows, list(stringsAsFactors = FALSE)))
    names(span_df_rows) <- ordered_cols
    rbind(span_df_rows, df)
  }

  apply_spanning_headers <- function(tbl_obj) {
    if (is.null(span_use) || nrow(span_use) == 0) return(tbl_obj)

    if (isTRUE(debug_spanning)) {
      message("SPANNING DEBUG: ordered_cols = ", paste(ordered_cols, collapse = ", "))
      message("SPANNING DEBUG: span_use = ")
      print(span_use)
      if (is.null(spanning_header_fn)) {
        message("SPANNING DEBUG: reporter::spanning_header not found in namespace. Using fallback rows.")
      }
    }

    if (is.null(spanning_header_fn)) return(tbl_obj)

    for (i in seq_len(nrow(span_use))) {
      from_col <- resolve_span_col(span_use$from[i], ordered_cols)
      to_col <- resolve_span_col(span_use$to[i], ordered_cols)
      from_idx <- resolve_span_index(span_use$from[i], ordered_cols)
      to_idx <- resolve_span_index(span_use$to[i], ordered_cols)
      label_val <- strip_md_bold(span_use$label[i])

      if (isTRUE(debug_spanning)) {
        message(sprintf("SPANNING DEBUG: from=%s to=%s label=%s | idx=%s-%s | cols=%s-%s",
                        as.character(span_use$from[i]),
                        as.character(span_use$to[i]),
                        as.character(label_val),
                        ifelse(is.null(from_idx), "NA", from_idx),
                        ifelse(is.null(to_idx), "NA", to_idx),
                        ifelse(is.null(from_col), "NA", from_col),
                        ifelse(is.null(to_col), "NA", to_col)))
      }

      if (is.null(label_val)) next
      if (is.null(from_col) && is.null(from_idx)) next
      if (is.null(to_col) && is.null(to_idx)) next

      # Prefer index-based spanning (more reliable with reporter)
      if (!is.null(from_idx) && !is.null(to_idx)) {
        tbl_obj <- spanning_header_fn(
          x = tbl_obj,
          from = from_idx,
          to = to_idx,
          label = label_val,
          standard_eval = TRUE
        )
      } else if (!is.null(from_col) && !is.null(to_col)) {
        tbl_obj <- spanning_header_fn(
          x = tbl_obj,
          from = from_col,
          to = to_col,
          label = label_val,
          standard_eval = TRUE
        )
      }
    }

    tbl_obj
  }

  build_table_spec <- function(df) {
    df <- apply_spanning_rows(df)
    label_header <- ""
    if (!is.null(label_overrides) && "label" %in% names(label_overrides)) {
      label_header <- label_overrides[["label"]]
    }

    tbl <- reporter::create_table(df, first_row_blank = TRUE, header_bold = TRUE, borders = c("top", "bottom")) %>%
      reporter::column_defaults(width = min(1.2, max(0.8, min(col_widths, na.rm = TRUE))), align = "left") %>%
      reporter::define(label, label = label_header, width = col_widths[["label"]], align = "left")

    if (length(group_cols_to_hide) > 0) {
      for (gcol in group_cols_to_hide) {
        if (!gcol %in% names(df)) next
        tbl <- do.call(
          reporter::define,
          list(x = tbl, var = as.name(gcol), visible = FALSE, blank_after = TRUE)
        )
      }
    }

    for (i in seq_len(nrow(col_map))) {
      this_col <- col_map$column[i]
      this_lab <- col_map$label[i]
      if (!is.null(label_overrides) && this_col %in% names(label_overrides)) {
        this_lab <- label_overrides[[this_col]]
      }

      if (!this_col %in% cols_to_define) next

      this_width <- if (this_col %in% names(col_widths)) col_widths[[this_col]] else NULL
      this_align <- if (this_col %in% center_cols) "center" else "left"

      tbl <- do.call(
        reporter::define,
        list(x = tbl, var = as.name(this_col), label = this_lab, width = this_width, align = this_align)
      )
    }

    tbl <- apply_spanning_headers(tbl)
    tbl
  }

  footnote_applied <- FALSE
  apply_tbl_footnotes <- function(tbl_obj) {
    if (length(footnotes_vec) == 0) return(tbl_obj)
    tryCatch(
      {
        footnote_applied <<- TRUE
        for (note in footnotes_vec) {
          tbl_obj <- reporter::footnotes(tbl_obj, note, blank_row = "none")
        }
        tbl_obj
      },
      error = function(e) {
        footnote_applied <<- FALSE
        tbl_obj
      }
    )
  }



  # F. Assemble Report
  print(processed_df)
  add_content_safe <- function(x, object, ...) {
    args <- list(...)
    keep <- intersect(names(args), names(formals(reporter::add_content)))
    args <- args[keep]
    do.call(reporter::add_content, c(list(x = x, object = object), args))
  }

  rpt <- reporter::create_report(
    font = "Courier",
    orientation = report_orientation,
    paper_size = report_paper_size,
    units = report_units,
    font_size = report_font_size
  ) %>%
    #page_header(left = "Protocol: STUDY-001", right = "Confidential") %>%
    {if (length(title) > 0) reporter::titles(., title, bold = TRUE, font_size = report_font_size, align = "left") else .}

  if (!is.null(report_margins)) {
    report_margins <- normalize_margins(report_margins, report_units)
    rpt <- do.call(
      reporter::set_margins,
      c(list(x = rpt), as.list(report_margins))
    )
  }

  if (is.null(rows_per_page)) {
    rows_per_page <- estimate_rows_per_page(
      row_count = nrow(processed_df),
      font_size = report_font_size,
      paper_size = report_paper_size,
      orientation = report_orientation,
      units = report_units,
      margins = report_margins
    )
  }

  if (!is.null(rows_per_page) && is.finite(rows_per_page) && rows_per_page > 0) {
    row_ids <- seq_len(nrow(processed_df))
    chunk_ids <- split(row_ids, ceiling(row_ids / rows_per_page))
    for (i in seq_along(chunk_ids)) {
      chunk_df <- processed_df[chunk_ids[[i]], , drop = FALSE]
      tbl_chunk <- build_table_spec(chunk_df)
      tbl_chunk <- apply_tbl_footnotes(tbl_chunk)
      rpt <- add_content_safe(
        rpt,
        tbl_chunk,
        blank_row = "none",
        align = "left",
        page_break = i < length(chunk_ids)
      )
    }
  } else {
    tbl <- build_table_spec(processed_df)
    tbl <- apply_tbl_footnotes(tbl)
    rpt <- add_content_safe(rpt, tbl, blank_row = "none", align = "left")
  }

  if (!isTRUE(footnote_applied) && length(footnotes_vec) > 0) {
    for (note in footnotes_vec) {
      rpt <- rpt %>%
        reporter::footnotes(note, blank_row = "none")
    }
  }

  output_types <- toupper(output_types)
  base_path <- sub("\\.[^.]+$", "", file_path)
  output_paths <- character(0)

  progname_val <- ""
  if (exists("progname", envir = parent.frame(), inherits = TRUE)) {
    progname_val <- as.character(get("progname", envir = parent.frame(), inherits = TRUE))
  }
  today <- Sys.Date()
  date9 <- toupper(sprintf("%02d%s%04d", as.integer(format(today, "%d")),
                           month.abb[as.integer(format(today, "%m"))],
                           as.integer(format(today, "%Y"))))
  hm_time <- format(Sys.time(), "%H:%M")
  hm_time <- sub("^0", "", hm_time)
  footer_right <- trimws(paste(progname_val, date9, hm_time))
  base_name <- tools::file_path_sans_ext(basename(file_path))

  if (isTRUE(save_rds)) {
    rds_full_dir <- dirname(file_path)
    if (is.null(rds_full_dir) || rds_full_dir == "." || !nzchar(rds_full_dir)) {
      rds_full_dir <- getwd()
    }
    if (!is.null(rds_full_dir)) {
      output_data_rds <- file.path(rds_full_dir, paste0(base_name, "_output_data.rds"))
      ard_rds <- file.path(rds_full_dir, paste0(base_name, "_ard.rds"))

      saveRDS(processed_df, output_data_rds)
      if (!is_plain_df) {
        ard_obj <- NULL
        if (exists("gather_ard", where = asNamespace("gtsummary"), inherits = FALSE)) {
          ard_obj <- tryCatch(gtsummary::gather_ard(gts_obj), error = function(e) NULL)
        } else if (exists("as_ard", where = asNamespace("cards"), inherits = FALSE)) {
          ard_obj <- tryCatch(cards::as_ard(gts_obj), error = function(e) NULL)
        }
        if (!is.null(ard_obj)) {
          saveRDS(ard_obj, ard_rds)
        } else {
          message("ARD not saved: as_ard not available or failed.")
        }
      }
    }
  }

  if ("RTF" %in% output_types) {
    rtf_path <- paste0(base_path, ".rtf")
    rpt_rtf <- rpt %>%
      reporter::page_footer(right = footer_right)
    reporter::write_report(rpt_rtf, rtf_path, output_type = "RTF")
    output_paths <- c(output_paths, rtf_path)
  }

  if ("TXT" %in% output_types) {
    txt_path <- paste0(base_path, ".txt")
    rpt_txt <- rpt %>%
      reporter::page_footer(right = footer_right) %>%
      reporter::options_fixed(uchar = "_")
    reporter::write_report(rpt_txt, txt_path, output_type = "TXT")
    output_paths <- c(output_paths, txt_path)
  }

  message(paste("Report generated at:", paste(output_paths, collapse = ", ")))
  return(output_paths)
}


# 1. Build gtsummary object
# my_gts_table <- trial %>%
#   select(trt, age, grade, response) %>%
#   tbl_summary(by = trt) %>%
#   add_p() %>%
#   # Add a spanning header common in clinical tables
#   modify_spanning_header(all_stat_cols() ~ "**Treatment Group**") %>%
#   bold_labels()
#
# my_gts_table
#
# # Optional metadata for auto titles/footnotes (variables in environment)
# title1 <- "Table 14.1 Summary of Demographic and Baseline Characteristics"
# footnote1 <- "aa"
# footnote2 <- "bb"
# footnote3 <- "cc"
# progname <- "ctr/t-bin-eff.r"
#
# # 2. Convert and Save
# final_path <- gtsummary_to_reporter_clinical(
#   gts_obj = my_gts_table,
#   file_path = "Clinical_Trial_Output.rtf"
# )
#
# # 2b. Spanning header example
# span_tbl <- trial %>%
#   select(trt, age, grade, response) %>%
#   tbl_summary(by = trt) %>%
#   modify_spanning_header(all_stat_cols() ~ "**Treatment Group**")
#
# title1 <- "Table 14.1b Spanning Header Example"
# progname <- "ctr/t-bin-eff.r"
#
# span_path <- gtsummary_to_reporter_clinical(
#   gts_obj = span_tbl,
#   file_path = "Clinical_Trial_Spanning_Header_Output.rtf",
#   spanning_headers = data.frame(
#     from = "stat_1",
#     to = "stat_2",
#     label = "Treatment Group",
#     stringsAsFactors = FALSE
#   ),
#   column_labels = c(
#     label = "Visit / Statistic",
#     stat_1 = "Placebo",
#     stat_2 = "Active",
#     stat_3 = "Total"
#   ),
#   debug=TRUE,
#   debug_spanning = TRUE
# )
#
#
# # 3. AE by SOC/PT nested example for pagination testing
#
#
# ae_tbl<-tbl_hierarchical(
#   data = cards::ADAE,
#   variables = c(AESOC, AETERM),
#   by = TRTA,
#   denominator = cards::ADSL,
#   id = USUBJID,
#   digits = everything() ~ list(p = 1),
#   overall_row = TRUE,
#   label = list(..ard_hierarchical_overall.. = "Any Adverse Event")
# )
#
# ae_tbl
#
# tbl_n<-cards::ADSL|>
#   mutate(PATIENT=1L)|>
#   tbl_summary(
#     by=ARM,
#     include=PATIENT,
#     type=PATIENT~"dichotomous",
#     value=PATIENT~1L,
#     statistic=PATIENT ~"{N} ({p}%)",
#     label=PATIENT~"Number of Patients (N %)"
#
#   )
#
# tbl_combined<-tbl_stack(
#   list(
#     tbl_n,
#     ae_tbl
#   )
# )
#
# title1 <- "Table 14.2 AE by SOC/PT (Pagination Test)"
# footnote1 <- "Note: Dummy AE data for pagination stress test."
# progname <- "ctr/t-bin-eff.r"
#
# ae_path <- gtsummary_to_reporter_clinical(
#   gts_obj = tbl_combined,
#   file_path = "Clinical_Trial_AE_SOC_PT_Output.rtf",
#   rows_per_page = 20,
#   debug_indent = TRUE,
#   column_widths = "10|5|5"
# )
#
#
# # 4. Plain tibble/data.frame example (no gtsummary styling/indent)
# simple_tbl <- tibble::tibble(
#   label = c(
#     "Row 1",
#     "  Row 2 (pre-indented)",
#     "Row 3",
#     "Row 4 - This is a very long label to test first column width without wrapping"
#   ),
#   stat_1 = c("10 (50%)", "5 (25%)", "5 (25%)", "20 (80%)"),
#   stat_2 = c("12 (60%)", "4 (20%)", "4 (20%)", "18 (72%)")
# )
#
# title1 <- "Table 14.3 Simple Tibble Input"
# progname <- "ctr/t-bin-eff.r"
#
# simple_path <- gtsummary_to_reporter_clinical(
#   gts_obj = simple_tbl,
#   file_path = "Clinical_Trial_Simple_Tibble_Output.rtf",
#   output_types = c("RTF", "TXT"),
#   column_widths = "10|5|5"
# )

