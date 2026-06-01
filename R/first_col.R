#' Calculate information for width
#'
#' Modifies the table
#' @inheritParams modify_header_rm_md
#'
#' @returns A gtsummary table.
#' @export
#'
#' @examples
#' tbl <- gtsummary::tbl_summary(data = ADSL, by = "SEX", include = "ACTARM") |>
#'     gtsummary::add_stat_label(location = "column")
#' calculate_widths(tbl)
calculate_widths <- function(x) {
  set_cli_abort_call()
  updated_call_list <- c(x$call_list, list(modify_table_styling = match.call()))

  check_class(x, "gtsummary")
  tb <- x$table_body
  which_rows <- tb$row_type %in% c("label", "level")
  which_type <- tb$row_type
  which_indent <- x$table_styling$indent$n_spaces
  names(which_indent) <- c("label", "level")
  chars_indent <- which_indent[match(which_type, names(which_indent))]
  chars_rows <- tb$label[which_rows]
  chars_split <- strsplit(chars_rows, "\\s")
  chars_spaces <- lengths(chars_split) - 1L

  chars_lengths <- lapply(chars_split, nchar)
  chars_length <- vapply(chars_lengths, sum, 1)
  df <- data.frame(chars_length, chars_indent, chars_spaces, chars_type = names(chars_indent))
  df$chars_lengths <- chars_lengths
  df$raw_chars <- chars_rows
  chr_l <- df$chars_type == "level"
  max_level_width <- max(df$chars_length[chr_l]+df$chars_indent[chr_l] + df$chars_spaces[chr_l])
  chars_gap <- max(df$chars_length[!chr_l]+df$chars_indent[!chr_l] + df$chars_spaces[!chr_l]) -
    max_level_width

  # Calculate best width
  # Where should newlines go? On labels
  # How many newlines should go? Currently one
  space2newlines <- lapply(df$chars_lengths[!chr_l],
         function(x) {which(cumsum(x) > max_level_width)[1]})

  out <- Map(function(x, y){
    paste0(paste(x[seq_len(y)], collapse = " "), "\n", paste(x[-seq_len(y)], collapse = " "))
  }, chars_split[!chr_l], space2newlines)
  out <- unlist(out, FALSE, FALSE)

  # Replace the original var_label and label with the new
  m <- match(tb$var_label, tb$label)
  # x$table_body$var_label <- out[m]
  x$table_body$label[!chr_l] <- out
  x$call_list <- updated_call_list
  x
}

modif_first_col <- function(x) {
  set_cli_abort_call()
  check_class(x, "gtsummary")

  # Update labels
  # update levels
  # in-place replacement
  x
}
