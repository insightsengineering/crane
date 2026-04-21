#' Create a gtsummary table with overlapping pooled columns
#'
#' @description
#' Generates a gtsummary table that includes both the original treatment arms
#' and user-defined pooled treatment arms. It does this by creating individual
#' tables for the original data and each pool, then merging them together
#' seamlessly using `gtsummary::tbl_merge()`. This approach keeps the underlying
#' dataset rows completely unique, preserving standard ADaM integrity while
#' bypassing the need for complex pre-processing.
#'
#' @param data (`data.frame`)\cr
#'   The main analysis dataset (e.g., `adae`).
#' @param pools (`list`)\cr
#'   Named list of custom pools. Use the keyword `"all"` to include a total column.
#' @param by (`character`)\cr
#'   The treatment arm variable name.
#' @param denominator (`data.frame` or `NULL`)\cr
#'   The denominator dataset (e.g., `adsl`). Leave as NULL for functions that do not
#'   use a denominator (like `tbl_summary`). Provide the dataset for functions that
#'   require it (like `tbl_hierarchical_rate_and_count`).
#' @param keep_original (`logical`)\cr
#'   Keep the unpooled treatment arms. Default is `TRUE`.
#' @param .tbl_fun (`function`)\cr
#'   The gtsummary function to apply (e.g., `tbl_summary`).
#'
#' @param ... Additional arguments passed directly to `.tbl_fun`.
#'
#' @return A merged `gtsummary` object of class `tbl_merge`.
#'
#' @examples
#' # Create minimal dummy ADaM data
#' adsl <- data.frame(
#'   USUBJID = c("001", "002", "003", "004", "005"),
#'   TRT01A = c("Drug A", "Drug A", "Drug B", "Drug C", "Drug C"),
#'   AGE = c(45, 50, 60, 65, 55),
#'   stringsAsFactors = FALSE
#' )
#'
#' adae <- data.frame(
#'   USUBJID = c("001", "001", "002", "004", "005"),
#'   TRT01A = c("Drug A", "Drug A", "Drug A", "Drug C", "Drug C"),
#'   AEBODSYS = c("SOC1", "SOC1", "SOC2", "SOC1", "SOC2"),
#'   AEDECOD = c("PT1", "PT2", "PT3", "PT1", "PT4"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Define the requested pools
#' my_pools <- list(
#'   "Drugs A and B" = c("Drug A", "Drug B"),
#'   "All Patients"  = "all"
#' )
#'
#' # Example A: Safe pooling with standard gtsummary (no denominator) ---------------
#' safe_pools <- list("Drugs A and B" = c("Drug A", "Drug B"))
#'
#' tbl_safe <- tbl_with_pools(
#'   data = adsl,
#'   pools = safe_pools,
#'   by = "TRT01A",
#'   denominator = NULL,
#'   keep_original = FALSE,
#'   .tbl_fun = tbl_summary,
#'   include = AGE
#' )
#' tbl_safe
#'
#' # Example B: Triggering the skipped pool warning ---------------------------------
#' # This throws a warning because 'Drug Z' has zero patients in the denominator
#' warning_pools <- list("Drug Z Pool" = c("Drug Z"))
#'
#' tbl_warning <- tbl_with_pools(
#'   data = adae,
#'   pools = warning_pools,
#'   by = "TRT01A",
#'   keep_original = TRUE,
#'   .tbl_fun = tbl_summary,
#'   include = AEBODSYS
#' )
#' tbl_warning
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true") && requireNamespace("yaml", quietly = TRUE)
#' # Example C: Use yaml to define the pools config and run the function ------------
#'
#' # Define the config as a standard R list
#' config_to_write <- list(
#'   tbl_with_pools_config = list(
#'     keep_original = FALSE,
#'     arm_var = "TRT01A",
#'     pools = list(
#'       "Drug A + B"   = c("Drug A", "Drug B"),
#'       "Drug C + B"   = c("Drug C", "Drug B"),
#'       "All Patients" = "all"
#'     )
#'   )
#' )
#'
#' # Write it to a file (using a temp file for this example)
#' yaml_path <- tempfile(fileext = ".yaml")
#' yaml::write_yaml(config_to_write, yaml_path)
#'
#' # Print out what the physical YAML file looks like
#' cat("--- Contents of the generated YAML file ---\n")
#' cat(readLines(yaml_path), sep = "\n")
#' cat("-------------------------------------------\n\n")
#'
#' # Read the YAML file back into R
#' arg_specs <- yaml::read_yaml(yaml_path)
#'
#' # Extract just the poolings config block
#' pool_args <- arg_specs$tbl_with_pools_config
#'
#' # Run the function using tbl_hierarchical_rate_and_count
#' if (!is.null(pool_args)) {
#'   tbl_pooled_yaml <- tbl_with_pools(
#'     data = adae,
#'     pools = pool_args$pools,
#'     by = pool_args$arm_var,
#'     denominator = adsl,
#'     keep_original = pool_args$keep_original,
#'     .tbl_fun = tbl_hierarchical_rate_and_count,
#'     variables = c(AEBODSYS, AEDECOD)
#'   )
#'   tbl_pooled_yaml
#' }
#'
#' @export
tbl_with_pools <- function(
  data,
  pools,
  by = "TRT01A",
  denominator = NULL,
  keep_original = TRUE,
  .tbl_fun,
  ...
) {
  # Validate required structures using cli
  if (!is.data.frame(data)) {
    cli::cli_abort(c(
      "x" = "{.arg data} must be a data frame.",
      "i" = "Provided data is of class {.cls {class(data)}}."
    ))
  }
  if (!by %in% names(data)) {
    cli::cli_abort(c("x" = "The grouping variable {.val {by}} must exist in {.arg data}."))
  }
  if (!is.null(denominator)) {
    if (!is.data.frame(denominator)) {
      cli::cli_abort(c("x" = "{.arg denominator} must be a data frame or NULL."))
    }
    if (!by %in% names(denominator)) {
      cli::cli_abort(c("x" = "The grouping variable {.val {by}} must exist in {.arg denominator}."))
    }
  }
  if (!is.list(pools) || is.null(names(pools)) || any(names(pools) == "")) {
    cli::cli_abort(c("x" = "{.arg pools} must be a fully named list."))
  }
  if (!is.function(.tbl_fun)) {
    cli::cli_abort(c("x" = "{.arg .tbl_fun} must be a function (e.g., {.code gtsummary::tbl_summary})."))
  }

  # Initialize the list that will hold all generated gtsummary objects
  tbl_list <- list()

  # Generate the base table with the original unpooled arms if requested
  if (keep_original) {
    if (is.null(denominator)) {
      tbl_list[["original"]] <- rlang::inject(.tbl_fun(data, by = !!by, ...))
    } else {
      tbl_list[["original"]] <- rlang::inject(.tbl_fun(data, by = !!by, denominator = denominator, ...))
    }
  }

  # Loop through the configuration list to generate a single-column table for each pool
  for (pool_name in names(pools)) {
    arm_values <- pools[[pool_name]]

    # Handle the "all" keyword safely
    if (length(arm_values) == 1 && tolower(arm_values) == "all") {
      sub_data <- data |> dplyr::mutate(!!dplyr::sym(by) := as.character(pool_name))
      if (!is.null(denominator)) {
        sub_denom <- denominator |> dplyr::mutate(!!dplyr::sym(by) := as.character(pool_name))
      }
    } else {
      # Filter safely bypassing factor constraints
      sub_data <- data |>
        dplyr::filter(as.character(.data[[by]]) %in% as.character(arm_values)) |>
        dplyr::mutate(!!dplyr::sym(by) := as.character(pool_name))

      if (!is.null(denominator)) {
        sub_denom <- denominator |>
          dplyr::filter(as.character(.data[[by]]) %in% as.character(arm_values)) |>
          dplyr::mutate(!!dplyr::sym(by) := as.character(pool_name))
      }
    }

    # INDEPENDENT CHECKS:
    # 1. Skip if data is 0 (gtsummary/cards will crash trying to process a 0-row analysis dataset)
    if (nrow(sub_data) == 0) {
      cli::cli_warn("Pool {.val {pool_name}} has 0 rows in the data. Skipping.")
      next
    }

    # 2. Skip if denominator is 0 (prevents division by zero)
    if (!is.null(denominator) && nrow(sub_denom) == 0) {
      cli::cli_warn("Pool {.val {pool_name}} has 0 patients in the denominator. Skipping.")
      next
    }
    # Note: If denom > 0 but sub_data == 0, we intentionally proceed so gtsummary prints 0 (0%)

    # Apply the specified gtsummary function dynamically using rlang::inject
    if (is.null(denominator)) {
      tbl_list[[pool_name]] <- rlang::inject(.tbl_fun(sub_data, by = !!by, ...))
    } else {
      tbl_list[[pool_name]] <- rlang::inject(.tbl_fun(sub_data, by = !!by, denominator = sub_denom, ...))
    }
  }

  # Check if any tables were successfully generated
  if (length(tbl_list) == 0) {
    cli::cli_abort("No tables were generated. Check your pool definitions and data.")
  }

  # Return immediately if only one table was created, skipping the merge logic
  if (length(tbl_list) == 1) {
    return(tbl_list[[1]])
  }

  # Merge all tables together seamlessly, stripping default spanning headers
  merged_tbl <- gtsummary::tbl_merge(
    tbls = tbl_list,
    tab_spanner = rep(NA_character_, length(tbl_list)),
    quiet = TRUE # <-- Suppresses the row mismatch message (expected)
  ) |>
    gtsummary::modify_spanning_header(gtsummary::everything() ~ NA_character_)

  attr(merged_tbl, "by") <- levels(factor(data[[by]]))

  class(merged_tbl) <- c("tbl_with_pools", class(merged_tbl))

  merged_tbl
}
