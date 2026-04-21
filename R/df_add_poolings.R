#' Add custom poolings to an ADaM dataset list - see **DISCLAIMER**
#'
#' @description
#' **DISCLAIMER**: _this is a risky function. Please consider using [tbl_with_pools()] instead._
#' This function allows you to create new pooled groups in your ADaM datasets based
#' on specified arm values. You can choose to keep the original unpooled rows or not.
#' **Important Note**: If you choose to keep the original rows and also add a pool that
#' includes all patients (using the "all" keyword), you will end up with duplicate
#' rows in your dataset. This can lead to incorrect patient counts if you later add a
#' total column. Use this option with caution and ensure that you do not add a
#' standard total column later to avoid double-counting.
#'
#' @param adam_db (`list`)\cr
#'   List of ADaM datasets containing at least the `adsl` data frame.
#' @param pools (`list`)\cr
#'   Named list where names are the new pooled labels, and values are character
#'   vectors of the original arm values to include. Use the special keyword "all"
#'   to include all patients.
#' @param arm_var (`character`)\cr
#'   String of the arm variable to evaluate and overwrite. Default is "TRT01A".
#' @param keep_original (`logical`)\cr
#'   Whether to keep the original unpooled rows. Default is TRUE.
#'
#' @return Updated list of ADaM datasets.
#'
#' @examples
#' # Create a minimal dummy adam_db
#' adsl <- data.frame(
#'   USUBJID = c("001", "002", "003", "004", "005"),
#'   TRT01A = c("Drug A", "Drug A", "Drug B", "Drug C", "Drug C"),
#'   stringsAsFactors = FALSE
#' )
#' adam_db <- list(adsl = adsl)
#'
#' # Define the requested pools
#' my_pools <- list(
#'   "Drugs A and B" = c("Drug A", "Drug B"),
#'   "All Patients"  = "all"
#' )
#'
#' # Example A: Safe pooling (keep_original = FALSE, no "all" pool) -----------------
#' safe_pools <- list("Drugs A and B" = c("Drug A", "Drug B"))
#' adam_db_safe <- df_add_poolings(adam_db, pools = safe_pools, keep_original = FALSE)
#' print(adam_db_safe$adsl)
#'
#' # Example B: Triggering the warnings (keep_original = TRUE and "all" pool) -------
#' # This will throw two warnings: one for duplicates, one for the "all" pool.
#' adam_db_warnings <- df_add_poolings(adam_db, pools = my_pools, keep_original = TRUE)
#' print(adam_db_warnings$adsl)
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true") && requireNamespace("yaml", quietly = TRUE)
#' # Example C: Use yaml to define the pools config and run the function ------------
#' # Creating Dummy Data
#' adex <- data.frame(
#'   USUBJID = c("001", "002", "003", "004"),
#'   AEDECOD = c("Headache", "Nausea", "Fatigue", "Dizziness"),
#'   stringsAsFactors = FALSE
#' )
#' adam_db <- list(adsl = adsl, adex = adex, adsl2 = adsl)
#'
#' # Define the config as a standard R list
#' config_to_write <- list(
#'   df_add_poolings_config = list(
#'     keep_original = FALSE,
#'     arm_var = "TRT01A",
#'     pools = list(
#'       "Drug A + B"   = c("Drug A", "Drug B"),
#'       "Drug C + B" = c("Drug C", "Drug B"),
#'       "All Patients"             = "all"
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
#' pool_args <- arg_specs$df_add_poolings_config
#'
#' # Run the function
#' if (!is.null(pool_args)) {
#'   adam_db_pooled <- df_add_poolings(
#'     adam_db       = adam_db,
#'     pools         = pool_args$pools,
#'     arm_var       = pool_args$arm_var,
#'     keep_original = pool_args$keep_original
#'   )
#' }
#'
#' # View the result
#' adam_db_pooled$adsl2
#'
#' @export
df_add_poolings <- function(adam_db, pools, arm_var = "TRT01A", keep_original = TRUE) {
  # Check if adam_db is a list of data frames
  if (!is.list(adam_db) || !all(vapply(adam_db, is.data.frame, logical(1)))) {
    cli::cli_abort(c(
      "x" = "{.arg adam_db} must be a list of data frames.",
      "i" = "You provided an object of class {.cls {class(adam_db)}}."
    ))
  }
  # Check if the arm variable exists in AT LEAST ONE of the data frames
  has_arm_var <- vapply(adam_db, function(x) arm_var %in% names(x), logical(1))

  if (!any(has_arm_var)) {
    cli::cli_abort(c(
      "x" = "The arm variable {.val {arm_var}} was not found in any dataset within {.arg adam_db}.",
      "i" = "Available columns in {.code adsl}: {.var {names(adam_db$adsl)}}."
    ))
  }
  # Check if pools is a named list
  if (!is.list(pools) || is.null(names(pools)) || any(names(pools) == "")) {
    cli::cli_abort(c(
      "x" = "{.arg pools} must be a fully named list.",
      "i" = "Ensure every element in your list has a label (e.g., {.code list(\"Group A\" = c(...))})."
    ))
  }

  # 2. Duplicate Data Warning
  if (keep_original && length(pools) > 0) {
    cli::cli_warn(c("Preserving original rows while adding pools creates duplicates. ",
                    i = "If you add a total column later, the patient counts will be incorrect."
    ))
  }

  # Apply poolings to all datasets that contain the arm variable
  for (ds_name in names(adam_db)[has_arm_var]) {
    dataset <- adam_db[[ds_name]]
    data_pieces <- list()

    if (keep_original) {
      data_pieces[["original"]] <- dataset
    }

    for (pool_label in names(pools)) {
      arm_values <- pools[[pool_label]]

      if (length(arm_values) == 1 && tolower(arm_values) == "all") {
        cli::cli_warn(c(
          "You are adding an 'all' patients pool to {.val {ds_name}}.",
          i = "Ensure you do not add a standard total column later."
        ))
        subset_data <- dataset
      } else {
        subset_data <- dataset |> dplyr::filter(.data[[arm_var]] %in% arm_values)
      }

      if (nrow(subset_data) > 0) {
        subset_data[[arm_var]] <- pool_label
        data_pieces[[pool_label]] <- subset_data
      }
    }
    adam_db[[ds_name]] <- dplyr::bind_rows(data_pieces)
  }

  adam_db
}
