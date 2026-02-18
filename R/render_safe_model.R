#' Render Safe Regression Model
#'
#' Executes a model expression safely. If the model fails or produces infinite
#' coefficients (perfect separation), it returns a specific formatted row
#' (">999.99") instead of crashing.
#'
#' @param data (`data.frame`)\cr
#'   The data subset.
#' @param expr (`expression`)\cr
#'   A quoted expression to run the model (e.g., `quote(coxph(...))`).
#' @param cov_name (`string`)\cr
#'   The name of the covariate (e.g., "TRT01P") for the label.
#' @param ... Additional arguments passed to `tbl_regression` (e.g., `exponentiate = TRUE`).
#'
#' @return A `gtsummary` object.
#'
#' @examples
#' # Group A: Normal Data (Events in both arms)
#'
#'
#'
#' @export
render_safe_model <- function(data, expr, cov_name, fallback_table = fallback_table(data, label_text = "NE"), ...) {
  # set_cli_abort_call()

  # 2. Execution Block
  tryCatch({

    # A. Run the Model (capturing warnings)
    # We use withCallingHandlers to catch the "Loglik converged" warning
    model <- withCallingHandlers(
      eval(expr, envir = list(.x = data, data = data)),

      warning = function(w) {
        # Check for Infinite Beta / Perfect Separation messages
        if (grepl("Loglik converged before|infinite|singular", w$message, ignore.case = TRUE)) {
          invokeRestart("muffleWarning")
          stop("Infinite Beta Detected") # Force jump to error handler
        }
      }
    )

    # B. Generate the standard table
    t <- tbl_regression(model, ...)

    # C. Check for Empty/Failed Body (Double check)
    if (nrow(t$table_body) == 0) {
      stop("Empty table body")
    }

    return(t)

  }, error = function(e) {

    # D. Error Handler: Return the Custom Infinite Row
    # If the error was our "Infinite Beta", we return the >999.99 format
    # If it was a generic crash (e.g. 0 events), you might want NA or the same.
    # Here we apply it to ALL failures for consistency in this edge case.
    return(fallback_table)
  })
}

# 1. Define the "Fallback" Table Generator
# This creates the row >999.99 (0.00, >999.99) when triggered
#'
#' @describeIn render_safe_model A helper function to generate a fallback table with
#'   the specified label and fixed values for estimate and confidence intervals.
#'   This function was mainly designed as an internal helper for `render_safe_model`
#'   and specifically for [tbl_roche_subgroups()] when using `survival::coxph()` models, but it can
#'   be used in other contexts where a similar fallback is needed.
#' @param label_text (`string`)\cr
#'   The text to display in the label column (e.g., "NE" for "Not Estimable").
#'
#' @export
fallback_table <- function(data, label_text = "NE") {

  # Create a dummy table based on the data structure
  data %>%
    dplyr::select(any_of(cov_name)) %>%
    gtsummary::tbl_summary(missing = "no") %>%
    gtsummary::modify_table_body(~ data.frame(
      variable = cov_name,
      row_type = "label",
      label = label_text, # "NE" or similar

      # WE MANUALLY SET THE OUTPUT VALUES HERE
      estimate = Inf,
      conf.low = 0,
      conf.high = Inf,

      # Formatting columns gtsummary uses for printing
      # This overrides the numeric formatting to show your specific string
      ci = "(0.00, >999.99)",
      estimate_str = ">999.99",

      p.value = NA_real_,
      N = nrow(data)
    )) %>%
    # Ensure the columns are interpreted as character so our strings stick
    gtsummary::modify_fmt_fun(
      c(estimate, conf.low, conf.high) ~ function(x) x # Pass-through
    ) %>%
    # Force the display of our custom strings
    gtsummary::modify_column_merge(
      pattern = "{estimate_str} {ci}",
      rows = !is.na(estimate_str)
    )
}

