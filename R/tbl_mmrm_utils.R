# Helper function to calculate relative reduction for each visit and arm combination
.get_relative_reduc_df <- function(estimates, arm, visit) {
  ref_arm_level <- levels(estimates[[arm]])[1L]

  estimates |>
    dplyr::select(dplyr::all_of(c(visit, arm)), estimate) |>
    tidyr::pivot_wider(names_from = dplyr::all_of(arm), values_from = estimate) |>
    dplyr::mutate(
      dplyr::across(
        -dplyr::all_of(c(visit, ref_arm_level)),
        # Replaced get() with .data[[]]
        ~ (.data[[ref_arm_level]] - .x) / .data[[ref_arm_level]],
        .names = "relative_reduc_{.col}"
      )
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with("relative_reduc_"),
      names_to = arm,
      names_prefix = "relative_reduc_",
      values_to = "relative_reduc"
    )
}

# Helper function to construct single visit contrast specifications
.get_single_visit_contrast_specs <- function(emmeans_res, arm, visit) {
  emmeans_res$grid$index <- seq_len(nrow(emmeans_res$grid))

  grid_by_visit <- split(emmeans_res$grid, emmeans_res$grid[[visit]])

  arm_levels <- emmeans_res$object@levels[[arm]]
  ref_arm_level <- arm_levels[1L]
  zeros_coefs <- numeric(nrow(emmeans_res$grid))
  overall_list <- list()
  arm_vec <- visit_vec <- c()
  for (j in seq_along(grid_by_visit)) {
    this_grid <- grid_by_visit[[j]]
    ref_index <- which(this_grid[[arm]] == ref_arm_level)
    this_visit <- names(grid_by_visit)[j]
    this_ref_coefs <- zeros_coefs
    this_ref_coefs[this_grid$index[ref_index]] <- -1
    this_list <- list()
    for (i in seq_len(nrow(this_grid))[-ref_index]) {
      this_coefs <- this_ref_coefs
      this_coefs[this_grid$index[i]] <- 1
      this_arm <- as.character(this_grid[[arm]][i])
      arm_vec <- c(arm_vec, this_arm)
      visit_vec <- c(visit_vec, this_visit)
      this_label <- paste(this_arm, this_visit, sep = ".")
      this_list[[this_label]] <- this_coefs
    }
    overall_list <- c(overall_list, this_list)
  }

  grid <- data.frame(arm = arm_vec, visit = visit_vec)
  names(grid) <- c(arm, visit)
  list(
    coefs = overall_list,
    grid = grid
  )
}

# Custom formatting functions internally --------------------------------
# These are workaround that need further detail
.get_n <- function(data, ...) {
  val <- if (nrow(data) == 0 || is.na(data$n[1])) "" else as.character(data$n[1])
  list(my_stat = val)
}

.get_adj_mean_se <- function(data, ...) {
  val <- if (nrow(data) == 0 || is.na(data$estimate_est[1])) {
    ""
  } else {
    sprintf("%.2f (%.3f)", data$estimate_est[1], data$se_est[1])
  }
  list(my_stat = val)
}

.get_adj_mean_ci <- function(data, ...) {
  val <- if (nrow(data) == 0 || is.na(data$lower_cl_est[1])) {
    ""
  } else {
    sprintf("(%.2f, %.2f)", data$lower_cl_est[1], data$upper_cl_est[1])
  }
  list(my_stat = val)
}

.get_diff_se <- function(data, ...) {
  val <- if (nrow(data) == 0 || is.na(data$estimate_contr[1])) {
    ""
  } else {
    sprintf("%.2f (%.3f)", data$estimate_contr[1], data$se_contr[1])
  }
  list(my_stat = val)
}

.get_diff_ci <- function(data, ...) {
  val <- if (nrow(data) == 0 || is.na(data$lower_cl_contr[1])) {
    ""
  } else {
    sprintf("(%.2f, %.2f)", data$lower_cl_contr[1], data$upper_cl_contr[1])
  }
  list(my_stat = val)
}

.get_pval <- function(data, ...) {
  val <- if (nrow(data) == 0 || is.na(data$p_value[1])) {
    ""
  } else {
    sprintf("%.4f", data$p_value[1])
  }
  list(my_stat = val)
}
