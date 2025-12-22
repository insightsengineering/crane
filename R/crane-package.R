#' @keywords internal
#' @import rlang
#' @import ggplot2
#' @import glue glue
#' @import patchwork
#' @import gtsummary
#' @importFrom broom tidy
#' @importFrom broom.helpers .assert_package
#' @importFrom cowplot plot_grid ggdraw draw_plot
#' @importFrom dplyr across starts_with ends_with contains matches num_range
#'                   all_of any_of everything last_col where mutate
#' @importFrom labeling extended
#' @importFrom survival coxph Surv survdiff
#' @importFrom stats pchisq median
#' @importFrom tidyr pivot_wider
#' @importFrom utils tail
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

utils::globalVariables(c("."))

# using pkgs to silence NOTE
.silence <- function() {
  # broom is used in tbl_survfit_quantiles() via cardx::ard_survfit()
  broom::tidy
  survival::Surv
}
