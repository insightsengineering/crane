#' @keywords internal
#' @import rlang
#' @import glue glue
#' @import ggplot2
#' @import patchwork
#' @importFrom broom.helpers tidy_plus_plus
#' @importFrom dplyr across starts_with ends_with contains matches num_range
#'                   all_of any_of everything last_col where
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
