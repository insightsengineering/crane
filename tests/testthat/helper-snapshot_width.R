# Set a wide console width for snapshot tests to prevent line-wrapping
# differences across platforms. Call inside test_that() blocks before
# expect_snapshot() — withr::local_options() resets automatically on exit.
local_wide_snapshot <- function(width = 220, .local_envir = parent.frame()) {
  withr::local_options(list(width = width), .local_envir = .local_envir)
}
