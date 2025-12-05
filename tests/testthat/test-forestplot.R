skip_on_cran()

test_that("test g_forest() works", {
  expect_no_error(tbl <-
   trial %>%
     tbl_subgroups(
       subgroups = c("grade", "stage"),
       ~ glm(response ~ trt, data = .x) %>%
         gtsummary::tbl_regression(
           show_single_row = trt,
           exponentiate = TRUE
         )
     ))
  expect_no_error(g_forest(tbl))
})
