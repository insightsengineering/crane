test_that("reverse_rate_difference() works with percentage suffix", {
  expect_equal(
    reverse_rate_difference(c("5.0%", "-3.2%", "0.0%")),
    c("-5.0%", "3.2%", "0.0%")
  )
})

test_that("reverse_rate_difference() works without suffix", {
  expect_equal(
    reverse_rate_difference(c("10.0", "-5.5")),
    c("-10.0", "5.5")
  )
})

test_that("reverse_rate_difference() handles NA and empty strings", {
  expect_equal(
    reverse_rate_difference(c("2.5%", NA, "", "-1.0%")),
    c("-2.5%", NA_character_, NA_character_, "1.0%")
  )
})

test_that("reverse_rate_difference() handles zero values", {
  expect_equal(
    reverse_rate_difference("0.0%"),
    "0.0%"
  )
})

test_that("reverse_rate_difference() returns original for non-numeric", {
 expect_equal(
    reverse_rate_difference("abc"),
    "abc"
  )
})

test_that("reverse_ci() works with basic input", {
  expect_equal(
    reverse_ci("(2.5%, 10.0%)"),
    "(-10.0%, -2.5%)"
  )
})

test_that("reverse_ci() negates and swaps bounds", {
  expect_equal(
    reverse_ci(c("(2.5%, 10.0%)", "(-5.0%, 3.0%)")),
    c("(-10.0%, -2.5%)", "(-3.0%, 5.0%)")
  )
})

test_that("reverse_ci() handles NA and empty strings", {
  expect_equal(
    reverse_ci(c("(1.0%, 5.0%)", NA, "")),
    c("(-5.0%, -1.0%)", NA_character_, NA_character_)
  )
})

test_that("reverse_ci() handles negative bounds", {
  expect_equal(
    reverse_ci("(-8.0%, -2.0%)"),
    "(2.0%, 8.0%)"
  )
})

test_that("reverse_ci() returns original for malformed input", {
  expect_equal(
    reverse_ci("(1.0%)"),
    "(1.0%)"
  )

  expect_equal(
    reverse_ci("no numbers here"),
    "no numbers here"
  )
})
