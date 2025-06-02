library(testthat)

test_that("Basic non-numeric detection", {
  input <- c("12.3", "45..6", "78a9", NA, "\uFF11\uFF12", "987", "45..6")

  # Default parameters
  expect_equal(check_nonnum(input), c("45..6", "78a9", "\uFF11\uFF12"))

  # return_idx = TRUE
  expect_equal(check_nonnum(input, return_idx = TRUE)$idx, c(2, 3, 5, 7))

  # show_unique = FALSE
  expect_equal(check_nonnum(input, show_unique = FALSE), c("45..6", "78a9", "\uFF11\uFF12", "45..6"))
})

test_that("Edge cases handling", {
  # Empty input
  expect_length(check_nonnum(character(0)), 0)

  # All NA
  expect_length(check_nonnum(c(NA, NA_character_)), 0)

  # All valid
  expect_length(check_nonnum(c("1.2", "3")), 0)
})

test_that("Parameter combinations", {
  input <- c("a", "a", "b", "c")

  # return_idx优先
  res <- check_nonnum(input, return_idx = TRUE, show_unique = FALSE)
  expect_type(res, "list")
  expect_named(res, c("value", "idx"))

  # show_unique生效
  expect_length(check_nonnum(input, show_unique = TRUE), 3)
})
