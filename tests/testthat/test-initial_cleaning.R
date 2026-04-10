test_that("auto_encoding_repair handles UTF-8 input correctly", {
  # Pure UTF-8 input should return unchanged
  x <- c("hello", "world", "123")
  result <- auto_encoding_repair(x)
  expect_equal(result, x)
})

test_that("auto_encoding_repair handles full-width characters (UTF-8)", {
  # Full-width characters are valid UTF-8, should return unchanged
  x <- c("\uFF11\uFF12\uFF13", "abc")
  result <- auto_encoding_repair(x)
  expect_equal(result, x)
})

test_that("auto_encoding_repair handles non-character input", {
  # Numeric input should return unchanged
  x <- c(1, 2, 3)
  result <- auto_encoding_repair(x)
  expect_equal(result, x)

  # Factor input should return unchanged
  x <- factor(c("a", "b", "c"))
  result <- auto_encoding_repair(x)
  expect_equal(result, x)
})

test_that("auto_encoding_repair handles empty input", {
  # Empty character vector
  x <- character(0)
  result <- auto_encoding_repair(x)
  expect_equal(result, x)

  # All NA input
  x <- c(NA, NA, NA)
  result <- auto_encoding_repair(x)
  expect_equal(result, x)
})

test_that("value_initial_cleaning works with fix_encoding = TRUE", {
  x <- c("123", "456", "789")
  result <- value_initial_cleaning(x, fix_encoding = TRUE)
  expect_equal(result, c("123", "456", "789"))
})

test_that("value_initial_cleaning works with fix_encoding = FALSE", {
  x <- c("123", "456", "789")
  result <- value_initial_cleaning(x, fix_encoding = FALSE)
  expect_equal(result, c("123", "456", "789"))
})

test_that("value_initial_cleaning converts full-width numbers", {
  x <- c("\uFF11\uFF12\uFF13", "456")
  result <- value_initial_cleaning(x)
  expect_equal(result, c("123", "456"))
})

test_that("value_initial_cleaning removes extra dots", {
  x <- c("11..23", "3...14", "5.6")
  result <- value_initial_cleaning(x)
  expect_equal(result, c("11.23", "3.14", "5.6"))
})

test_that("value_initial_cleaning removes spaces by default", {
  x <- c("1 2 3", "4 5 6")
  result <- value_initial_cleaning(x)
  expect_equal(result, c("123", "456"))
})

test_that("value_initial_cleaning handles remove_inequal parameter", {
  x <- c("<10", ">5", "20")
  result <- value_initial_cleaning(x, remove_inequal = TRUE)
  expect_equal(result, c("10", "5", "20"))
})

test_that("value_initial_cleaning converts empty strings to NA", {
  x <- c("123", "", "456", "")
  result <- value_initial_cleaning(x)
  expect_equal(result, c("123", NA, "456", NA))
})

test_that("char_initial_cleaning works with fix_encoding = TRUE", {
  x <- c("  hello  ", "world")
  result <- char_initial_cleaning(x, fix_encoding = TRUE)
  expect_equal(result, c("hello", "world"))
})

test_that("char_initial_cleaning works with fix_encoding = FALSE", {
  x <- c("  hello  ", "world")
  result <- char_initial_cleaning(x, fix_encoding = FALSE)
  expect_equal(result, c("hello", "world"))
})

test_that("char_initial_cleaning squish whitespace", {
  x <- c("hello   world", "test    string")
  result <- char_initial_cleaning(x)
  expect_equal(result, c("hello world", "test string"))
})

test_that("char_initial_cleaning trims leading and trailing whitespace", {
  x <- c("  hello  ", "\tworld\t", "\n test \n")
  result <- char_initial_cleaning(x)
  expect_equal(result, c("hello", "world", "test"))
})

test_that("char_initial_cleaning converts empty strings to NA", {
  x <- c("hello", "", "world", "")
  result <- char_initial_cleaning(x)
  expect_equal(result, c("hello", NA, "world", NA))
})

test_that("char_initial_cleaning converts full-width characters", {
  x <- c("\uff41\uff42\uff43", "hello")
  result <- char_initial_cleaning(x)
  expect_equal(result, c("abc", "hello"))
})
