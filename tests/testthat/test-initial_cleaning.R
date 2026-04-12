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

test_that("auto_encoding_repair repairs GBK-encoded Chinese characters", {
  skip_if_not(l10n_info()$"UTF-8", "Test requires UTF-8 locale")

  # Create GBK-encoded bytes from UTF-8 Chinese characters "你好"
  gbk_bytes <- iconv("\u4f60\u597d", from = "UTF-8", to = "GBK")

  # Verify it's not valid UTF-8
  skip_if(validUTF8(gbk_bytes), "iconv did not produce non-UTF-8 bytes")

  # Repair should convert back to UTF-8
  result <- suppressWarnings(auto_encoding_repair(gbk_bytes))
  expect_equal(result, "\u4f60\u597d")
})

test_that("auto_encoding_repair repairs Latin-1 encoded characters", {
  skip_if_not(l10n_info()$"UTF-8", "Test requires UTF-8 locale")

  # Create Latin-1 encoded bytes from UTF-8 "caf\u00e9"
  latin1_bytes <- iconv("caf\u00e9", from = "UTF-8", to = "latin1")

  # Verify it's not valid UTF-8
  skip_if(validUTF8(latin1_bytes), "iconv did not produce non-UTF-8 bytes")

  # Repair should convert back to UTF-8
  result <- auto_encoding_repair(latin1_bytes)
  expect_equal(result, "caf\u00e9")
})

test_that("auto_encoding_repair handles mixed UTF-8 and GBK encoding", {
  skip_if_not(l10n_info()$"UTF-8", "Test requires UTF-8 locale")

  # Create a mixed vector: UTF-8 "hello", GBK "\u4e2d\u6587" (中文), UTF-8 "world"
  gbk_chinese <- iconv("\u4e2d\u6587", from = "UTF-8", to = "GBK")
  skip_if(validUTF8(gbk_chinese), "iconv did not produce non-UTF-8 bytes")

  mixed <- c("hello", gbk_chinese, "world")

  # Repair should convert GBK part to UTF-8 while keeping others unchanged
  result <- auto_encoding_repair(mixed)
  expect_equal(result, c("hello", "\u4e2d\u6587", "world"))
})

test_that("auto_encoding_repair with explicit from_encoding parameter", {
  skip_if_not(l10n_info()$"UTF-8", "Test requires UTF-8 locale")

  # Create GBK-encoded bytes
  gbk_bytes <- iconv("\u4f60\u597d", from = "UTF-8", to = "GBK")
  skip_if(validUTF8(gbk_bytes), "iconv did not produce non-UTF-8 bytes")

  # Explicitly specify GBK as source encoding
  result <- auto_encoding_repair(gbk_bytes, from_encoding = "GBK")
  expect_equal(result, "\u4f60\u597d")
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
