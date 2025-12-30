test_that("mad_outlier correctly identifies outliers", {
  # Basic functionality test with known outlier
  x <- c(1, 2, 3, 4, 5, 100)
  expect_equal(mad_outlier(x), c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE))

  # Test with no outliers
  x <- 1:10
  expect_equal(all(mad_outlier(x) == FALSE), TRUE)

  # Test with custom threshold
  x <- c(1, 2, 3, 4, 5, 10)
  expect_equal(mad_outlier(x, threshold = 1.4826 * 10), c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))
})

test_that("mad_outlier handles edge cases correctly", {
  # Test with single value
  x <- 5
  expect_equal(mad_outlier(x), FALSE)

  # Test with all identical values
  x <- rep(5, 10)
  expect_equal(all(mad_outlier(x) == FALSE), TRUE)

  # Test with NA values
  x <- c(1, 2, NA, 4, 5, 100)
  expect_equal(mad_outlier(x), c(FALSE, FALSE, NA, FALSE, FALSE, TRUE))
})

test_that("iqr_outlier correctly identifies outliers", {
  # Basic functionality test with known outlier
  x <- c(1, 2, 3, 4, 5, 10)
  expect_equal(iqr_outlier(x), c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE))

  # Test with no outliers
  x <- 1:10
  expect_equal(all(iqr_outlier(x) == FALSE), TRUE)

  # Test with custom threshold
  x <- c(1, 2, 3, 4, 5, 10)
  expect_equal(iqr_outlier(x, threshold = 2.0), c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE))
  expect_equal(iqr_outlier(x, threshold = 3.0), c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))
})

test_that("iqr_outlier handles edge cases correctly", {
  # Test with single value
  x <- 5
  expect_equal(iqr_outlier(x), FALSE)

  # Test with all identical values
  x <- rep(5, 10)
  expect_equal(all(iqr_outlier(x) == FALSE), TRUE)

  # Test with NA values
  x <- c(1, 2, NA, 4, 5, 100)
  expect_equal(iqr_outlier(x), c(FALSE, FALSE, NA, FALSE, FALSE, TRUE))
})

test_that("zscore_outlier correctly identifies outliers", {
  # Basic functionality test with known outlier
  x <- c(1, 2, 3, 4, 5, 100)
  expect_equal(zscore_outlier(x), c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)) # Default threshold is 3
  expect_equal(zscore_outlier(x, threshold = 2.0), c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE))

  # Test with no outliers
  x <- 1:10
  expect_equal(all(zscore_outlier(x) == FALSE), TRUE)
})

test_that("zscore_outlier handles edge cases correctly", {
  # Test with single value
  x <- 5
  expect_equal(zscore_outlier(x), FALSE)

  # Test with all identical values
  x <- rep(5, 10)
  expect_equal(all(zscore_outlier(x) == FALSE), TRUE)

  # Test with NA values
  x <- c(1, 2, NA, 4, 5, 100)
  expect_equal(zscore_outlier(x), c(FALSE, FALSE, NA, FALSE, FALSE, FALSE))
})

test_that("detect_outliers works correctly with all methods", {
  # Test data with known outlier
  x <- c(1, 2, 3, 4, 5, 100)

  # Test with IQR method
  result <- detect_outliers(x, method = "iqr")
  expect_equal(result$outlier_count, 1)
  expect_equal(result$outlier_pct, 16.67)
  expect_true(all(result$summary$before$min == 1, result$summary$before$max == 100))

  # Test with MAD method
  result <- detect_outliers(x, method = "mad")
  expect_equal(result$outlier_count, 1)
  expect_equal(result$outlier_pct, 16.67)

  # Test with Z-score method
  result <- detect_outliers(x, method = "zscore", threshold = 2.0)
  expect_equal(result$outlier_count, 1)
  expect_equal(result$outlier_pct, 16.67)
})

test_that("detect_outliers handles edge cases correctly", {
  # Test with empty vector
  result <- detect_outliers(numeric(0))
  expect_equal(result$outlier_count, 0)
  expect_equal(result$outlier_pct, 0)

  # Test with all NA values
  result <- detect_outliers(c(NA, NA, NA))
  expect_equal(result$outlier_count, 0)
  expect_equal(result$outlier_pct, 0)

  # Test with single value
  result <- detect_outliers(c(5))
  expect_equal(result$outlier_count, 0)
  expect_equal(result$outlier_pct, 0)

  # Test with all identical values
  result <- detect_outliers(rep(5, 10))
  expect_equal(result$outlier_count, 0)
  expect_equal(result$outlier_pct, 0)
})

test_that("detect_outliers uses default thresholds correctly", {
  x <- c(1, 2, 3, 4, 5, 100)

  # Test with NULL threshold (should use defaults)
  result_iqr <- detect_outliers(x, method = "iqr", threshold = NULL)
  result_mad <- detect_outliers(x, method = "mad", threshold = NULL)
  result_zscore <- detect_outliers(x, method = "zscore", threshold = NULL)

  # All methods should detect the outlier with their default thresholds
  expect_equal(result_iqr$outlier_count, 1)
  expect_equal(result_mad$outlier_count, 1)
  expect_equal(result_zscore$outlier_count, 0) # Z-score default is 3, which is stricter
})
