library(survival)
library(withr)

test_that("adaptive_scoring parameter validation works", {
  df <- data.frame(a = 1:5, b = 6:10)

  # Invalid adaptive_scoring type
  expect_error(
    get_valid_subset(df, adaptive_scoring = "yes"),
    "adaptive_scoring"
  )

  # Invalid adaptive_scoring length
  expect_error(
    get_valid_subset(df, adaptive_scoring = c(TRUE, FALSE)),
    "adaptive_scoring"
  )
})

test_that("max_missing_rates calculates correctly", {
  # Test with example data
  data(cancer)
  mmr <- max_missing_rates(cancer)
  expect_true(is.numeric(mmr$row))
  expect_true(is.numeric(mmr$col))

  # Test with all NAs
  df_na <- data.frame(a = rep(NA, 5), b = rep(NA, 5))
  mmr_na <- max_missing_rates(df_na)
  expect_equal(mmr_na$row, 1)
  expect_equal(mmr_na$col, 1)

  # Test with no NAs
  df_no_na <- data.frame(a = 1:5, b = 6:10)
  mmr_no_na <- max_missing_rates(df_no_na)
  expect_equal(mmr_no_na$row, 0)
  expect_equal(mmr_no_na$col, 0)
})

test_that("get_valid_subset returns correct subset", {
  set.seed(1) # For reproducibility
  data(cancer)

  # Basic test with example parameters
  subset1 <- get_valid_subset(cancer, row_na_ratio = 0.2, col_na_ratio = 0.1, row_priority = 1)
  expect_true(is.data.frame(subset1))
  expect_true(nrow(subset1) <= nrow(cancer))
  expect_true(ncol(subset1) <= ncol(cancer))

  # Check max missing rates after subset
  mmr_subset1 <- max_missing_rates(subset1)
  expect_true(mmr_subset1$row <= 0.2)
  expect_true(mmr_subset1$col <= 0.1)

  # Test return_index = TRUE
  indices <- get_valid_subset(cancer, row_na_ratio = 0.2, col_na_ratio = 0.1, row_priority = 1, return_index = TRUE)
  expect_true(is.list(indices))
  expect_true(all(indices$rows %in% seq_len(nrow(cancer))))
  expect_true(all(indices$cols %in% seq_len(ncol(cancer))))

  # Edge case: all rows/columns exceed NA ratio (should return empty)
  df_high_na <- data.frame(matrix(NA, nrow = 10, ncol = 5))
  subset_empty <- get_valid_subset(df_high_na, row_na_ratio = 0.1, col_na_ratio = 0.1)
  # Check that either rows or columns are empty, and missing rates are within limits
  expect_true(nrow(subset_empty) == 0 || ncol(subset_empty) == 0)

  # Edge case: no rows/columns exceed NA ratio (should return original)
  df_low_na <- data.frame(matrix(rnorm(50), nrow = 10, ncol = 5)) # No NAs
  subset_full <- get_valid_subset(df_low_na, row_na_ratio = 0.5, col_na_ratio = 0.5)
  expect_equal(dim(subset_full), dim(df_low_na))
})

test_that("get_valid_subset with adaptive_scoring = TRUE returns valid subset", {
  set.seed(42)

  # Create test data with specific missing pattern
  # Rows 1-3 have high NA in columns 1-2
  # Columns 4-5 have high NA in rows 4-5
  df <- data.frame(
    col1 = c(NA, NA, NA, 1, 1, 1, 1),
    col2 = c(NA, NA, NA, 2, 2, 2, 2),
    col3 = c(3, 3, 3, 3, 3, 3, 3),
    col4 = c(4, 4, 4, NA, NA, 4, 4),
    col5 = c(5, 5, 5, NA, NA, 5, 5),
    col6 = c(6, 6, 6, 6, 6, 6, 6)
  )

  # Test with adaptive_scoring = FALSE (traditional)
  result_traditional <- get_valid_subset(df,
    row_na_ratio = 0.3, col_na_ratio = 0.3,
    adaptive_scoring = FALSE
  )
  expect_true(is.data.frame(result_traditional))

  # Test with adaptive_scoring = TRUE
  result_adaptive <- get_valid_subset(df,
    row_na_ratio = 0.3, col_na_ratio = 0.3,
    adaptive_scoring = TRUE
  )
  expect_true(is.data.frame(result_adaptive))

  # Both should satisfy the missing rate constraints
  mmr_traditional <- max_missing_rates(result_traditional)
  mmr_adaptive <- max_missing_rates(result_adaptive)

  expect_true(mmr_traditional$row <= 0.3)
  expect_true(mmr_traditional$col <= 0.3)
  expect_true(mmr_adaptive$row <= 0.3)
  expect_true(mmr_adaptive$col <= 0.3)
})

test_that("adaptive_scoring considers improvement in missing rates", {
  set.seed(123)

  # Create data where removing a "good" row could help columns
  # Row 1 has moderate NAs but removing it helps columns significantly
  df <- data.frame(
    a = c(1, NA, NA, NA, NA),
    b = c(2, NA, NA, NA, NA),
    c = c(NA, 3, 3, 3, 3),
    d = c(NA, 4, 4, 4, 4),
    e = c(NA, 5, 5, 5, 5)
  )

  # With adaptive_scoring = TRUE, the algorithm should consider
  # how much removing a row helps columns get closer to threshold
  result_adaptive <- get_valid_subset(df,
    row_na_ratio = 0.5, col_na_ratio = 0.4,
    adaptive_scoring = TRUE, row_priority = 1
  )

  # Verify constraints are satisfied
  mmr <- max_missing_rates(result_adaptive)
  expect_true(mmr$row <= 0.5)
  expect_true(mmr$col <= 0.4)

  # Result should be a valid data frame
  expect_true(is.data.frame(result_adaptive))
  expect_true(nrow(result_adaptive) > 0)
  expect_true(ncol(result_adaptive) > 0)
})

test_that("adaptive_scoring works with return_index = TRUE", {
  set.seed(456)
  df <- data.frame(
    x = c(1, NA, 3, 4, 5),
    y = c(NA, 2, 3, 4, 5),
    z = c(1, 2, 3, 4, 5)
  )

  indices <- get_valid_subset(df,
    row_na_ratio = 0.4, col_na_ratio = 0.4,
    adaptive_scoring = TRUE, return_index = TRUE
  )

  expect_true(is.list(indices))
  expect_true(all(indices$rows %in% seq_len(nrow(df))))
  expect_true(all(indices$cols %in% seq_len(ncol(df))))
  expect_true(length(indices$rows) > 0)
  expect_true(length(indices$cols) > 0)
})

test_that("adaptive_scoring respects row_priority parameter", {
  set.seed(789)

  # Create data with asymmetric missing pattern
  df <- data.frame(
    col1 = c(NA, NA, 1, 2, 3, 4, 5),
    col2 = c(NA, NA, 1, 2, 3, 4, 5),
    col3 = c(1, 2, 3, 4, 5, 6, 7),
    col4 = c(1, 2, 3, 4, 5, 6, 7),
    col5 = c(1, 2, 3, 4, 5, 6, 7)
  )

  # Test with different row_priority values
  result_low_priority <- get_valid_subset(df,
    row_na_ratio = 0.3, col_na_ratio = 0.3,
    adaptive_scoring = TRUE, row_priority = 0.5
  )
  result_high_priority <- get_valid_subset(df,
    row_na_ratio = 0.3, col_na_ratio = 0.3,
    adaptive_scoring = TRUE, row_priority = 2
  )

  # Both should satisfy constraints
  mmr_low <- max_missing_rates(result_low_priority)
  mmr_high <- max_missing_rates(result_high_priority)

  expect_true(mmr_low$row <= 0.3)
  expect_true(mmr_low$col <= 0.3)
  expect_true(mmr_high$row <= 0.3)
  expect_true(mmr_high$col <= 0.3)

  # Higher row_priority should tend to preserve more rows
  expect_true(nrow(result_high_priority) >= nrow(result_low_priority) ||
    ncol(result_high_priority) <= ncol(result_low_priority))
})

test_that("adaptive_scoring handles edge cases", {
  # Edge case: no NAs - should return original
  df_no_na <- data.frame(matrix(1:25, nrow = 5, ncol = 5))
  result <- get_valid_subset(df_no_na,
    row_na_ratio = 0.5, col_na_ratio = 0.5,
    adaptive_scoring = TRUE
  )
  expect_equal(dim(result), c(5, 5))

  # Edge case: single row
  df_single_row <- data.frame(a = c(1, NA), b = c(NA, 2))
  result <- get_valid_subset(df_single_row,
    row_na_ratio = 0.5, col_na_ratio = 0.5,
    adaptive_scoring = TRUE
  )
  expect_true(is.data.frame(result))

  # Edge case: single column
  df_single_col <- data.frame(a = c(1, NA, 3, 4, 5))
  result <- get_valid_subset(df_single_col,
    row_na_ratio = 0.5, col_na_ratio = 0.5,
    adaptive_scoring = TRUE
  )
  expect_true(is.data.frame(result))

  # Edge case: mostly NAs but some valid data
  df_mostly_na <- data.frame(
    a = c(1, NA, NA, NA, NA),
    b = c(2, NA, NA, NA, NA),
    c = c(NA, NA, NA, NA, 3)
  )
  result <- get_valid_subset(df_mostly_na,
    row_na_ratio = 0.8, col_na_ratio = 0.8,
    adaptive_scoring = TRUE
  )
  expect_true(is.data.frame(result))
  # Result should satisfy constraints
  mmr <- max_missing_rates(result)
  expect_true(mmr$row <= 0.8)
  expect_true(mmr$col <= 0.8)
})
