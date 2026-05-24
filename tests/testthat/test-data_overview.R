test_that("data_overview works with basic dataset", {
  # Use mtcars dataset as test data
  data(mtcars)

  # Test with default parameters
  result <- data_overview(mtcars)

  # Check basic structure
  expect_true(is.list(result))
  expect_true(all(c("variable_types", "summary_stats", "quality_issues", "recommendations", "overall") %in% names(result)))

  # Check variable types
  expect_true("numeric" %in% names(result$variable_types))
  expect_equal(length(result$variable_types$numeric), 11)

  # Check summary stats
  expect_true("numeric" %in% names(result$summary_stats))
  expect_equal(nrow(result$summary_stats$numeric), 11)
})

test_that("data_overview works with different outlier methods", {
  data(mtcars)

  # Test with IQR method (default)
  result_iqr <- data_overview(mtcars, outlier_method = "iqr")

  # Test with MAD method
  result_mad <- data_overview(mtcars, outlier_method = "mad")

  # Test with Z-score method
  result_zscore <- data_overview(mtcars, outlier_method = "zscore")

  # All methods should return valid results
  expect_true(is.list(result_iqr))
  expect_true(is.list(result_mad))
  expect_true(is.list(result_zscore))

  # Results may vary by method, but should all have the same structure
  expect_equal(names(result_iqr), names(result_mad))
  expect_equal(names(result_iqr), names(result_zscore))
})

test_that("data_overview works with custom outlier threshold", {
  data(mtcars)

  # Test with custom IQR threshold
  result_default <- data_overview(mtcars, outlier_method = "iqr")
  result_custom <- data_overview(mtcars, outlier_method = "iqr", outlier_threshold = 3.0)

  # With higher threshold, should detect fewer outliers
  expect_true(length(result_default$quality_issues$outliers) >= length(result_custom$quality_issues$outliers))

  # Test with custom Z-score threshold
  result_z_default <- data_overview(mtcars, outlier_method = "zscore")
  result_z_custom <- data_overview(mtcars, outlier_method = "zscore", outlier_threshold = 2.0)

  # With lower threshold, should detect more outliers
  expect_true(length(result_z_custom$quality_issues$outliers) >= length(result_z_default$quality_issues$outliers))
})

test_that("data_overview detects quality issues correctly", {
  # Create a test dataset with known issues
  test_df <- data.frame(
    id = 1:10,
    # Numeric stored as character
    age = c(as.character(1:8), "100+", "missing"),
    # Normal numeric with outliers
    income = c(rnorm(8, 50000, 10000), 200000, 300000),
    # Character with some numeric values
    score = c(as.character(1:8), "A+", "B-"),
    # Factor with missing values
    gender = sample(c("M", "F", NA), 10, replace = TRUE),
    # Constant variable
    constant = rep("same", 10),
    # Logical with missing values
    active = sample(c(TRUE, FALSE, NA), 10, replace = TRUE)
  )

  # Add duplicate rows
  test_df <- rbind(test_df, test_df[1:2, ])

  result <- data_overview(test_df)

  # Check that quality issues are detected
  expect_true("numeric_as_character" %in% names(result$quality_issues))
  expect_true("outliers" %in% names(result$quality_issues))
  expect_true("missing_values" %in% names(result$quality_issues))
  expect_true("near_zero_variance" %in% names(result$quality_issues))
  expect_true("duplicate_rows" %in% names(result$quality_issues))

  # Check specific issues
  expect_true("age" %in% names(result$quality_issues$numeric_as_character))
  expect_true("score" %in% names(result$quality_issues$numeric_as_character))
  expect_true("income" %in% names(result$quality_issues$outliers))
  expect_true("constant" %in% names(result$quality_issues$near_zero_variance))
  expect_equal(result$quality_issues$duplicate_rows, 2)
})

test_that("data_overview handles edge cases correctly", {
  # Test with empty data.frame
  empty_df <- data.frame()
  result <- data_overview(empty_df)
  expect_true(is.list(result))
  expect_equal(result$overall$n_rows, 0)
  expect_equal(result$overall$n_columns, 0)

  # Test with single column data.frame
  single_col_df <- data.frame(x = 1:10)
  result <- data_overview(single_col_df)
  expect_true(is.list(result))
  expect_equal(result$overall$n_columns, 1)

  # Test with all NA data
  all_na_df <- data.frame(x = rep(NA, 10), y = rep(NA, 10))
  result <- data_overview(all_na_df)
  expect_true(is.list(result))
  expect_true("missing_values" %in% names(result$quality_issues))
})

test_that("data_overview handles different data types", {
  # Create a test dataset with various data types
  test_df <- data.frame(
    numeric_col = 1:10,
    integer_col = as.integer(1:10),
    character_col = letters[1:10],
    factor_col = factor(rep(c("A", "B"), 5)),
    logical_col = c(TRUE, FALSE, rep(NA, 8)),
    date_col = as.Date("2020-01-01") + 0:9,
    posixct_col = as.POSIXct("2020-01-01 12:00:00") + 0:9 * 3600
  )

  result <- data_overview(test_df)

  # Check variable type classification
  expect_true("numeric" %in% names(result$variable_types))
  expect_true("character" %in% names(result$variable_types))
  expect_true("logical" %in% names(result$variable_types))
  expect_true("date" %in% names(result$variable_types))

  # Check summary stats for different types
  expect_true("numeric" %in% names(result$summary_stats))
  expect_true("character" %in% names(result$summary_stats))
  expect_true("logical" %in% names(result$summary_stats))
})

test_that("data_overview provides meaningful recommendations", {
  # Create a test dataset with quality issues
  test_df <- data.frame(
    id = 1:10,
    income = c(rnorm(8, 50000, 10000), 200000, 300000),
    constant = rep("same", 10),
    gender = sample(c("M", "F", NA), 10, replace = TRUE, prob = c(0.4, 0.4, 0.2))
  )

  result <- data_overview(test_df)

  # Check that recommendations are provided
  expect_true(length(result$recommendations) > 0)

  # Check specific recommendations
  expect_true(any(grepl("outliers", result$recommendations)))
  expect_true(any(grepl("constant", result$recommendations)))
  expect_true(any(grepl("missing", result$recommendations)))
})

test_that("data_overview detects negative values in predominantly positive variables", {
  test_df <- data.frame(
    id = 1:11,
    age = c(20, 21, 22, 23, 24, 25, 26, 27, 28, 29, -5),
    height = c(170, 171, 172, 173, 174, 175, 176, 177, 178, 179, 180)
  )

  result <- data_overview(test_df)

  expect_true("negative_in_positive" %in% names(result$quality_issues))
  expect_true("age" %in% names(result$quality_issues$negative_in_positive))
  expect_equal(result$quality_issues$negative_in_positive$age$n_negative, 1)
  expect_true(any(grepl("negative", result$recommendations)))
})

test_that("data_overview detects empty columns", {
  test_df <- data.frame(
    id = 1:10,
    normal = 1:10,
    empty_col = rep(NA, 10)
  )

  result <- data_overview(test_df)

  expect_true("empty_columns" %in% names(result$quality_issues))
  expect_true("empty_col" %in% result$quality_issues$empty_columns)
  expect_true(any(grepl("empty", result$recommendations)))
})

test_that("data_overview detects empty rows", {
  test_df <- data.frame(
    id = 1:10,
    value = 1:10
  )
  test_df <- rbind(test_df, data.frame(id = NA, value = NA))
  test_df <- rbind(test_df, data.frame(id = NA, value = NA))

  result <- data_overview(test_df)

  expect_true("empty_rows" %in% names(result$quality_issues))
  expect_equal(length(result$quality_issues$empty_rows), 2)
  expect_true(any(grepl("empty", result$recommendations)))
})

test_that("data_overview detects suspicious dates", {
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  
  test_df <- data.frame(
    id = 1:10,
    normal_date = as.Date("2000-01-01") + 0:9,
    suspicious_date = as.Date(c(
      "1980-01-01", "1975-05-15", "1990-08-20",
      "1800-12-31",  # Too old
      as.Date(paste(current_year + 1, "06-15", sep = "-")),  # Future date
      rep("1995-01-01", 5)
    ))
  )

  result <- data_overview(test_df)

  expect_true("suspicious_dates" %in% names(result$quality_issues))
  expect_true("suspicious_date" %in% names(result$quality_issues$suspicious_dates))
  expect_true(result$quality_issues$suspicious_dates$suspicious_date$n_suspicious >= 1)
  expect_true(any(grepl("suspicious dates", result$recommendations)))
})

test_that("data_overview detects low cardinality variables", {
  test_df <- data.frame(
    id = 1:10,
    education = c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1),
    gender = c("M", "F", "M", "F", "M", "F", "M", "F", "M", "F"),
    score = round(rnorm(10, 75, 10))
  )

  result <- data_overview(test_df)

  expect_true("low_cardinality" %in% names(result$quality_issues))
  expect_true("education" %in% names(result$quality_issues$low_cardinality))
  expect_equal(result$quality_issues$low_cardinality$education$type, "numeric")
  expect_true(any(grepl("low-cardinality", result$recommendations)))
})

test_that("data_overview detects case inconsistency issues", {
  test_df <- data.frame(
    id = 1:12,
    city = c(
      "New York", "new york", "NEW YORK",
      "Boston", "BOSTON", "boston",
      "Chicago", "CHICAGO", "Chicago",
      "Miami", "MIAMI", "Miami"
    )
  )

  result <- data_overview(test_df)

  expect_true("case_issues" %in% names(result$quality_issues))
  expect_true("city" %in% names(result$quality_issues$case_issues))
  expect_true(result$quality_issues$case_issues$city$reduction > 0)
  expect_true(any(grepl("case inconsistency", result$recommendations)))
})
