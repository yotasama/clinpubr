# Unit tests for time_roc.R

library(testthat)

# Create synthetic survival data for testing
set.seed(123)
n <- 200
time <- rexp(n, rate = 0.01)  # Survival time
event <- rbinom(n, 1, 0.6)     # Event indicator (1 = event, 0 = censored)
maker <- rnorm(n, mean = 50, sd = 10)  # Marker variable

# Adjust marker to be associated with survival
maker <- ifelse(event == 1, maker + rnorm(n, mean = 10, sd = 5), maker)

test_data <- data.frame(time = time, status = event, age = maker)

# Test calc_cindex function
test_that("calc_cindex returns a valid C-index", {
  # Basic functionality
  expect_warning(c_index <- calc_cindex(test_data, "time", "status", "age"))
  expect_type(c_index, "double")
  expect_length(c_index, 1)
  expect_true(c_index >= 0.5 && c_index <= 1)
  
  # Test with reversed relationship
  # Create marker with negative correlation to survival
  neg_marker <- 100 - maker
  neg_test_data <- test_data
  neg_test_data$neg_age <- neg_marker
  
  neg_c_index <- calc_cindex(neg_test_data, "time", "status", "neg_age")
  expect_type(neg_c_index, "double")
  expect_length(neg_c_index, 1)
  expect_true(neg_c_index >= 0.5 && neg_c_index <= 1)
})

# Test time_roc_plot function
test_that("time_roc_plot returns a valid list with time_roc and plot", {
  result <- time_roc_plot(
    data = test_data,
    time_var = "time",
    event_var = "status",
    marker_var = "age",
    times = c(100, 200, 300)
  )
  
  # Check return structure
  expect_type(result, "list")
  expect_length(result, 2)
  expect_named(result, c("time_roc", "plot"))
  
  # Check time_roc element
  expect_s3_class(result$time_roc, "ipcwsurvivalROC")
  expect_equal(length(result$time_roc$times), 3)
  
  # Check plot element
  vdiffr::expect_doppelganger("time_roc_plot", result$plot)
})

test_that("time_roc_plot handles different parameters correctly", {
  # Test with custom colors
  result <- time_roc_plot(
    data = test_data,
    time_var = "time",
    event_var = "status",
    marker_var = "age",
    times = c(150, 250),
    colors = c("red", "blue"),
    title = TRUE
  )
  
  expect_equal(length(result$time_roc$times), 2)
  vdiffr::expect_doppelganger("time_roc_plot_custom", result$plot)
})

test_that("time_roc_plot works with single time point", {
  result <- time_roc_plot(
    data = test_data,
    time_var = "time",
    event_var = "status",
    marker_var = "age",
    times = 200
  )
  
  expect_true(length(result$time_roc$times) >= 1)
  vdiffr::expect_doppelganger("time_roc_plot_single", result$plot)
})
