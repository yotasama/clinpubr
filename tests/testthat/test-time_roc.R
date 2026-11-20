# Unit tests for time_roc.R

library(testthat)
library(survival) # timeROC/pec resolve `Surv` from the attached survival package

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
test_that("calc_cindex flips a C-index below 0.5 by default", {
  raw <- Hmisc::rcorr.cens(maker, survival::Surv(time, event))["C Index"]
  expect_lt(raw, 0.5)

  # default: flipped to 1 - C-index with a warning
  expect_warning(
    rev_c_index <- calc_cindex(test_data, "time", "status", "age"),
    "reversed"
  )
  expect_equal(rev_c_index, 1 - raw)

  # flip = FALSE returns the raw value without warning
  expect_silent(no_flip <- calc_cindex(test_data, "time", "status", "age", flip = FALSE))
  expect_equal(no_flip, raw)
})

test_that("time_roc_plot skips time points without cases or controls", {
  expect_warning(
    result <- time_roc_plot(
      data = test_data,
      time_var = "time",
      event_var = "status",
      marker_var = "age",
      times = c(100, 10000)
    ),
    "skipped"
  )
  expect_equal(nlevels(result$plot$data$time), 1)
  expect_false(any(is.nan(result$plot$data$fpr) | is.nan(result$plot$data$tpr)))
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
