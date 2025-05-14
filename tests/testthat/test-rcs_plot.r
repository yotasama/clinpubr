library(testthat)
library(ggplot2)
library(rms)
library(survival)
library(withr)

# Load test data
data(cancer, package = "survival")
cancer$dead <- cancer$status == 2  # Binary outcome for logistic model

set.seed(1)  # For reproducibility

test_that("rcs_plot works for Cox model", {
  withr::with_tempdir({
    result <- rcs_plot(cancer, x = "age", y = "status", time = "time", covs = "ph.karno", save_plot = TRUE)
    expect_s3_class(result, "gg")
    vdiffr::expect_doppelganger("Cox model RCS plot", result)
    expect_true(file.exists(list.files(pattern = ".png")))
  })

  test_that("rcs_plot works for linear model", {
    withr::with_tempdir({
      # Create continuous outcome
      cancer$continuous_y <- rnorm(nrow(cancer))
      result <- rcs_plot(cancer, x = "age", y = "continuous_y", covs = "ph.karno")
      expect_s3_class(result, "gg")
      vdiffr::expect_doppelganger("Linear model RCS plot", result)
      
      # Test with return_details
      details <- rcs_plot(cancer, x = "age", y = "continuous_y", covs = "ph.karno", return_details = TRUE)
      expect_named(details, c("aics", "knot", "n.valid", "n.plot", "phassump", "phresidual", "pvalue_all", "pvalue_nonlin", "ref", "plot"))
      expect_snapshot(details)
    })
  })
})

test_that("rcs_plot works for logistic model", {
  withr::with_tempdir({
    result <- rcs_plot(cancer, x = "age", y = "dead", covs = "ph.karno", save_plot = FALSE)
    expect_s3_class(result, "gg")
    expect_false(any(grepl(".png", list.files())))
  })

  test_that("rcs_plot works for linear model", {
    withr::with_tempdir({
      # Create continuous outcome
      cancer$continuous_y <- rnorm(nrow(cancer))
      result <- rcs_plot(cancer, x = "age", y = "continuous_y", covs = "ph.karno")
      expect_s3_class(result, "gg")
      vdiffr::expect_doppelganger("Linear model RCS plot", result)
      
      # Test with return_details
      details <- rcs_plot(cancer, x = "age", y = "continuous_y", covs = "ph.karno", return_details = TRUE)
      expect_named(details, c("aics", "knot", "n.valid", "n.plot", "phassump", "phresidual", "pvalue_all", "pvalue_nonlin", "ref", "plot"))
      expect_snapshot(details)
    })
  })
})

test_that("rcs_plot handles custom knots", {
  result <- rcs_plot(cancer, x = "age", y = "dead", knot = 5, return_details = TRUE)
  expect_equal(result$knot, 5)
  expect_s3_class(result$plot, "gg")
})

test_that("rcs_plot warns about missing data", {
  cancer_missing <- cancer
  cancer_missing$age[1:5] <- NA
  expect_warning(rcs_plot(cancer_missing, x = "age", y = "dead"), "incomplete cases excluded")
})

test_that("rcs_plot returns details when requested", {
  details <- rcs_plot(cancer, x = "age", y = "dead", return_details = TRUE)
  expect_named(details, c("aics", "knot", "n.valid", "n.plot", "phassump", "phresidual", "pvalue_all", "pvalue_nonlin", "ref", "plot"))
  # Exclude plot from snapshot as it's tested with vdiffr
  details_no_plot <- details[!names(details) %in% "plot"]
  expect_snapshot(details_no_plot)
})