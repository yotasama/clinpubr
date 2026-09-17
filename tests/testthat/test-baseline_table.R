library(withr)
library(survival)

test_that("get_var_types correctly classifies variables", {
  data(cancer, package = "survival")

  with_tempdir({
    set.seed(1)
    res <- get_var_types(cancer, strata = "sex", save_qqplots = TRUE)

    expect_s3_class(res, "var_types")
    expect_true(dir.exists("qqplots"))
    expect_equal(length(list.files("qqplots")), 14)

    expect_snapshot(res)
  })
})

test_that("baseline_table generates correct output files 2", {
  skip_if_not_installed("tableone")
  with_tempdir({
    set.seed(1)
    var_types <- get_var_types(mtcars, strata = "vs") # Automatically infer variable types
    baseline_table(mtcars, var_types = var_types, contDigits = 1, seed = 1, save_table = TRUE,
                   filename = "baseline.csv")

    expect_snapshot(read.csv("baseline.csv", check.names = FALSE))
    expect_snapshot(read.csv("baseline_missing.csv", check.names = FALSE))
  })
})

test_that("baseline_table generates correct output files", {
  skip_if_not_installed("tableone")
  skip_if_not_installed("rstatix")
  data(cancer, package = "survival")
  cancer$ph.ecog_cat <- factor(cancer$ph.ecog, levels = c(0:3), labels = c("0", "1", ">=2", ">=2"))

  with_tempdir({
    set.seed(1)
    var_types <- get_var_types(cancer, strata = "ph.ecog_cat")
    baseline_table(cancer, var_types = var_types, contDigits = 1, seed = 1, save_table = TRUE,
                   filename = "test_output.csv")

    expect_true(file.exists("test_output.csv"))
    expect_true(file.exists("test_output_missing.csv"))
    expect_true(file.exists("test_output_pairwise.csv"))

    expect_snapshot(read.csv("test_output.csv", check.names = FALSE))
    expect_snapshot(read.csv("test_output_missing.csv", check.names = FALSE))
    expect_snapshot(read.csv("test_output_pairwise.csv", check.names = FALSE))
  })
})

test_that("alpha_by_n calculates appropriate thresholds", {
  expect_equal(alpha_by_n(50), 0.05)
  set.seed(1)
  expect_snapshot(alpha_by_n(500))
})

test_that("test_normality classifies distributions correctly", {
  set.seed(42)
  expect_true(test_normality(rnorm(500)))
  expect_false(test_normality(rexp(500)))
  expect_false(test_normality(rlnorm(500, 0, 2)))

  # Positive normal data (CV < 1, e.g. age/BMI-like variables) must not be flagged
  # as right-skewed by the heuristic.
  x <- rnorm(1000, mean = 100, sd = 5)
  expect_true(all(x >= 0))
  expect_true(sd(x) < mean(x))
  expect_true(test_normality(x))
})

test_that("test_normality handles insufficient data", {
  expect_warning(val <- test_normality(c(1, 2)), "Insufficient")
  expect_false(val)
  expect_false(suppressWarnings(test_normality(numeric(0))))
})

test_that("test_normality does not flag non-normality when all tests fail (NA p-values)", {
  # A constant vector makes every normality test error (zero variance).
  # No rejecting evidence means it must not be classified as non-normal.
  expect_true(suppressWarnings(test_normality(rep(5, 20))))
})
