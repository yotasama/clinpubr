load_all()
library(testthat)

set.seed(1)

test_that("regression_basic_results works for Cox regression", {
  withr::with_tempdir({
    data(cancer, package = "survival")
    regression_basic_results(cancer,
      x = "age", y = "status", time = "time",
      model_covs = list(Crude = c(), Model1 = c("ph.karno"))
    )
    expect_true(file.exists("cox_results_age/table_age.csv"))
    expect_true(any(grepl("kmplot_x.quartile.png", list.files("cox_results_age"))))
  })
})

set.seed(1)

test_that("regression_basic_results works for logistic regression", {
  withr::with_tempdir({
    data(cancer, package = "survival")
    cancer$dead <- cancer$status == 2
    regression_basic_results(cancer,
      x = "age", y = "dead",
      model_covs = list(Crude = c(), Model1 = c("ph.karno"))
    )
    expect_true(file.exists("logistic_results_age/table_age.csv"))
  })
})

set.seed(1)

test_that("regression_basic_results works for linear regression", {
  withr::with_tempdir({
    data(cancer, package = "survival")
    # Create continuous outcome and introduce missing data
    cancer$bmi <- rnorm(nrow(cancer), mean = 25, sd = 3)
    cancer$ph.karno[sample(1:nrow(cancer), 10)] <- NA
    regression_basic_results(cancer,
      x = "age", y = "bmi",
      model_covs = list(Crude = c(), Model1 = c("ph.karno"))
    )
    expect_true(file.exists("linear_results_age/table_age.csv"))
  })
})

set.seed(1)

test_that("regression_basic_results CSV output matches snapshot", {
  withr::with_tempdir({
    data(cancer, package = "survival")
    regression_basic_results(cancer,
      x = "age", y = "status", time = "time",
      model_covs = list(Crude = c(), Model1 = c("ph.karno"))
    )
    csv_file <- list.files("cox_results_age", pattern = "table_age.csv", full.names = TRUE)
    expect_snapshot(read.csv(csv_file))
  })
})

set.seed(1)

test_that("regression_basic_results KM plot matches reference image", {
  if (requireNamespace("vdiffr", quietly = TRUE)) {
    withr::with_tempdir({
      data(cancer, package = "survival")
      regression_basic_results(cancer,
        x = "age", y = "status", time = "time",
        model_covs = list(Crude = c(), Model1 = c("ph.karno")),
        colors = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728")
      )
      png_file <- list.files("cox_results_age", pattern = "kmplot_x.quartile.png", full.names = TRUE)
      vdiffr::expect_doppelganger("cox_km_plot", png_file)
    })
  }
})

set.seed(1)

test_that("regression_basic_results logistic CSV matches snapshot", {
  withr::with_tempdir({
    data(cancer, package = "survival")
    cancer$dead <- cancer$status == 2
    regression_basic_results(cancer,
      x = "age", y = "dead",
      model_covs = list(Crude = c(), Model1 = c("ph.karno"))
    )
    csv_file <- list.files("logistic_results_age", pattern = "table_age.csv", full.names = TRUE)
    expect_snapshot(read.csv(csv_file))
  })
})

set.seed(1)

test_that("regression_forest generates valid plot", {
  withr::with_tempdir({
    data(cancer, package = "survival")
    cancer$ph.ecog_cat <- factor(cancer$ph.ecog, levels = c(0:3), labels = c("0", "1", "≥2", "≥2"))
    p <- regression_forest(cancer,
      model_vars = c("age", "sex", "wt.loss", "ph.ecog_cat"), y = "status", time = "time",
      as_univariate = TRUE, save_plot = TRUE
    )
    expect_true(inherits(p, "gtable"))
    expect_true(file.exists("regression_forest_univariate.png"))
  })
})

set.seed(1)

test_that("regression_forest handles factor variables", {
  withr::with_tempdir({
    data(cancer, package = "survival")
    cancer$ph.ecog_cat <- factor(cancer$ph.ecog, levels = c(0:3), labels = c("0", "1", "≥2", "≥2"))
    p <- regression_forest(cancer,
      model_vars = list(M1 = c("age", "ph.ecog_cat")), y = "status", time = "time"
    )
    expect_true(inherits(p, "gtable"))
  })
})
