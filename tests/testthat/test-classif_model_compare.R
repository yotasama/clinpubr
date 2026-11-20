library(withr)
library(survival)

test_that("classif_model_compare outputs correct files", {
  skip_if_not_installed("pROC")
  skip_if_not_installed("caret")
  skip_if_not_installed("dcurves")
  skip_if_not_installed("ResourceSelection")
  data(cancer, package = "survival")
  df <- cancer
  df$status <- ifelse(df$status == 1, 0, 1)
  df$model1 <- runif(nrow(df))
  df$model2 <- df$model1 * 0.8 + 0.1

  with_tempdir({
    classif_model_compare(df, "status", c("model1", "model2"), save_output = TRUE,
                          output_prefix = "test", positive_class = "1")

    expect_true(file.exists("test_table.csv"))
    expect_true(file.exists("test_dca.png"))
    expect_true(file.exists("test_roc.png"))
    expect_true(file.exists("test_calibration.png"))

    tbl <- read.csv("test_table.csv")
    expect_equal(nrow(tbl), 2)
    expect_true(all(c("AUC", "Accuracy", "Sensitivity") %in% names(tbl)))
  })
})

test_that("save_output=FALSE suppresses file generation", {
  skip_if_not_installed("pROC")
  skip_if_not_installed("caret")
  skip_if_not_installed("dcurves")
  skip_if_not_installed("ResourceSelection")
  
  set.seed(123)
  data <- data.frame(
    target = factor(rbinom(100, 1, 0.5)),
    model = runif(100)
  )

  res <- classif_model_compare(data, "target", "model", save_output = FALSE, positive_class = "1")

  expect_false(file.exists("model_compare_table.csv"))
  expect_false(file.exists("model_compare_dca.png"))
  expect_named(res, c("metric_table", "dca_plot", "roc_plot", "pr_plot", "calibration_plot"))

  vdiffr::expect_doppelganger("dca-plot", res$dca_plot)
  vdiffr::expect_doppelganger("roc-plot", res$roc_plot)
  vdiffr::expect_doppelganger("pr-plot", res$pr_plot)
  vdiffr::expect_doppelganger("calibration-plot", res$calibration_plot)
  expect_snapshot(res$metric_table)
})

test_that("as_probability handles value conversion", {
  skip_if_not_installed("pROC")
  skip_if_not_installed("caret")
  skip_if_not_installed("dcurves")
  skip_if_not_installed("ResourceSelection")
  df <- data.frame(
    target = factor(rbinom(100, 1, 0.5)),
    modelA = runif(100),
    modelB = runif(100) * 5 - 2
  )

  res1 <- classif_model_compare(df, "target", "modelA",
    as_probability = TRUE,
    save_output = FALSE, positive_class = "1"
  )
  expect_true(all(res1$metric_table$Brier <= 1))

  expect_error(
    classif_model_compare(df, "target", "modelB", as_probability = FALSE),
    "not in range 0 to 1"
  )
})

test_that("positive_class controls the direction of every metric", {
  skip_if_not_installed("pROC")
  skip_if_not_installed("caret")
  skip_if_not_installed("dcurves")
  skip_if_not_installed("ResourceSelection")

  set.seed(1)
  y <- factor(c("case", "control")[sample(1:2, 200, TRUE)], levels = c("case", "control"))
  pred <- ifelse(y == "case", rnorm(200, 1), rnorm(200, 0))
  d <- data.frame(y = y, m1 = pred)

  ref_case <- as.numeric(pROC::roc(d$y, d$m1,
    levels = c("control", "case"), direction = "<", quiet = TRUE
  )$auc)

  res_case <- classif_model_compare(d, "y", "m1", positive_class = "case", save_output = FALSE, as_probability = TRUE)
  expect_equal(res_case$metric_table$AUC, round(ref_case, 3))

  res_control <- classif_model_compare(d, "y", "m1", positive_class = "control", save_output = FALSE, as_probability = TRUE)
  expect_equal(res_control$metric_table$AUC, round(1 - ref_case, 3))

  # default: levels[2] == "control", with an explicit warning
  expect_warning(
    res_default <- classif_model_compare(d, "y", "m1", save_output = FALSE, as_probability = TRUE),
    "positive_class"
  )
  expect_equal(res_default$metric_table$AUC, res_control$metric_table$AUC)
})

test_that("positive_class on the first level handles infinite best thresholds", {
  skip_if_not_installed("pROC")
  skip_if_not_installed("caret")
  skip_if_not_installed("dcurves")
  skip_if_not_installed("ResourceSelection")

  set.seed(1)
  n <- 120
  y <- factor(sample(c("control", "case"), n, TRUE), levels = c("control", "case"))
  pred <- runif(n, 0.05, 0.95)
  pred <- pmin(ifelse(y == "case", pred + 0.2, pred), 0.99)
  d <- data.frame(y = y, m1 = pred, m2 = runif(n, 0.05, 0.95))

  # pROC returns an infinite "best" threshold for m1 here, which used to break cut()
  res <- classif_model_compare(d, "y", c("m1", "m2"), positive_class = "control", save_output = FALSE)
  expect_equal(nrow(res$metric_table), 2)
  expect_true(all(is.finite(res$metric_table$cutoff)))
})

test_that("invalid positive_class and NA predictions are rejected", {
  skip_if_not_installed("pROC")
  skip_if_not_installed("caret")
  skip_if_not_installed("dcurves")
  skip_if_not_installed("ResourceSelection")

  d <- data.frame(y = factor(c("case", "control", "case", "control")), m1 = c(0.2, 0.7, 0.8, 0.1))

  expect_error(
    classif_model_compare(d, "y", "m1", positive_class = "bogus", save_output = FALSE),
    "positive_class"
  )

  d_na <- d
  d_na$m1[2] <- NA
  expect_error(
    classif_model_compare(d_na, "y", "m1", positive_class = "case", save_output = FALSE),
    "m1"
  )
})

test_that("small samples skip the calibration curve instead of crashing", {
  skip_if_not_installed("pROC")
  skip_if_not_installed("caret")
  skip_if_not_installed("dcurves")
  skip_if_not_installed("ResourceSelection")

  set.seed(2)
  d <- data.frame(
    y = factor(sample(c("0", "1"), 10, TRUE)),
    m1 = runif(10)
  )

  expect_warning(
    res <- classif_model_compare(d, "y", "m1", positive_class = "1", save_output = FALSE),
    "样本量不足"
  )
  expect_null(res$calibration_plot)
  expect_equal(nrow(res$metric_table), 1)
})

test_that("parameter validation works", {
  skip_if_not_installed("pROC")
  skip_if_not_installed("caret")
  skip_if_not_installed("dcurves")
  skip_if_not_installed("ResourceSelection")
  df <- data.frame(y = factor(c(1, 0)), m1 = c(0.5, 0.5))

  expect_error(classif_model_compare(df, "y", "invalid_model"))

  df$y_num <- as.numeric(df$y)
  expect_error(classif_model_compare(df, "y_num", "m1"))
})
