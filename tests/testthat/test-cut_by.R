test_that("cut_by handles quantile breaks", {
  set.seed(123)
  x <- rnorm(100)

  res <- cut_by(x, c(0.3, 0.7), breaks_as_quantiles = TRUE, label_type = "LMH")
  expect_equal(levels(res), c("Low", "Medium", "High"))

  res <- cut_by(x, c(0.4), breaks_as_quantiles = TRUE, label_type = "combined")
  expect_equal(levels(res), c("Low [-2.31,-0.223)", "High [-0.223,2.19]"))

  res <- cut_by(x, c(0.23), labels = c("A", "b"))
  expect_snapshot(res)
})

test_that("boundary conditions handled", {
  expect_error(cut_by(1:10, c(-1, 2)))
})
