test_that("group_by_range handles basic example", {
  x <- c(1, 2, 5, 7, 10, 11, 11, 11)
  expect_equal(group_by_range(x, 3), c(1L, 1L, 2L, 2L, 3L, 3L, 3L, 3L))
})

test_that("group_by_range handles identical values", {
  x <- rep(5, 10)
  expect_equal(group_by_range(x, 0), rep(1L, 10))
})

test_that("group_by_range handles boundary condition (range == max_range)", {
  x <- c(1, 4, 7, 10)
  expect_equal(group_by_range(x, 3), c(1L, 1L, 2L, 2L))
})

test_that("group_by_range handles one element per group", {
  x <- c(1, 10, 20, 30)
  expect_equal(group_by_range(x, 5), 1:4)
})

test_that("group_by_range handles empty vector", {
  expect_equal(group_by_range(numeric(0), 3), integer(0))
})

test_that("group_by_range handles single element", {
  expect_equal(group_by_range(5, 3), 1L)
})

test_that("group_by_range errors on unsorted input", {
  x <- c(10, 1, 11, 2, 5, 7, 11, 11)
  expect_error(group_by_range(x, 3), "must be sorted")
})

test_that("group_by_range handles negative and decimal values", {
  x <- c(-5.5, -2.1, 0.0, 1.3, 4.8, 10.2)
  expect_equal(group_by_range(x, 3), c(1L, 2L, 2L, 3L, 4L, 5L))
})

test_that("group_by_range handles Date input", {
  x <- as.Date(c("2024-01-01", "2024-01-02", "2024-01-05", "2024-01-07"))
  expect_equal(group_by_range(x, 3), c(1L, 1L, 2L, 2L))
})

test_that("group_by_range validates inputs", {
  expect_error(group_by_range(c(2, 1), 3), "must be sorted")
  expect_error(group_by_range(1:5, -1), "must be a single non-negative")
  expect_error(group_by_range(1:5, c(1, 2)), "must be a single non-negative")
})
