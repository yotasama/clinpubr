# Source the necessary files to load the functions
source("../../R/mark_outlier.R")
source("../../R/merge.R")

test_that("merge_str_contains works with basic dataset", {
  df <- data.frame(
    name = c("AB", "B,C", "A..", "ACD"),
    value = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )
  match_df <- data.frame(
    ori = c("A", "B", "C", "ACD", "AB"),
    category = c("cat1", "cat2", "cat3", "cat4", "cat1"),
    stringsAsFactors = FALSE
  )

  result <- merge_str_contains(df, match_df, new_cols = "category")

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 4)
  expect_equal(ncol(result), 3)
  expect_equal(names(result), c("name", "value", "category"))
  expect_true("category" %in% names(result))
  expect_equal(result$name, c("A..", "AB", "ACD", "B,C"))
})

test_that("merge_str_contains works with multiple new columns", {
  df <- data.frame(
    name = c("AB", "B,C", "A..", "ACD"),
    value = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )
  match_df <- data.frame(
    ori = c("A", "B", "C", "ACD", "AB"),
    category = c("cat1", "cat2", "cat3", "cat4", "cat1"),
    code = c("001", "002", "003", "004", "001"),
    stringsAsFactors = FALSE
  )

  result <- merge_str_contains(df, match_df, new_cols = c("category", "code"))

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 4)
  expect_equal(ncol(result), 4)
  expect_equal(names(result), c("name", "value", "category", "code"))
  expect_true("category" %in% names(result))
  expect_true("code" %in% names(result))
})

test_that("merge_str_contains handles unmatched rows with NA", {
  df <- data.frame(
    name = c("AB", "XYZ", "A.."),
    value = c(1, 2, 3),
    stringsAsFactors = FALSE
  )
  match_df <- data.frame(
    ori = c("A", "AB"),
    category = c("cat1", "cat2"),
    stringsAsFactors = FALSE
  )

  result <- merge_str_contains(df, match_df, new_cols = "category")

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 3)
  expect_true(any(is.na(result$category)))
})

test_that("merge_str_contains handles empty match_df", {
  df <- data.frame(
    name = c("AB", "B,C"),
    value = c(1, 2),
    stringsAsFactors = FALSE
  )
  match_df <- data.frame(
    ori = character(0),
    category = character(0),
    stringsAsFactors = FALSE
  )

  result <- merge_str_contains(df, match_df, new_cols = "category")

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2)
  expect_equal(names(result), c("name", "value"))
})

test_that("merge_str_contains handles duplicate patterns in same group", {
  df <- data.frame(
    name = c("AB", "B,C", "A.."),
    value = c(1, 2, 3),
    stringsAsFactors = FALSE
  )
  match_df <- data.frame(
    ori = c("A", "A", "A", "B"),
    category = c("cat1", "cat1", "cat1", "cat2"),
    stringsAsFactors = FALSE
  )

  result <- merge_str_contains(df, match_df, new_cols = "category")

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 3)
  expect_true(all(result$category %in% c("cat1", "cat2")))
})

test_that("merge_str_contains works with default column names", {
  df <- data.frame(
    name = c("AB", "B,C", "A.."),
    value = c(1, 2, 3),
    stringsAsFactors = FALSE
  )
  match_df <- data.frame(
    ori = c("A", "B", "AB"),
    new = c("cat1", "cat2", "cat3"),
    stringsAsFactors = FALSE
  )

  result <- merge_str_contains(df, match_df)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 3)
  expect_true("new" %in% names(result))
})

test_that("merge_str_contains validates input parameters", {
  df <- data.frame(name = c("AB", "B,C"), value = c(1, 2))
  match_df <- data.frame(ori = c("A", "B"), category = c("cat1", "cat2"))

  expect_error(merge_str_contains("not_a_df", match_df))
  expect_error(merge_str_contains(df, "not_a_df"))
  expect_error(merge_str_contains(df, match_df, key_col = "nonexistent"))
  expect_error(merge_str_contains(df, match_df, ori_col = "nonexistent"))
  expect_error(merge_str_contains(df, match_df, new_cols = "nonexistent"))
})

test_that("merge_str_contains handles NA patterns", {
  df <- data.frame(
    name = c("AB", "B,C", "A.."),
    value = c(1, 2, 3),
    stringsAsFactors = FALSE
  )
  match_df <- data.frame(
    ori = c("A", NA, "AB"),
    category = c("cat1", "cat2", "cat3"),
    stringsAsFactors = FALSE
  )

  result <- merge_str_contains(df, match_df, new_cols = "category")

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 3)
})

test_that("merge_str_contains works with special regex characters", {
  df <- data.frame(
    name = c("A.B", "A+B", "A?B", "A*B"),
    value = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )
  match_df <- data.frame(
    ori = c("A.B", "A+B"),
    category = c("cat1", "cat2"),
    stringsAsFactors = FALSE
  )

  result <- merge_str_contains(df, match_df, new_cols = "category")

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 4)
  expect_equal(result$category[result$name == "A.B"], "cat1")
})
