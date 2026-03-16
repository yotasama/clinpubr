test_that("merge_by_substring works with basic dataset", {
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

  result <- merge_by_substring(df, match_df, search_col = "name", key_col = "ori", value_cols = "category")

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 8)
  expect_equal(ncol(result), 3)
  expect_equal(names(result), c("name", "value", "category"))
  expect_true("category" %in% names(result))
  expect_equal(result$name, c("A..", "AB", "AB", "ACD", "ACD", "ACD", "B,C", "B,C"))
})

test_that("merge_by_substring works with multiple new columns", {
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

  result <- merge_by_substring(df, match_df, search_col = "name", key_col = "ori", value_cols = c("category", "code"))

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 8)
  expect_equal(ncol(result), 4)
  expect_equal(names(result), c("name", "value", "category", "code"))
  expect_true("category" %in% names(result))
  expect_true("code" %in% names(result))
})

test_that("merge_by_substring handles unmatched rows with NA", {
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

  result <- merge_by_substring(df, match_df, search_col = "name", key_col = "ori", value_cols = "category")

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 4)
  expect_true(any(is.na(result$category)))
})

test_that("merge_by_substring handles empty match_df", {
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

  result <- merge_by_substring(df, match_df, search_col = "name", key_col = "ori", value_cols = "category")

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2)
  expect_equal(names(result), c("name", "value"))
})

test_that("merge_by_substring handles duplicate patterns in same group", {
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

  result <- merge_by_substring(df, match_df, search_col = "name", key_col = "ori", value_cols = "category")

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 4)
  expect_true(all(result$category %in% c("cat1", "cat2")))
})

test_that("merge_by_substring works with default column names", {
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

  result <- merge_by_substring(df, match_df, search_col = "name", key_col = "ori", value_cols = "new")

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 5)
  expect_true("new" %in% names(result))
})

test_that("merge_by_substring validates input parameters", {
  df <- data.frame(name = c("AB", "B,C"), value = c(1, 2))
  match_df <- data.frame(ori = c("A", "B"), category = c("cat1", "cat2"))

  expect_error(merge_by_substring("not_a_df", match_df))
  expect_error(merge_by_substring(df, "not_a_df"))
  expect_error(merge_by_substring(df, match_df, search_col = "nonexistent", key_col = "ori", value_cols = "category"))
  expect_error(merge_by_substring(df, match_df, search_col = "name", value_cols = "nonexistent", key_col = "nonexistent"))
  expect_error(merge_by_substring(df, match_df, search_col = "name", key_col = "ori", value_cols = "nonexistent"))
})

test_that("merge_by_substring handles NA patterns", {
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

  result <- merge_by_substring(df, match_df, search_col = "name", key_col = "ori", value_cols = "category")

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 4)
})

test_that("merge_by_substring works with special regex characters", {
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

  result <- merge_by_substring(df, match_df, search_col = "name", key_col = "ori", value_cols = "category")

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 4)
  expect_equal(result$category[result$name == "A.B"], "cat1")
})

test_that("merge_by_date_range matches exact keys and inclusive date ranges", {
  admissions <- data.frame(
    patient_id = c(1, 1, 2),
    visit_id = c("A", "B", "A"),
    date_start = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01")),
    date_end = as.Date(c("2024-01-10", "2024-02-10", "2024-03-05")),
    ward = c("W1", "W2", "W3")
  )
  examinations <- data.frame(
    patient_id = c(1, 1, 1, 2),
    visit_id = c("A", "B", "B", "A"),
    exam_date = as.Date(c("2024-01-01", "2024-02-10", "2024-02-11", "2024-03-03")),
    exam_name = c("CT", "MRI", "US", "XR")
  )

  result <- merge_by_date_range(
    x = admissions,
    y = examinations,
    by = c("patient_id", "visit_id"),
    x_start = "date_start",
    x_end = "date_end",
    y_date = "exam_date"
  )

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 3)
  expect_equal(result$exam_name, c("CT", "MRI", "XR"))
  expect_equal(result$ward, c("W1", "W2", "W3"))
})

test_that("merge_by_date_range supports open interval boundaries", {
  x <- data.frame(
    id = 1,
    date_start = as.Date("2024-01-01"),
    date_end = as.Date("2024-01-10"),
    stringsAsFactors = FALSE
  )
  y <- data.frame(
    id = c(1, 1, 1),
    exam_date = as.Date(c("2024-01-01", "2024-01-05", "2024-01-10")),
    stringsAsFactors = FALSE
  )

  result <- merge_by_date_range(
    x = x,
    y = y,
    by = "id",
    x_start = "date_start",
    x_end = "date_end",
    y_date = "exam_date",
    include_start = FALSE,
    include_end = FALSE
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$exam_date, as.Date("2024-01-05"))
})

test_that("merge_by_date_range keeps unmatched rows when requested", {
  x <- data.frame(
    id = c(1, 2),
    date_start = as.Date(c("2024-01-01", "2024-02-01")),
    date_end = as.Date(c("2024-01-10", "2024-02-05")),
    admission_type = c("A", "B"),
    stringsAsFactors = FALSE
  )
  y <- data.frame(
    id = c(1, 3),
    exam_date = as.Date(c("2024-01-03", "2024-03-01")),
    admission_type = c("same_name", "other"),
    stringsAsFactors = FALSE
  )

  result <- merge_by_date_range(
    x = x,
    y = y,
    by = "id",
    x_start = "date_start",
    x_end = "date_end",
    y_date = "exam_date",
    all.x = TRUE,
    all.y = TRUE
  )

  expect_equal(nrow(result), 3)
  expect_true(all(c("admission_type.x", "admission_type.y") %in% names(result)))
  expect_equal(sum(is.na(result$exam_date)), 1)
  expect_equal(sum(is.na(result$date_start)), 1)
})

test_that("merge_by_date_range validates inputs", {
  x <- data.frame(
    id = 1,
    date_start = as.Date("2024-01-02"),
    date_end = as.Date("2024-01-01")
  )
  y <- data.frame(
    id = 1,
    exam_date = as.Date("2024-01-01")
  )

  expect_error(merge_by_date_range("bad", y, "id", "date_start", "date_end", "exam_date"))
  expect_error(merge_by_date_range(x, "bad", "id", "date_start", "date_end", "exam_date"))
  expect_error(merge_by_date_range(x, y, "missing", "date_start", "date_end", "exam_date"))
  expect_error(merge_by_date_range(x, y, "id", "date_start", "date_end", "exam_date"))
  expect_error(
    merge_by_date_range(
      transform(x, date_end = as.Date("2024-01-01")),
      y,
      "id",
      "date_start",
      "date_end",
      "exam_date"
    )
  )
})

test_that("merge_by_date_range data.table and base engines are consistent", {
  skip_if_not_installed("data.table")

  x <- data.frame(
    id = c(1, 1, 2, 2),
    grp = c("A", "A", "B", "B"),
    date_start = as.Date(c("2024-01-01", "2024-01-05", "2024-02-01", "2024-02-10")),
    date_end = as.Date(c("2024-01-10", "2024-01-08", "2024-02-05", "2024-02-20")),
    x_value = c("x1", "x2", "x3", "x4"),
    stringsAsFactors = FALSE
  )
  y <- data.frame(
    id = c(1, 1, 2, 2, 3),
    grp = c("A", "A", "B", "B", "C"),
    exam_date = as.Date(c("2024-01-05", "2024-01-09", "2024-02-03", "2024-02-21", "2024-03-01")),
    y_value = c("y1", "y2", "y3", "y4", "y5"),
    stringsAsFactors = FALSE
  )

  result_base <- merge_by_date_range(
    x = x,
    y = y,
    by = c("id", "grp"),
    x_start = "date_start",
    x_end = "date_end",
    y_date = "exam_date",
    all.x = TRUE,
    all.y = TRUE,
    engine = "base"
  )

  result_dt <- merge_by_date_range(
    x = x,
    y = y,
    by = c("id", "grp"),
    x_start = "date_start",
    x_end = "date_end",
    y_date = "exam_date",
    all.x = TRUE,
    all.y = TRUE,
    engine = "data.table"
  )

  order_cols <- c("id", "grp", "date_start", "date_end", "exam_date", "x_value", "y_value")
  result_base <- result_base[do.call(order, result_base[order_cols]), , drop = FALSE]
  result_dt <- result_dt[do.call(order, result_dt[order_cols]), , drop = FALSE]

  expect_equal(result_dt, result_base)
})
