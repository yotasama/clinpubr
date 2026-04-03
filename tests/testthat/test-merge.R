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

test_that("merge_by_range matches exact keys and returns since_start", {
  admissions <- data.frame(
    patient_id = c(1, 1, 2),
    visit_id = c("A", "B", "A"),
    date_start = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01")),
    date_end = as.Date(c("2024-01-10", "2024-02-10", "2024-03-05")),
    ward = c("W1", "W2", "W3"),
    stringsAsFactors = FALSE
  )
  examinations <- data.frame(
    patient_id = c(1, 1, 1, 2),
    visit_id = c("A", "B", "B", "A"),
    exam_date = as.Date(c("2024-01-01", "2024-02-10", "2024-02-11", "2024-03-03")),
    exam_name = c("CT", "MRI", "US", "XR"),
    stringsAsFactors = FALSE
  )

  result <- merge_by_range(
    x = admissions,
    y = examinations,
    by = c("patient_id", "visit_id"),
    x_start = "date_start",
    x_end = "date_end",
    y_val = "exam_date",
    all_y = FALSE
  )

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 3)
  expect_equal(result$exam_name, c("CT", "MRI", "XR"))
  expect_equal(result$ward, c("W1", "W2", "W3"))
  expect_equal(result$since_start, c(0, 9, 2))
})

test_that("merge_by_range supports x_end = NULL and range_relax", {
  x <- data.frame(
    id = 1,
    visit_start = as.Date("2024-01-10"),
    label = "visit",
    stringsAsFactors = FALSE
  )
  y <- data.frame(
    id = c(1, 1, 1),
    event_date = as.Date(c("2024-01-09", "2024-01-10", "2024-01-11")),
    stringsAsFactors = FALSE
  )

  result <- merge_by_range(
    x = x,
    y = y,
    by = "id",
    x_start = "visit_start",
    y_val = "event_date",
    range_relax = c(1, 0),
    all_y = FALSE
  )

  expect_equal(nrow(result), 2)
  expect_equal(result$event_date, as.Date(c("2024-01-09", "2024-01-10")))
  expect_equal(result$since_start, c(-1, 0))
})

test_that("merge_by_range supports different by columns and keeps unmatched y rows", {
  x <- data.frame(
    patient_id = c(1, 2),
    range_start = c(10, 20),
    range_end = c(15, 25),
    admission_type = c("A", "B"),
    stringsAsFactors = FALSE
  )
  y <- data.frame(
    pid = c(1, 3),
    event_value = c(12, 99),
    admission_type = c("same_name", "other"),
    stringsAsFactors = FALSE
  )

  result <- merge_by_range(
    x = x,
    y = y,
    by = list(x = "patient_id", y = "pid"),
    x_start = "range_start",
    x_end = "range_end",
    y_val = "event_value",
    all_y = TRUE
  )

  expect_equal(nrow(result), 2)
  expect_true(all(c("admission_type.x", "admission_type.y") %in% names(result)))
  expect_equal(sum(is.na(result$range_start)), 1)
  expect_equal(sum(is.na(result$since_start)), 1)
})

test_that("merge_by_range validates inputs", {
  x <- data.frame(
    id = 1,
    date_start = as.Date("2024-01-02"),
    date_end = as.Date("2024-01-01")
  )
  y <- data.frame(
    id = 1,
    exam_date = as.Date("2024-01-01")
  )

  expect_error(merge_by_range("bad", y, "id", "date_start", "date_end", "exam_date"))
  expect_error(merge_by_range(x, "bad", "id", "date_start", "date_end", "exam_date"))
  expect_error(merge_by_range(x, y, "missing", "date_start", "date_end", "exam_date"))
  expect_error(merge_by_range(x, y, "id", "date_start", "date_end", "exam_date", range_relax = c(0)))
  expect_error(merge_by_range(x, y, "id", "date_start", "date_end", "exam_date", range_relax = c(-1, 0)))
  expect_error(
    merge_by_range(
      transform(x, date_end = as.Date("2024-01-01")),
      y,
      "id",
      "date_start",
      "date_end",
      "exam_date"
    )
  )
})

test_that("merge_by_range clips relax-only overlap to a single neighboring range", {
  x <- data.frame(
    id = c(1, 1),
    start = c(1, 6),
    end = c(4, 8),
    label = c("left", "right"),
    stringsAsFactors = FALSE
  )
  y <- data.frame(
    id = 1,
    value = 5,
    stringsAsFactors = FALSE
  )

  result <- merge_by_range(
    x = x,
    y = y,
    by = "id",
    x_start = "start",
    x_end = "end",
    y_val = "value",
    range_relax = c(1, 1),
    all_y = FALSE
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$label, "right")
  expect_equal(result$since_start, -1)
  expect_false(".cp_y_row_id" %in% names(result))
})

test_that("merge_by_range keeps duplicates for fully overlapping core ranges and warns", {
  x <- data.frame(
    id = c(1, 1),
    start = c(1, 1),
    end = c(4, 4),
    label = c("left", "right"),
    stringsAsFactors = FALSE
  )
  y <- data.frame(
    id = 1,
    value = 2,
    stringsAsFactors = FALSE
  )

  expect_warning(
    result <- merge_by_range(
      x = x,
      y = y,
      by = "id",
      x_start = "start",
      x_end = "end",
      y_val = "value",
      range_relax = c(1, 1),
      all_y = FALSE
    ),
      "matched multiple clipped ranges"
  )

  expect_equal(nrow(result), 2)
  expect_equal(sort(result$label), c("left", "right"))
  expect_true(".cp_y_row_id" %in% names(result))
  expect_true(all(result$.cp_y_row_id == 1))
})

test_that("merge_by_range clips x_end NULL extensions before retaining true duplicate starts", {
  x <- data.frame(
    id = c(1, 1, 1, 1),
    start = c(1, 4, 4, 8),
    label = c("a", "b1", "b2", "c"),
    stringsAsFactors = FALSE
  )
  y <- data.frame(
    id = c(1, 1, 1, 1),
    value = c(2, 4, 5, 8),
    stringsAsFactors = FALSE
  )

  expect_warning(
    result <- merge_by_range(
      x = x,
      y = y,
      by = "id",
      x_start = "start",
      y_val = "value",
      range_relax = c(0, Inf),
      all_y = FALSE
    ),
      "matched multiple clipped ranges"
  )

    result <- result[order(result[["value"]], result[["label"]]), , drop = FALSE]
  expect_equal(result$label, c("a", "b1", "b2", "b1", "b2", "c"))
  expect_equal(result$value, c(2, 4, 4, 5, 5, 8))
  expect_equal(result$since_start, c(1, 0, 0, 1, 1, 0))
  expect_true(".cp_y_row_id" %in% names(result))
})


