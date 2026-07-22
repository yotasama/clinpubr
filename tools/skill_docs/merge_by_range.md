# `merge_by_range()`

> **Merge Data Frames by Exact Keys and Value Range**

## Description

Merge two data frames where shared keys in `by` must match exactly and the
value in `y[[y_val]]` must fall within the range defined by
`x[[x_start]]` and `x[[x_end]]`.

This function is particularly useful for date-based matching scenarios,
where you need to match events (e.g., examinations, treatments) to
time intervals (e.g., hospital admissions, visits). While the function
accepts any ordered values (numeric, Date, POSIXt), date matching is
the primary use case.

This avoids constructing the full Cartesian product that would be produced by
a regular equality join followed by range filtering.

Merge two data frames where shared keys in `by` must match exactly and the
value in `y[[y_val]]` must fall within the range defined by
`x[[x_start]]` and `x[[x_end]]`.

This function is particularly useful for date-based matching scenarios,
where you need to match events (e.g., examinations, treatments) to
time intervals (e.g., hospital admissions, visits). While the function
accepts any ordered values (numeric, Date, POSIXt), date matching is
the primary use case.

This avoids constructing the full Cartesian product that would be produced by
a regular equality join followed by range filtering.

## Usage

```r
merge_by_range(
  x,
  y,
  by,
  x_start,
  x_end = NULL,
  y_val,
  range_relax = c(0, 0),
  all_y = TRUE,
  suffixes = c(".x", ".y")
)

merge_by_range(
  x,
  y,
  by,
  x_start,
  x_end = NULL,
  y_val,
  range_relax = c(0, 0),
  all_y = TRUE,
  suffixes = c(".x", ".y")
)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `x` | A data frame containing the range columns. |
| `y` | A data frame containing the point-in-time value column. |
| `by` | Either a character vector of column names that must match exactly in both data frames, or a named list with elements `x` and `y` specifying different column names in each data frame (e.g., `list(x = c("id1", "id2"), y = c("ID1", "ID2"))`). The two vectors must have the same length and are matched by position. Use `character(0)` when no exact-match keys are needed. |
| `x_start` | Column name in `x` containing the range start. |
| `x_end` | Column name in `x` containing the range end. If `NULL`, defaults to `x_start` (treating the range as a single point). |
| `y_val` | Column name in `y` containing the value to be matched. |
| `range_relax` | A numeric vector of length 2 specifying how to extend the matching range. The first element extends backwards from `x_start`, the second extends forwards from `x_end`. Default is `c(0, 0)` (no extension). Both values must be >= 0. |
| `all_y` | Logical, whether to keep rows from `y` that have no match. |
| `suffixes` | Character vector of length 2 used for duplicated non-key column names from `x` and `y`. |

## Value

A data frame containing matched rows from `x` and `y`. The output
includes a `since_start` column indicating the numeric difference between
`y_val` and `x_start` (in the units of the values, e.g., days for Date objects).

A data frame containing matched rows from `x` and `y`. The output
includes a `since_start` column indicating the numeric difference between
`y_val` and `x_start` (in the units of the values, e.g., days for Date objects).

## Details

Matching proceeds in three stages:

 Rows in `x` and `y` are first grouped by the exact-match keys in `by`.
 Within each group, `y[[y_val]]` is matched against the interval defined by
`x[[x_start]]` and `x[[x_end]]`, optionally extended by `range_relax`.
 When `range_relax` is non-zero, the relaxed intervals are clipped against
neighboring core intervals before matching, but never clipped further than
the original core interval.


If any row from `y` still matches multiple clipped ranges in `x`, a warning
is issued and `.cp_y_row_id` is retained in the output so duplicate matches
can be identified downstream.

When `all_y = TRUE`, rows from `y` with no match are appended to the result
with `NA` values for columns coming from `x` and for `since_start`.

Matching proceeds in three stages:

 Rows in `x` and `y` are first grouped by the exact-match keys in `by`.
 Within each group, `y[[y_val]]` is matched against the interval defined by
`x[[x_start]]` and `x[[x_end]]`, optionally extended by `range_relax`.
 When `range_relax` is non-zero, the relaxed intervals are clipped against
neighboring core intervals before matching, but never clipped further than
the original core interval.


If any row from `y` still matches multiple clipped ranges in `x`, a warning
is issued and `.cp_y_row_id` is retained in the output so duplicate matches
can be identified downstream.

When `all_y = TRUE`, rows from `y` with no match are appended to the result
with `NA` values for columns coming from `x` and for `since_start`.

## Examples

```r
admissions <- data.frame(
  patient_id = c(1, 1, 2),
  date_start = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01")),
  date_end = as.Date(c("2024-01-10", "2024-02-10", "2024-03-05")),
  ward = c("A", "B", "C")
)
examinations <- data.frame(
  patient_id = c(1, 1, 2, 3),
  exam_date = as.Date(c("2024-01-05", "2024-02-10", "2024-03-07", "2024-01-01")),
  exam_name = c("CT", "MRI", "XR", "US")
)

merge_by_range(
  x = admissions,
  y = examinations,
  by = "patient_id",
  x_start = "date_start",
  x_end = "date_end",
  y_val = "exam_date"
)

admissions <- data.frame(
  patient_id = c(1, 1, 2),
  date_start = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01")),
  date_end = as.Date(c("2024-01-10", "2024-02-10", "2024-03-05")),
  ward = c("A", "B", "C")
)
examinations <- data.frame(
  patient_id = c(1, 1, 2, 3),
  exam_date = as.Date(c("2024-01-05", "2024-02-10", "2024-03-07", "2024-01-01")),
  exam_name = c("CT", "MRI", "XR", "US")
)

merge_by_range(
  x = admissions,
  y = examinations,
  by = "patient_id",
  x_start = "date_start",
  x_end = "date_end",
  y_val = "exam_date"
)
```

