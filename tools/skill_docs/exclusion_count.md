# `exclusion_count()`

> **Count the number of excluded samples at each step**

## Description

This function sequentially applies exclusion criteria to a data frame and counts the number of samples
removed at each step.

## Usage

```r
exclusion_count(.df, ..., .criteria_names = NULL, .na_exclude = TRUE)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `.df` | A data frame. |
| `...` | Exclusion criteria. Logical expressions that define which rows to exclude. |
| `.criteria_names` | An optional character vector of names for the criteria. If `NULL`, the expressions themselves are used as names. |
| `.na_exclude` | A logical value. If `TRUE`, rows where the criterion evaluates to `NA` will be excluded, and a warning will be issued. Defaults to `FALSE`, where `NA` values are not excluded. |

## Value

A data frame with two columns: 'Criteria' and 'N', showing the number of samples at the start, the number
excluded at each step, and the final number remaining.

## Examples

```r
cohort <- data.frame(
  age = c(17, 25, 30, NA, 50, 60),
  sex = c("M", "F", "F", "M", "F", "M"),
  value = c(1, NA, 3, 4, 5, NA),
  dementia = c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE)
)
exclusion_count(
  cohort,
  age < 18,
  is.na(value),
  dementia == TRUE,
  .criteria_names = c(
    "Age < 18 years",
    "Missing value",
    "History of dementia"
  )
)
```

