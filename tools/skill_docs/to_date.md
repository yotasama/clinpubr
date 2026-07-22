# `to_date()`

> **Convert numerical or character date to date.**

## Description

Convert numerical (especially Excel date) or character date to date. Can deal with
common formats and allow different formats in one vector.

## Usage

```r
to_date(
  x,
  from_excel = TRUE,
  verbose = TRUE,
  try_formats = c("%Y-%m-%d", "%Y/%m/%d", "%Y%m%d", "%Y.%m.%d")
)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `x` | A vector that stores dates in numerical or character types. |
| `from_excel` | If TRUE, treat numerical values as Excel dates. |
| `verbose` | If TRUE, print the values that cannot be converted. |
| `try_formats` | A character vector of date formats to try. Same as `tryFormats` in `as.Date`. |

## Value

A single valid value from the vector. `NA` if all values are invalid.

## Examples

```r
to_date(c(43562, "2020-01-01", "2020/01/01", "20200101", "2020.01.01"))
```

