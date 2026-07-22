# `indicate_duplicates()`

> **Determine duplicate elements including their first occurrence.**

## Description

If an element is duplicated, all of its occurrence will be labeled `TRUE`.
Useful to list and compare all duplicates.

## Usage

```r
indicate_duplicates(x)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `x` | A vector. |

## Value

A logical vector.

## Examples

```r
indicate_duplicates(c(1, 2, NA, NA, 1))
indicate_duplicates(c(1, 2, 3, 4, 4))

# Useful to check duplicates in data frames.
df <- data.frame(
  id = c(1, 2, 1, 2, 3), year = c(2010, 2011, 2010, 2010, 2011),
  value = c(1, 2, 3, 4, 5)
)
df[indicate_duplicates(df[, c("id", "year")]), ]
```

