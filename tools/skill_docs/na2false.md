# `na2false()`

> **Replace NA values with FALSE**

## Description

Replace `NA` values with `FALSE` in logical vectors.
For other vectors, the behavior relies on R's automatic conversion rules.

## Usage

```r
na2false(x)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `x` | A vector. |

## Value

A vector with `NA` values replaced by `FALSE`.

## Examples

```r
na2false(c(TRUE, FALSE, NA, TRUE, NA))
na2false(c(1, 2, NA))
```

