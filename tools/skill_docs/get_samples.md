# `get_samples()`

> **Generate a sample of values from a vector and collapse them.**

## Description

Generate a string summary of a vector by picking samples.

## Usage

```r
get_samples(x, unique_only = FALSE, n_samples = 10, collapse = "\n")
```

## Arguments

| 参数 | 说明 |
|------|------|
| `x` | A vector of values. |
| `unique_only` | A logical value indicating whether to return unique values only. |
| `n_samples` | The number of samples to return. |
| `collapse` | The separator to use for collapsing the values. |

## Value

A character string.

## Examples

```r
get_samples(c(1, 2, 3, 4, 5))
get_samples(c(1, 2, 3, 4, 5), n_samples = 2)
get_samples(c(1, 2, 3, 3, 3), n_samples = 2, unique_only = TRUE)
get_samples(c(1, 2, 3, 4, 5), collapse = ", ")
```

