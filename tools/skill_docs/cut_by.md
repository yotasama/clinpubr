# `cut_by()`

> **Convert Numeric to Factor**

## Description

Divide numeric data into different groups. Easier to use than `base::cut()`.

## Usage

```r
cut_by(
  x,
  breaks,
  breaks_as_quantiles = FALSE,
  group = NULL,
  labels = NULL,
  label_type = "ori",
  right = FALSE,
  drop_empty = TRUE,
  verbose = FALSE,
  ...
)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `x` | A numeric vector. |
| `breaks` | A numeric vector of internal cut points. If `breaks_as_quantiles` is TRUE, this is a proportion of the data. See `Details`. |
| `breaks_as_quantiles` | If `TRUE`, `breaks` is interpreted as a proportion of the data. |
| `group` | A character vector of the same length as `x`, used to group the data before cut. Only effective when `breaks_as_quantiles` is `TRUE`. |
| `labels` | A vector of labels for the resulting factor levels. |
| `label_type` | If `labels` is `NULL`, this sets the label type. `"ori"` for original labels, `"LMH"` for "Low Medium High" style. `"combined"` labels that combine `"LMH"` type or provided `labels` with the original range labels. Only `"LMH"` is supported when `group` is specified. |
| `right` | logical, indicating if the intervals should be closed on the right (and open on the left) or vice versa. Note that the default is `FALSE`, which is different from `base::cut()`. |
| `drop_empty` | If `TRUE`, drop empty levels. |
| `verbose` | If `TRUE`, print the cut points. |
| `...` | Other arguments passed to `base::cut()`. |

## Value

A factor.

## Details

`cut_by()` is a wrapper for `base::cut()`. Compared with the argument `breaks` in `base::cut()`,
`breaks` here automatically sets the minimum and maximum. `breaks` outside the range of `x` are not allowed.

## Note

The argument `right` in `base::cut()` is always set to `FALSE`, which means the levels follow the
left closed right open convention.

## Examples

```r
set.seed(123)
cut_by(rnorm(100), c(0, 1, 2))
cut_by(rnorm(100), c(1 / 3, 2 / 3), breaks_as_quantiles = TRUE, label_type = "LMH")
```

