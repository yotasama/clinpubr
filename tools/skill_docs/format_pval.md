# `format_pval()`

> **Format p-value for publication**

## Description

Format p-value with modified default settings suitable for publication.

## Usage

```r
format_pval(
  p,
  text_ahead = NULL,
  digits = 1,
  nsmall = 2,
  eps = 0.001,
  na_empty = TRUE
)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `p` | The numerical p values to be formatted. |
| `text_ahead` | A string to be added before the p value. If not `NULL`, this string will be connected to the formatted p value with `"="` or `"<"`. |
| `digits` | The number of digits to be used. Same as in `base::format.pval`. |
| `nsmall` | The number of digits after the decimal point. Same as in `base::format.pval`. |
| `eps` | The threshold for rounding p values to 0. Same as in `base::format.pval`. |
| `na_empty` | If `TRUE`, replace `"NA"` in result with an empty string. |

## Value

A string vector of formatted p values.

## Examples

```r
format_pval(c(0.001, 0.0001, 0.05, 0.1123456))
format_pval(c(0.001, 0.0001, 0.05, 0.1123456), text_ahead = "p value")
```

