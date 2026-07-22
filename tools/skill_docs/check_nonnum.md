# `check_nonnum()`

> **Check elements that are not numeric**

## Description

Finds the elements that cannot be converted to numeric in a character vector.
Useful when setting the strategy to clean numeric values.

## Usage

```r
check_nonnum(
  x,
  return_idx = FALSE,
  show_unique = TRUE,
  max_count = NULL,
  random_sample = FALSE,
  fix_len = FALSE
)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `x` | A string vector that stores numerical values. |
| `return_idx` | A logical value. If `TRUE`, return the index of the elements that are not numeric. |
| `show_unique` | A logical value. If `TRUE`, return the unique elements that are not numeric. Omitted if `return_idx` is `TRUE`. |
| `max_count` | An integer. The maximum number of elements to show. If `NULL` or `0`, show all elements. Omitted if `return_idx` is `TRUE`. |
| `random_sample` | A logical value. If `TRUE`, randomly sample the elements to show. Only works if `max_count` is not `NULL` or `0`. |
| `fix_len` | A logical value. If `TRUE`, fill the vector with `NA` to fix the length to `max_count`. |

## Value

The (unique) elements that cannot be converted to numeric,
and their indexes if `return_idx` is `TRUE`.

## Details

The function uses the `as.numeric()` function to try to convert the elements to numeric.
If the conversion fails, the element is considered non-numeric.

## Examples

```r
check_nonnum(c("\uFF11\uFF12\uFF13", "11..23", "3.14", "2.131", "35.2."))
```

