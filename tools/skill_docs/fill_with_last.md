# `fill_with_last()`

> **Fill NA values with the last valid value**

## Description

Fill NA values with the last valid value. Can be used to fill excel combined cells.

## Usage

```r
fill_with_last(x)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `x` | A vector. |

## Value

A vector.

## Examples

```r
fill_with_last(c(1, 2, NA, 4, NA, 6))
```

