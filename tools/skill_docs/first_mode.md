# `first_mode()`

> **Calculate the first mode**

## Description

Calculate the first mode of a vector. Ignore NA values.
Can be used if any mode is acceptable.

## Usage

```r
first_mode(x, empty_return)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `x` | A vector. |
| `empty_return` | The value to return if the vector is empty. |

## Value

The first mode of the vector.

## Examples

```r
first_mode(c(1, 1, 2, 2, 3, 3, 3, NA, NA, NA))
```

