# `replace_elements()`

> **Replacing elements in a vector**

## Description

Replacing elements in a vector

## Usage

```r
replace_elements(x, from, to)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `x` | A vector. |
| `from` | A vector of elements to be replaced. |
| `to` | A vector of elements to replace the original ones. |

## Value

A vector.

## Examples

```r
replace_elements(c("a", "x", "1", NA, "a"), c("a", "b", NA), c("A", "B", "XX"))
```

