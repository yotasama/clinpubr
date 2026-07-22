# `str_match_replace()`

> **Match string and replace with corresponding value**

## Description

Partially match a string and replace with corresponding value. This function is useful to recover
the original names of variables after legalized using `make.names` or modified by other functions.

## Usage

```r
str_match_replace(x, to_match, to_replace)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `x` | A vector. |
| `to_match` | A vector of strings to be matched. |
| `to_replace` | A vector of strings to replace the matched ones, must have the same length as `to_match`. |

## Value

A vector.

## Examples

```r
ori_names <- c("xx (mg/dl)", "b*x", "Covid-19")
modified_names <- c("v1", "v2", "v3")
x <- c("v1.v2", "v3.yy", "v4")
str_match_replace(x, modified_names, ori_names)
```

