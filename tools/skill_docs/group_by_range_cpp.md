# `group_by_range_cpp()`

> **Group Sorted Vector by Range**

## Description

Divide a sorted numeric vector into groups such that
`max(x[group]) - min(x[group]) <= max_range`.

## Usage

```r
group_by_range_cpp(x, max_range)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `x` | A sorted numeric vector, Date vector, or POSIXt vector. |
| `max_range` | A non-negative numeric threshold for the within-group range. |

## Value

An IntegerVector of group IDs (1-indexed).

