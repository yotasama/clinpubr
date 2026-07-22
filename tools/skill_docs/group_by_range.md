# `group_by_range()`

> **Group Sorted Vector by Range**

## Description

Divide a numeric vector into groups such that the range
(`max - min`) within each group does not exceed a given threshold.
This is useful for clustering continuous values into contiguous bins
controlled by a maximum span.

## Usage

```r
group_by_range(x, max_range)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `x` | A sorted numeric vector, Date vector, or POSIXt vector. |
| `max_range` | A non-negative numeric threshold for the within-group range. |

## Value

An integer vector of group IDs (1-indexed).

## Examples

```r
group_by_range(c(1, 2, 5, 7, 10, 11, 11, 11), 3)
```

