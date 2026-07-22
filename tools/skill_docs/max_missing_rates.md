# `max_missing_rates()`

> **Get the maximum missing rate of rows and columns.**

## Description

Get the maximum missing rate of rows and columns.

## Usage

```r
max_missing_rates(df)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `df` | A data frame. |

## Value

A list that contains the maximum missing rate of rows and columns.

## Examples

```r
data(cancer, package = "survival")
max_missing_rates(cancer)
```

