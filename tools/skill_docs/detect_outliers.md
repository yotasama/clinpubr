# `detect_outliers()`

> **Detect outliers in a numeric vector.**

## Description

Detect outliers in a numeric vector using various methods.

## Usage

```r
detect_outliers(x, method = "iqr", threshold = NULL)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `x` | A numeric vector. |
| `method` | The method to use for outlier detection. One of "mad", "iqr", or "zscore". |
| `threshold` | The threshold value for detecting outliers. Defaults depend on the method. |

## Value

A list containing:

 outlier_mask: Logical vector indicating outliers, `NA` for missing values
 outlier_count: Number of outliers detected
 outlier_pct: Percentage of outliers in the data
 summary: Summary statistics including:

 Before removing outliers: max, min, variance
 After removing outliers: max, min, variance
 Method-specific details

## Details

This function provides a unified interface for detecting outliers using different methods.

 "mad": Median absolute deviation method
 "iqr": Interquartile range method
 "zscore": Z-score method

## See Also

`mad_outlier`, `iqr_outlier`, `zscore_outlier`

## Examples

```r
x <- c(1, 2, 3, 4, 5, 100)
detect_outliers(x, method = "iqr")
```

