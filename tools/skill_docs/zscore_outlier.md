# `zscore_outlier()`

> 这是 [`mad_outlier()`](mad_outlier.md) 的别名，完整文档请查看主函数。

# `mad_outlier()`

> **Mark possible outliers using different methods.**

**别名**: `iqr_outlier()`, `zscore_outlier()`

## Description

Mark possible outliers in a numeric vector using various methods.
These functions return a logical vector indicating which values are outliers.

## Usage

```r
mad_outlier(x, threshold = 1.4826 * 3)

iqr_outlier(x, threshold = 1.5)

zscore_outlier(x, threshold = 3)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `x` | A numeric vector. |
| `threshold` | The threshold value for detecting outliers. Defaults depend on the method:   For MAD method: 1.4826 * 3 (approximately 3 standard deviations)  For IQR method: 1.5 (Tukey's rule)  For Z-score method: 3 (3 standard deviations) |

## Value

A logical vector indicating which values are outliers.

## Details

MAD method: Uses median absolute deviation to identify outliers.
Values with absolute deviation from the median greater than the threshold are considered outliers.
 IQR method: Uses interquartile range to identify outliers.
Values below Q1 - threshold * IQR or above Q3 + threshold * IQR are considered outliers.
 Z-score method: Uses standardized Z-scores to identify outliers.
Values with an absolute Z-score greater than the threshold are considered outliers.

## Examples

```r
x <- c(1, 2, 3, 4, 5, 100, NA)
mad_outlier(x)
iqr_outlier(x, threshold = 2.0)
zscore_outlier(x, threshold = 2.5)
```

