# `data_overview()`

> **Data Overview and Quality Check**

## Description

This function provides a comprehensive overview of a data.frame, including variable types,
summary statistics, and potential data quality issues. It serves as a starting point for
data cleaning by identifying problems that need attention.

## Usage

```r
data_overview(
  df,
  outlier_method = "iqr",
  outlier_threshold = NULL,
  verbose = TRUE,
  sample = 10000
)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `df` | A data.frame to be analyzed |
| `outlier_method` | Method for detecting outliers, one of "iqr" (default), "zscore", or "mad" |
| `outlier_threshold` | Threshold value for detecting outliers. If NULL (default), uses method-specific defaults:   For MAD method: 1.4826 * 3 (approximately 3 standard deviations)  For IQR method: 1.5 (Tukey's rule)  For Z-score method: 3 (3 standard deviations) |
| `verbose` | If TRUE (default), prints result messages |
| `sample` | Maximum number of rows to sample for large datasets (default is 10000). Set to `NULL` `NA`, or `0` to disable sampling. |

## Value

A list containing:

 variable_types: Classification of variables by type
 summary_stats: Summary statistics for each variable
 quality_issues: Identified data quality problems
 recommendations: Suggestions for data cleaning

## Examples

```r
# Basic usage
data(mtcars)
overview <- data_overview(mtcars)
print(overview$variable_types)
print(overview$quality_issues)
```

