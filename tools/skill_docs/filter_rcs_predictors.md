# `filter_rcs_predictors()`

> **Filter predictors for RCS**

## Description

Filter predictors that can be used to fit for RCS models.

## Usage

```r
filter_rcs_predictors(data, predictors = NULL)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `data` | A data frame. |
| `predictors` | A vector of predictor names to be filtered. |

## Value

A vector of predictor names. These variables are numeric and have more than 5 unique values.

## Examples

```r
filter_rcs_predictors(mtcars)
```

