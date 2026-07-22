# `interaction_p_value()`

> **Calculate interaction p-value**

## Description

This function calculates the interaction p-value between a predictor and a group variable in a
linear, logistic, or Cox proportional hazards model.

## Usage

```r
interaction_p_value(
  data,
  y,
  predictor,
  group_var,
  time = NULL,
  time2 = NULL,
  covars = NULL,
  cluster = NULL,
  rcs_knots = NULL
)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `data` | A data frame. |
| `y` | A character string of the outcome variable. The variable should be binary or numeric and determines the type of model to be used. If the variable is binary, logistic or Cox regression is used. If the variable is numeric, linear regression is used. |
| `predictor` | A character string of the predictor variable. |
| `group_var` | A character string of the group variable. The variable should be categorical. If a numeric variable is provided, it will be split by the median value. |
| `time` | A character string of the time variable. If `NULL`, linear or logistic regression is used. Otherwise, Cox proportional hazards regression is used. |
| `time2` | A character string of the ending time of the interval for interval censored or counting process data only. |
| `covars` | A character vector of covariate names. |
| `cluster` | A character string of the cluster variable. If set, correct for heteroscedasticity and for correlated responses from cluster samples using `rms::robcov()`. |
| `rcs_knots` | The number of rcs knots. If `NULL`, a linear model would be fitted instead. |

## Value

A numerical, the interaction p-value

## Examples

```r
data(cancer, package = "survival")
interaction_p_value(
  data = cancer, y = "status", predictor = "age", group_var = "sex",
  time = "time", rcs_knots = 4
)
```

