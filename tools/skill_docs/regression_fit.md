# `regression_fit()`

> **Obtain regression results**

## Description

This function fit the regression of a predictor in a
linear, logistic, or Cox proportional hazards model.

## Usage

```r
regression_fit(
  data,
  y,
  predictor,
  time = NULL,
  time2 = NULL,
  covars = NULL,
  cluster = NULL,
  rcs_knots = NULL,
  returned = c("full", "predictor_split", "predictor_combined")
)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `data` | A data frame. |
| `y` | A character string of the outcome variable. The variable should be binary or numeric and determines the type of model to be used. If the variable is binary, logistic or cox regression is used. If the variable is numeric, linear regression is used. |
| `predictor` | A character string of the predictor variable. |
| `time` | A character string of the time variable. If `NULL`, linear or logistic regression is used. Otherwise, Cox proportional hazards regression is used. |
| `time2` | A character string of the ending time of the interval for interval censored or counting process data only. |
| `covars` | A character vector of covariate names. |
| `cluster` | A character string of the cluster variable. If set, correct for heteroscedasticity and for correlated responses from cluster samples using `rms::robcov()`. |
| `rcs_knots` | The number of rcs knots. If `NULL`, a linear model would be fitted instead. |
| `returned` | The return mode of this function.   `"full"`: return the full regression result.  `"predictor_split"`: return the regression parameter of the predictor, could have multiple lines.  `"predictor_combined"`: return the regression parameter of the predictor, test the predictor as a whole and takes only one line. |

## Value

A list containing the regression ratio and p-value of the predictor. If `rcs_knots` is not `NULL`,
the list contains the overall p-value and the nonlinear p-value of the rcs model. If `return_full_result`
is `TRUE`, the complete result of the regression model is returned.

## Examples

```r
data(cancer, package = "survival")
regression_fit(data = cancer, y = "status", predictor = "age", time = "time", rcs_knots = 4)
```

