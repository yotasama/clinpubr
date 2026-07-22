# `regression_forest()`

> **Forest plot of regression results**

## Description

Generate the forest plot of logistic or Cox regression with different models.

## Usage

```r
regression_forest(
  data,
  model_vars,
  y,
  time = NULL,
  time2 = NULL,
  cluster = NULL,
  as_univariate = FALSE,
  est_nsmall = 2,
  p_nsmall = 3,
  show_vars = NULL,
  save_plot = FALSE,
  filename = NULL,
  ...
)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `data` | A data frame. |
| `model_vars` | A character vector or a named list of predictor variables for different models. |
| `y` | A character string of the outcome variable. |
| `time` | A character string of the time variable. If `NULL`, logistic regression is used. Otherwise, Cox proportional hazards regression is used. |
| `time2` | A character string of the ending time of the interval for interval censored or counting process data only. |
| `cluster` | A character string of the cluster variable. If set, correct for heteroscedasticity and for correlated responses from cluster samples using `rms::robcov()`. |
| `as_univariate` | A logical value indicating whether to treat the model_vars as univariate. |
| `est_nsmall` | An integer specifying the precision for the estimates in the plot. |
| `p_nsmall` | An integer specifying the number of decimal places for the p-values. |
| `show_vars` | A character vector of variable names to be shown in the plot. If `NULL`, all variables are shown. |
| `save_plot` | A logical value indicating whether to save the plot. |
| `filename` | A character string specifying the filename for the plot. If `NULL`, a default filename is used. |
| `...` | Additional arguments passed to the `forestploter::forest` function. |

## Value

A `gtable` object.

## Examples

```r
data(cancer, package = "survival")
cancer$ph.ecog_cat <- factor(cancer$ph.ecog, levels = c(0:3), labels = c("0", "1", ">=2", ">=2"))
regression_forest(cancer,
  model_vars = c("age", "sex", "wt.loss", "ph.ecog_cat", "meal.cal"), y = "status", time = "time",
  as_univariate = TRUE, save_plot = FALSE
)

regression_forest(cancer,
  model_vars = c("age", "sex", "wt.loss", "ph.ecog_cat", "meal.cal"), y = "status", time = "time",
  show_vars = c("age", "sex", "ph.ecog_cat", "meal.cal"), save_plot = FALSE
)

regression_forest(cancer,
  model_vars = list(
    M0 = c("age"),
    M1 = c("age", "sex", "wt.loss", "ph.ecog_cat", "meal.cal"),
    M2 = c("age", "sex", "wt.loss", "ph.ecog_cat", "meal.cal", "pat.karno")
  ),
  y = "status", time = "time",
  show_vars = c("age", "sex", "ph.ecog_cat", "meal.cal"), save_plot = FALSE
)
```

