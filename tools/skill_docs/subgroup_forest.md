# `subgroup_forest()`

> **Create subgroup forest plot.**

## Description

Create subgroup forest plot with `glm` or `coxph` models. The interaction p-values
are calculated using likelihood ratio tests.

## Usage

```r
subgroup_forest(
  data,
  subgroup_vars,
  x,
  y,
  time = NULL,
  time2 = NULL,
  standardize_x = FALSE,
  covars = NULL,
  cluster = NULL,
  est_nsmall = 2,
  p_nsmall = 3,
  group_cut_quantiles = 0.5,
  save_plot = FALSE,
  filename = NULL,
  ...
)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `data` | A data frame. |
| `subgroup_vars` | A character vector of variable names to be used as subgroups. It's recommended that the variables are categorical. If the variables are continuous, they will be cut into groups. |
| `x` | A character string of the predictor variable. |
| `y` | A character string of the outcome variable. |
| `time` | A character string of the time variable. If `NULL`, logistic regression is used. Otherwise, Cox proportional hazards regression is used. |
| `time2` | A character string of the ending time of the interval for interval censored or counting process data only. |
| `standardize_x` | A logical value. If `TRUE`, the predictor variable will be standardized. |
| `covars` | A character vector of covariate names. |
| `cluster` | A character string of the cluster variable. If set, correct for heteroscedasticity and for correlated responses from cluster samples using `rms::robcov()`. |
| `est_nsmall` | An integer specifying the precision for the estimates in the plot. |
| `p_nsmall` | An integer specifying the number of decimal places for the p-values. |
| `group_cut_quantiles` | A vector of numerical values between 0 and 1, specifying the quantile to use for cutting continuous subgroup variables. |
| `save_plot` | A logical value indicating whether to save the plot. |
| `filename` | A character string specifying the filename for the plot. If `NULL`, a default filename is used. |
| `...` | Additional arguments passed to the `forestploter::forest` function. |

## Value

A `gtable` object.

## Examples

```r
data(cancer, package = "survival")
# coxph model with time assigned
subgroup_forest(cancer,
  subgroup_vars = c("age", "sex", "wt.loss"), x = "ph.ecog", y = "status",
  time = "time", covars = "ph.karno", ticks_at = c(1, 2), save_plot = FALSE
)

# logistic model with time not assigned
cancer$dead <- cancer$status == 2
subgroup_forest(cancer,
  subgroup_vars = c("age", "sex", "wt.loss"), x = "ph.ecog", y = "dead",
  covars = "ph.karno", ticks_at = c(1, 2), save_plot = FALSE
)

cancer$ph.ecog_cat <- factor(cancer$ph.ecog, levels = c(0:3), labels = c("0", "1", ">=2", ">=2"))
subgroup_forest(cancer,
  subgroup_vars = c("sex", "wt.loss"), x = "ph.ecog_cat", y = "dead",
  covars = "ph.karno", ticks_at = c(1, 2), save_plot = FALSE
)
```

