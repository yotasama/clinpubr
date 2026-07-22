# `regression_basic_results()`

> **Basic results of logistic or Cox regression.**

## Description

Generate the result table of logistic or Cox regression with different settings of the predictor
variable and covariates. Also generate KM curves for Cox regression.

## Usage

```r
regression_basic_results(
  data,
  x,
  y,
  time = NULL,
  time2 = NULL,
  model_covs = NULL,
  cluster = NULL,
  pers = c(0.1, 10, 100),
  factor_breaks = NULL,
  factor_labels = NULL,
  quantile_breaks = NULL,
  quantile_labels = NULL,
  label_with_range = FALSE,
  save_output = FALSE,
  figure_type = "png",
  ref_levels = "lowest",
  est_nsmall = 2,
  p_nsmall = 3,
  pval_eps = 0.001,
  median_nsmall = 0,
  colors = NULL,
  xlab = NULL,
  legend_title = x,
  legend_pos = c(0.8, 0.8),
  pval_pos = NULL,
  n_y_pos = 0.9,
  height = 6,
  width = 6,
  ...
)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `data` | A data frame. |
| `x` | A character string of the predictor variable. |
| `y` | A character string of the outcome variable. |
| `time` | A character string of the time variable. If `NULL`, logistic regression is used. Otherwise, Cox proportional hazards regression is used. |
| `time2` | A character string of the ending time of the interval for interval censored or counting process data only. |
| `model_covs` | A character vector or a named list of covariates for different models. If `NULL`, only the crude model is used. |
| `cluster` | A character string of the cluster variable. If set, correct for heteroscedasticity and for correlated responses from cluster samples using `rms::robcov()`. |
| `pers` | A numeric vector of the denominators of variable `x`. Set this denominator to obtain a reasonable OR or HR. |
| `factor_breaks` | A numeric vector of the breaks to factorize the `x` variable. |
| `factor_labels` | A character vector of the labels for the factor levels. |
| `quantile_breaks` | A numeric vector of the quantile breaks to factorize the `x` variable. |
| `quantile_labels` | A character vector of the labels for the quantile levels. |
| `label_with_range` | A logical value indicating whether to add the range of the levels to the labels. |
| `save_output` | A logical value indicating whether to save the results. |
| `figure_type` | A character string of the figure type. Can be `"png"`, `"pdf"`, and other types that `ggplot2::ggsave()` support. |
| `ref_levels` | A vector of strings of the reference levels of the factor variable. You can use `"lowest"` or `"highest"` to select the lowest or highest level as the reference level. Otherwise, any level that matches the provided strings will be used as the reference level. |
| `est_nsmall` | An integer specifying the precision for the estimates in the plot. |
| `p_nsmall` | An integer specifying the number of decimal places for the p-values. |
| `pval_eps` | The threshold for rounding p values to 0. |
| `median_nsmall` | The minimum number of digits to the right of the decimal point for the median survival time. |
| `colors` | A vector of colors for the KM curves. |
| `xlab` | A character string of the x-axis label of the survival plot. |
| `legend_title` | A character string of the title of the legend. |
| `legend_pos` | A numeric vector of the position of the legend. |
| `pval_pos` | A numeric vector of the position of the p-value. |
| `n_y_pos` | A numerical of range 0 to 1 to assign the y position of total sample count. `NULL` to hide. |
| `height` | The height of the plot. |
| `width` | The width of the plot. |
| `...` | Additional arguments passed to the `survminer::ggsurvplot` function for KM curve. |

## Value

A list of results, including the regression table and the KM curve plots.

## Details

The function `regression_basic_results` generates the result table of logistic or Cox regression with
different settings of the predictor variable and covariates. The setting of the predictor variable includes
the original `x`, the standardized `x`, the log of `x`, and `x` divided by denominators in `pers` as continuous
variables, and the factorization of the variable including split by median, by quartiles, and by `factor_breaks`
and `quantile_breaks`. The setting of the covariates includes different models with different covariates.

## Note

For factor variables with more than 2 levels, p value for trend is also calculated.

## Examples

```r
data(cancer, package = "survival")
# coxph model with time assigned
res1 <- regression_basic_results(cancer,
  x = "age", y = "status", time = "time",
  model_covs = list(Crude = c(), Model1 = c("ph.karno"), Model2 = c("ph.karno", "sex")),
  save_output = FALSE,
  ggtheme = survminer::theme_survminer(font.legend = c(14, "plain", "black")) # theme for KM
)

# logistic model with time not assigned
cancer$dead <- cancer$status == 2
res2 <- regression_basic_results(cancer,
  x = "age", y = "dead", ref_levels = c("Q3", "High"),
  model_covs = list(Crude = c(), Model1 = c("ph.karno"), Model2 = c("ph.karno", "sex")),
  save_output = FALSE
)
```

