# `regression_scan()`

> **Scan for significant regression predictors**

## Description

Scan for significant regression predictors and output results. Both logistic and Cox
proportional hazards regression models are supported. The predictor variables in the model are can be
used both in linear form or in restricted cubic spline form.

## Usage

```r
regression_scan(
  data,
  y,
  time = NULL,
  time2 = NULL,
  predictors = NULL,
  covars = NULL,
  cluster = NULL,
  num_to_factor = 5,
  p_adjust_method = "BH",
  save_table = FALSE,
  filename = NULL
)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `data` | A data frame. |
| `y` | A character string of the outcome variable. |
| `time` | A character string of the time variable. If `NULL`, logistic regression is used. Otherwise, Cox proportional hazards regression is used. |
| `time2` | A character string of the ending time of the interval for interval censored or counting process data only. |
| `predictors` | The predictor variables to be scanned for relationships. If `NULL`, all variables except `y` and `time` are taken as predictors. |
| `covars` | A character vector of covariate names. |
| `cluster` | A character string of the cluster variable. If set, correct for heteroscedasticity and for correlated responses from cluster samples using `rms::robcov()`. |
| `num_to_factor` | An integer. Numerical variables with number of unique values below or equal to this value would be considered a factor. |
| `p_adjust_method` | The method to use for p-value adjustment. Default is "BH". See `?p.adjust.methods`. Note that the p-value adjustment is only applied column wise, not applied among all available p-values in the table. |
| `save_table` | A logical value indicating whether to save the results as a table. |
| `filename` | The name of the file to save the results. File will be saved in `.csv` format. |

## Value

A data frame containing the results of the regression analysis.

## Details

The function first determines the type of each predictor variable (`numerical`, `factor`,
`num_factor` (numerical but with less unique values than or equal to `num_to_factor`), or
`other`). Then, it performs regression analysis for available transforms of each predictor variable
and saves the results.

## Examples

```r
data(cancer, package = "survival")
regression_scan(cancer, y = "status", time = "time", save_table = FALSE)
```

