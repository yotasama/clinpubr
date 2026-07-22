# `interaction_scan()`

> **Scan for interactions between variables**

## Description

Scan for interactions between variables and output results. Both logistic and Cox
proportional hazards regression models are supported. The predictor variables in the model are can be
used both in linear form or in restricted cubic spline form.

## Usage

```r
interaction_scan(
  data,
  y,
  time = NULL,
  time2 = NULL,
  predictors = NULL,
  group_vars = NULL,
  covars = NULL,
  cluster = NULL,
  try_rcs = TRUE,
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
| `predictors` | The predictor variables to be scanned for interactions. If `NULL`, all variables except `y` and `time` are taken as predictors. |
| `group_vars` | The group variables to be scanned for interactions. If `NULL`, all variables except `y` and `time` are taken as group variables. The group variables should be categorical. If a numeric variable is included, it will be split by the median value. |
| `covars` | A character vector of covariate names. |
| `cluster` | A character string of the cluster variable. If set, correct for heteroscedasticity and for correlated responses from cluster samples using `rms::robcov()`. |
| `try_rcs` | A logical value indicating whether to perform restricted cubic spline interaction analysis. |
| `p_adjust_method` | The method to use for p-value adjustment for pairwise comparison. Default is "BH". See `?p.adjust.methods`. |
| `save_table` | A logical value indicating whether to save the results as a table. |
| `filename` | The name of the file to save the results. File will be saved in `.csv` format. |

## Value

A data frame containing the results of the interaction analysis.

## Examples

```r
data(cancer, package = "survival")
interaction_scan(cancer, y = "status", time = "time", save_table = FALSE)
```

