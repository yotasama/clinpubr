# `interaction_plot()`

> **Plot interactions**

## Description

Plot interactions between variables. Both logistic and Cox proportional hazards regression models
are supported. The predictor variables in the model are can be used both in linear form or in restricted cubic
spline form.

## Usage

```r
interaction_plot(
  data,
  y,
  predictor,
  group_var,
  time = NULL,
  time2 = NULL,
  covars = NULL,
  cluster = NULL,
  group_colors = NULL,
  save_plot = FALSE,
  filename = NULL,
  height = 4,
  width = 4,
  xlab = predictor,
  ylab = NULL,
  show_n = TRUE,
  group_title = group_var,
  ...
)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `data` | A data frame. |
| `y` | A character string of the outcome variable. |
| `predictor` | A character string of the predictor variable. |
| `group_var` | A character string of the group variable. The variable should be categorical. If a numeric variable is provided, it will be split by the median value. |
| `time` | A character string of the time variable. If `NULL`, logistic regression is used. Otherwise, Cox proportional hazards regression is used. |
| `time2` | A character string of the ending time of the interval for interval censored or counting process data only. |
| `covars` | A character vector of covariate names. |
| `cluster` | A character string of the cluster variable. If set, correct for heteroscedasticity and for correlated responses from cluster samples using `rms::robcov()`. |
| `group_colors` | A character vector of colors for the plot. If `NULL`, the default colors are used. |
| `save_plot` | A logical value indicating whether to save the plot. |
| `filename` | The name of the file to save the plot. Support both `.png` and `.pdf` formats. |
| `height` | The height of the saved plot. |
| `width` | The width of the saved plot. |
| `xlab` | The label of the x-axis. |
| `ylab` | The label of the y-axis. |
| `show_n` | A logical value indicating whether to show the number of observations in the plot. |
| `group_title` | The title of the group variable. |
| `...` | Additional arguments passed to the `ggplot` function. |

## Value

A `ggplot` object.

## Examples

```r
data(cancer, package = "survival")
interaction_plot(cancer,
  y = "status", time = "time", predictor = "age", group_var = "sex",
  save_plot = FALSE
)
interaction_plot(cancer,
  y = "status", predictor = "age", group_var = "sex",
  save_plot = FALSE
)
interaction_plot(cancer,
  y = "wt.loss", predictor = "age", group_var = "sex",
  save_plot = FALSE
)
```

