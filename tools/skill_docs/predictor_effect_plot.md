# `predictor_effect_plot()`

> **Plot the effect of a predictor variable**

## Description

This is a versatile function to plot the relationship between a predictor variable and the outcome.
It supports numeric (linear or RCS) and categorical predictors for logistic, linear, and Cox models.
It can display the distribution of the predictor variable as a histogram (for numeric) or bar plot (for categorical).

## Usage

```r
predictor_effect_plot(
  data,
  x,
  y,
  time = NULL,
  time2 = NULL,
  covars = NULL,
  cluster = NULL,
  method = "auto",
  knot = 4,
  add_hist = TRUE,
  ref = "x_median",
  ref_digits = 3,
  show_total_n = TRUE,
  group_by_ref = TRUE,
  group_title = NULL,
  group_labels = NULL,
  group_colors = NULL,
  breaks = 20,
  line_color = "#e23e57",
  print_p_ph = TRUE,
  trans = "identity",
  save_plot = FALSE,
  create_dir = FALSE,
  filename = NULL,
  y_lim = NULL,
  hist_max = NULL,
  xlim = NULL,
  height = 6,
  width = 6,
  return_details = FALSE
)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `data` | A data frame. |
| `x` | A character string of the predictor variable. |
| `y` | A character string of the outcome variable. |
| `time` | A character string of the time variable for Cox models. If `NULL`, logistic or linear regression is used. |
| `time2` | A character string of the ending time for interval-censored or counting process data. |
| `covars` | A character vector of covariate names. |
| `cluster` | A character string of the cluster variable for robust variance estimation. |
| `method` | A character string specifying the method for handling the predictor `x`. Can be `"auto"`, `"rcs"`, `"linear"`, or `"categorical"`. If `"auto"`, the function decides based on the type of `x`. |
| `knot` | The number of knots for RCS. If `NULL`, AIC is used to find the optimal number. |
| `add_hist` | A logical value. If `TRUE`, add a distribution plot (histogram or bar plot). |
| `ref` | The reference value for numeric predictors, or the reference level for categorical predictors. For numeric `x`, can be `"x_median"`, `"x_mean"`, `"ratio_min"`, or a numeric value. |
| `ref_digits` | The number of digits for the reference value label. |
| `show_total_n` | A logical value. If `TRUE`, show the total number of samples. |
| `group_by_ref` | A logical value. If `TRUE` and `x` is numeric, split the histogram at the reference value. |
| `group_title` | A character string for the group legend title. |
| `group_labels` | A character vector for group labels. |
| `group_colors` | A character vector of colors for the distribution plot. If `NULL`, the default colors are used. If `group_by_ref` is `FALSE`, the first color is used as fill color. |
| `breaks` | The number of breaks for the histogram. |
| `line_color` | The color for the effect line/points. |
| `print_p_ph` | A logical value. If `TRUE` (and model is Cox), print the p-value for the proportional hazards test. |
| `trans` | The transformation for the y-axis. Passed to `ggplot2::scale_y_continuous(transform = trans)`. |
| `save_plot` | A logical value indicating whether to save the plot. |
| `create_dir` | A logical value for creating the save directory. |
| `filename` | A character string for the saved plot filename. |
| `y_lim` | The y-axis limits. |
| `hist_max` | The maximum value for the histogram y-axis. |
| `xlim` | The x-axis limits for numeric predictors. If `NULL`, the limits are the `0.025` and `0.975` quantiles. The actual plot range might be slightly larger than this range to fit the histogram. |
| `height` | The height of the saved plot. |
| `width` | The width of the saved plot. |
| `return_details` | A logical value indicating whether to return plot details. |

## Value

A `ggplot` object, or a list with the plot and details if `return_details` is `TRUE`.

## Examples

```r
data(cancer, package = "survival")
cancer$dead <- cancer$status == 2
cancer <- cancer[!is.na(cancer$inst), ]
predictor_effect_plot(
  data = cancer,
  x = "age",
  y = "dead",
  method = "linear",
  covars = "ph.karno",
  add_hist = FALSE,
  trans = "log2",
  save_plot = FALSE,
  cluster = "inst"
)
```

