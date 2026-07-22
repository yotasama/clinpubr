# `time_roc_plot()`

> **Calculate and plot time-dependent ROC curves**

## Description

Calculate time-dependent ROC curves using the `timeROC` package and plot them using `ggplot2`.

## Usage

```r
time_roc_plot(
  data,
  time_var,
  event_var,
  marker_var,
  times = c(12, 36, 60),
  time_unit = "months",
  weighting = "marginal",
  cause = 1,
  colors = NULL,
  title = FALSE,
  save_plot = FALSE,
  filename = "time_roc.png"
)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `data` | A data frame containing the survival time, event indicator, and marker variable. |
| `time_var` | A string specifying the name of the survival time variable in the data frame. |
| `event_var` | A string specifying the name of the event indicator variable in the data frame. |
| `marker_var` | A string specifying the name of the marker variable in the data frame. |
| `times` | A numeric vector of times at which to compute the time-dependent ROC curves. |
| `time_unit` | A character string specifying the unit of the time variable, recommended to be in plural form. Default is "months". |
| `weighting` | A character string specifying the weighting method. Default is "marginal". See `timeROC::timeROC()` for details. |
| `cause` | The value of the event indicator that denotes the event of interest. Default is `1`. |
| `colors` | A vector of colors to use for the ROC curves. If NULL, uses default colors. |
| `title` | A logical value indicating whether to include a title. Default is `FALSE`. |
| `save_plot` | A logical value indicating whether to save the plot to a file. Default is `FALSE`. |
| `filename` | A string specifying the filename to save the plot. Default is "time_roc.png". |

## Value

A list containing:

 time_roc: The timeROC result object.
 plot: A ggplot object of the time-dependent ROC curves.

## Examples

```r
# Plot time-dependent ROC curves using lung dataset from survival package
library(survival)
data(cancer, package = "survival")
# Use age as the marker variable, plot at 180, 365, and 730 days
lung$status <- lung$status == 2
result <- time_roc_plot(lung, "time", "status", "age", times = c(180, 365, 730), time_unit = "days")
result$plot

# Save the plot to a file
# time_roc_plot(lung, "time", "status", "age", times = c(180, 365, 730), 
#               time_unit = "days", save_plot = TRUE)
```

