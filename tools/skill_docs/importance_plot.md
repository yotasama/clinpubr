# `importance_plot()`

> **Importance plot**

## Description

Creates an importance plot from a named vector of values.

## Usage

```r
importance_plot(
  x,
  x_lab = "Importance",
  top_n = NULL,
  color = c("#56B1F7", "#132B43"),
  show_legend = FALSE,
  split_at = NULL,
  show_labels = TRUE,
  digits = 2,
  nsmall = 3,
  scientific = TRUE,
  label_color = "black",
  label_size = 3,
  label_hjust = max(x)/10,
  save_plot = FALSE,
  filename = "importance.png"
)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `x` | A named vector of values, typically importance scores from models. |
| `x_lab` | A character string for the x-axis label. |
| `top_n` | The number of top values to show. If NULL, all values are shown. |
| `color` | A length-2 vector of low and high colors, or a single color for the bars. |
| `show_legend` | A logical value indicating whether to show the legend. |
| `split_at` | The index at which to split the plot into two halves, usually used to illustrate variable selection. If NULL, no split is made. |
| `show_labels` | A logical value indicating whether to show the value labels on the bars. |
| `digits, nsmall, scientific` | Controls the formatting of labels. Passed to `format()`. |
| `label_color` | The color of the labels. |
| `label_size` | The size of the labels. |
| `label_hjust` | The horizontal justification of the labels. |
| `save_plot` | A logical value indicating whether to save the plot. |
| `filename` | The filename to save the plot as. |

## Value

A `ggplot` object

## Details

The importance plot is a bar plot that shows the importance of each variable in a model.
The variables are sorted in descending order of importance, and the top_n variables are shown.
If top_n is NULL, all variables are shown. The plot can be split into two halves at a specified
index, which is useful for illustrating variable selection.

## Examples

```r
set.seed(1)
dummy_importance <- runif(20)^5
names(dummy_importance) <- paste0("var", 1:20)
importance_plot(dummy_importance, top_n = 15, split_at = 10, save_plot = FALSE)
```

