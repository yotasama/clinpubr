# `classif_model_compare()`

> **Performance comparison of classification models**

## Description

Compare the performance of classification models by commonly used
metrics, and generate commonly used plots including receiver operating characteristic
curve plot, decision curve analysis plot, and calibration plot.

## Usage

```r
classif_model_compare(
  data,
  target_var,
  model_names,
  colors = NULL,
  save_output = FALSE,
  figure_type = "png",
  output_prefix = "model_compare",
  as_probability = FALSE,
  auto_order = TRUE
)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `data` | A data frame containing the target variable and the predicted values. |
| `target_var` | A string specifying the name of the target variable in the data frame. |
| `model_names` | A vector of strings specifying the names of the models to compare. |
| `colors` | A vector of colors to use for the plots. The last 2 colors are used for the "Treat all" and "Treat none" lines in the DCA plot. |
| `save_output` | A logical value indicating whether to output the results to files. |
| `figure_type` | A character string of the figure type. Can be `"png"`, `"pdf"`, and other types that `ggplot2::ggsave()` support. |
| `output_prefix` | A string specifying the prefix for the output files. |
| `as_probability` | A logical or a vector of variable names. The logical value indicates whether to convert variables not in range 0 to 1 into this range. The vector of variable names means to convert these variables to the range of 0 to 1. |
| `auto_order` | A logical value indicating whether to automatically order the models by their AUCs. If `TRUE`, the models will be ordered by their AUCs in descending order. If `FALSE`, the order in `model_names` will be retained. |

## Value

A list of various results. If the output files are not in desired format,
these results can be modified for further use.

 metric_table: A data frame containing the performance metrics for each model.
 roc_plot: A `ggplot` object of Receiver Operating Characteristic curves.
 pr_plot: A `ggplot` object of Precision-Recall curves.
 dca_plot: A `ggplot` object of decision curve analysis plots.
 calibration_plot: A `ggplot` object of calibration plots.

## Examples

```r
data(cancer, package = "survival")
df <- kidney
df$dead <- ifelse(df$time <= 100 & df$status == 0, NA, df$time <= 100)
df <- na.omit(df[, -c(1:3)])

model0 <- glm(dead ~ age + frail, family = binomial(), data = df)
model <- glm(dead ~ ., family = binomial(), data = df)
df$base_pred <- predict(model0, type = "response")
df$full_pred <- predict(model, type = "response")

classif_model_compare(df, "dead", c("base_pred", "full_pred"), save_output = FALSE)
```

