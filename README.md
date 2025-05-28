# clinpubr: Clinical Publication with R

## Overview
`clinpubr` is an R package designed to streamline the workflow from clinical data processing to publication-ready outputs. It provides tools for clinical data cleaning, significant result screening, and generating tables/figures suitable for medical journals.

## Key Features
- **Clinical Data Cleaning**: Functions to handle missing values, standardize units, convert dates, and clean numerical/categorical variables.
- **Significant Result Screening**: Tools for interaction analysis, model comparison, and regression result with common variable transformations to identify key findings.
- **Publication-Ready Outputs**: Generate baseline characteristic tables, forest plots, RCS curves, and other visualizations formatted for medical publications.

## Installation
Install the development version from GitHub:

```r
# Install devtools if needed
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

devtools::install_github("yotasama/clinpubr")
```

## Basic Usage
### Example 1: Clean Numerical Data
```r
library(clinpubr)

# Sample messy data
messy_data <- data.frame(values = c("１２．３", "0..45", "  67 ", "", "ａｂａｎｄｏｎ"))
clean_data <- num_simple_cleaning(messy_data$values)
print(clean_data)

# "12.3"    "0.45"     "67"      NA        "abandon"
```

### Example 2: Automatic Type Infer and Baseline Table Generation
```r
# Using example data `mtcars`
var_types <- get_var_types(mtcars, strata = "vs") # Automatically infer variable types
baseline_table(mtcars, var_types = var_types, contDigits = 1, filename = "baseline.csv")
```
The generated table: baseline.csv
|                    |Overall        |vs: 0          |vs: 1          |p      |test    |
|:-------------------|:--------------|:--------------|:--------------|:------|:-------|
|n                   |32             |18             |14             |       |        |
|mpg (mean (SD))     |20.1 (6.0)     |16.6 (3.9)     |24.6 (5.4)     |<0.001 |        |
|cyl (%)             |               |               |               |<0.001 |exact   |
|4                   |11 (34.4)      |1 (5.6)        |10 (71.4)      |       |        |
|6                   |7 (21.9)       |3 (16.7)       |4 (28.6)       |       |        |
|8                   |14 (43.8)      |14 (77.8)      |0 (0.0)        |       |        |
|disp (mean (SD))    |230.7 (123.9)  |307.1 (106.8)  |132.5 (56.9)   |<0.001 |        |
|hp (mean (SD))      |146.7 (68.6)   |189.7 (60.3)   |91.4 (24.4)    |<0.001 |        |
|drat (median [IQR]) |3.7 [3.1, 3.9] |3.2 [3.1, 3.7] |3.9 [3.7, 4.1] |0.013  |nonnorm |
|wt (mean (SD))      |3.2 (1.0)      |3.7 (0.9)      |2.6 (0.7)      |0.001  |        |
|qsec (mean (SD))    |17.8 (1.8)     |16.7 (1.1)     |19.3 (1.4)     |<0.001 |        |
|am = 1 (%)          |13 (40.6)      |6 (33.3)       |7 (50.0)       |0.556  |        |
|gear (%)            |               |               |               |0.001  |exact   |
|3                   |15 (46.9)      |12 (66.7)      |3 (21.4)       |       |        |
|4                   |12 (37.5)      |2 (11.1)       |10 (71.4)      |       |        |
|5                   |5 (15.6)       |4 (22.2)       |1 (7.1)        |       |        |
|carb (median [IQR]) |2.0 [2.0, 4.0] |4.0 [2.2, 4.0] |1.5 [1.0, 2.0] |<0.001 |nonnorm |


### Example 3: Publish-ready Figure Generation
#### Example 3.1: RCS Plot
```r
# Using example data `cancer` from the `survival` package
data(cancer, package = "survival")

# Performing cox regression, which is inferred by `y` and `time`
rcs_plot(cancer, x = "age", y = "status", time = "time", covars = c("sex", "ph.karno"), save_plot = TRUE)
```
The generated figure: cox_rcs_status_with_age_4_knots_with_2_covars.png
![rcs plot](sample_plots/cox_rcs_status_with_age_4_knots_with_2_covars.png)

#### Example 3.2: Regression Forest Plot
```r
data(cancer, package = "survival")
cancer$dead <- cancer$status == 2 # Preparing a binary variable for logistic regression

# Performing multivairate logistic regression
regression_forest(cancer, model_vars = c("age", "sex", "wt.loss"), y = "dead",
  as_univariate = FALSE, save_plot = TRUE
)

# Comparing multiple models
regression_forest(
  cancer,
  model_vars = list(
    Crude = c("age"),
    Model1 = c("age", "sex"),
    Model2 = c("age", "sex", "wt.loss")
  ),
  y = "dead",
  save_plot = TRUE
)
```
The generated figure: logistic_regression_forest_with_1_models.png
![regression_forest plot 1](sample_plots/logistic_regression_forest_with_1_models.png)

The generated figure: logistic_regression_forest_with_3_models.png
![regression_forest plot 2](sample_plots/logistic_regression_forest_with_3_models.png)


#### Example 3.3: Interaction Plot
```r
data(cancer, package = "survival")

# Generating interaction plot of both linear and RCS models
interaction_plot(cancer,
  y = "status", time = "time", predictor = "age",
  group_var = "sex", save_plot = TRUE
)
```
The generated figure: lin_cox_interaction_status_with_age_by_sex_with_0_covars.png
![interaction plot 1](sample_plots/lin_cox_interaction_status_with_age_by_sex_with_0_covars.png)

The generated figure: rcs_cox_interaction_status_with_age_by_sex_with_0_covars.png
![interaction plot 2](sample_plots/rcs_cox_interaction_status_with_age_by_sex_with_0_covars.png)


#### Example 3.4: Classification model comparison
```r
# Preparing sample data
data(cancer, package = "survival")
df <- kidney
df$dead <- ifelse(df$time <= 100 & df$status == 0, NA, df$time <= 100)
df <- na.omit(df[, -c(1:3)])

# Building classification models
model0 <- glm(dead ~ age + frail, family = binomial(), data = df)
df$base_pred <- predict(model0, type = "response")
model <- glm(dead ~ ., family = binomial(), data = df)
df$full_pred <- predict(model, type = "response")

# Generating comparison results
classif_model_compare(df, target_var ="dead", model_names =c("base_pred", "full_pred"))
```
The generated table: model_compare_table.csv
|Model     |AUC                  | Accuracy| Sensitivity| Specificity| Pos Pred Value| Neg Pred Value|    F1| Kappa| Brier| cutoff| Youden| HosLem|
|:---------|:--------------------|--------:|-----------:|-----------:|--------------:|--------------:|-----:|-----:|-----:|------:|------:|------:|
|base_pred |0.822 (0.711, 0.933) |    0.806|         0.8|       0.815|          0.848|          0.759| 0.824| 0.610| 0.171|   0.49|  0.615|  0.405|
|full_pred |0.931 (0.869, 0.994) |    0.855|         0.8|       0.926|          0.933|          0.781| 0.862| 0.711| 0.102|   0.63|  0.726|  0.577|

The generated figure: model_compare_roc.png
![model_compare_roc](sample_plots/model_compare_roc.png)

The generated figure: model_compare_calibration.png
![model_compare_calibration](sample_plots/model_compare_calibration.png)

The generated figure: model_compare_dca.png
![model_compare_dca](sample_plots/model_compare_dca.png)

#### Example 3.5: Variable Importance Plot
```r
# Generating dummy importance data
set.seed(5)
dummy_importance <- runif(20, 0.2, 0.6) ^ 5
names(dummy_importance) <- paste0("var", 1:20)

# Plotting variable importance, keeping only top 15 and splitting at 10
importance_plot(dummy_importance, top_n = 15, split_at = 10)
```
The generated figure: importance.png
![importance plot](sample_plots/importance.png)



## Documentation
For detailed usage, refer to the package vignettes (coming soon) or the [GitHub repository](https://github.com/yotasama/clinpubr).

## Contributing
Bug reports and feature requests are welcome via the [issue tracker](https://github.com/yotasama/clinpubr/issues).

## License
`clinpubr` is licensed under GPL (>= 3).