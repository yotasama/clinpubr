
<!-- README.md is generated from README.Rmd. Please edit that file -->

# clinpubr: Clinical Publication

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/clinpubr)](https://CRAN.R-project.org/package=clinpubr)
[![Codecov test
coverage](https://codecov.io/gh/yotasama/clinpubr/graph/badge.svg)](https://app.codecov.io/gh/yotasama/clinpubr)
[![R-CMD-check](https://github.com/yotasama/clinpubr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/yotasama/clinpubr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Overview

`clinpubr` is an R package designed to streamline the workflow from
clinical data processing to publication-ready outputs. It provides tools
for clinical data cleaning, significant result screening, and generating
tables/figures suitable for medical journals.

## Key Features

- **Clinical Data Cleaning**: Functions to handle missing values,
  standardize units, convert dates, and clean numerical/categorical
  variables.  
- **Result Screening**: Screening results of regression and interaction
  analysis with common variable transformations to identify key
  findings.  
- **Publication-Ready Outputs**: Generate baseline characteristic
  tables, forest plots, RCS curves, and other visualizations formatted
  for medical publications.

## Installation

You can install `clinpubr` from CRAN with:

``` r
install.packages("clinpubr")
```

## Basic Usage

### Cleaning Tools

#### Example 1.1: Standardize Values in Medical Records

``` r
library(clinpubr)

# Sample messy data
messy_data <- data.frame(values = c("１２．３", "0..45", "  67 ", "", "ａｂａｎｄｏｎ"))
clean_data <- value_initial_cleaning(messy_data$values)
print(clean_data)
#> [1] "12.3"    "0.45"    "67"      NA        "abandon"
```

#### Example 1.2: Check Non-numerical Values

``` r
# Sample messy data
x <- c("1.2(XXX)", "1.5", "0.82", "5-8POS", "NS", "FULL")
print(check_nonnum(x))
#> [1] "1.2(XXX)" "5-8POS"   "NS"       "FULL"
```

This function filters out non-numerical values, which helps you choose
the appropriate method to handle them.

#### Example 1.3: Extracting Numerical Values from Text

``` r
# Sample messy data
x <- c("1.2(XXX)", "1.5", "0.82", "5-8POS", "NS", "FULL")
print(extract_num(x))
#> [1] 1.20 1.50 0.82 5.00   NA   NA

print(extract_num(x,
  res_type = "first", # Extract the first number
  multimatch2na = TRUE, # Convert illegal multiple matches to NA
  zero_regexp = "NEG|NS", # Convert "NEG" and "NS" (matched using regex) to 0
  max_regexp = "FULL", # Convert "FULL" (matched using regex) to some specified quantile
  max_quantile = 0.95
))
#> [1] 1.20 1.50 0.82   NA 0.00 1.47
```

#### Other Cleaning Functions

- *`to_date()`*: Convert text to date, can handle mixed format.
- *`unit_view()`* and *`unit_standardize()`*: Provide a pipeline to
  standardize conflicting units.  
- *`cut_by()`*: Split numerics into factors, offers a variety of
  splitting options and auto labeling.  
- And more…

### Screening Results to Identify Potential Findings

``` r
data(cancer, package = "survival")

# Screening for potential findings with regression models in the cancer dataset
scan_result <- regression_scan(cancer, y = "status", time = "time", save_table = FALSE)
#> Taking all variables as predictors
knitr::kable(scan_result)
```

|  | predictor | nvalid | original.HR | original.pval | original.padj | logarithm.HR | logarithm.pval | logarithm.padj | categorized.HR | categorized.pval | categorized.padj | rcs.overall.pval | rcs.overall.padj | rcs.nonlinear.pval | rcs.nonlinear.padj | best.var.trans |
|:---|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| 4 | ph.ecog | 227 | 1.6095320 | 0.0000269 | 0.0002154 | NA | NA | NA | NA | 0.0001530 | 0.0012237 | NA | NA | NA | NA | original |
| 6 | pat.karno | 225 | 0.9803456 | 0.0002824 | 0.0011296 | 0.2709544 | 0.0003071 | 0.0015356 | 0.5755627 | 0.0006608 | 0.0026431 | 0.0025848 | 0.0155086 | 0.5908952 | 0.8863427 | original |
| 3 | sex | 228 | 0.5880028 | 0.0014912 | 0.0039766 | NA | NA | NA | 0.5880028 | 0.0014912 | 0.0039766 | NA | NA | NA | NA | categorized |
| 5 | ph.karno | 227 | 0.9836863 | 0.0049579 | 0.0099157 | 0.3184168 | 0.0079468 | 0.0198669 | 0.6352465 | 0.0077670 | 0.0155339 | 0.0128462 | 0.0385385 | 0.2307961 | 0.6848245 | original |
| 2 | age | 228 | 1.0188965 | 0.0418531 | 0.0669650 | 3.0256773 | 0.0466926 | 0.0778209 | 1.1440790 | 0.3910647 | 0.3957558 | 0.0825447 | 0.1650894 | 0.3424123 | 0.6848245 | original |
| 1 | inst | 227 | 0.9903692 | 0.3459838 | 0.4613117 | 0.9292046 | 0.3181432 | 0.3976790 | 0.8384047 | 0.2600040 | 0.3466720 | 0.8175277 | 0.8707131 | 0.9839705 | 0.9839705 | categorized |
| 7 | meal.cal | 181 | 0.9998762 | 0.5929402 | 0.6776459 | 0.9141580 | 0.6128095 | 0.6128095 | 0.8620604 | 0.3957558 | 0.3957558 | 0.8707131 | 0.8707131 | 0.8227256 | 0.9839705 | categorized |
| 8 | wt.loss | 214 | 1.0013201 | 0.8281974 | 0.8281974 | NA | NA | NA | 1.3190185 | 0.0909098 | 0.1454557 | 0.1128907 | 0.1693361 | 0.0514936 | 0.3089618 | rcs.nonlinear |

### Generating Publication-Ready Tables and Figures

#### Example 3.1: Automatic Type Infer and Baseline Table Generation

``` r
var_types <- get_var_types(mtcars, strata = "vs") # Automatically infer variable types
print(var_types)
#> $factor_vars
#> [1] "cyl"  "vs"   "am"   "gear"
#> 
#> $exact_vars
#> [1] "cyl"  "gear"
#> 
#> $nonnormal_vars
#> [1] "drat" "carb"
#> 
#> $omit_vars
#> NULL
#> 
#> $strata
#> [1] "vs"
#> 
#> attr(,"class")
#> [1] "var_types"

tables <- baseline_table(mtcars,
  var_types = var_types, contDigits = 1, save_table = FALSE,
  filename = "baseline.csv", seed = 1 # set seed for simulated fisher exact test
)
knitr::kable(tables$baseline) # Display the table
```

|  | Overall | vs: 0 | vs: 1 | p | test |
|:---|:---|:---|:---|:---|:---|
| n | 32 | 18 | 14 |  |  |
| mpg (mean (SD)) | 20.1 (6.0) | 16.6 (3.9) | 24.6 (5.4) | \<0.001 |  |
| cyl (%) |  |  |  | \<0.001 | exact |
| 4 | 11 (34.4) | 1 (5.6) | 10 (71.4) |  |  |
| 6 | 7 (21.9) | 3 (16.7) | 4 (28.6) |  |  |
| 8 | 14 (43.8) | 14 (77.8) | 0 (0.0) |  |  |
| disp (mean (SD)) | 230.7 (123.9) | 307.1 (106.8) | 132.5 (56.9) | \<0.001 |  |
| hp (mean (SD)) | 146.7 (68.6) | 189.7 (60.3) | 91.4 (24.4) | \<0.001 |  |
| drat (median \[IQR\]) | 3.7 \[3.1, 3.9\] | 3.2 \[3.1, 3.7\] | 3.9 \[3.7, 4.1\] | 0.013 | nonnorm |
| wt (mean (SD)) | 3.2 (1.0) | 3.7 (0.9) | 2.6 (0.7) | 0.001 |  |
| qsec (mean (SD)) | 17.8 (1.8) | 16.7 (1.1) | 19.3 (1.4) | \<0.001 |  |
| am = 1 (%) | 13 (40.6) | 6 (33.3) | 7 (50.0) | 0.556 |  |
| gear (%) |  |  |  | 0.002 | exact |
| 3 | 15 (46.9) | 12 (66.7) | 3 (21.4) |  |  |
| 4 | 12 (37.5) | 2 (11.1) | 10 (71.4) |  |  |
| 5 | 5 (15.6) | 4 (22.2) | 1 (7.1) |  |  |
| carb (median \[IQR\]) | 2.0 \[2.0, 4.0\] | 4.0 \[2.2, 4.0\] | 1.5 \[1.0, 2.0\] | \<0.001 | nonnorm |

#### Example 3.2: RCS Plot

``` r
data(cancer, package = "survival")

# Performing cox regression, which is inferred by `y` and `time`
p <- rcs_plot(cancer, x = "age", y = "status", time = "time", covars = c("sex", "ph.karno"), save_plot = FALSE)
#> Warning in rcs_plot(cancer, x = "age", y = "status", time = "time", covars =
#> c("sex", : 1 incomplete cases excluded.
plot(p)
```

<img src="man/figures/README-example_3.2-1.png" width="60%" />

#### Example 3.3: Interaction Plot

``` r
data(cancer, package = "survival")

# Generating interaction plot of both linear and RCS models
p <- interaction_plot(cancer,
  y = "status", time = "time", predictor = "age",
  group_var = "sex", save_plot = FALSE
)
plot(p$lin)
```

<img src="man/figures/README-example_3.3-1.png" width="60%" />

``` r
plot(p$rcs)
```

<img src="man/figures/README-example_3.3-2.png" width="60%" />

#### Example 3.4: Regression Forest Plot

``` r
data(cancer, package = "survival")
cancer$dead <- cancer$status == 2 # Preparing a binary variable for logistic regression
cancer$`age per 1 sd` <- c(scale(cancer$age)) # Standardizing age

# Performing multivairate logistic regression
p1 <- regression_forest(cancer,
  model_vars = c("age per 1 sd", "sex", "wt.loss"), y = "dead",
  as_univariate = FALSE, save_plot = FALSE
)
plot(p1)
```

<img src="man/figures/README-example_3.4-1.png" width="60%" />

``` r

p2 <- regression_forest(
  cancer,
  model_vars = list(
    Crude = c("age per 1 sd"),
    Model1 = c("age per 1 sd", "sex"),
    Model2 = c("age per 1 sd", "sex", "wt.loss")
  ),
  y = "dead",
  save_plot = FALSE
)
plot(p2)
```

<img src="man/figures/README-example_3.4-2.png" width="60%" />

#### Example 3.5: Subgroup Forest Plot

``` r
data(cancer, package = "survival")
# coxph model with time assigned
p <- subgroup_forest(cancer,
  subgroup_vars = c("age", "sex", "wt.loss"), x = "ph.ecog", y = "status",
  time = "time", covars = "ph.karno", ticks_at = c(1, 2), save_plot = FALSE
)
plot(p)
```

<img src="man/figures/README-example_3.5-1.png" width="70%" />

#### Example 3.6: Classification Model Performance

``` r
# Building models with example data
data(cancer, package = "survival")
df <- kidney
df$dead <- ifelse(df$time <= 100 & df$status == 0, NA, df$time <= 100)
df <- na.omit(df[, -c(1:3)])

model0 <- glm(dead ~ age + frail, family = binomial(), data = df)
model1 <- glm(dead ~ ., family = binomial(), data = df)
df$base_pred <- predict(model0, type = "response")
df$full_pred <- predict(model1, type = "response")

# Generating most of the useful plots and metrics for model comparison
results <- classif_model_compare(df, "dead", c("base_pred", "full_pred"), save_output = FALSE)
#> Assuming 'TRUE' is [Event] and 'FALSE' is [non-Event]

knitr::kable(results$metric_table)
```

|  | Model | AUC | Accuracy | Sensitivity | Specificity | Pos Pred Value | Neg Pred Value | F1 | Kappa | Brier | cutoff | Youden | HosLem |
|:---|:---|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 2 | full_pred | 0.915 (0.847, 0.984) | 0.839 | 0.8 | 0.889 | 0.903 | 0.774 | 0.848 | 0.677 | 0.114 | 0.626 | 0.689 | 0.944 |
| 1 | base_pred | 0.822 (0.711, 0.933) | 0.806 | 0.8 | 0.815 | 0.848 | 0.759 | 0.824 | 0.610 | 0.171 | 0.490 | 0.615 | 0.405 |

``` r
plot(results$roc_plot)
```

<img src="man/figures/README-example_3.6-1.png" width="60%" />

``` r
plot(results$calibration_plot)
```

<img src="man/figures/README-example_3.6-2.png" width="60%" />

``` r
plot(results$dca_plot)
```

<img src="man/figures/README-example_3.6-3.png" width="60%" />

#### Example 3.7: Importance Plot

``` r
# Generating a dummy importance vector
set.seed(5)
dummy_importance <- runif(20, 0.2, 0.6)^5
names(dummy_importance) <- paste0("var", 1:20)

# Plotting variable importance, keeping only top 15 and splitting at 10
p <- importance_plot(dummy_importance, top_n = 15, split_at = 10, save_plot = FALSE)
plot(p)
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_bar()`).
```

<img src="man/figures/README-example_3.7-1.png" width="60%" />

## Documentation

For detailed usage, refer to the package vignettes (coming soon) or the
[GitHub repository](https://github.com/yotasama/clinpubr).

## Contributing

Bug reports and feature requests are welcome via the [issue
tracker](https://github.com/yotasama/clinpubr/issues).

## License

`clinpubr` is licensed under GPL (\>= 3).
