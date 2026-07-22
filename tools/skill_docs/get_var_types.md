# `get_var_types()`

> **Get variable types for baseline table**

## Description

Automatic variable type and method determination for baseline table.

## Usage

```r
get_var_types(
  data,
  strata = NULL,
  norm_test_by_group = TRUE,
  omit_factor_above = 20,
  num_to_factor = 5,
  save_qqplots = FALSE,
  folder_name = "qqplots"
)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `data` | A data frame. |
| `strata` | A character string indicating the column name of the strata variable. |
| `norm_test_by_group` | A logical value indicating whether to perform normality tests by group. |
| `omit_factor_above` | An integer indicating the maximum number of levels for a variable to be considered a factor. |
| `num_to_factor` | An integer. Numerical variables with number of unique values below or equal to this value would be considered a factor. |
| `save_qqplots` | A logical value indicating whether to save QQ plots. Sometimes the normality tests do not work well for some variables, and the QQ plots can be used to check the distribution. |
| `folder_name` | A character string indicating the folder name for saving QQ plots. |

## Value

An object from class `var_types`, which is just list containing the following elements:
factor_varsA character vector of variables that are factors.
exact_varsA character vector of variables that require fisher exact test.
nonnormal_varsA character vector of variables that are nonnormal.
omit_varsA character vector of variables that are excluded form the baseline table.
strataA character vector of the strata variable.

## Note

This function performs normality tests on the variables in the data frame and determines
whether they are normal. This is done by performing Shapiro-Wilk, Lilliefors, Anderson-Darling,
Jarque-Bera, and Shapiro-Francia tests. If at least two of these tests indicate that the variable
is nonnormal, then it is considered nonnormal. To alleviate the problem that normality tests become
too sensitive when sample size gets larger, the alpha level is determined by an experience formula
that decrease with sample size.

This function also marks the factor variables that require fisher exact tests if any cell haves
expected frequency less than or equal to 5. Note that this criterion less strict than the commonly
used one.

## Examples

```r
data(cancer, package = "survival")
get_var_types(cancer, strata = "sex") # set save_qqplots = TRUE to check the QQ plots

var_types <- get_var_types(cancer, strata = "sex")
# for some reason we want the variable "pat.karno" ro be considered normal.
var_types$nonnormal_vars <- setdiff(var_types$nonnormal_vars, "pat.karno")
```

