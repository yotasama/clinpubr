# `baseline_table()`

> **Create a baseline table for a dataset.**

## Description

Create a baseline table and a table of missing values. If the strata variable has more
than 2 levels, a pairwise comparison table will also be created.

## Usage

```r
baseline_table(
  data,
  var_types = NULL,
  strata = NULL,
  vars = NULL,
  factor_vars = NULL,
  exact_vars = NULL,
  nonnormal_vars = NULL,
  seed = NULL,
  omit_missing_strata = FALSE,
  save_table = FALSE,
  filename = NULL,
  multiple_comparison_test = TRUE,
  p_adjust_method = "BH",
  smd = FALSE,
  ...
)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `data` | A data frame. |
| `var_types` | An object from class `var_types` returned by `get_var_types` function. |
| `strata` | A variable to stratify the table. Overwrites the strata variable in `var_types`. |
| `vars` | A vector of variables to include in the table. |
| `factor_vars` | A vector of factor variables. Overwrites the factor variables in `var_types`. |
| `exact_vars` | A vector of variables to test for exactness. Overwrites the exact variables in `var_types`. |
| `nonnormal_vars` | A vector of variables to test for normality. Overwrites the nonnormal variables in `var_types`. |
| `seed` | A seed for the random number generator. This seed can be set for consistent simulation when performing fisher exact tests. |
| `omit_missing_strata` | A logical value indicating whether to omit missing values in the strata variable. |
| `save_table` | A logical value indicating whether to save the result tables. |
| `filename` | The name of the file to save the table. The file names for accompanying tables will be the same as the main table, but with "_missing" and "_pairwise" appended. |
| `multiple_comparison_test` | A logical value indicating whether to perform multiple comparison tests. Variables in `factor_vars` and `exact_vars` are tested with pairwise `chisq.test` or `fisher.test`, and other variables are tested with `rstatix::dunn_test()` or `rstatix::games_howell_test()`. |
| `p_adjust_method` | The method to use for p-value adjustment for pairwise comparison. Default is "BH". See `?p.adjust.methods`. |
| `smd` | A logical value indicating whether to include SMD in the table. Passed to `tableone::print.TableOne()`. |
| `...` | Additional arguments passed to `tableone::print.TableOne()`. |

## Value

A list containing the baseline table and accompanying tables.

## Examples

```r
withr::with_tempdir(
  {
    data(cancer, package = "survival")
    var_types <- get_var_types(cancer, strata = "sex")
    baseline_table(cancer, var_types = var_types, filename = "baseline.csv")

    # baseline table with pairwise comparison
    cancer$ph.ecog_cat <- factor(cancer$ph.ecog,
      levels = c(0:3),
      labels = c("0", "1", ">=2", ">=2")
    )
    var_types <- get_var_types(cancer, strata = "ph.ecog_cat")
    baseline_table(cancer, var_types = var_types, save_table = TRUE, filename = "baseline.csv")
    print(paste0("files saved to: ", getwd()))
  },
  clean = FALSE
)
```

