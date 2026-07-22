# `unit_standardize()`

> **Standardize units of numeric data.**

## Description

Standardize units of numeric data, especially for data of medical records with different units.

## Usage

```r
unit_standardize(
  df,
  subject_col,
  value_col,
  unit_col,
  change_rules,
  extract_numbers = FALSE,
  verbose = FALSE
)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `df` | A data frame of medical records that contains test subject, value, and unit cols. |
| `subject_col` | The name of the subject column. |
| `value_col` | The name of the value column. |
| `unit_col` | The name of the unit column. |
| `change_rules` | A data frame or a list of lists. See details |
| `extract_numbers` | A logical value indicating whether to apply `extract_num` to extract numeric values from the value column. |
| `verbose` | A logical value indicating whether to print progress messages. |

## Value

A data frame with subject units standardized.

## Details

`change_rules` can accept two formats:
If a data frame, it must contain the following columns:

 `subject`: The subject to be standardized.
 `unit`: The units of the subject.
 `label`: The role of the unit, the rule is as follows:

 `"t"`: the target unit to be standardized to. If not specified,
the function will use the most common unit in the data (retrieved by `first_mode()`).
 `"r"`: The units to be removed, and the corresponding values be set to `NA`.
Set this when data with this unit cannot be used.
 A number: Set the multiplier of this unit, the standardized value will be `value * multiplier`.
And `NA` and `""` is considered the same as `1`.


If a list of lists, each list contains the following elements:

 `subject`: The subject to be standardized.
 `target_unit`: The target unit to be standardized to. If not specified,
the function will use the most common unit in the data (retrieved by `first_mode()`).
 `units2change`: The units to be changed. If not specified, the function will use
all units except the target unit. Must be specified to apply different `coeffs`.
 `coeffs`: The coefficients to be used for the conversion. If not specified, the
function will use 1 for all units to be changed.
 `units2remove`: The units to be removed, and the corresponding values be set to `NA`.
Set this when data with this unit cannot be used.

It's recommended to use the labeled result from `unit_view()` as the input.

## Examples

```r
# Example 1: Using the list as change_rules is more convenient for small datasets.
df <- data.frame(
  subject = c("a", "a", "b", "b", "b", "c", "c"), value = c(1, 2, 3, 4, 5, 6, 7),
  unit = c(NA, "x", "x", "x", "y", "a", "b")
)
change_rules <- list(
  list(subject = "a", target_unit = "x", units2change = c(NA), coeffs = c(20)),
  list(subject = "b"),
  list(subject = "c", target_unit = "b")
)
unit_standardize(df,
  subject_col = "subject", value_col = "value", unit_col = "unit",
  change_rules = change_rules
)

# Example 2: Using the labeled result from `unit_view()` as the input
# is more robust for large datasets.
df <- data.frame(subject = sample(c("a", "b"), 1000, replace = TRUE), value = runif(1000))
df$unit <- NA
df$unit[df$subject == "a"] <- sample(c("mg/L", "g/l", "g/L"),
  sum(df$subject == "a"),
  replace = TRUE
)
df$value[df$subject == "a" & df$unit == "mg/L"] <-
  df$value[df$subject == "a" & df$unit == "mg/L"] * 1000
df$unit[df$subject == "b"] <- sample(c(NA, "m.g", "mg"), sum(df$subject == "b"),
  prob = c(0.3, 0.05, 0.65), replace = TRUE
)
df$value[df$subject == "b" & df$unit %in% "mg"] <-
  df$value[df$subject == "b" & df$unit %in% "mg"] * 1000
df$value[df$subject == "b" & is.na(df$unit)] <- df$value[df$subject == "b" & is.na(df$unit)] *
  sample(c(1, 1000), size = sum(df$subject == "b" & is.na(df$unit)), replace = TRUE)

unit_table <- unit_view(
  df = df, subject_col = "subject",
  value_col = "value", unit_col = "unit", save_table = FALSE
)
unit_table$label <- c("t", NA, 1e-3, NA, NA, "r") # labeling the units

df_standardized <- unit_standardize(
  df = df, subject_col = "subject", value_col = "value",
  unit_col = "unit", change_rules = unit_table
)
unit_view(
  df = df_standardized, subject_col = "subject", value_col = "value", unit_col = "unit",
  save_table = FALSE, conflicts_only = FALSE
)
```

