# `calc_cindex()`

> **Calculate C-index for survival data**

## Description

Calculate C-index for survival data. It's a wrapper function for `Hmisc::rcorr.cens()`.

## Usage

```r
calc_cindex(data, time_var, event_var, marker_var)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `data` | A data frame containing the survival time, event indicator, and marker variable. |
| `time_var` | A string specifying the name of the survival time variable in the data frame. |
| `event_var` | A string specifying the name of the event indicator variable in the data frame. |
| `marker_var` | A string specifying the name of the marker variable in the data frame. |

## Value

The C-index value.

## Examples

```r
# Calculate C-index using lung dataset from survival package
data(cancer, package = "survival")
# Use age as the marker variable
calc_cindex(lung, "time", "status", "age")
```

