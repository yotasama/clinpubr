# `merge_by_substring()`

> **Merge Data Frame by String Key Matching**

## Description

This function merges two data frames based on string key matching.
It searches for keys from `key_df[[key_col]]` in `data[[search_col]]`
and adds corresponding columns from `key_df` to `data`.

## Usage

```r
merge_by_substring(
  data,
  key_df,
  search_col,
  key_col,
  value_cols,
  case_insensitive = TRUE,
  ...
)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `data` | The primary data frame to be enhanced with additional columns |
| `key_df` | A data frame containing string keys and their corresponding values |
| `search_col` | Column name in `data` to search for keys (default: "name") |
| `key_col` | Column name in `key_df` containing keys to match (default: "key") |
| `value_cols` | Column name(s) in `key_df` to add to `data` (default: "value") Can be a single column name or a character vector of column names. Defaults to "value" if not provided. |
| `case_insensitive` | Whether to perform case-insensitive matching (default: TRUE) Defaults to TRUE if not provided. |
| `...` | Additional arguments passed to `stringi::stri_detect_regex()`. |

## Value

A data frame with all columns from `data` plus matched columns from `key_df`.
Unmatched rows will have NA values in the added columns.

## Examples

```r
# Basic usage
main_data <- data.frame(
  name = c("AB", "B,C", "A..", "ACD"),
  value = c(1, 2, 3, 4)
)
key_lookup <- data.frame(
  key = c("A", "B", "C", "ACD", "AB"),
  category = c("cat1", "cat2", "cat3", "cat4", "cat1"),
  code = c("001", "002", "003", "004", "001")
)
result <- merge_by_substring(main_data, key_lookup,
  search_col = "name",
  key_col = "key", value_cols = c("category", "code")
)
print(result)
```

