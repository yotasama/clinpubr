# `get_valid_subset()`

> **Get the subset that satisfies the missing rate condition.**

## Description

Get the subset of a data frame that satisfies the missing rate condition using a greedy algorithm.

## Usage

```r
get_valid_subset(
  df,
  row_na_ratio = 0.5,
  col_na_ratio = 0.2,
  row_priority = 1,
  adaptive_scoring = FALSE,
  speedup_ratio = 0,
  return_index = FALSE
)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `df` | A data frame. |
| `row_na_ratio` | The maximum acceptable missing rate of rows. Should be in range of [0, 1]. |
| `col_na_ratio` | The maximum acceptable missing rate of columns. Should be in range of [0, 1]. |
| `row_priority` | A positive numerical, the priority to keep rows. The higher the value, the higher the priority, with `1` indicating equal priority for rows and columns. |
| `adaptive_scoring` | A logical, whether to use adaptive scoring that considers the improvement in missing rates for the other dimension. When TRUE, the score reflects how much removing a row/column helps the columns/rows get closer to their thresholds. Setting `adaptive_scoring = TRUE` would allow the algorithm to search in a wider range of candidates, but significantly increases the running time. Default is FALSE. |
| `speedup_ratio` | A numerical in [0, 1]. Controls how many rows/columns to remove per iteration. `0` removes one at a time (most precise), `1` removes all candidates at once (most aggressive). |
| `return_index` | A logical, whether to return only the row and column indices of the subset. |

## Value

The subset data frame, or a list that contains the row and column indices of the subset.

## Details

The function is based on a greedy algorithm. It iteratively removes the row or column with
the highest excessive missing rate weighted by the inverse of `row_priority` until the missing rates
of all rows and columns are below the specified threshold. Then it reversely tries to add rows and columns that
do not break the conditions back and finalize the subset. The result depends on the `row_priority` parameter
drastically, so it's recommended to try different `row_priority` values to find the most satisfying one.

When `adaptive_scoring = TRUE`, the scoring considers how much removing a row/column improves the
missing rates of the other dimension. The score is calculated as:

 For rows: sum of improvements in column missing rates (how much closer columns get to col_na_ratio)
 For columns: sum of improvements in row missing rates (how much closer rows get to row_na_ratio)
This allows the algorithm to consider removing rows/columns even if they don't exceed thresholds,
if doing so helps other dimensions satisfy their thresholds.

## Examples

```r
data(cancer, package = "survival")
dim(cancer)
max_missing_rates(cancer)

cancer_valid <- get_valid_subset(cancer, row_na_ratio = 0.2, col_na_ratio = 0.1, row_priority = 1)
dim(cancer_valid)
max_missing_rates(cancer_valid)
```

