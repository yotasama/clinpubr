# `extract_history()`

> **Extract Medical History from Clinical Text**

## Description

Extract medical history information (disease, smoking, drinking, surgery, etc.)
from clinical text records, supports Chinese text. The function uses vectorized string operations
for high performance on large datasets.

## Usage

```r
extract_history(
  text,
  keywords,
  extract_duration = TRUE,
  duration_unit = c("original", "years", "days"),
  negation_window = 20,
  return_format = c("simple", "detailed", "data.frame")
)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `text` | A character vector of clinical text records. |
| `keywords` | A character vector of keywords to search for. Can also be a named list where names are category names and values are keyword vectors. |
| `extract_duration` | Logical. If `TRUE`, extract duration information (years/months/days) when available. Default is `TRUE`. |
| `duration_unit` | Character. The unit for duration output. Can be `"original"` (keep as extracted), `"years"` (convert all to years), or `"days"` (convert all to days). Default is `"original"`. |
| `negation_window` | Integer. The maximum character distance to look for negation words before the keyword. Default is 20. |
| `return_format` | Character. The format of return values. Can be `"simple"` (`TRUE`/`FALSE`/`NA`), `"detailed"` (duration strings like "5 years" when available, "yes"/"no" otherwise), or `"data.frame"` (separate columns for status and duration). Default is `"simple"`. |

## Value

Depending on `return_format`:

 `"simple"`: A logical vector with values `TRUE`, `FALSE`, or `NA`.
 `"detailed"`: A character vector with duration strings (e.g., "5 years", "30 days") when available,
otherwise `TRUE`, `FALSE`, or `NA`.
 `"data.frame"`: A data frame with columns `status` (logical) and `duration` (character).


If `keywords` is a named list with multiple categories, returns a data frame with
one column per category.

## Examples

```r
test_data <- c(
  "hypertension history 20 years",
  "appendectomy surgery history"
)
extract_history(test_data, "hypertension")
```

