# `extract_history()`

> **Extract Medical History from Clinical Text in Chinese**

## Description

Extract medical history information (disease, smoking, drinking, surgery, etc.)
from clinical text records, now only supports Chinese text. The function uses vectorized string operations
for high performance on large datasets.

## Usage

```r
extract_history(
  text,
  keywords,
  extract_duration = TRUE,
  duration_unit = c("original", "years", "days"),
  negation_window = 20,
  return_format = c("detailed", "simple", "data.frame")
)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `text` | A character vector of clinical text records. |
| `keywords` | A character vector of keywords to search for (e.g., "\u9ad8\u8840\u538b", "\u5438\u70df"). Can also be a named list where names are category names and values are keyword vectors. |
| `extract_duration` | Logical. If `TRUE`, extract duration information (years/months/days) when available. Default is `TRUE`. |
| `duration_unit` | Character. The unit for duration output. Can be `"original"` (keep as extracted, e.g., "5\u5e74", "3\u6708"), `"years"` (convert all to years), or `"days"` (convert all to days). Default is `"original"`. |
| `negation_window` | Integer. The maximum character distance to look for negation words before the keyword. Default is 20. |
| `return_format` | Character. The format of return values. Can be `"simple"` ("\u6709"/"\u65e0"/NA), `"detailed"` (duration included, e.g., "\u6709:5\u5e74"/"\u65e0"/NA), or `"data.frame"` (separate columns for status and duration). Default is `"simple"`. |

## Value

Depending on `return_format`:

 `"simple"`: A character vector with values "\u6709" (yes), "\u65e0" (no), or `NA` (unknown).
 `"detailed"`: A character vector with duration information when available.
 `"data.frame"`: A data frame with columns `status` (\u6709/\u65e0/NA) and `duration` (duration value).


If `keywords` is a named list with multiple categories, returns a data frame with
one column per category.

## Examples

```r
test_data <- c(
  "\u5426\u8ba4\u809d\u708e\u3001\u7ed3\u6838\u3001\u759f\u75be\u53f2\uff0c\u5426\u8ba4\u9ad8\u8840\u538b\u3001\u5fc3\u810f\u75c5\u53f2\uff0c\u5426\u8ba4\u7cd6\u5c3f\u75c5\uff0c\u65e0\u624b\u672f\u53f2\uff0c\u65e0\u8fc7\u654f\u53f2\u3002",
  "\u65e2\u5f80\u6709\u9ad8\u8840\u538b\u75c5\u53f220\u5e74\uff0c\u53e3\u670d\u964d\u538b\u836f\u63a7\u5236\u826f\u597d\u3002",
  "\u6709\u9611\u5c3e\u708e\u5207\u9664\u624b\u672f\u53f2\uff0c\u5426\u8ba4\u9ad8\u8840\u538b\u3002",
  "\u65e0\u9ad8\u8840\u538b\u3001\u5fc3\u810f\u75c5\u53f2\uff0c\u4f46\u6709\u7cd6\u5c3f\u75c5\u75c5\u53f2\u3002",
  "\u60a3\u8005\u9ad8\u8840\u538b\u75c5\u53f2\u3001\u51a0\u5fc3\u75c5\u53f2\uff0c\u65e0\u5438\u70df\u53f2\u3002",
  "\u9ad8\u8840\u538b\u75c5\u53f210\u5e74\uff0c\u5438\u70df\u53f215\u5e74\uff0c\u6bcf\u592910\u652f\uff0c\u996e\u9152\u53f25\u5e74\u3002"
)

# Extract single keyword
extract_history(test_data, "\u9ad8\u8840\u538b")

# Extract multiple keywords
extract_history(test_data, c("\u9ad8\u8840\u538b", "\u5438\u70df", "\u996e\u9152"))

# Extract with duration
extract_history(test_data, "\u9ad8\u8840\u538b", return_format = "detailed")

# Extract multiple categories with named list
keywords <- list(
  hypertension = "\u9ad8\u8840\u538b",
  diabetes = "\u7cd6\u5c3f\u75c5",
  smoking = c("\u5438\u70df", "\u62bd\u70df"),
  drinking = c("\u996e\u9152", "\u559d\u9152")
)
extract_history(test_data, keywords)
```

