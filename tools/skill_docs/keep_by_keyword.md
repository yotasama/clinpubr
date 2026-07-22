# `keep_by_keyword()`

> **Keep string segment by regex keyword position**

## Description

Desensitize a character vector by removing the unneeded part of each string.
The retained part is determined by keyword matches from a regular expression.

## Usage

```r
keep_by_keyword(
  x,
  keyword,
  from = c("start", "first", "last"),
  to = c("first", "last", "end"),
  include_keyword = TRUE
)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `x` | A character vector. |
| `keyword` | A regular expression keyword pattern. |
| `from` | Left boundary of retained text:   `"start"`: start of the string.  `"first"`: first keyword match.  `"last"`: last keyword match. |
| `to` | Right boundary of retained text:   `"first"`: first keyword match.  `"last"`: last keyword match.  `"end"`: end of the string. |
| `include_keyword` | Logical. Whether to include the keyword match used as split point in output. |

## Value

A character vector with retained text only.

## Examples

```r
urls <- c(
  "https://hospital.example.com/patient/123?token=abc",
  "https://trial.example.org/visit/456"
)
# Keep domain only
keep_by_keyword(urls, "com|org|net", from = "start", to = "last", include_keyword = TRUE)

ids <- c("SITE-2026-0001", "CTR-2025-0912")
# Keep site prefix before first '-'
keep_by_keyword(ids, "-", from = "start", to = "first", include_keyword = FALSE)
```

