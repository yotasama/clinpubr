# `auto_encoding_repair()`

> **Auto-detect and repair encoding issues in character vectors**

## Description

Detects non-UTF-8 encoded characters and attempts to repair them.
Uses base::validUTF8() for fast detection and stringi for conversion.
This is useful for handling data imported from various sources with mixed encodings.

## Usage

```r
auto_encoding_repair(x, from_encoding = "auto")
```

## Arguments

| 参数 | 说明 |
|------|------|
| `x` | A character vector. |
| `from_encoding` | Character, the source encoding to convert from. If "auto" (default), attempts to detect the most likely encoding. Common values: "GBK", "GB2312", "Latin-1", "UTF-8". |

## Value

A character vector with repaired encoding.

## Note

This function will warn when encoding repairs are made.

## Examples

```r
# UTF-8 input (no repair needed)
auto_encoding_repair(c("hello", "world"))

# Full-width characters (UTF-8, no repair needed)
auto_encoding_repair(c("\uFF11\uFF12\uFF13", "abc"))

# Simulate GBK-encoded Chinese characters that need repair
# (using iconv to create non-UTF-8 bytes for demonstration)
if (l10n_info()$"UTF-8") {
  # Create GBK-encoded bytes from UTF-8 Chinese characters
  gbk_bytes <- iconv("\u4f60\u597d", from = "UTF-8", to = "GBK")
  auto_encoding_repair(gbk_bytes)
}

# Mixed encoding vector (UTF-8 and GBK)
if (l10n_info()$"UTF-8") {
  mixed <- c("hello",
             iconv("\u4e2d\u6587", from = "UTF-8", to = "GBK"),
             "world")
  auto_encoding_repair(mixed)
}
```

