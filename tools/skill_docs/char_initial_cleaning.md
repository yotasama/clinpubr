# `char_initial_cleaning()`

> 这是 [`value_initial_cleaning()`](value_initial_cleaning.md) 的别名，完整文档请查看主函数。

# `value_initial_cleaning()`

> **Preliminarily cleaning string vectors**

**别名**: `char_initial_cleaning()`

## Description

Cleaning illegal characters in string vectors that store numerical values.
The function is useful for cleaning electrical health records in Chinese.

`char_initial_cleaning()` will convert full-width characters to half-width characters,
removes whitespace at the start and end, replaces all internal whitespace with a single space,
and replace empty strings with `NA`.

`value_initial_cleaning()` will additionally remove all spaces and extra dots.

## Usage

```r
value_initial_cleaning(x, remove_inequal = FALSE, fix_encoding = TRUE)

char_initial_cleaning(x, fix_encoding = TRUE)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `x` | A string vector. |
| `remove_inequal` | A logical value. If `TRUE`, remove comparison symbols such as `<`, `>` from the string |
| `fix_encoding` | Logical. If `TRUE`, automatically detect and repair non-UTF-8 encoding issues before cleaning. Default is `TRUE`. |

## Value

A string vector with less illegal characters.

## Note

When `fix_encoding = TRUE`, a warning will be issued if encoding repairs are made.

## Examples

```r
x <- c("\uFF11\uFF12\uFF13", "11..23", "\uff41\uff42\uff41\uff4e\uff44\uff4f\uff4e",
       "hello world ")
value_initial_cleaning(x)
char_initial_cleaning(x)
```

