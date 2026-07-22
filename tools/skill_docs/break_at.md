# `break_at()`

> **Generate breaks for histogram**

## Description

Generate breaks for histogram that covers xlim and includes a ref_val.

## Usage

```r
break_at(xlim, breaks, ref_val = NULL)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `xlim` | A vector of length 2. |
| `breaks` | The number of breaks. |
| `ref_val` | The reference value to include in breaks. |

## Value

A vector of breaks of length `breaks + 1`.

## Examples

```r
break_at(xlim = c(0, 10), breaks = 12, ref_val = 3.12)
```

