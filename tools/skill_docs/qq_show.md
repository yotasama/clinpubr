# `qq_show()`

> **QQ plot**

## Description

QQ plot for a sample.

## Usage

```r
qq_show(
  x,
  title = NULL,
  save = FALSE,
  filename = "QQplot.png",
  width = 2,
  height = 2
)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `x` | A sample. |
| `title` | Title of the plot. |
| `save` | If TRUE, save the plot. |
| `filename` | Filename of the plot. |
| `width` | Width of the plot. |
| `height` | Height of the plot. |

## Value

A plot.

## Examples

```r
qq_show(rnorm(100))
```

