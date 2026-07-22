# `vec2code()`

> **Generate code from string vector**

**别名**: `name2code()`

## Description

Generate the code that can be used to generate the string vector.
`name2code()` is a wrapper of `vec2code(names(x))` to generate code for names of a
vector, list, data frame, or any object with names.

## Usage

```r
vec2code(x)

name2code(x)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `x` | A string vector. |

## Value

A string that contains the code to generate the vector.

## Examples

```r
vec2code(colnames(mtcars))
name2code(mtcars)
```

