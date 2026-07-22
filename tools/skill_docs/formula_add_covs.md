# `formula_add_covs()`

> **Add covariates to a formula**

## Description

Add covariates to a formula. Support both formula and character string.

## Usage

```r
formula_add_covs(formula, covars)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `formula` | A formula. Should be a formula or a character string of formula. |
| `covars` | A vector of covariates. |

## Value

A formula.

## Examples

```r
formula_add_covs("y ~ a + b", c("c", "d"))
```

