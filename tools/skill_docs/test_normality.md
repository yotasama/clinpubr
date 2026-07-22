# `test_normality()`

> **Test normality of a numeric variable**

## Description

Perform multiple normality tests on a numeric variable and determine if it follows normal distribution.

## Usage

```r
test_normality(x, alpha = 0.05, all_positive = NULL)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `x` | A numeric vector to test for normality. |
| `alpha` | The significance level for normality tests. Default is 0.05. |
| `all_positive` | A logical value indicating whether all values are non-negative. If TRUE and standard deviation is less than mean, the variable is considered non-normal (likely right-skewed). |

## Value

A logical value indicating whether the variable is normal (TRUE) or non-normal (FALSE).

## Note

This function performs Shapiro-Wilk, Lilliefors, Anderson-Darling, Jarque-Bera, and
Shapiro-Francia tests. If at least two of these tests indicate that the variable is nonnormal
(p < alpha), then it is considered nonnormal. For positive variables, if SD < mean, it's also
considered non-normal as it suggests right skewness.

## Examples

```r
# Test normal data
normal_data <- rnorm(100)
test_normality(normal_data)

# Test non-normal data
skewed_data <- rexp(100)
test_normality(skewed_data)
```

