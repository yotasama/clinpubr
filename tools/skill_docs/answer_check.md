# `answer_check()`

> **Check answers of multiple choice questions**

## Description

Check answers of multiple choice questions by matching the answers with the correct sequence.

## Usage

```r
answer_check(dat, seq, multi_column = FALSE)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `dat` | A data frame of answers. |
| `seq` | A vector of correct answers, one element for each question. |
| `multi_column` | Logical, whether the multi-answers are in multiple columns. |

## Value

A data frame of boolean values, with ncol equals the number of questions.

## Details

If `multi_column` is TRUE, the answers for Multiple-Answer Questions should be in multiple columns
of logicals, with each column representing a choice. The `seq` should be a string of `"T"` and `"F"`.
If `multi_column` is `FALSE`, the answers for Multiple-Answer Questions should be in one column, and the function
would expect an exact match of `seq`.

## Examples

```r
dat <- data.frame(Q1 = c("A", "B", "C"), Q2 = c("AD", "AE", "ABF"))
seq <- c("A", "AE")
answer_check(dat, seq)
dat <- data.frame(
  Q1 = c("A", "B", "C"), Q2.A = c(TRUE, TRUE, FALSE),
  Q2.B = c(TRUE, FALSE, TRUE), Q2.C = c(FALSE, TRUE, FALSE)
)
seq <- c("A", "TFT")
answer_check(dat, seq, multi_column = TRUE)
```

