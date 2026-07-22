# `merge_ordered_vectors()`

> **Merging vectors while maintaining order**

## Description

Merge multiple vectors into one while trying to maintain
the order of elements in each vector. The relative order of elements
is compared by their first occurrence in the vectors in the list.
This function is useful when merging slightly different vectors,
such as questionnaires of different versions.

## Usage

```r
merge_ordered_vectors(vectors)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `vectors` | A list of vectors to be merged. |

## Value

A vector.

## Examples

```r
merge_ordered_vectors(list(c(1, 3, 4, 5, 7, 10), c(2, 5, 6, 7, 8), c(1, 7, 5, 10)))
```

