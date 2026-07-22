# `combine_files()`

> **combine multiple data files into a single data frame**

## Description

combine multiple data files into a single data frame

## Usage

```r
combine_files(
  path = ".",
  pattern = NULL,
  recursive = FALSE,
  add_file_name = FALSE,
  unique_only = TRUE,
  reader_fun = read.csv,
  ...
)
```

## Arguments

| 参数 | 说明 |
|------|------|
| `path` | A string as the path to find the data files. |
| `pattern` | A file pattern to filter the required data files. |
| `recursive` | A logical value to indicate whether to search files recursively in subdirectories. |
| `add_file_name` | A logical value to indicate whether to add the file name as a column. Note that the added file name will affect the uniqueness of the data. |
| `unique_only` | A logical value to indicate whether to remove the duplicated rows. |
| `reader_fun` | A function to read the data files. Can be `read.csv`, `openxlsx::read.xlsx`, etc. |
| `...` | Other parameters passed to the `reader_fun`. |

## Value

A data frame. If no data files found, return `NULL`.

## Examples

```r
library(withr)
with_tempdir({
  write.csv(data.frame(x = 1:3, y = 4:6), "file1.csv", row.names = FALSE)
  write.csv(data.frame(x = 7:9, y = 10:12), "file2.csv", row.names = FALSE)
  dat <- combine_files(pattern = "file")
})
print(dat)
```

