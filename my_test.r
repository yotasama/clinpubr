set.seed(1)
load_all()
library(knitr)
library(dtplyr)
library(clinpubr)
library(tictoc)

df <- data.frame(
  x = c("1", "2", "3..3", "4", "6a"),
  y = c("1", "ss", "aa.a", "4", "xx"),
  z = c("1", "2", "3", "4", "6")
)
df=dtplyr::lazy_dt(df)
df_view_nonnum(df)
df_view_nonnum <- function(df, max_count = 20, random_sample = FALSE, long_df = FALSE,
                           subject_col = NULL, value_col = NULL) {
  if (ncol(df) == 0) {
    return(data.frame())
  }
  if (long_df && ncol(df) != 2 && (is.null(subject_col) || is.null(value_col))) {
    stop("`subject_col` and `value_col` must be specified for long `data.frame` with more than 2 columns.")
  }
  if (long_df) {
    if (inherits(df, "data.frame")) {
      cols <- colnames(df)
    } else if (inherits(df, "dtplyr_step")) {
      cols <- df$vars
    }
    if (is.null(subject_col)) subject_col <- cols[1]
    if (is.null(value_col)) value_col <- cols[2]
    df_long <- df %>% dplyr::select(!!rlang::sym(subject_col), !!rlang::sym(value_col))
  } else {
    subject_col <- "subject"
    value_col <- "value"
    df_long <- df %>%
      tidyr::pivot_longer(
        cols = dplyr::everything(),
        names_to = subject_col,
        values_to = value_col
      )
  }
  if (is.null(max_count) || max_count == 0) {
    max_count <- df_long %>%
      dplyr::count(!!rlang::sym(subject_col)) %>%
      dplyr::pull(n) %>%
      max()
  }
  
  res <- df_long %>%
    dplyr::group_by(!!rlang::sym(subject_col)) %>%
    dplyr::group_map(~ {
      # 注意这里使用 .x 代替原来的 .SD
      x <- check_nonnum(.x[[value_col]],
                        max_count = max_count,
                        random_sample = random_sample,
                        fix_len = TRUE)
      
      # 返回包含分组标识和结果的数据框
      dplyr::tibble(
        !!rlang::sym(subject_col) := .y[[1]],  # 添加分组标识
        row = seq_along(x),
        value = x
      )
    }) %>%
    dplyr::bind_rows() %>%  # 合并所有分组结果
    tidyr::pivot_wider(
      names_from = !!rlang::sym(subject_col),
      values_from = value,
      values_fill = NA
    ) %>%
    dplyr::select(-row) %>%
    dplyr::filter(dplyr::if_any(dplyr::everything(), ~ !is.na(.x))) %>%
    dplyr::select(dplyr::where(~ any(!is.na(.x))))
  
  res
}
tmp_fun_factory <- function(value_col, max_count, random_sample) {
  function(.x, .y) {
    x <- check_nonnum(.x[[value_col]], 
                      max_count = max_count,
                      random_sample = random_sample,
                      fix_len = TRUE)
    tibble::tibble(row = seq_along(x), value = x)
  }
}
