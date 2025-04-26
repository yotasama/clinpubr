#' Get the subset that satisfies the missing rate condition.
#' @description Get the subset that satisfies the missing rate condition.
#' @param x A string vector that stores numerical values.
#' @return A cleaner string vector that stores numerical values.
#' @export
#' @examples
#' num_simple_cleaning(c("１２３＊＿＋", "11..23", "11ａ：　Ａ"))
get_valid_subset <- function(df,
                             row_na_ratio = 0.1,
                             col_na_ratio = 0.05,
                             return_id = FALSE) {
  # 参数校验
  if (!is.data.frame(df)) stop("输入必须为数据框")
  if (row_na_ratio < 0 || row_na_ratio > 1) stop("行缺失比例需在[0,1]区间")
  if (col_na_ratio < 0 || col_na_ratio > 1) stop("列缺失比例需在[0,1]区间")
  
  # 初始化日志
  removed_rows <- integer(0)
  removed_cols <- integer(0)
  original_dim <- dim(df)
  
  repeat {
    # 计算当前行列缺失比例
    row_na <- rowSums(is.na(df)) / ncol(df)
    col_na <- colSums(is.na(df)) / nrow(df)
    
    # 判断终止条件
    if (all(row_na <= row_na_ratio) && all(col_na <= col_na_ratio)) break
    
    # 记录被删除的行列索引
    bad_rows <- which(row_na > row_na_ratio)
    bad_cols <- which(col_na > col_na_ratio)
    if (length(bad_rows) > 0) removed_rows <- c(removed_rows, as.numeric(names(bad_rows)))
    if (length(bad_cols) > 0) removed_cols <- c(removed_cols, as.numeric(names(bad_cols)))
    
    # 动态删除不合格行列
    df <- df[row_na <= row_na_ratio, col_na <= col_na_ratio, drop = FALSE]
  }
  
  # 返回结果列表
  list(
    filtered_df = df,
    removed_rows = unique(removed_rows),
    removed_cols = unique(removed_cols),
    original_dim = original_dim,
    final_dim = dim(df)
  )
}
