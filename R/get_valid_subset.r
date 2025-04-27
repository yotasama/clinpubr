#' Get the subset that satisfies the missing rate condition.
#' @description Get the subset of a data frame that satisfies the missing rate condition using a greedy algorithm.
#' @param df A data frame.
#' @param row_na_ratio The maximum acceptable missing rate of rows.
#' @param col_na_ratio The maximum acceptable missing rate of columns.
#' @param row_priority A positive numerical, the priority to keep rows. The higher the value, the higher the priority.
#' @param return_index A logical, whether to return only the row and column indices of the subset.
#' @return The subset data frame, or a list that contains the row and column indices of the subset.
#' @details The function is based on a greedy algorithm. It iteratively removes the row or column with
#'   the highest excessive missing rate weighted by the inverse of `row_priority` until the missing rates
#'   of all rows and columns are below the specified threshold. Unfortunately, this algorithm does not
#'   guarantee to find the optimal solution, and the result depends on the `row_priority` parameter drastically.
#'   It happens sometimes that some `row_priority` values may lead to a smaller subset than some other values
#'   both in the number of rows and columns. It's recommended to try different `row_priority` values to find the
#'   most satisfying one.
#' @export
#' @examples
#' data(cancer, package = "survival")
#' dim(cancer)
#' max_missing_rates(cancer)
#'
#' cancer_valid <- get_valid_subset(cancer, row_na_ratio = 0.2, col_na_ratio = 0.1, row_priority = 1)
#' dim(cancer_valid)
#' max_missing_rates(cancer_valid)
get_valid_subset <- function(df, row_na_ratio = 0.5, col_na_ratio = 0.2, row_priority = 1, return_index = FALSE) {
  ori_nrow <- nrow(df)
  ori_ncol <- ncol(df)
  na_mat <- as.matrix(is.na(df))
  current_rows <- seq_len(ori_nrow)
  current_cols <- seq_len(ori_ncol)

  repeat {
    row_missing_rate <- rowSums(na_mat[current_rows, current_cols, drop = FALSE]) / length(current_cols)
    col_missing_rate <- colSums(na_mat[current_rows, current_cols, drop = FALSE]) / length(current_rows)

    bad_row_id <- row_missing_rate > row_na_ratio
    bad_col_id <- col_missing_rate > col_na_ratio
    bad_rows <- current_rows[bad_row_id]
    bad_cols <- current_cols[bad_col_id]

    if (length(bad_rows) + length(bad_cols) == 0) break

    tmp_df <- NULL
    if (length(bad_rows) > 0) {
      tmp_df <- rbind(
        tmp_df,
        data.frame(
          type = "row", id = bad_rows,
          score = (row_missing_rate[bad_row_id] - row_na_ratio) / row_priority
        )
      )
    }
    if (length(bad_cols) > 0) {
      tmp_df <- rbind(
        tmp_df,
        data.frame(
          type = "col", id = bad_cols,
          score = col_missing_rate[bad_col_id] - col_na_ratio
        )
      )
    }
    best <- tmp_df[which.max(tmp_df$score), ]
    if (best$type == "row") {
      current_rows <- setdiff(current_rows, best$id)
    } else {
      current_cols <- setdiff(current_cols, best$id)
    }
  }

  if (return_index) {
    list(rows = current_rows, cols = current_cols)
  } else {
    df[current_rows, current_cols, drop = FALSE]
  }
}


#' Get the maximum missing rate of rows and columns.
#' @description Get the maximum missing rate of rows and columns.
#' @param x A data frame.
#' @return A list that contains the maximum missing rate of rows and columns.
#' @export
#' @examples
#' data(cancer, package = "survival")
#' max_missing_rates(cancer)
max_missing_rates <- function(df) {
  list(
    row = max(rowMeans(is.na(df))),
    col = max(colMeans(is.na(df)))
  )
}
