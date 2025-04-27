#' Get the subset that satisfies the missing rate condition.
#' @description Get the subset of a data frame that satisfies the missing rate condition using a greedy algorithm.
#' @param df A data frame.
#' @param row_na_ratio The maximum acceptable missing rate of rows.
#' @param col_na_ratio The maximum acceptable missing rate of columns.
#' @param row_priority A positive numerical, the priority to keep rows. The higher the value, the higher the priority,
#'   with `1` indicating equal priority for rows and columns.
#' @param return_index A logical, whether to return only the row and column indices of the subset.
#' @return The subset data frame, or a list that contains the row and column indices of the subset.
#' @details The function is based on a greedy algorithm. It iteratively removes the row or column with
#'   the highest excessive missing rate weighted by the inverse of `row_priority` until the missing rates
#'   of all rows and columns are below the specified threshold. Then it reversely tries to add rows and columns that
#'   do not break the conditions back and finalize the subset. The result depends on the `row_priority` parameter
#'   drastically, so it's recommended to try different `row_priority` values to find the most satisfying one.
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

  for (direction in c("remove", "add")) {
    repeat {
      if (direction == "remove") {
        target_rows <- current_rows
        target_cols <- current_cols
        compare_fun <- `>`
      } else {
        target_rows <- setdiff(seq_len(ori_nrow), current_rows)
        target_cols <- setdiff(seq_len(ori_ncol), current_cols)
        compare_fun <- `<=`
      }

      row_missing_rate <- rowMeans(na_mat[target_rows, current_cols, drop = FALSE])
      col_missing_rate <- colMeans(na_mat[current_rows, target_cols, drop = FALSE])

      candidate_row_id <- compare_fun(row_missing_rate, row_na_ratio)
      candidate_col_id <- compare_fun(col_missing_rate, col_na_ratio)
      if (sum(candidate_row_id) + sum(candidate_col_id) == 0) break

      candidate_rows <- target_rows[candidate_row_id]
      candidate_cols <- target_cols[candidate_col_id]

      tmp_df <- NULL
      if (length(candidate_rows) > 0) {
        tmp_df <- rbind(
          tmp_df,
          data.frame(
            type = "row", id = candidate_rows,
            score = (row_missing_rate[candidate_row_id] - row_na_ratio) / row_priority
          )
        )
      }
      if (length(candidate_cols) > 0) {
        tmp_df <- rbind(
          tmp_df,
          data.frame(
            type = "col", id = candidate_cols,
            score = col_missing_rate[candidate_col_id] - col_na_ratio
          )
        )
      }
      if (direction == "add") {
        real_candidate_ids <- c()
        for (i in seq_len(nrow(tmp_df))) {
          if (tmp_df$type[i] == "row") {
            if (max(colMeans(na_mat[c(current_rows, tmp_df$id[i]), current_cols, drop = FALSE])) <= col_na_ratio) {
              real_candidate_ids <- c(real_candidate_ids, i)
            }
          } else {
            if (max(rowMeans(na_mat[current_rows, c(current_cols, tmp_df$id[i]), drop = FALSE])) <= row_na_ratio) {
              real_candidate_ids <- c(real_candidate_ids, i)
            }
          }
        }
        if (length(real_candidate_ids) == 0) {
          break
        }
        tmp_df <- tmp_df[real_candidate_ids, ]
      }
      best <- tmp_df[which.max(tmp_df$score), ]
      if (direction == "remove") {
        if (best$type == "row") {
          current_rows <- setdiff(current_rows, best$id)
        } else {
          current_cols <- setdiff(current_cols, best$id)
        }
      } else {
        if (best$type == "row") {
          current_rows <- union(current_rows, best$id)
        } else {
          current_cols <- union(current_cols, best$id)
        }
      }
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
