#' Get the subset that satisfies the missing rate condition.
#' @description Get the subset of a data frame that satisfies the missing rate condition using a greedy algorithm.
#' @param df A data frame.
#' @param row_na_ratio The maximum acceptable missing rate of rows. Should be in range of `[0, 1]`.
#' @param col_na_ratio The maximum acceptable missing rate of columns. Should be in range of `[0, 1]`.
#' @param row_priority A positive numerical, the priority to keep rows. The higher the value, the higher the priority,
#'   with `1` indicating equal priority for rows and columns.
#' @param adaptive_scoring A logical, whether to use adaptive scoring that considers the improvement in 
#'   missing rates for the other dimension. When TRUE, the score reflects how much removing a row/column
#'   helps the columns/rows get closer to their thresholds. Setting `adaptive_scoring = TRUE` would allow the algorithm 
#'   to search in a wider range of candidates, but significantly increases the running time. Default is FALSE.
#' @param speedup_ratio A numerical in `[0, 1]`. Controls how many rows/columns to remove per iteration.
#'   `0` removes one at a time (most precise), `1` removes all candidates at once (most aggressive).
#' @param return_index A logical, whether to return only the row and column indices of the subset.
#' @details The function is based on a greedy algorithm. It iteratively removes the row or column with
#'   the highest excessive missing rate weighted by the inverse of `row_priority` until the missing rates
#'   of all rows and columns are below the specified threshold. Then it reversely tries to add rows and columns that
#'   do not break the conditions back and finalize the subset. The result depends on the `row_priority` parameter
#'   drastically, so it's recommended to try different `row_priority` values to find the most satisfying one.
#'   
#'   When `adaptive_scoring = TRUE`, the scoring considers how much removing a row/column improves the 
#'   missing rates of the other dimension. The score is calculated as:
#'   - For rows: sum of improvements in column missing rates (how much closer columns get to col_na_ratio)
#'   - For columns: sum of improvements in row missing rates (how much closer rows get to row_na_ratio)
#'   This allows the algorithm to consider removing rows/columns even if they don't exceed thresholds,
#'   if doing so helps other dimensions satisfy their thresholds.
#' @returns The subset data frame, or a list that contains the row and column indices of the subset.
#' @export
#' @examples
#' data(cancer, package = "survival")
#' dim(cancer)
#' max_missing_rates(cancer)
#'
#' cancer_valid <- get_valid_subset(cancer, row_na_ratio = 0.2, col_na_ratio = 0.1, row_priority = 1)
#' dim(cancer_valid)
#' max_missing_rates(cancer_valid)
get_valid_subset <- function(df, row_na_ratio = 0.5, col_na_ratio = 0.2, row_priority = 1, 
                             adaptive_scoring = FALSE, speedup_ratio = 0, return_index = FALSE) {
  # ---- Input validation ----
  if (!is.data.frame(df)) stop("'df' must be a data frame.")
  if (nrow(df) == 0 || ncol(df) == 0) stop("'df' must have at least one row and one column.")
  if (!is.numeric(row_na_ratio) || length(row_na_ratio) != 1 || row_na_ratio < 0 || row_na_ratio > 1)
    stop("'row_na_ratio' must be a single numeric value in [0, 1].")
  if (!is.numeric(col_na_ratio) || length(col_na_ratio) != 1 || col_na_ratio < 0 || col_na_ratio > 1)
    stop("'col_na_ratio' must be a single numeric value in [0, 1].")
  if (!is.numeric(row_priority) || length(row_priority) != 1 || row_priority <= 0)
    stop("'row_priority' must be a single positive numeric value.")
  if (!is.numeric(speedup_ratio) || length(speedup_ratio) != 1 || speedup_ratio < 0 || speedup_ratio > 1)
    stop("'speedup_ratio' must be a single numeric value in [0, 1].")
  if (!is.logical(return_index) || length(return_index) != 1)
    stop("'return_index' must be a single logical value.")
  if (!is.logical(adaptive_scoring) || length(adaptive_scoring) != 1)
    stop("'adaptive_scoring' must be a single logical value.")

  # ---- Initialization ----
  ori_nrow <- nrow(df)
  ori_ncol <- ncol(df)
  na_mat <- as.matrix(is.na(df))

  current_rows <- seq_len(ori_nrow)
  current_cols <- seq_len(ori_ncol)

  # Incremental NA counts: row_na_count[i] = number of NAs in row i within current_cols
  #                       col_na_count[j] = number of NAs in col j within current_rows
  row_na_count <- rowSums(na_mat)  # initial: all columns active
  col_na_count <- colSums(na_mat)  # initial: all rows active

  # ---- Helper: compute missing rate from count ----
  row_missing_rate_from_count <- function(count) count / length(current_cols)
  col_missing_rate_from_count <- function(count) count / length(current_rows)

  # ---- Helper: compute adaptive scores ----
  # Returns improvement in missing rates for the OTHER dimension
  # Normalized by the number of affected elements
  compute_adaptive_scores <- function(current_rows, current_cols, row_na_count, col_na_count) {
    n_rows <- length(current_rows)
    n_cols <- length(current_cols)
    
    # Current missing rates
    row_miss <- row_na_count[current_rows] / n_cols
    col_miss <- col_na_count[current_cols] / n_rows
    
    # For each row: compute how much removing it helps columns
    # Improvement = average reduction in column excess missing rates
    row_scores <- vapply(current_rows, function(r) {
      if (n_rows <= 1) return(-Inf)  # Don't remove last row
      
      # New column missing rates after removing row r
      new_col_na <- col_na_count[current_cols] - na_mat[r, current_cols]
      new_col_miss <- new_col_na / (n_rows - 1)
      
      # Improvement: how much closer columns get to their threshold (average per column)
      old_excess <- pmax(0, col_miss - col_na_ratio)
      new_excess <- pmax(0, new_col_miss - col_na_ratio)
      improvement <- mean(old_excess - new_excess)  # Average improvement per column
      
      # Also add own excess as penalty (normalized)
      own_excess <- max(0, row_na_count[r] / n_cols - row_na_ratio)
      
      return((improvement + own_excess) / row_priority)
    }, numeric(1))
    
    # For each column: compute how much removing it helps rows
    # Improvement = average reduction in row excess missing rates
    col_scores <- vapply(current_cols, function(c) {
      if (n_cols <= 1) return(-Inf)  # Don't remove last column
      
      # New row missing rates after removing column c
      new_row_na <- row_na_count[current_rows] - na_mat[current_rows, c]
      new_row_miss <- new_row_na / (n_cols - 1)
      
      # Improvement: how much closer rows get to their threshold (average per row)
      old_excess <- pmax(0, row_miss - row_na_ratio)
      new_excess <- pmax(0, new_row_miss - row_na_ratio)
      improvement <- mean(old_excess - new_excess)  # Average improvement per row
      
      # Also add own excess as penalty (normalized)
      own_excess <- max(0, col_na_count[c] / n_rows - col_na_ratio)
      
      return(improvement + own_excess)
    }, numeric(1))
    
    list(row_scores = row_scores, col_scores = col_scores)
  }

  # ---- Remove phase ----
  remove_phase <- function(current_rows, current_cols, row_na_count, col_na_count) {
    repeat {
      n_rows <- length(current_rows)
      n_cols <- length(current_cols)
      if (n_rows == 0 || n_cols == 0) break

      # Compute missing rates from cached counts (O(n_rows + n_cols) instead of O(n_rows * n_cols))
      row_miss <- row_na_count[current_rows] / n_cols
      col_miss <- col_na_count[current_cols] / n_rows

      # Find candidates exceeding thresholds
      cand_row_logical <- row_miss > row_na_ratio
      cand_col_logical <- col_miss > col_na_ratio
      
      if (!adaptive_scoring && sum(cand_row_logical) + sum(cand_col_logical) == 0) break

      # Build candidate score table
      score_list <- list()
      
      if (adaptive_scoring) {
        # Use adaptive scoring: consider all rows and columns
        scores <- compute_adaptive_scores(current_rows, current_cols, row_na_count, col_na_count)
        
        # Only consider candidates with positive scores (actual improvement)
        if (any(scores$row_scores > 0)) {
          cand_rows <- current_rows[scores$row_scores > 0]
          score_list <- c(score_list, list(data.frame(
            type = "row", id = cand_rows,
            score = scores$row_scores[scores$row_scores > 0],
            stringsAsFactors = FALSE
          )))
        }
        if (any(scores$col_scores > 0)) {
          cand_cols <- current_cols[scores$col_scores > 0]
          score_list <- c(score_list, list(data.frame(
            type = "col", id = cand_cols,
            score = scores$col_scores[scores$col_scores > 0],
            stringsAsFactors = FALSE
          )))
        }
        if (length(score_list) == 0) break  # No beneficial removal found
      } else {
        # Traditional scoring: only consider exceeders
        if (sum(cand_row_logical) + sum(cand_col_logical) == 0) break
        
        if (sum(cand_row_logical) > 0) {
          cand_rows <- current_rows[cand_row_logical]
          score_list <- c(score_list, list(data.frame(
            type = "row", id = cand_rows,
            score = (row_miss[cand_row_logical] - row_na_ratio) / row_priority,
            stringsAsFactors = FALSE
          )))
        }
        if (sum(cand_col_logical) > 0) {
          cand_cols <- current_cols[cand_col_logical]
          score_list <- c(score_list, list(data.frame(
            type = "col", id = cand_cols,
            score = col_miss[cand_col_logical] - col_na_ratio,
            stringsAsFactors = FALSE
          )))
        }
      }
      tmp_df <- do.call(rbind, score_list)

      # Select top candidates based on speedup_ratio
      n_to_remove <- max(1L, round(speedup_ratio * nrow(tmp_df)))
      best <- tmp_df[order(tmp_df$score, decreasing = TRUE)[seq_len(n_to_remove)], , drop = FALSE]

      # Remove selected rows and update counts incrementally
      rows_to_remove <- best$id[best$type == "row"]
      cols_to_remove <- best$id[best$type == "col"]

      if (length(rows_to_remove) > 0) {
        # When rows are removed, decrease col_na_count by the NA contributions of those rows
        col_na_count <- col_na_count - colSums(na_mat[rows_to_remove, , drop = FALSE])
        current_rows <- setdiff(current_rows, rows_to_remove)
      }
      if (length(cols_to_remove) > 0) {
        # When columns are removed, decrease row_na_count by the NA contributions of those columns
        row_na_count <- row_na_count - rowSums(na_mat[, cols_to_remove, drop = FALSE])
        current_cols <- setdiff(current_cols, cols_to_remove)
      }

      if (length(current_rows) == 0 || length(current_cols) == 0) break
    }
    list(current_rows = current_rows, current_cols = current_cols,
         row_na_count = row_na_count, col_na_count = col_na_count)
  }

  # ---- Add phase ----
  add_phase <- function(current_rows, current_cols, row_na_count, col_na_count) {
    repeat {
      n_rows <- length(current_rows)
      n_cols <- length(current_cols)
      if (n_rows == 0 || n_cols == 0) break

      removed_rows <- setdiff(seq_len(ori_nrow), current_rows)
      removed_cols <- setdiff(seq_len(ori_ncol), current_cols)
      if (length(removed_rows) == 0 && length(removed_cols) == 0) break

      # Compute missing rates for removed rows/cols using cached counts
      # For a removed row r: its NA count within current_cols = row_na_count[r] (already reflects current_cols)
      # But we need to recompute since row_na_count only tracks active rows' counts
      # Recompute for removed rows: sum of NA in that row restricted to current_cols
      row_miss_removed <- if (length(removed_rows) > 0) {
        rowSums(na_mat[removed_rows, current_cols, drop = FALSE]) / n_cols
      } else numeric(0)

      col_miss_removed <- if (length(removed_cols) > 0) {
        colSums(na_mat[current_rows, removed_cols, drop = FALSE]) / n_rows
      } else numeric(0)

      # Find candidates that satisfy thresholds
      cand_row_logical <- row_miss_removed <= row_na_ratio
      cand_col_logical <- col_miss_removed <= col_na_ratio
      if (sum(cand_row_logical) + sum(cand_col_logical) == 0) break

      # Build candidate score table
      score_list <- list()
      if (sum(cand_row_logical) > 0) {
        cand_rows <- removed_rows[cand_row_logical]
        score_list <- c(score_list, list(data.frame(
          type = "row", id = cand_rows,
          score = (row_miss_removed[cand_row_logical] - row_na_ratio) / row_priority,
          stringsAsFactors = FALSE
        )))
      }
      if (sum(cand_col_logical) > 0) {
        cand_cols <- removed_cols[cand_col_logical]
        score_list <- c(score_list, list(data.frame(
          type = "col", id = cand_cols,
          score = col_miss_removed[cand_col_logical] - col_na_ratio,
          stringsAsFactors = FALSE
        )))
      }
      tmp_df <- do.call(rbind, score_list)

      # Filter: only keep candidates that don't break constraints after addition
      # For a candidate row: adding it must not cause any current column to exceed col_na_ratio
      #   i.e., for each col j in current_cols: (col_na_count[j] + na_mat[cand_row, j]) / (n_rows + 1) <= col_na_ratio
      #   equivalently: col_na_count[j] + na_mat[cand_row, j] <= col_na_ratio * (n_rows + 1)
      real_candidate_ids <- integer(0)
      for (i in seq_len(nrow(tmp_df))) {
        if (tmp_df$type[i] == "row") {
          cand_row <- tmp_df$id[i]
          new_col_na <- col_na_count[current_cols] + na_mat[cand_row, current_cols]
          if (max(new_col_na) <= col_na_ratio * (n_rows + 1)) {
            real_candidate_ids <- c(real_candidate_ids, i)
          }
        } else {
          cand_col <- tmp_df$id[i]
          new_row_na <- row_na_count[current_rows] + na_mat[current_rows, cand_col]
          if (max(new_row_na) <= row_na_ratio * (n_cols + 1)) {
            real_candidate_ids <- c(real_candidate_ids, i)
          }
        }
      }
      if (length(real_candidate_ids) == 0) break
      tmp_df <- tmp_df[real_candidate_ids, , drop = FALSE]

      # Select the best candidate (closest to threshold)
      best <- tmp_df[which.max(tmp_df$score), , drop = FALSE]

      # Add and update counts incrementally
      if (best$type == "row") {
        added_row <- best$id
        col_na_count <- col_na_count + na_mat[added_row, ]
        row_na_count[added_row] <- sum(na_mat[added_row, current_cols])
        current_rows <- union(current_rows, added_row)
      } else {
        added_col <- best$id
        row_na_count <- row_na_count + na_mat[, added_col]
        col_na_count[added_col] <- sum(na_mat[current_rows, added_col])
        current_cols <- union(current_cols, added_col)
      }
    }
    list(current_rows = current_rows, current_cols = current_cols)
  }

  # ---- Execute phases ----
  remove_result <- remove_phase(current_rows, current_cols, row_na_count, col_na_count)
  add_result <- add_phase(
    remove_result$current_rows, remove_result$current_cols,
    remove_result$row_na_count, remove_result$col_na_count
  )

  # ---- Return ----
  if (return_index) {
    list(rows = add_result$current_rows, cols = add_result$current_cols)
  } else {
    df[add_result$current_rows, add_result$current_cols, drop = FALSE]
  }
}


#' Get the maximum missing rate of rows and columns.
#' @description Get the maximum missing rate of rows and columns.
#' @param df A data frame.
#' @returns A list that contains the maximum missing rate of rows and columns.
#' @export
#' @examples
#' data(cancer, package = "survival")
#' max_missing_rates(cancer)
max_missing_rates <- function(df) {
  tmp <- is.na(df)
  list(
    row = na_max(rowMeans(tmp)),
    col = na_max(colMeans(tmp))
  )
}
