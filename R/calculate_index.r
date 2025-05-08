#' Calculate index based on conditions
#'
#' @param .df A data frame
#' @param ... Conditions to evaluate
#' @param .weight Weight for each condition, should be of length 1 or equal to the number of conditions.
#' @param .na_replace Value to replace `NA`, should be of length 1 or equal to the number of conditions.
#' @return A numeric vector of index scores
#' @export
#' @examples
#' df <- data.frame(x = c(1, 2, 3, 4, 5), y = c(1, 2, NA, 4, NA))
#' calculate_index(df, x > 3, y < 3, .weight = c(1, 2), .na_replace = 0)
calculate_index <- function(.df, ..., .weight = 1, .na_replace = 0) {
  conditions <- rlang::enquos(...)
  n_conds <- length(conditions)

  if (n_conds == 0) stop("Expressions must be provided")
  if (!length(.weight) %in% c(1, n_conds)) {
    stop("`.weight` must be of length 1 or equal to the number of conditions")
  }
  if (!length(.na_replace) %in% c(1, n_conds)) {
    stop("`.na_replace` must be of length 1 or equal to the number of conditions")
  }

  weight <- rep(.weight, length.out = n_conds)
  na_replace <- rep(.na_replace, length.out = n_conds)

  total_score <- rep(0, nrow(.df))

  for (i in seq_len(n_conds)) {
    cond_result <- rlang::eval_tidy(conditions[[i]], data = .df)
    num_vec <- as.integer(cond_result)
    current_score <- num_vec * weight[i]

    current_score <- ifelse(
      is.na(current_score),
      na_replace[i],
      current_score
    )

    total_score <- total_score + current_score
  }
  total_score
}
