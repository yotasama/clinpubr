#' Count the number of excluded samples at each step
#'
#' @description This function sequentially applies exclusion criteria to a data frame and counts the number of samples
#'   removed at each step.
#'
#' @param .df A data frame.
#' @param ... Exclusion criteria. Logical expressions that define which rows to exclude.
#' @param .criteria_names An optional character vector of names for the criteria. If `NULL`, the expressions themselves
#'   are used as names.
#' @param .na_exclude A logical value. If `TRUE`, rows where the criterion evaluates to `NA` will be excluded, and a
#'   warning will be issued. Defaults to `FALSE`, where `NA` values are not excluded.
#' @return A data frame with two columns: 'Criteria' and 'N', showing the number of samples at the start, the number
#'   excluded at each step, and the final number remaining.
#' @export
#' @examples
#' cohort <- data.frame(
#'   age = c(17, 25, 30, NA, 50, 60),
#'   sex = c("M", "F", "F", "M", "F", "M"),
#'   value = c(1, NA, 3, 4, 5, NA),
#'   dementia = c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE)
#' )
#' exclusion_count(
#'   cohort,
#'   age < 18,
#'   is.na(value),
#'   dementia == TRUE,
#'   .criteria_names = c(
#'     "Age < 18 years",
#'     "Missing value",
#'     "History of dementia"
#'   )
#' )
exclusion_count <- function(.df, ..., .criteria_names = NULL, .na_exclude = TRUE) {
  criteria <- rlang::enquos(...)
  n_criteria <- length(criteria)

  if (n_criteria == 0) {
    stop("At least one exclusion criterion must be provided.")
  }

  if (is.null(.criteria_names)) {
    .criteria_names <- vapply(criteria, rlang::quo_text, character(1))
  } else if (length(.criteria_names) != n_criteria) {
    stop("`.criteria_names` must have the same length as the number of criteria.")
  }

  initial_n <- nrow(.df)
  results <- data.frame(
    Criteria = "Initial N",
    N = initial_n,
    stringsAsFactors = FALSE
  )

  current_df <- .df
  for (i in seq_len(n_criteria)) {
    n_before <- nrow(current_df)
    # Evaluate the exclusion criterion
    is_excluded <- rlang::eval_tidy(criteria[[i]], data = current_df)

    # Handle NA values
    na_indices <- is.na(is_excluded)
    if (any(na_indices)) {
      if (.na_exclude) {
        warning(paste0(
          "Criterion '", .criteria_names[i], "' resulted in NA values. ",
          "These rows have been excluded by default. ",
          "Consider adding an explicit check for missing values (e.g., is.na(variable)) as a preceding criterion."
        ))
        is_excluded[na_indices] <- TRUE # Exclude NAs
      } else {
        is_excluded[na_indices] <- FALSE # Do not exclude NAs
      }
    }

    # Keep rows that are NOT excluded
    current_df <- current_df[!is_excluded, , drop = FALSE]
    n_after <- nrow(current_df)
    n_excluded <- n_before - n_after

    results <- rbind(results, data.frame(
      Criteria = .criteria_names[i],
      N = n_excluded,
      stringsAsFactors = FALSE
    ))
  }

  final_n <- nrow(current_df)
  results <- rbind(results, data.frame(
    Criteria = "Final N",
    N = final_n,
    stringsAsFactors = FALSE
  ))

  return(results)
}
