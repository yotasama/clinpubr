#' Standardize units of numeric data.
#' @description Standardize units of numeric data, especially for data of medical records with different units.
#' @param df A data frame of medical records that contains test subject, value, and unit cols.
#' @param subject_col The name of the subject column.
#' @param value_col The name of the value column.
#' @param unit_col The name of the unit column.
#' @param change_list A list of lists, each list contains the following elements:
#'   \itemize{
#'     \item `subject`: The subject to be standardized.
#'     \item `target_unit`: The target unit to be standardized to. If not specified,
#'       the function will use the most common unit in the data (retrieved by `first_mode()`).
#'     \item `units2change`: The units to be changed. If not specified, the function will use
#'       all units except the target unit. Must be specified to apply different `coeffs`.
#'     \item `coeffs`: The coefficients to be used for the conversion. If not specified, the
#'       function will use 1 for all units to be changed.
#'   }
#'
#' @returns A data frame with subject units standardized.
#' @export
#' @examples
#' df <- data.frame(subject = c("a", "a", "b", "b", "b", "c", "c"), value = c(1, 2, 3, 4, 5, 6, 7),
#'                  unit = c(NA, "x", "x", "x", "y", "a", "b"))
#' change_list <- list(
#'   list(subject = "a", target_unit = "x", units2change = c(NA), coeffs = c(20)),
#'   list(subject = "b"),
#'   list(subject = "c", target_unit = "b")
#' )
#' unit_standardize(df, subject_col = "subject", value_col = "value", unit_col = "unit",
#'                  change_list = change_list)
unit_standardize <- function(df, subject_col, value_col, unit_col, change_list) {
  for (i in seq_along(change_list)) {
    flag <- df[, subject_col] %in% change_list[[i]]$subject
    df[flag, c(value_col, unit_col)] <- unit_standardize_(df[flag, c(value_col, unit_col)],
      target_unit = change_list[[i]]$target_unit,
      units2change = change_list[[i]]$units2change,
      coeffs = change_list[[i]]$coeffs
    )
  }
  return(df)
}

unit_standardize_ <- function(df, target_unit = NULL, units2change = NULL, coeffs = NULL) {
  if (is.null(target_unit)) {
    if (length(unique(df[, 2])) > 1) {
      target_unit <- first_mode(df[, 2])
    } else {
      return(df)
    }
  }
  if (length(target_unit) > 1) {
    stop("too many targets!")
  }
  if (is.null(units2change)) {
    units2change <- setdiff(unique(df[, 2]), target_unit)
  }
  if (!is.null(coeffs) && length(units2change) != length(coeffs)) {
    stop("coeffs should have the same length as units2change!")
  } else if (is.null(coeffs)) {
    coeffs <- rep(1, length(units2change))
  }
  for (i in seq_along(units2change)) {
    flag <- df[, 2] %in% units2change[i]
    df[flag, 1] <- as.numeric(df[flag, 1]) * coeffs[i]
  }
  df[, 2] <- target_unit
  return(df)
}
