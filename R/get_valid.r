#' Get one valid value from vector.
#' @description Extract one valid (non-NA) value from a vector.
#' @param x A vector.
#' @param mode The mode of the valid value to extract. `"first"` extracts the first valid value,
#'   `"last"` extracts the last valid value, and `"mid"` extracts the middle valid value.
#' @param disjoint If TRUE, the values extracted by the three modes are forced to be different.
#'   This behavior might be desired when trying to extract different values with different modes.
#'   The three modes extract values in the sequence: "first", "last", "mid".
#'
#' @returns A single valid value from the vector. `NA` if all values are invalid.
#' @export
#' @examples
#' get_valid(c(NA, 1, 2, NA, 3, NA, 4))
#' get_valid(c(NA, 1, NA), mode = "last", disjoint = TRUE)
get_valid <- function(x, mode = c("first", "mid", "last"), disjoint = FALSE) {
  mode <- match.arg(mode)
  tmp <- na.omit(x)
  if (length(tmp) > 0) {
    if (disjoint) {
      if (mode == "first") {
        tmp[1]
      } else if ((mode == "last") && (length(tmp) > 1)) {
        tmp[length(tmp)]
      } else if ((mode == "mid") && (length(tmp) > 2)) {
        tmp[round((length(tmp) + 0.5) / 2)]
      } else {
        NA
      }
    } else {
      if (mode == "first") {
        tmp[1]
      } else if ((mode == "last")) {
        tmp[length(tmp)]
      } else if ((mode == "mid")) {
        tmp[round((length(tmp) + 0.5) / 2)]
      } else {
        NA
      }
    }
  } else {
    NA
  }
}
