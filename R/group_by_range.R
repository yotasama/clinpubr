#' Group Sorted Vector by Range
#'
#' @description Divide a numeric vector into groups such that the range
#'   (`max - min`) within each group does not exceed a given threshold.
#'   This is useful for clustering continuous values into contiguous bins
#'   controlled by a maximum span.
#'
#' @param x A sorted numeric vector, Date vector, or POSIXt vector.
#' @param max_range A non-negative numeric threshold for the within-group range.
#'
#' @returns An integer vector of group IDs (1-indexed).
#' @export
#' @import Rcpp
#' @useDynLib clinpubr, .registration = TRUE
#'
#' @examples
#' group_by_range(c(1, 2, 5, 7, 10, 11, 11, 11), 3)
group_by_range <- function(x, max_range) {
  if (isTRUE(is.unsorted(x))) {
    stop("`x` must be sorted in non-decreasing order.")
  }
  if (!is.numeric(max_range) || length(max_range) != 1 || max_range < 0) {
    stop("`max_range` must be a single non-negative numeric value.")
  }
  group_by_range_cpp(x, max_range)
}
