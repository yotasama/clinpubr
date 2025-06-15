#' Mark possible outliers with MAD.
#' @description Mark possible outliers using the median absolute deviation (MAD) method.
#' @param x A numeric vector.
#' @param constant The constant multiplier for the MAD. Default is 1.4826 * 3.
#' @details The function calculates the median absolute deviation of the input vector and uses
#'   it to identify possible outliers. The default constant multiplier is 1.4826 * 3, which gives
#'   approximately the `3 sigma` of the normal distribution.
#' @seealso \code{\link[stats]{mad}}
#' @returns A logical vector.
#' @export
#' @examples
#' x <- c(1, 2, 3, 4, 5, 100)
#' mad_outlier(x)
mad_outlier <- function(x, constant = 1.4826 * 3) {
  sample_mad=stats::mad(x, constant = constant, na.rm = TRUE)
  return(abs(x - stats::median(x, na.rm = TRUE)) > sample_mad)
}
