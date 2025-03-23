#' Convert Numeric to Factor
#' @description Divide numeric data into different groups. Easier to use than `base::cut()`.
#' @param x A numeric vector.
#' @param breaks A numeric vector of internal cut points. If `breaks_as_quantiles` is TRUE, this
#'   is a proportion of the data. See `Details`.
#' @param breaks_as_quantiles If TRUE, `breaks` is interpreted as a proportion of the data.
#' @param labels A vector of labels for the resulting factor levels.
#' @param label_type If `labels` is `NULL`, this sets the label type. `"ori"` for original labels,
#'   `"LMH"` for "Low Medium High" style.
#'
#' @details `cut_by()` is a wrapper for `base::cut()`. Compared with the argument `breaks` in `base::cut()`,
#'   `breaks` here automatically sets the minimum and maximum to `-Inf` and `Inf`.
#' @note The argument `right` in `base::cut()` is always set to `FALSE`, which means the levels follow the
#'   left closed right open convention.
#' @returns A factor.
#' @export
#' @examples
#' set.seed(123)
#' cut_by(rnorm(100), c(0, 1, 2))
#' cut_by(rnorm(100), c(1/3,2/3), breaks_as_quantiles = TRUE, label_type = "LMH")
cut_by <- function(x, breaks,
                   breaks_as_quantiles = FALSE,
                   labels = NULL,
                   label_type = "ori", # 'LMH' to set to 'Low Medium High' style
                   ...) {
  if (!is.null(labels)) {
    cut.labels <- labels
  } else if (label_type == "LMH") {
    if (length(breaks) == 1) {
      cut.labels <- c("Low", "High")
    } else if (length(breaks) == 2) {
      cut.labels <- c("Low", "Medium", "High")
    }
  } else {
    cut.labels <- NULL
  }
  if (breaks_as_quantiles) {
    cut(x, c(quantile(x, c(0, breaks, 1), na.rm = T)),
      right = F,
      include.lowest = T, labels = cut.labels, ...
    )
  } else {
    cut(x, c(-Inf, breaks, Inf),
      right = F,
      include.lowest = T, labels = cut.labels, ...
    )
  }
}
