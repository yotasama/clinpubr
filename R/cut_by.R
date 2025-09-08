#' Convert Numeric to Factor
#' @description Divide numeric data into different groups. Easier to use than `base::cut()`.
#' @param x A numeric vector.
#' @param breaks A numeric vector of internal cut points. If `breaks_as_quantiles` is TRUE, this
#'   is a proportion of the data. See `Details`.
#' @param breaks_as_quantiles If `TRUE`, `breaks` is interpreted as a proportion of the data.
#' @param group A character vector of the same length as `x`, used to group the data before cut.
#'   Only effective when `breaks_as_quantiles` is `TRUE`.
#' @param labels A vector of labels for the resulting factor levels.
#' @param label_type If `labels` is `NULL`, this sets the label type. `"ori"` for original labels,
#'   `"LMH"` for "Low Medium High" style. `"combined"` labels that combine `"LMH"` type or provided `labels`
#'   with the original range labels. Only `"LMH"` is supported when `group` is specified.
#' @param right logical, indicating if the intervals should be closed on the right (and open on the left) or
#'   vice versa. Note that the default is `FALSE`, which is different from `base::cut()`.
#' @param drop_empty If `TRUE`, drop empty levels.
#' @param verbose If `TRUE`, print the cut points.
#' @param ... Other arguments passed to `base::cut()`.
#'
#' @details `cut_by()` is a wrapper for `base::cut()`. Compared with the argument `breaks` in `base::cut()`,
#'   `breaks` here automatically sets the minimum and maximum. `breaks` outside the range of `x` are not allowed.
#' @note The argument `right` in `base::cut()` is always set to `FALSE`, which means the levels follow the
#'   left closed right open convention.
#' @returns A factor.
#' @export
#' @examples
#' set.seed(123)
#' cut_by(rnorm(100), c(0, 1, 2))
#' cut_by(rnorm(100), c(1 / 3, 2 / 3), breaks_as_quantiles = TRUE, label_type = "LMH")
cut_by <- function(x, breaks,
                   breaks_as_quantiles = FALSE,
                   group = NULL,
                   labels = NULL,
                   label_type = "ori",
                   right = FALSE,
                   drop_empty = TRUE,
                   verbose = FALSE,
                   ...) {
  if (breaks_as_quantiles && !is.null(group)) {
    if (label_type != "LMH") {
      warning("`label_type` is set to `LMH` since `group` is specified")
      label_type <- "LMH"
    }
  }
  if (!is.null(labels)) {
    cut_labels <- labels
  } else if (label_type %in% c("combined", "LMH")) {
    if (length(breaks) == 1) {
      cut_labels <- c("Low", "High")
    } else if (length(breaks) == 2) {
      cut_labels <- c("Low", "Medium", "High")
    }
  } else {
    cut_labels <- NULL
  }
  if (!is.null(cut_labels) && length(cut_labels) != length(breaks) + 1) {
    stop("the number of labels and levels does not match")
  }
  m1 <- min(x, na.rm = TRUE)
  m2 <- max(x, na.rm = TRUE)
  if (breaks_as_quantiles) {
    if (any(breaks < 0) || any(breaks > 1)) {
      stop("Some of quantiles are not in the range of 0 to 1!")
    }
  } else {
    if (any(breaks < m1) || any(breaks > m2)) {
      stop("Some of `breaks` are not in the range of `x`!")
    }
  }

  if (breaks_as_quantiles) {
    if (!is.null(group)) {
      groups <- unique(group)
      res <- vector("character", length(group))
      for (j in seq_along(groups)) {
        flag <- group %in% groups[j]
        q <- quantile(x[flag], probs = c(0, breaks, 1), na.rm = TRUE, names = FALSE)
        q <- force_increasing(q)

        if (verbose) {
          message("Quantile breaks for group ", groups[j], ": ", paste(q[-c(1, length(q))], collapse = ", "))
        }
        res[flag] <- cut(x[flag], breaks = q, right = right, labels = cut_labels, include.lowest = TRUE)
      }
      res <- factor(res)
    } else {
      q <- quantile(x, probs = c(0, breaks, 1), na.rm = TRUE, names = FALSE)
      q <- force_increasing(q)

      if (verbose) {
        message("Quantiles breaks: ", paste(q, collapse = ", "))
      }
      res <- cut(x, breaks = q, right = right, include.lowest = TRUE, ...)
    }
  } else {
    q <- c(min(x, na.rm = TRUE), breaks, max(x, na.rm = TRUE))
    q <- force_increasing(q)

    res <- cut(x, breaks = q, right = right, include.lowest = TRUE, ...)
  }
  if (!is.null(cut_labels)) {
    if (label_type == "combined") {
      levels(res) <- paste(cut_labels, levels(res), sep = " ")
    } else {
      levels(res) <- cut_labels
    }
  }
  if(drop_empty){
    prev_levels = levels(res)
    res <- droplevels(res)
    after_levels = levels(res)
    if(length(prev_levels) != length(after_levels)){
      warning("Empty levels are dropped: ", paste(setdiff(prev_levels, after_levels), collapse = ", "))
    }
  }
  res
}

force_increasing <- function(x, increment = 1e-8) {
  q <- x
  for (i in 2:length(q)) {
    if (q[i] <= q[i - 1]) {
      q[i] <- q[i - 1] + increment
    }
  }
  q
}
