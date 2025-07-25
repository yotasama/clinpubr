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
#' @param ... Other arguments passed to `base::cut()`.
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
#' cut_by(rnorm(100), c(1 / 3, 2 / 3), breaks_as_quantiles = TRUE, label_type = "LMH")
cut_by <- function(x, breaks,
                   breaks_as_quantiles = FALSE,
                   group = NULL,
                   labels = NULL,
                   label_type = "ori",
                   ...) {
  if(breaks_as_quantiles && !is.null(group)){
    if(label_type != "LMH"){
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
      # Calculate quantiles by group
      data_df <- data.frame(x = x, group = group, idx = seq_along(x))
      
      # Function to calculate quantiles and cut for a single group
      cut_group <- function(group_data, breaks, ...) {
        q <- quantile(group_data$x, probs = c(0, breaks, 1), na.rm = TRUE, names = FALSE)
        
        # Ensure breaks are strictly increasing
        for (i in 2:length(q)) {
          if (q[i] <= q[i - 1]) {
            q[i] <- q[i - 1] + 1e-8
          }
        }
        
        # Cut the data and return with original index
        result <- cut(group_data$x, breaks = q, right = FALSE, include.lowest = TRUE, labels = cut_labels, ...)
        data.frame(result = result, idx = group_data$idx)
      }
      
      # Split data by group, apply cut_group to each, and combine results
      groups <- split(data_df, data_df$group)
      cut_results <- lapply(groups, cut_group, breaks = breaks, ...)
      combined_results <- do.call(rbind, cut_results)
      
      # Order results to match original data order
      res <- combined_results[order(combined_results$idx), "result"]
    } else {
      q <- quantile(x, probs = c(0, breaks, 1), na.rm = TRUE, names = FALSE)
      
      # Ensure breaks are strictly increasing
      for (i in 2:length(q)) {
        if (q[i] <= q[i - 1]) {
          q[i] <- q[i - 1] + 1e-8
        }
      }
      
      res <- cut(x, breaks = q, right = FALSE, include.lowest = TRUE, ...)
    }
  } else {
    q <- c(min(x, na.rm = TRUE), breaks, max(x, na.rm = TRUE))
    
    # Ensure breaks are strictly increasing
    for (i in 2:length(q)) {
      if (q[i] <= q[i - 1]) {
        q[i] <- q[i - 1] + 1e-8
      }
    }
    
    res <- cut(x, breaks = q, right = FALSE, include.lowest = TRUE, ...)
  }
  if (!is.null(cut_labels)) {
    if (label_type == "combined") {
      levels(res) <- paste(cut_labels, levels(res), sep = " ")
    } else {
      levels(res) <- cut_labels
    }
  }
  res
}
