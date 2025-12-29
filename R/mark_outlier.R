#' Mark possible outliers using different methods.
#' @description Mark possible outliers in a numeric vector using various methods.
#'   These functions return a logical vector indicating which values are outliers.
#' @param x A numeric vector.
#' @param threshold The threshold value for detecting outliers. Defaults depend on the method:
#'   - For MAD method: 1.4826 * 3 (approximately 3 standard deviations)
#'   - For IQR method: 1.5 (Tukey's rule)
#'   - For Z-score method: 3 (3 standard deviations)
#' @details
#'   - **MAD method**: Uses median absolute deviation to identify outliers.
#'     Values with absolute deviation from the median greater than the threshold are considered outliers.
#'   - **IQR method**: Uses interquartile range to identify outliers.
#'     Values below Q1 - threshold * IQR or above Q3 + threshold * IQR are considered outliers.
#'   - **Z-score method**: Uses standardized Z-scores to identify outliers.
#'     Values with an absolute Z-score greater than the threshold are considered outliers.
#' @returns A logical vector indicating which values are outliers.
#' @export
#' @examples
#' x <- c(1, 2, 3, 4, 5, 100, NA)
#' mad_outlier(x)
#' iqr_outlier(x, threshold = 2.0)
#' zscore_outlier(x, threshold = 2.5)
mad_outlier <- function(x, threshold = 1.4826 * 3) {
  sample_mad <- stats::mad(x, constant = 1.4826, na.rm = TRUE)
  return(abs(x - stats::median(x, na.rm = TRUE)) > threshold * sample_mad)
}

#' @rdname mad_outlier
#' @export
iqr_outlier <- function(x, threshold = 1.5) {
  q1 <- stats::quantile(x, 0.25, na.rm = TRUE)
  q3 <- stats::quantile(x, 0.75, na.rm = TRUE)
  iqr_val <- q3 - q1
  lower_bound <- q1 - threshold * iqr_val
  upper_bound <- q3 + threshold * iqr_val
  return(x < lower_bound | x > upper_bound)
}

#' @rdname mad_outlier
#' @export
zscore_outlier <- function(x, threshold = 3) {
  z_scores <- c(scale(x))
  return(abs(z_scores) > threshold)
}

#' Detect outliers in a numeric vector.
#' @description Detect outliers in a numeric vector using various methods.
#' @param x A numeric vector.
#' @param method The method to use for outlier detection. One of "mad", "iqr", or "zscore".
#' @param threshold The threshold value for detecting outliers. Defaults depend on the method.
#' @seealso \code{\link{mad_outlier}}, \code{\link{iqr_outlier}}, \code{\link{zscore_outlier}}
#' @details This function provides a unified interface for detecting outliers using different methods.
#'   - "mad": Median absolute deviation method
#'   - "iqr": Interquartile range method
#'   - "zscore": Z-score method
#' @returns A list containing:
#'   - outlier_mask: Logical vector indicating outliers, `NA` for missing values
#'   - outlier_count: Number of outliers detected
#'   - outlier_pct: Percentage of outliers in the data
#'   - summary: Summary statistics including:
#'     - Before removing outliers: max, min, variance
#'     - After removing outliers: max, min, variance
#'     - Method-specific details
#' @export
#' @examples
#' x <- c(1, 2, 3, 4, 5, 100)
#' detect_outliers(x, method = "iqr")
detect_outliers <- function(x, method = "iqr", threshold = NULL) {
  # Validate input
  if (!is.numeric(x)) {
    stop("Input must be a numeric vector")
  }

  x_no_na <- x[!is.na(x)]
  if (length(x_no_na) == 0) {
    return(list(
      outlier_mask = rep(FALSE, length(x)),
      outlier_count = 0,
      outlier_pct = 0,
      summary = list(
        before = list(
          min = NA,
          max = NA,
          variance = NA
        ),
        after = list(
          min = NA,
          max = NA,
          variance = NA
        ),
        method = method
      )
    ))
  }

  # Detect outliers based on method
  outlier_mask_no_na <- switch(method,
    "mad" = mad_outlier(x_no_na, threshold = threshold),
    "iqr" = iqr_outlier(x_no_na, threshold = threshold),
    "zscore" = zscore_outlier(x_no_na, threshold = threshold),
    stop("Invalid outlier method. Use 'mad', 'iqr', or 'zscore'")
  )

  # Calculate statistics before removing outliers
  before_stats <- list(
    min = min(x_no_na),
    max = max(x_no_na),
    variance = stats::var(x_no_na, na.rm = TRUE)
  )

  # Calculate statistics after removing outliers
  x_no_outliers <- x_no_na[!outlier_mask_no_na]
  after_stats <- list(
    min = ifelse(length(x_no_outliers) > 0, min(x_no_outliers), NA),
    max = ifelse(length(x_no_outliers) > 0, max(x_no_outliers), NA),
    variance = ifelse(length(x_no_outliers) > 1, stats::var(x_no_outliers), 0)
  )

  # Combine all summary information
  summary_info <- list(
    before = before_stats,
    after = after_stats,
    method = method,
    threshold = threshold
  )

  # Create full outlier mask (including NA values)
  outlier_mask <- rep(NA, length(x))
  outlier_mask[!is.na(x)] <- outlier_mask_no_na

  # Calculate outlier statistics
  outlier_count <- sum(outlier_mask_no_na)
  outlier_pct <- round(outlier_count / length(x_no_na) * 100, 2)

  # Return results
  return(list(
    outlier_mask = outlier_mask,
    outlier_count = outlier_count,
    outlier_pct = outlier_pct,
    summary = summary_info
  ))
}
