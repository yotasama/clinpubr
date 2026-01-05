#' Calculate C-index for survival data
#' @description Calculate C-index for survival data. It's a wrapper function for `Hmisc::rcorr.cens()`.
#' @param data A data frame containing the survival time, event indicator, and marker variable.
#' @param time_var A string specifying the name of the survival time variable in the data frame.
#' @param event_var A string specifying the name of the event indicator variable in the data frame.
#' @param marker_var A string specifying the name of the marker variable in the data frame.
#' @returns The C-index value.
#' @export
#' @examples
#' # Calculate C-index using lung dataset from survival package
#' data(cancer, package = "survival")
#' # Use age as the marker variable
#' calc_cindex(lung, "time", "status", "age")
calc_cindex <- function(data, time_var, event_var, marker_var) {
  res <- Hmisc::rcorr.cens(
    data[[marker_var]],
    survival::Surv(data[[time_var]], data[[event_var]])
  )
  c_index <- res["C Index"]
  if (c_index < 0.5) {
    c_index <- 1 - c_index
    warning("C-index is less than 0.5, possibly indicating a reversed relationship. Reversed to 1 - C-index.")
  }

  c_index
}

#' Calculate and plot time-dependent ROC curves
#' @description Calculate time-dependent ROC curves using the `timeROC` package and plot them using `ggplot2`.
#' @param data A data frame containing the survival time, event indicator, and marker variable.
#' @param time_var A string specifying the name of the survival time variable in the data frame.
#' @param event_var A string specifying the name of the event indicator variable in the data frame.
#' @param marker_var A string specifying the name of the marker variable in the data frame.
#' @param times A numeric vector of times at which to compute the time-dependent ROC curves.
#' @param weighting A character string specifying the weighting method. Default is "marginal".
#'   See `timeROC::timeROC()` for details.
#' @param cause The value of the event indicator that denotes the event of interest. Default is `1`.
#' @param colors A vector of colors to use for the ROC curves. If NULL, uses default colors.
#' @param title A logical value indicating whether to include a title. Default is `FALSE`.
#' @param save_plot A logical value indicating whether to save the plot to a file. Default is `FALSE`.
#' @param filename A string specifying the filename to save the plot. Default is "time_roc.png".
#' @returns A list containing:
#'   - time_roc: The timeROC result object.
#'   - plot: A ggplot object of the time-dependent ROC curves.
#' @export
#' @examples
#' # Plot time-dependent ROC curves using lung dataset from survival package
#' library(survival)
#' data(cancer, package = "survival")
#' # Use age as the marker variable, plot at 6, 12, and 24 months
#' lung$status <- lung$status == 2
#' result <- time_roc_plot(lung, "time", "status", "age", times = c(180, 365, 730))
#' result$plot
#'
#' # Save the plot to a file
#' # time_roc_plot(lung, "time", "status", "age", times = c(180, 365, 730), save_plot = TRUE)
time_roc_plot <- function(data, time_var, event_var, marker_var, times = c(12, 36, 60),
                          weighting = "marginal", cause = 1, colors = NULL, title = FALSE,
                          save_plot = FALSE, filename = "time_roc.png") {
  check_package("timeROC", "time-dependent ROC curve calculations")
  check_package("ggplot2", "ggplot2 plotting")

  # Calculate time-dependent ROC curves
  time_roc <- timeROC::timeROC(
    T = data[[time_var]],
    delta = data[[event_var]],
    marker = data[[marker_var]],
    cause = cause,
    weighting = weighting,
    times = times,
    ROC = TRUE
  )

  if (is.null(colors)) {
    colors <- emp_colors
  }

  # Filter out automatically added t=0 time point
  # This happens when only one time point is specified
  original_times <- times
  # Get the indices of times that are in the original user-specified times
  # Also exclude t=0 if it's not in the original times
  valid_indices <- which(time_roc$times %in% original_times | !(time_roc$times == 0))

  # If valid_indices is empty (shouldn't happen), use all indices
  if (length(valid_indices) == 0) {
    valid_indices <- seq_along(time_roc$times)
  }

  # Extract ROC data for each valid time point
  roc_data_list <- list()
  valid_time_roc_times <- time_roc$times[valid_indices]
  valid_time_roc_auc <- time_roc$AUC[valid_indices]

  for (i in seq_along(valid_indices)) {
    # Get actual index in time_roc object
    actual_index <- valid_indices[i]

    # Get total number of false positives and true positives for denominator
    total_fp <- time_roc$FP[nrow(time_roc$FP), actual_index]
    total_tp <- time_roc$TP[nrow(time_roc$TP), actual_index]

    # Calculate FPR and TPR
    fpr_values <- time_roc$FP[, actual_index] / total_fp
    tpr_values <- time_roc$TP[, actual_index] / total_tp

    # Create data frame for this time point
    roc_data <- data.frame(
      fpr = fpr_values,
      tpr = tpr_values,
      time = rep(paste0(valid_time_roc_times[i], " months"), length(fpr_values))
    )
    roc_data_list[[i]] <- roc_data
  }

  # Combine all ROC data
  roc_data_all <- do.call(rbind, roc_data_list)
  roc_data_all$time <- factor(roc_data_all$time, levels = paste0(valid_time_roc_times, " months"))

  # Add legend with AUC values
  auc_labels <- paste0(valid_time_roc_times, " months AUC=", sprintf("%.3f", valid_time_roc_auc))

  # Create ggplot with proper color scale
  p <- ggplot2::ggplot(roc_data_all, aes(x = fpr, y = tpr, color = time)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_color_manual(
      values = colors[seq_along(valid_time_roc_times)],
      labels = auc_labels
    ) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2, alpha = 0.5) +
    ggplot2::theme_classic() +
    ggplot2::labs(x = "1 - Specificity", y = "Sensitivity") +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 12),
      axis.title = ggplot2::element_text(size = 15),
      legend.title = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 10),
      legend.position = "inside",
      legend.position.inside = c(0.7, 0.25)
    )

  if (title) {
    p <- p + ggplot2::ggtitle("Time-dependent ROC Curves")
  }

  if (save_plot) {
    ggplot2::ggsave(filename, p, width = 6, height = 6)
  }

  list(time_roc = time_roc, plot = p)
}
