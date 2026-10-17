#' Performance comparison of classification models
#' @description Compare the performance of classification models by commonly used
#'   metrics, and generate commonly used plots including receiver operating characteristic
#'   curve plot, decision curve analysis plot, and calibration plot.
#' @param data A data frame containing the target variable and the predicted values.
#' @param target_var A string specifying the name of the target variable in the data frame.
#' @param model_names A vector of strings specifying the names of the models to compare.
#' @param colors A vector of colors to use for the plots. The last 2 colors are used for the
#'   "Treat all" and "Treat none" lines in the DCA plot.
#' @param save_output A logical value indicating whether to output the results to files.
#' @param figure_type A character string of the figure type. Can be `"png"`, `"pdf"`, and other types that
#'   `ggplot2::ggsave()` support.
#' @param output_prefix A string specifying the prefix for the output files.
#' @param as_probability A logical or a vector of variable names. The logical value indicates
#'   whether to convert variables not in range 0 to 1 into this range.
#'   The vector of variable names means to convert these variables to the range of 0 to 1.
#' @param auto_order A logical value indicating whether to automatically order the models by their AUCs.
#'   If `TRUE`, the models will be ordered by their AUCs in descending order. If `FALSE`, the order
#'   in `model_names` will be retained.
#' @section Metrics:
#'   - AUC: Area Under the Receiver Operating Characteristic Curve
#'   - PRAUC: Area Under the Precision-Recall Curve
#'   - Accuracy: Overall accuracy
#'   - Sensitivity: True positive rate
#'   - Specificity: True negative rate
#'   - Pos Pred Value: Positive predictive value
#'   - Neg Pred Value: Negative predictive value
#'   - F1: F1 score
#'   - Kappa: Cohen's kappa
#'   - Brier: Brier score
#'   - cutoff: Optimal cutoff for classification, metrics that require a cutoff are
#'     based on this value.
#'   - Youden: Youden's J statistic
#'   - HosLem: Hosmer-Lemeshow test p-value
#'
#' @returns A list of various results. If the output files are not in desired format,
#'   these results can be modified for further use.
#'   - metric_table: A data frame containing the performance metrics for each model.
#'   - roc_plot: A `ggplot` object of Receiver Operating Characteristic curves.
#'   - pr_plot: A `ggplot` object of Precision-Recall curves.
#'   - dca_plot: A `ggplot` object of decision curve analysis plots.
#'   - calibration_plot: A `ggplot` object of calibration plots.
#' @export
#' @examples
#' data(cancer, package = "survival")
#' df <- kidney
#' df$dead <- ifelse(df$time <= 100 & df$status == 0, NA, df$time <= 100)
#' df <- na.omit(df[, -c(1:3)])
#'
#' model0 <- glm(dead ~ age + frail, family = binomial(), data = df)
#' model <- glm(dead ~ ., family = binomial(), data = df)
#' df$base_pred <- predict(model0, type = "response")
#' df$full_pred <- predict(model, type = "response")
#'
#' classif_model_compare(df, "dead", c("base_pred", "full_pred"), save_output = FALSE)
classif_model_compare <- function(data, target_var, model_names, colors = NULL, save_output = FALSE,
                                  figure_type = "png", output_prefix = "model_compare", as_probability = FALSE,
                                  auto_order = TRUE) {
  # Check required packages
  check_package("pROC", "ROC analysis and AUC calculations")
  check_package("caret", "confusion matrix calculations")
  check_package("ResourceSelection", "Hosmer-Lemeshow test")
  check_package("dcurves", "decision curve analysis")

  if (!target_var %in% names(data)) {
    stop("`target_var` not found in `data`.")
  }
  missing_models <- setdiff(model_names, names(data))
  if (length(missing_models) > 0) {
    stop("Prediction column(s) not found in `data`: ", paste(missing_models, collapse = ", "))
  }
  if (is.null(colors)) colors <- emp_colors

  # Validate a binary target: numeric must be coded as 0/1, factor/character must have 2 levels
  target_raw <- data[[target_var]]
  if (is.numeric(target_raw)) {
    target_levels <- unique(target_raw[!is.na(target_raw)])
    if (!all(target_levels %in% c(0, 1))) {
      stop("`target_var` is numeric but not coded as 0/1. Convert it to a binary factor or 0/1 coding.")
    }
  }
  data[[target_var]] <- factor(data[[target_var]])
  target <- data[[target_var]]
  tmp <- is.na(target)
  if (any(tmp)) {
    data <- data[!tmp, ]
    target <- target[!tmp]
    warning(paste0(
      "The target variable contains missing values. ",
      sum(tmp), " rows with missing target values are removed."
    ))
  }
  if (nlevels(target) != 2) {
    stop("`target_var` must be binary: exactly 2 non-missing levels are required.")
  }

  if (isTRUE(as_probability)) {
    vars_to_prob <- model_names[apply(
      data[, model_names, drop = FALSE], 2,
      function(x) any(x < 0 | x > 1, na.rm = TRUE)
    )]
  } else if (is.character(as_probability)) {
    vars_to_prob <- as_probability
  } else {
    vars_to_prob <- NULL
  }
  for (var in vars_to_prob) {
    pred_range <- range(data[[var]], na.rm = TRUE)
    if (pred_range[1] == pred_range[2]) {
      stop("Cannot rescale '", var, "' to probabilities: all non-missing values are identical.")
    }
    data[[var]] <- (data[[var]] - pred_range[1]) / (pred_range[2] - pred_range[1])
  }
  pred_values <- unlist(data[, model_names, drop = FALSE], use.names = FALSE)
  if (any(pred_values < 0 | pred_values > 1, na.rm = TRUE)) {
    stop(paste0(
      "Only predicted probabilities are allowed, detected values not in range 0 to 1.\n",
      "Set `as_probability = TRUE` to convert illegal variables to the range of 0 to 1.\n",
      "You can also pass a vector of variable names to `as_probability` to convert only those variables.\n"
    ))
  }

  metric_table <- data.frame(matrix(NA, nrow = length(model_names), ncol = 16))
  colnames(metric_table) <- c(
    "Model", "AUC", "AUC_lower", "AUC_upper", "PRAUC", "Accuracy", "Sensitivity", "Specificity",
    "Pos Pred Value", "Neg Pred Value", "F1", "Kappa", "Brier", "cutoff", "Youden", "HosLem"
  )
  metric_table$Model <- model_names
  sens_metrics <- c(
    "Sensitivity", "Specificity", "Pos Pred Value",
    "Neg Pred Value", "F1"
  )
  acc_metrics <- c("Accuracy", "Kappa")
  model_aucs <- c()
  for (i in seq_along(model_names)) {
    model_name <- model_names[i]
    roc_res <- pROC::roc(target, data[[model_name]], direction = "<", quiet = TRUE)
    tmp <- pROC::coords(roc_res, "best")
    tmp2 <- pROC::coords(roc_res, "all", ret = c("precision", "recall"))
    valid_points <- !is.na(tmp2$precision) & !is.na(tmp2$recall)
    tmp2 <- data.frame(recall = tmp2$recall[valid_points], precision = tmp2$precision[valid_points])
    tmp2 <- tmp2[order(tmp2$recall), ]
    pr_auc <- sum(diff(tmp2$recall) * (utils::head(tmp2$precision, -1) + utils::tail(tmp2$precision, -1)) / 2)
    metric_table$PRAUC[i] <- round(pr_auc, 3)

    metric_table$cutoff[i] <- get_valid(tmp$threshold, mode = "last", disjoint = FALSE)
    model_predict <- cut(data[[model_name]], c(-Inf, metric_table$cutoff[i], Inf), right = FALSE)
    levels(model_predict) <- levels(target)
    cm <- caret::confusionMatrix(model_predict, target,
      mode = "everything",
      positive = levels(target)[2]
    )
    aucs <- pROC::ci.auc(target, data[[model_name]], direction = "<", quiet = TRUE)
    model_aucs[i] <- aucs[2]
    metric_table$AUC[i] <- aucs[2]
    metric_table$AUC_lower[i] <- aucs[1]
    metric_table$AUC_upper[i] <- aucs[3]
    for (j in seq_along(sens_metrics)) {
      metric_table[i, sens_metrics[j]] <- cm$byClass[sens_metrics[j]]
    }
    for (j in seq_along(acc_metrics)) {
      metric_table[i, acc_metrics[j]] <- cm$overall[acc_metrics[j]]
    }
    metric_table$Brier[i] <- DescTools::BrierScore(as.numeric(target) - 1, data[[model_name]])
    metric_table$Youden[i] <- metric_table$Sensitivity[i] + metric_table$Specificity[i] - 1
    metric_table$HosLem[i] <- ResourceSelection::hoslem.test(as.numeric(target) - 1, data[[model_name]])$p.value
  }
  for (i in 2:ncol(metric_table)) {
    metric_table[, i] <- round(metric_table[, i], digits = 3)
  }
  if (auto_order) {
    metric_table <- metric_table[order(model_aucs, decreasing = TRUE), ]
    model_names <- metric_table$Model
  }
  if (save_output) {
    write.csv(metric_table, file = paste0(output_prefix, "_table.csv"), row.names = FALSE, na = "")
  }

  plot_formula <- as.formula(paste0(target_var, " ~ ", paste(model_names, collapse = " + ")))

  # Plot DCA curves
  pos_rate <- mean(as.numeric(as.factor(data[[target_var]]))) - 1
  if (pos_rate < 0.5) {
    legend_pos <- c(0.75, 0.75)
  } else {
    legend_pos <- c(0.25, 0.4)
  }
  if (pos_rate < 0.2) {
    dca_thresholds <- seq(0.005, 0.5, by = 0.005)
  } else {
    dca_thresholds <- seq(0.01, 0.99, by = 0.01)
  }

  dca_plot <- dcurves::dca(plot_formula, data = data, thresholds = dca_thresholds) %>%
    plot(smooth = TRUE) +
    ggplot2::scale_color_manual(
      values = c(utils::tail(colors, 2), rep_len(colors, length(model_names)))
    ) +
    ggplot2::labs(x = "Threshold probability") +
    theme_pub(legend_pos = legend_pos)
  if (save_output) {
    ggplot2::ggsave(dca_plot, file = paste0(output_prefix, "_dca.", figure_type), width = 4, height = 4)
  }

  # plot ROC curves
  roc_list <- pROC::roc(plot_formula, data = data, direction = "<", quiet = TRUE)
  if (length(model_names) == 1) {
    roc_list <- list(roc_list)
    names(roc_list) <- model_names
  }
  names(roc_list) <- paste0(names(roc_list), " (", sapply(roc_list, function(x) sprintf("%.3f", x$auc)), ")")
  roc_plot <- pROC::ggroc(roc_list, legacy.axes = TRUE, linewidth = 1) +
    ggplot2::scale_color_manual(values = rep_len(colors, length(model_names))) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2, alpha = 0.5) +
    ggplot2::labs(x = "1 - Specificity", y = "Sensitivity") +
    theme_pub(legend_pos = c(0.7, 0.25))
  if (save_output) {
    ggplot2::ggsave(roc_plot, file = paste0(output_prefix, "_roc.", figure_type), width = 4, height = 4)
  }

  # plot PRAUC curves
  for (i in seq_along(roc_list)) {
    pr_data <- pROC::coords(roc_list[[i]], "all", ret = c("precision", "recall"))
    roc_list[[i]] <- data.frame(
      recall = pr_data$recall, precision = pr_data$precision,
      Model = paste0(metric_table$Model[i], " (", metric_table$PRAUC[i], ")")
    )
  }
  pr_data_all <- data.table::rbindlist(roc_list)
  pr_data_all$precision[is.nan(pr_data_all$precision)] <- 1
  pr_data_all$Model <- factor(pr_data_all$Model, levels = paste0(metric_table$Model, " (", metric_table$PRAUC, ")"))
  pr_plot <- ggplot2::ggplot(pr_data_all, aes(x = recall, y = precision, color = Model)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_color_manual(values = rep_len(colors, length(model_names))) +
    ggplot2::lims(x = c(0, 1), y = c(0, 1)) +
    ggplot2::labs(x = "Recall", y = "Precision") +
    theme_pub(legend_pos = c(0.25, 0.25))
  if (save_output) {
    ggplot2::ggsave(pr_plot, file = paste0(output_prefix, "_pr.", figure_type), width = 4, height = 4)
  }

  # plot calibration curves
  n_tiles <- min(round(nrow(data) / 10), 20)
  data_calibration <- data %>%
    pivot_longer(all_of(model_names)) %>%
    group_by(name) %>%
    mutate(decile = cut(value, break_at(c(0, 1), n_tiles), right = F, include.lowest = T)) %>%
    group_by(decile, name) %>%
    summarise(
      obsRate = mean(as.numeric(!!sym(target_var)), na.rm = T) - 1,
      predRate = mean(value, na.rm = T),
      .groups = "drop"
    ) %>%
    as.data.frame()
  data_calibration$name <- factor(data_calibration$name, levels = model_names)
  calibration_plot <- ggplot(data = data_calibration, aes(
    y = obsRate, x = predRate, group = name, color = name
  )) +
    geom_smooth(method = "loess", formula = y ~ x, se = FALSE, span = 1) +
    lims(x = c(0, 1), y = c(0, 1)) +
    geom_abline(intercept = 0, slope = 1, linetype = 2, alpha = 0.5) +
    ggplot2::scale_color_manual(values = rep_len(colors, length(model_names))) +
    ggplot2::labs(x = "Predicted probability", y = "Observed probability") +
    theme_pub(legend_pos = c(0.7, 0.25))
  if (save_output) {
    ggplot2::ggsave(calibration_plot, file = paste0(output_prefix, "_calibration.", figure_type), width = 4, height = 4)
  }
  return(list(
    metric_table = metric_table,
    dca_plot = dca_plot,
    roc_plot = roc_plot,
    pr_plot = pr_plot,
    calibration_plot = calibration_plot
  ))
}
