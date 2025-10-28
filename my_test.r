data(cancer, package = "survival")
df <- kidney
df$dead <- ifelse(df$time <= 100 & df$status == 0, NA, df$time <= 100)
df <- na.omit(df[, -c(1:3)])

model0 <- glm(dead ~ age + frail, family = binomial(), data = df)
model <- glm(dead ~ ., family = binomial(), data = df)
df$base_pred <- predict(model0, type = "response")
df$full_pred <- predict(model, type = "response")

data=df
target_var="dead"
model_names=c("base_pred", "full_pred")
colors = NULL; save_output = FALSE;
figure_type = "png"; output_prefix = "model_compare"; as_probability = FALSE;
auto_order = TRUE
classif_model_compare <- function(data, target_var, model_names, colors = NULL, save_output = FALSE,
                                  figure_type = "png", output_prefix = "model_compare", as_probability = FALSE,
                                  auto_order = TRUE) {
  # Check required packages
  check_package("pROC", "ROC analysis and AUC calculations")
  check_package("caret", "confusion matrix calculations")
  check_package("ResourceSelection", "Hosmer-Lemeshow test")
  check_package("dcurves", "decision curve analysis")

  if (isTRUE(as_probability)) {
    vars_to_prob <- model_names[apply(data[, model_names, drop = FALSE], 2, function(x) any(x < 0 | x > 1))]
  } else if (is.character(as_probability)) {
    vars_to_prob <- as_probability
  } else {
    vars_to_prob <- NULL
  }
  for (var in vars_to_prob) {
    data[[var]] <- (data[[var]] - min(data[[var]])) / (max(data[[var]]) - min(data[[var]]))
  }
  if (max(data[, model_names]) > 1 || min(data[, model_names]) < 0) {
    stop(paste0(
      "Only predicted probabilities are allowed, detected values not in range 0 to 1.\n",
      "Set `as_probability = TRUE` to convert illegal variables to the range of 0 to 1.\n",
      "You can also pass a vector of variable names to `as_probability` to convert only those variables.\n"
    ))
  }
  if (is.null(colors)) colors <- emp_colors
  data[[target_var]] <- factor(data[[target_var]])
  target <- data[[target_var]]
  tmp <- is.na(target)
  if (any(tmp)) {
    data <- data[!tmp, ]
    target <- target[!tmp]
    warning(paste0(
      "The target variable contains missing values.",
      sum(tmp), " rows with missing target values are removed."
    ))
  }
  metric_table <- data.frame(matrix(NA, nrow = length(model_names), ncol = 14))
  colnames(metric_table) <- c(
    "Model", "AUC", "PRAUC", "Accuracy", "Sensitivity", "Specificity", "Pos Pred Value",
    "Neg Pred Value", "F1", "Kappa", "Brier", "cutoff", "Youden", "HosLem"
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
    pr_auc <- sum(diff(tmp2$recall) * (head(tmp2$precision, -1) + tail(tmp2$precision, -1)) / 2)
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
    aucs <- format(aucs, digits = 2, nsmall = 3)
    metric_table$AUC[i] <- paste0(aucs[2], " (", aucs[1], ", ", aucs[3], ")")
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
  for (i in 3:ncol(metric_table)) {
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
    ggplot2::scale_color_manual(values = colors[c(length(colors) - 1:0, seq_along(model_names))]) +
    ggplot2::labs(x = "Threshold probability") +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 15),
      legend.title = element_blank(),
      legend.background = element_blank(),
      legend.text = element_text(size = 10),
      legend.position = "inside",
      legend.position.inside = legend_pos,
    )
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
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2, alpha = 0.5) +
    ggplot2::theme_classic() +
    ggplot2::labs(x = "1 - Specificity", y = "Sensitivity") +
    ggplot2::theme(
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 15),
      legend.title = element_blank(),
      legend.background = element_blank(),
      legend.text = element_text(size = 10),
      legend.position = "inside",
      legend.position.inside = c(0.7, 0.25),
    )
  if (save_output) {
    ggplot2::ggsave(roc_plot, file = paste0(output_prefix, "_roc.", figure_type), width = 4, height = 4)
  }

  # plot PRAUC curves
  for(i in seq_along(roc_list)) {
    pr_data <- pROC::coords(roc_list[[i]], "all", ret = c("precision", "recall"))
    roc_list[[i]] <- data.frame(recall = pr_data$recall, precision = pr_data$precision,
                                Model = paste0(metric_table$Model[i], " (", metric_table$PRAUC[i], ")"))
  }
  pr_data_all <- dplyr::bind_rows(roc_list)
  pr_data_all$precision[is.nan(pr_data_all$precision)] <- 1
  pr_plot <- ggplot2::ggplot(pr_data_all, aes(x = recall, y = precision, color = Model)) +
    ggplot2::geom_line() +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::geom_abline(slope = -1, intercept = 1, linetype = 2, alpha = 0.5) +
    ggplot2::theme_classic() +
    ggplot2::lims(x = c(0, 1), y = c(0, 1)) +
    ggplot2::labs(x = "Recall", y = "Precision") +
    ggplot2::theme(
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 15),
      legend.title = element_blank(),
      legend.background = element_blank(),
      legend.text = element_text(size = 10),
      legend.position = "inside",
      legend.position.inside = c(0.25, 0.25),
    )
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
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::theme_classic() +
    ggplot2::labs(x = "Predicted probability", y = "Observed probability") +
    ggplot2::theme(
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 15),
      legend.title = element_blank(),
      legend.background = element_blank(),
      legend.text = element_text(size = 10),
      legend.position = "inside",
      legend.position.inside = c(0.7, 0.25),
    )
  if (save_output) {
    ggplot2::ggsave(calibration_plot, file = paste0(output_prefix, "_calibration.", figure_type), width = 4, height = 4)
  }
  return(list(
    metric_table = metric_table,
    dca_plot = dca_plot,
    roc_plot = roc_plot,
    calibration_plot = calibration_plot
  ))
}

res$pr_plot$data
