set.seed(1)
load_all()
library(knitr)
library(dtplyr)

data=prediction_res;target_var = "target"; model_names = bmr2$learners$learner_id
colors = NULL; save_output = TRUE;
output_prefix = "model_compare"; as_probability = FALSE
classif_model_compare <- function(data, target_var, model_names, colors = NULL, save_output = TRUE,
                                  output_prefix = "model_compare", as_probability = FALSE) {
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
  metric_table <- data.frame(matrix(NA, nrow = length(model_names), ncol = 13))
  colnames(metric_table) <- c(
    "Model", "AUC", "Accuracy", "Sensitivity", "Specificity", "Pos Pred Value",
    "Neg Pred Value", "F1", "Kappa", "Brier", "cutoff", "Youden", "HosLem"
  )
  metric_table$Model <- model_names
  sens_metrics <- c(
    "Sensitivity", "Specificity", "Pos Pred Value",
    "Neg Pred Value", "F1"
  )
  acc_metrics <- c("Accuracy", "Kappa")
  for (i in seq_along(model_names)) {
    model_name <- model_names[i]
    tmp <- pROC::coords(pROC::roc(target, data[[model_name]], direction = "<", quiet = TRUE), "best")
    metric_table$cutoff[i] <- get_valid(tmp$threshold, mode = "last", disjoint = FALSE)
    model_predict <- cut(data[[model_name]], c(-Inf, metric_table$cutoff[i], Inf), right = FALSE)
    levels(model_predict) <- levels(target)
    cm <- caret::confusionMatrix(model_predict, target,
                                 mode = "everything",
                                 positive = levels(target)[2]
    )
    aucs <- pROC::ci.auc(target, data[[model_name]], direction = "<", quiet = TRUE)
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
    ggplot2::ggsave(dca_plot, file = paste0(output_prefix, "_dca.png"), width = 4, height = 4)
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
    ggplot2::ggsave(roc_plot, file = paste0(output_prefix, "_roc.png"), width = 4, height = 4)
  }
  
  # plot calibration curves
  n_tiles <- min(round(nrow(data) / 10), 20)
  data_calibration <- data %>%
    pivot_longer(all_of(model_names)) %>%
    group_by(name) %>%
    mutate(decile = ntile(value, n_tiles)) %>%
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
    ggplot2::ggsave(calibration_plot, file = paste0(output_prefix, "_calibration.png"), width = 4, height = 4)
  }
  return(list(
    metric_table = metric_table,
    dca_plot = dca_plot,
    roc_plot = roc_plot,
    calibration_plot = calibration_plot
  ))
}

x=data.frame(t=sample(c(T,F),1000,replace = T,prob = c(0.2,0.8)),
             value=runif(1000,0,0.6))
y=x%>%mutate(decile = cut(value, break_at(c(0, 1), 20), right = F, include.lowest = T)) %>%
  group_by(decile) %>%
  summarise(
    obsRate = mean(t, na.rm = T),
    predRate = mean(value, na.rm = T),
    .groups = "drop"
  ) %>%
  as.data.frame()