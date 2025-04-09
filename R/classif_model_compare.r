#' Performance comparison of classification models
#' @description Compare the performance of classification models by commonly used
#'   metrics.
#' @param data A data frame containing the target variable and the predicted values.
#' @param target_var A string specifying the name of the target variable in the data frame.
#' @param model_names A vector of strings specifying the names of the models to compare.
#' @param colors A vector of colors to use for the plots.
#' @param output_files A logical value indicating whether to output the results to files.
#' @param output_prefix A string specifying the prefix for the output files.
#' @param return_results A logical value indicating whether to return the results.
#' @details This function calculates various performance metrics for each model
#'   and output the comparison table and plots.
#' @section Metrics:
#'   - AUC: Area Under the Receiver Operating Characteristic Curve
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
#' @returns A data frame.
#' @export
#' @examples
#' data(cancer, package = "survival")
#' cancer$dead <- factor(cancer$status == 2)
#' cancer <- na.omit(cancer[, c("dead", "age", "sex", "meal.cal")])
#'
#' model0 <- glm(dead ~ age, family = binomial(), data = cancer)
#' cancer$age_pred <- predict(model0, type = "response")
#' model <- glm(dead ~ age + sex + meal.cal, family = binomial(), data = cancer)
#' cancer$logistic_pred <- predict(model, type = "response")
#'
#' classif_model_compare(cancer, "dead", c("logistic_pred","age_pred"))
classif_model_compare <- function(data, target_var, model_names, colors = NULL, output_files = TRUE,
                                  output_prefix = "model_compare", return_results = !output_files) {
  if (!output_files && !return_results) stop("Results are neither saved or returned!")
  if (is.null(colors)) colors <- .color_panel
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
    metric_table$HosLem[i] <- hoslem.test(as.numeric(target) - 1, data[[model_name]])$p.value
  }
  for (i in 3:ncol(metric_table)) {
    metric_table[, i] <- round(metric_table[, i], digits = 3)
  }
  if (output_files) {
    write.csv(metric_table, file = paste0(output_prefix, "_table.csv"), row.names = FALSE)
  }

  plot_formula <- as.formula(paste0(target_var, " ~ ", paste(model_names, collapse = " + ")))

  # Plot DCA curves
  vars_as_probability <- character()
  for (var in model_names) {
    if (any(data[[var]] < 0) || any(data[[var]] > 1)) {
      vars_as_probability <- union(vars_as_probability, var)
    }
  }
  dca_plot <- dcurves::dca(plot_formula, data = data, as_probability = vars_as_probability) %>%
    plot(smooth = TRUE) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::labs(x = "Threshold probability") +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 15)
    )
  if (output_files) {
    ggplot2::ggsave(dca_plot, file = paste0(output_prefix, "_dca.png"), width = 5, height = 4)
  }

  # plot ROC curves
  roc_list <- pROC::roc(plot_formula, data = data, direction = "<", quiet = TRUE)
  names(roc_list) <- paste0(names(roc_list), " (", sapply(roc_list, function(x) sprintf("%.3f", x$auc)), ")")
  roc_plot <- pROC::ggroc(roc_list, legacy.axes = TRUE) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2, alpha = 0.5) +
    ggplot2::theme_classic() +
    ggplot2::labs(x = "1 - Specificity", y = "Sensitivity") +
    ggplot2::theme(
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 15),
      legend.title = element_blank(),
      legend.text = element_text(size = 10),
      legend.position = "inside",
      legend.position.inside = c(0.7, 0.25),
    )
  if (output_files) {
    ggplot2::ggsave(roc_plot, file = paste0(output_prefix, "_roc.png"), width = 4, height = 4)
  }

  # plot calibration curves
  n_tiles = min(round(nrow(data)/10),20)
  data_calibration <- data %>%
    pivot_longer(model_names)%>%
    group_by(name) %>%
    mutate(decile = ntile(value, n_tiles)) %>%
    group_by(decile, name) %>%
    summarise(
      obsRate = mean(as.numeric(!!sym(target_var)), na.rm = T)-1,
      predRate = mean(value, na.rm = T),
      .groups = "drop"
    ) %>%
    as.data.frame()
  data_calibration$name <- factor(data_calibration$name, levels = model_names)
  calibration_plot <- ggplot(data = data_calibration, aes(
    y = obsRate, x = predRate, group = name, color = name)) +
    geom_smooth() +
    lims(x = ifelse(rep(
      is.null(x_lim),
      2
    ), c(min(data_calibration$predRate), max(data_calibration$predRate)),
    x_lim
    ), y = ifelse(rep(is.null(y_lim), 2), c(
      min(data_calibration$obsRate_LCL),
      max(data_calibration$obsRate_UCL)
    ), y_lim)) +
    geom_errorbar(aes(
      ymax = .data$obsRate_UCL,
      ymin = .data$obsRate_LCL
    )) +
    geom_abline(
      intercept = 0,
      slope = 1
    ) +
    scale_color_manual(values = ifelse(rep(
      is.null(points_col_list),
      n_groups
    ), (ggplot2::scale_colour_brewer(palette = "Set3")$palette(8)[c(
      4:8,
      1:3
    )])[c(1:n_groups)], points_col_list)) +
    labs(
      x = ifelse(is.null(xlab),
        pred, xlab
      ), y = ifelse(is.null(ylab), obs, ylab),
      title = title
    ) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(), panel.background = element_blank(),
      axis.line = element_line(colour = "black"), legend.key = element_rect(fill = "white"),
      axis.text = element_text(colour = "black", size = 12),
      legend.position = legendPosition
    )
}
