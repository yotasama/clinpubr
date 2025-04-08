#' Performance comparison of classification models
#' @description Compare the performance of classification models by commonly used
#'   metrics.
#' @param pred_list A named list of predictions from models.
#' @param target A favtor vector of classification labels with two levels. The second
#'   level would be considered positive. Must be of the same length as the prediction
#'   sublists form `pred_list`.
#' @param filename A string specifying the filename to save the comparison results.
#' @details This function calculates various performance metrics for each model
#'   in the `pred_list` and saves the results to a CSV file.
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
#' @returns A data frame with additional columns.
#' @export
#' @examples
#' data(cancer, package = "survival")
#' cancer$dead <- factor(cancer$status == 2)
#'
#' model <- glm(dead ~ age + sex + ph.ecog, family = binomial(), data = cancer)
#' logistic_pred <- predict(model, newdata = cancer, type = "response")
#'
#' set.seed(123)
#' dummy_pred <- rnorm(length(pred))
#' classif_model_compare(list(log=logistic_pred, dum=dummy_pred), cancer$dead)
classif_model_compare <- function(pred_list, target, filename = "model_compare.csv") {
  target <- factor(target)
  model_val_comp <- data.frame(matrix(NA, nrow = length(pred_list), ncol = 13))
  colnames(model_val_comp) <- c(
    "Model", "AUC", "Accuracy", "Sensitivity", "Specificity", "Pos Pred Value",
    "Neg Pred Value", "F1", "Kappa", "Brier", "cutoff", "Youden", "HosLem"
  )
  model_val_comp$Model <- names(pred_list)
  sens_metrics <- c(
    "Sensitivity", "Specificity", "Pos Pred Value",
    "Neg Pred Value", "F1"
  )
  acc_metrics <- c("Accuracy", "Kappa")
  for (i in seq_along(pred_list)) {
    tmp <- pROC::coords(pROC::roc(target, pred_list[[i]], direction = "<", quiet = TRUE), "best")
    model_val_comp$cutoff[i] <- get_valid(tmp$threshold, mode = "last", disjoint = FALSE)
    model_predict <- cut(pred_list[[i]], c(-Inf, model_val_comp$cutoff[i], Inf), right = FALSE)
    levels(model_predict) <- levels(target)
    cm <- caret::confusionMatrix(model_predict, target,
      mode = "everything",
      positive = levels(target)[2]
    )
    aucs <- pROC::ci.auc(target, pred_list[[i]], direction = "<", quiet = TRUE)
    aucs <- format(aucs, digits = 2, nsmall = 3)
    model_val_comp$AUC[i] <- paste0(aucs[2], " (", aucs[1], ", ", aucs[3], ")")
    for (j in seq_along(sens_metrics)) {
      model_val_comp[i, sens_metrics[j]] <- cm$byClass[sens_metrics[j]]
    }
    for (j in seq_along(acc_metrics)) {
      model_val_comp[i, acc_metrics[j]] <- cm$overall[acc_metrics[j]]
    }
    model_val_comp$Brier[i] <- DescTools::BrierScore(as.numeric(target) - 1, pred_list[[i]])
    model_val_comp$Youden[i] <- model_val_comp$Sensitivity[i] + model_val_comp$Specificity[i] - 1
    model_val_comp$HosLem[i] <- hoslem.test(as.numeric(target) - 1, pred_list[[i]])$p.value
  }
  for (i in 3:ncol(model_val_comp)) {
    model_val_comp[, i] <- round(model_val_comp[, i], digits = 3)
  }
  write.csv(model_val_comp, file = filename, row.names = FALSE)
  model_val_comp
}