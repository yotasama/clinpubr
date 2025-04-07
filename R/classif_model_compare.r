#' @include utils.R
NULL
#' Split multichoice data into columns
#' @description Split multichoice data into columns, each new column consists of
#'   booleans whether a choice is presented.
#' @param df A data frame.
#' @param quest_cols A vector of column names that contain multichoice data.
#' @param split A string to split the data. Default is `""`.
#' @param remove_space If `TRUE`, remove space in the data.
#' @param link A string to link the column name and the option. Default is `"_"`.
#' @param remove_cols If `TRUE`, remove the original columns.
#'
#' @returns A data frame with additional columns.
#' @export
#' @examples
#' df <- data.frame(q1 = c("a b", "c d a", "b a"), q2 = c("a b", "a c", "d"))
#' split_multichoice(df, quest_cols = c("q1", "q2"))
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
    tmp <- pROC::coords(pROC::roc(target, pred_list[[i]]), "best")
    model_val_comp$cutoff[i] <- get_valid(tmp$threshold, mode = "last", disjoint = TRUE)
    model_predict <- cut(pred_list[[i]], c(-Inf, model_val_comp$cutoff[i], Inf), right = FALSE)
    levels(model_predict) <- levels(target)
    cm <- caret::confusionMatrix(model_predict, target,
      mode = "everything",
      positive = levels(target)[2]
    )
    aucs <- pROC::ci.auc(target, pred_list[[i]])
    aucs <- format(aucs, digits = 2, nsmall = 3)
    model_val_comp$AUC[i] <- paste0(aucs[2], " (", aucs[1], ", ", aucs[3], ")")
    for (j in seq_along(sens_metrics)) {
      model_val_comp[i, sens_metrics[j]] <- CM$byClass[sens_metrics[j]]
    }
    for (j in seq_along(acc_metrics)) {
      model_val_comp[i, acc_metrics[j]] <- CM$overall[acc_metrics[j]]
    }
    model_val_comp$Brier[i] <- BrierScore(as.numeric(target) - 1, pred_list[[i]])
    model_val_comp$Youden[i] <- model_val_comp$Sensitivity[i] + model_val_comp$Specificity[i] - 1
    model_val_comp$HosLem[i] <- hoslem.test(as.numeric(target) - 1, pred_list[[i]])$p.value
  }
  for (i in 3:ncol(model_val_comp)) {
    model_val_comp[, i] <- round(model_val_comp[, i], digits = 3)
  }
  write.csv(model_val_comp, file = filename, row.names = FALSE)
}