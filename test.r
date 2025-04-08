data(cancer, package = "survival")
var_types <- get_var_types(cancer, strata = "sex")
baseline_table(cancer, var_types = var_types)


baseline_table <- function(data, var_types = NULL, strata = NULL, vars = setdiff(colnames(data), strata),
                           factor_vars = NULL, exact_vars = NULL, nonnormal_vars = NULL, seed = NULL,
                           filename = "baseline.csv", p_adjust_method = "BH", ...) {
  if (!is.null(var_types) && !"var_types" %in% class(var_types)) {
    stop("Invalid 'var_types' arguement! Please use result from get_var_types function.")
  }
  if (!grepl(".csv", filename)) stop("please save as .csv file")
  if (is.null(strata) & !is.null(var_types)) strata <- var_types$strata
  if (!is.null(strata)) data <- data[!is.na(data[[strata]]), ]
  if (is.null(factor_vars) & !is.null(var_types)) factor_vars <- var_types$factor_vars
  if (is.null(exact_vars) & !is.null(var_types)) exact_vars <- var_types$exact_vars
  if (is.null(nonnormal_vars) & !is.null(var_types)) nonnormal_vars <- var_types$nonnormal_vars
  if (!is.null(var_types$omitvars)) vars <- setdiff(vars, var_types$omitvars)
  if (is.null(seed)) set.seed(seed)
  
  factor_vars = union(factor_vars, exact_vars)
  
  if (is.null(strata)) {
    tab1 <- CreateTableOne(
      vars = vars, argsNormal = list(var.equal = F),
      argsExact = list(workspace = 2 * 10^5, simulate.p.value = TRUE, B=1e4),
      data = data, factorVars = factor_vars, addOverall = TRUE
    )
  } else {
    tab1 <- CreateTableOne(
      vars = vars, strata = strata, argsNormal = list(var.equal = F),
      argsExact = list(workspace = 2 * 10^5, simulate.p.value = TRUE, B=1e4),
      data = data, factorVars = factor_vars, addOverall = TRUE
    )
  }
  printed_table <- print(tab1,
                         nonnormal = nonnormal_vars, exact = exact_vars,
                         quote = FALSE, noSpaces = TRUE, printToggle = FALSE, ...
  )
  write.csv(printed_table, file = filename)
  
  missing_df <- as.data.frame(is.na(data))
  for (i in 1:ncol(missing_df)) {
    missing_df[, i] <- factor(missing_df[, i], levels = c(F, T))
  }
  if (is.null(strata)) {
    tab2 <- CreateTableOne(
      vars = vars,
      data = missing_df, addOverall = TRUE
    )
  } else {
    missing_df[[strata]] <- data[[strata]]
    tab2 <- CreateTableOne(
      vars = vars, strata = strata,
      data = missing_df, addOverall = TRUE
    )
  }
  printed_table <- print(tab2, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, ...)
  write.csv(printed_table, file = str_replace(filename, ".csv", "_missing.csv"))
  
  if (!is.null(strata) && length(na.omit(unique(data[[strata]]))) > 2) {
    g <- factor(data[[strata]])
    pairwise_result <- data.frame()
    for (var in vars) { 
      if (var %in% exact_vars) {
        cont_table <- table(data[[var]], g)
        compare_levels <- function(i, j) {
          tryCatch(
            {
              fisher.test(cont_table[, c(i, j)], simulate.p.value = TRUE, B=1e4)$p.value
            },
            error = function(e) {
              NA
            }
          )
        }
        pt <- pairwise.table(compare_levels, levels(g), p_adjust_method)
      } else if (var %in% factor_vars) {
        cont_table <- table(data[[var]], g)
        compare_levels <- function(i, j) {
          chisq.test(cont_table[, c(i, j)])$p.value
        }
        pt <- pairwise.table(compare_levels, levels(g), p_adjust_method)
      } else if (var %in% nonnormal_vars) {
        compare_levels <- function(i, j) {
          xi <- data[as.integer(g) == i, var]
          xj <- data[as.integer(g) == j, var]
          wilcox_test_pval(xi, xj)
        }
        pt <- pairwise.table(compare_levels, levels(g), p_adjust_method)
      } else {
        pt <- pairwise.t.test(data[[var]], g, p.adjust.method = p_adjust_method)$p.value
      }
      tmp <- as.data.frame(as.table(pt))
      tmp$Var1 <- factor(tmp$Var1, levels = levels(g))
      tmp$Var2 <- factor(tmp$Var2, levels = levels(g))
      p_values_long <- tmp %>%
        filter(as.numeric(Var1) > as.numeric(Var2)) %>%
        mutate(Comparison = paste(Var1, Var2, sep = "_")) %>%
        select(Comparison, Freq)
      p_values_wide <- as.data.frame(pivot_wider(p_values_long, names_from = Comparison, values_from = Freq))
      pairwise_result <- rbind(pairwise_result, p_values_wide)
    }
    rownames(pairwise_result) <- vars
    write.csv(pairwise_result, file = str_replace(filename, ".csv", "_pairwise.csv"))
  }
  invisible(NULL)
}

data(cancer, package = "survival")
cancer$dead <- factor(cancer$status == 2)
cancer <- na.omit(cancer[, c("dead", "age", "sex", "ph.ecog")])

model <- glm(dead ~ age + sex + ph.ecog, family = binomial(), data = cancer)
logistic_pred <- predict(model, newdata = cancer, type = "response")

set.seed(123)
dummy_pred <- rnorm(length(cancer$dead))
classif_model_compare(list(log=logistic_pred, dum=dummy_pred), cancer$dead)
pred_list=list(log=logistic_pred, dum=dummy_pred)
target=cancer$dead
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
    model_val_comp$Brier[i] <- BrierScore(as.numeric(target) - 1, pred_list[[i]])
    model_val_comp$Youden[i] <- model_val_comp$Sensitivity[i] + model_val_comp$Specificity[i] - 1
    model_val_comp$HosLem[i] <- hoslem.test(as.numeric(target) - 1, pred_list[[i]])$p.value
  }
  for (i in 3:ncol(model_val_comp)) {
    model_val_comp[, i] <- round(model_val_comp[, i], digits = 3)
  }
  write.csv(model_val_comp, file = filename, row.names = FALSE)
}
