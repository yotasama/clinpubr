#' Scan for interactions between variables
#' @description Scan for interactions between variables and output results as tables. Both logistic and Cox
#'   proportional hazards regression models are supported. The predictor variables in the model are can be
#'   used both in linear form or in ristricted cubic spline form.
#' @param data A data frame.
#' @param y A character string of the outcome variable.
#' @param time A character string of the time variable. If `NULL`, logistic regression is used.
#'   Otherwise, Cox proportional hazards regression is used.
#' @param predictors The predictor variables to be scanned for interactions. If `NULL`, all variables
#'   except `y` and `time` are taken as predictors.
#' @param group_vars The group variables to be scanned for interactions. If `NULL`, all variables
#'   except `y` and `time` are taken as group variables. The group variables should be categorical. If a
#'   numeric variable is included, it will be splited by the median value.
#' @param try_rcs A logical value indicating whether to perform ristricted cubic spline interaction analysis.
#' @param save_table A logical value indicating whether to save the results as a table.
#' @param filename The name of the file to save the results. Support both `.xlsx` and `.csv` formats.
#' @return A data frame containing the results of the interaction analysis.
#' @export
#' @examples
#' data(cancer, package = "survival")
#' int_scan(cancer, y = "status", time = "time")
int_scan <- function(data, y, time = NULL, predictors = NULL, group_vars = NULL,
                     try_rcs = TRUE, save_table = TRUE, filename = NULL) {
  analysis_type <- ifelse(is.null(time), "logistic", "cox")
  if (is.null(predictors)) {
    predictors <- setdiff(colnames(data), c(y, time))
    message("Taking all variables as interaction predictors")
  }
  if (is.null(group_vars)) {
    group_vars <- setdiff(colnames(data), c(y, time))
    message("Taking all variables as group variables")
  }

  tmp <- data[, group_vars]
  for (var in group_vars) {
    if (is.numeric(tmp[[var]]) && length(unique(tmp[[var]])) > 5) {
      tmp[[var]] <- cut_by(tmp[[var]], 0.5, breaks_as_quantiles = T)
    } else {
      tmp[[var]] <- as.factor(tmp[[var]])
    }
  }
  colnames(tmp) <- paste0(group_vars, "_tmp")

  tmp2 <- data[, predictors]
  colnames(tmp2) <- paste0(predictors, "_p")
  data <- cbind(data[, c(y, time), drop = F], tmp, tmp2)

  if (try_rcs) {
    res_df <- data.frame(matrix(NA, nrow = length(predictors) * length(group_vars), ncol = 5))
    colnames(res_df) <- c("predictor", "group.by", "nvalid", "lin.pval", "rcs.pval")
  }else {
    res_df <- data.frame(matrix(NA, nrow = length(predictors) * length(group_vars), ncol = 4))
    colnames(res_df) <- c("predictor", "group.by", "nvalid", "lin.pval")
  }
  irow <- 1
  for (predictor in predictors) {
    predictor_name = paste0(predictor, "_p")
    for (var in group_vars) {
      var_name = paste0(var, "_tmp")
      if (var != predictor) {
        nvalid = sum(complete.cases(data[, c(time, y, predictor_name, var_name)]))
        if (nvalid < 10) {
          next
        }
        if (analysis_type == "cox") {
          outcome <- paste0("Surv(", time, ",", y, ")")
        }else {
          outcome <- y
        }

        formula1 <- paste0(outcome, "~", predictor_name, "+", var_name)
        formula2 <- paste0(outcome, "~", predictor_name, "*", var_name)

        if (analysis_type == "cox") {
          model1 <- coxph(as.formula(formula1), data = data)
          model2 <- coxph(as.formula(formula2), data = data)
        }else {
          model1 <- glm(as.formula(formula1), data = data, family = binomial())
          model2 <- glm(as.formula(formula2), data = data, family = binomial())
        }
        tmp1 <- anova(model1, model2, test = "LRT")
        p1 <- broom::tidy(tmp1)$p.value[2]
        if (try_rcs && length(unique(data[, predictor_name])) > 10) {
          formula3 <- paste0(outcome, "~rcs(", predictor_name, ",4) + ", var)
          formula4 <- paste0(outcome, "~rcs(", predictor_name, ",4) * ", var)
          p2 <- tryCatch(
            {
              if (analysis_type == "cox") {
                model3 <- coxph(as.formula(formula1), data = data)
                model4 <- coxph(as.formula(formula2), data = data)
              }else {
                model3 <- glm(as.formula(formula1), data = data, family = binomial())
                model4 <- glm(as.formula(formula2), data = data, family = binomial())
              }
              tmp2 <- anova(model3, model4, test = "LRT")
              broom::tidy(tmp2)$p.value[2]
            },
            error = function(e) {
              NA
            }
          )
        } else {
          p2 <- NA
        }
        res_df$predictor[irow] <- predictor
        res_df$group.by[irow] <- var
        res_df$nvalid[irow] <- nvalid
        res_df$lin.pval[irow] <- p1
        if (try_rcs) {
          res_df$rcs.pval[irow] <- p2
        }
        irow <- irow + 1
      }
    }
  }
  res_df <- res_df[order(res_df$lin.pval, decreasing = F), ]
  res_df$lin.p.adj = p.adjust(res_df$lin.pval)
  res_df$rcs.p.adj = p.adjust(res_df$rcs.pval)
  if (save_table) {
    if (is.null(filename)) {
      filename = "interaction_scan.xlsx"
    }
    if (grepl(".csv", filename)) {
      write.csv(res_df, filename, row.names = F)
    }else if (grepl(".xlsx", filename)) {
      openxlsx::write.xlsx(res_df, filename)
    }
  }
  res_df[!is.na(res_df$predictor),]
}
