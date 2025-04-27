#' Basic results of logistic or Cox regression.
#' @description Generate the result table of logistic or Cox regression with different settings of the predictor
#'   variable and covariates. Also generate KM curves for Cox regression.
#' @param data A data frame.
#' @param x A character string of the predictor variable.
#' @param y A character string of the outcome variable.
#' @param time A character string of the time variable. If `NULL`, logistic regression is used.
#'   Otherwise, Cox proportional hazards regression is used.
#' @param model_covs A character vector or a named list of covariates for different models.
#'   If `NULL`, only the crude model is used.
#' @param pers A numeric vector of the denominators of variable `x`. Set this denominator to obtain a reasonable
#'   OR or HR.
#' @param factor_breaks A numeric vector of the breaks to factorize the `x` variable.
#' @param factor_labels A character vector of the labels for the factor levels.
#' @param quantile_breaks A numeric vector of the quantile breaks to factorize the `x` variable.
#' @param quantile_labels A character vector of the labels for the quantile levels.
#' @param label_with_range A logical value indicating whether to add the range of the levels to the labels.
#' @param output_dir A character string of the directory to save the output files.
#' @param ref_levels A vector of strings of the reference levels of the factor variable. You can use `"lowest"`
#'   or `"highest"` to select the lowest or highest level as the reference level. Otherwise, any level that
#'   matches the provided strings will be used as the reference level.
#' @param ratio_nsmall The minimum number of digits to the right of the decimal point for the OR or HR.
#' @param pval_nsmall The minimum number of digits to the right of the decimal point for the p-value.
#' @param pval_eps The threshold for rounding p values to 0.
#' @param median_nsmall The minimum number of digits to the right of the decimal point for the median survival time.
#' @param colors A vector of colors for the KM curves.
#' @param xlab A character string of the x-axis label.
#' @param legend_title A character string of the title of the legend.
#' @param legend_pos A numeric vector of the position of the legend.
#' @param height The height of the plot.
#' @param width The width of the plot.
#' @param pval_pos A numeric vector of the position of the p-value.
#' @param ... Additional arguments passed to the `survminer::ggsurvplot` function for KM curve.
#'
#' @details The function `regression_basic_results` generates the result table of logistic or Cox regression with
#'   different settings of the predictor variable and covariates. The setting of the predictor variable includes
#'   the original `x`, the standardized `x`, the log of `x`, and `x` divided by denominators in `pers` as continuous
#'   variables, and the factorization of the variable including split by median, by quartiles, and by `factor_breaks`
#'   and `quantile_breaks`. The setting of the covariates includes different models with different covariates.
#' @note For factor variables with more than 2 levels, p value for trend is also calculated.
#' @export
#' @examples
#' data(cancer, package = "survival")
#' # coxph model with time assigned
#' regression_basic_results(cancer,
#'   x = "age", y = "status", time = "time",
#'   model_covs = list(Crude = c(), Model1 = c("ph.karno"), Model2 = c("ph.karno", "sex"))
#' )
#'
#' # logistic model with time not assigned
#' cancer$dead <- cancer$status == 2
#' regression_basic_results(cancer,
#'   x = "age", y = "dead", ref_levels = c("Q3", "High"),
#'   model_covs = list(Crude = c(), Model1 = c("ph.karno"), Model2 = c("ph.karno", "sex"))
#' )
regression_basic_results <- function(data, x, y, time = NULL, model_covs = NULL, pers = c(0.1, 10, 100),
                                     factor_breaks = NULL, factor_labels = NULL, quantile_breaks = NULL,
                                     quantile_labels = NULL, label_with_range = FALSE,
                                     output_dir = NULL, ref_levels = "lowest", ratio_nsmall = 2, pval_nsmall = 3,
                                     pval_eps = 1e-3, median_nsmall = 0, colors = NULL, xlab = NULL, legend_title = x,
                                     legend_pos = c(0.8, 0.8), height = 6, width = 6, pval_pos = NULL, ...) {
  if (is.null(colors)) {
    colors <- emp_colors
  }

  if (is.null(time)) {
    analysis_type <- "logistic"
    ratio_type <- "OR"
    new_time_var <- NULL
  } else {
    analysis_type <- "cox"
    ratio_type <- "HR"
    new_time_var <- "time"
  }

  label_type <- ifelse(label_with_range, "combined", "LMH")

  if (is.null(model_covs)) {
    model_covs <- list(Crude = c())
  } else if (is.vector(model_covs, mode = "character")) {
    model_covs <- list(Crude = model_covs)
  } else if (!is.list(model_covs)) {
    stop("model_covs should be a character vector of covariates or a named list of covariates of multiple models.")
  }

  ref_levels <- str_replace_all(ref_levels, c("\\[" = "\\\\[", "\\(" = "\\\\(", "\\]" = "\\\\]", "\\)" = "\\\\)"))

  covs <- unique(unlist(model_covs))
  if (any(c(y, time, x) %in% covs)) {
    stop("conflict of model variables!")
  }

  dat <- dplyr::select(data, all_of(c(y, x, time, covs)))
  if (length(model_covs) > 1) {
    model_complete_cases <- sapply(model_covs, function(tmp_covs) {
      complete.cases(dat[, tmp_covs])
    })
    if (any(!rowSums(model_complete_cases) %in% c(0, length(model_covs)))) {
      warn(paste0(
        "The missing values are present differently across models.\n",
        "The analysis will be performed with the complete cases only.\n",
        "Consider imputing the data before doing this analysis."
      ))
    }
  }
  dat <- na.omit(dat)
  colnames(dat)[c(1:2)] <- c("y", "x")
  if (analysis_type == "cox") {
    start_col <- 4
    colnames(dat)[3] <- new_time_var
  } else {
    start_col <- 3
  }
  ori_covs <- covs
  if (!is.null(covs)) {
    covs <- paste0(".cov", seq_along(covs))
    colnames(dat)[start_col:(start_col + length(covs) - 1)] <- covs
  }
  if (is.null(output_dir)) {
    output_dir <- paste(analysis_type, "results", x, sep = "_")
  }
  if (!file.exists(output_dir)) {
    dir.create(output_dir, recursive = T)
  }
  if (is.null(xlab)) {
    xlab <- time
  }

  ncol_res <- length(model_covs) * 2 + 2
  nrow_res <- 0
  if (is.numeric(dat$x) && (length(na.omit(unique(dat$x))) > 5)) {
    nrow_res <- 12 + length(pers)
    for (per in pers) {
      dat[[paste0("x.", per)]] <- dat$x / per
    }
    dat$x.std <- c(scale(dat$x))
    if (all(dat$x > 0)) {
      dat$x.log <- log(dat$x)
      nrow_res <- nrow_res + 1
    }
    dat$x.quartile <- cut_by(dat$x, c(1:3) / 4,
      breaks_as_quantiles = TRUE, labels = paste0("Q", 1:4),
      label_type = label_type
    )
    dat$x.median <- cut_by(dat$x, 1 / 2, breaks_as_quantiles = TRUE, label_type = label_type)
    if (!is.null(factor_breaks)) {
      dat$x.clin <- cut_by(dat$x, factor_breaks, labels = factor_labels, label_type = label_type)
      if (length(factor_breaks) == 1) {
        nrow_res <- nrow_res + 3
      } else {
        nrow_res <- nrow_res + length(factor_breaks) + 3
      }
    }
    if (!is.null(quantile_breaks)) {
      dat$x.quantile <- cut_by(dat$x, quantile_breaks,
        breaks_as_quantiles = T, labels = quantile_labels,
        label_type = label_type
      )
      if (length(quantile_breaks) == 1) {
        nrow_res <- nrow_res + 3
      } else {
        nrow_res <- nrow_res + length(quantile_breaks) + 3
      }
    }
  } else {
    dat$x <- as.factor(dat$x)
    if (length(levels(dat$x)) == 2) {
      nrow_res <- 4
    } else {
      nrow_res <- length(levels(dat$x)) + 3
    }
  }
  dat0 <- dat
  vars <- colnames(dat)[grep("x", colnames(dat))]
  for (var in vars) {
    if (is.factor(dat[[var]])) {
      formula <- create_formula("y", var, time = new_time_var)
      if (analysis_type == "cox") {
        fit <- survminer::surv_fit(formula = formula, data = dat)
        log_rank_p <- survdiff(formula = formula, data = dat)$pvalue
        log_rank_p <- format_pval(log_rank_p,
          text_ahead = "Log-rank\np",
          nsmall = pval_nsmall, eps = pval_eps
        )
        p <- survminer::ggsurvplot(fit,
          pval = log_rank_p,
          pval_pos = pval_pos,
          legend = legend_pos,
          legend_title = legend_title,
          legend.labs = levels(dat[[var]]),
          xlab = xlab,
          risk.table = T,
          palette = colors,
          ...
        )
        if (!is.null(fit$strata) || is.matrix(fit$surv)) {
          .table <- as.data.frame(summary(fit)$table)
        } else {
          .table <- t(as.data.frame(summary(fit)$table))
          rownames(.table) <- "All"
        }
        xline <- round(as.vector(.table[["median"]]), median_nsmall)
        tmp <- data.frame(level = levels(dat[[var]]), x = xline, text = paste0("t=", xline))
        if (nrow(tmp) > 0) {
          tmp$y <- c((1:10) / 20)[seq_len(nrow(tmp))]
          p$plot <- p$plot +
            annotate("text",
              label = paste0("N = ", sum(p$data.survtable$n.risk[p$data.survtable$time == 0])), size = 5,
              x = mean(ggplot_build(p$plot)$layout$panel_params[[1]]$x.range),
              y = max(ggplot_build(p$plot)$layout$panel_params[[1]]$y.range) * 0.9,
              hjust = 0.5, vjust = 0.5
            ) +
            geom_text(data = tmp, aes(x, y, label = text, color = level), hjust = 1, show.legend = F) +
            geom_segment(
              data = tmp, aes(
                x = x, xend = x, y = 0, yend = 0.5,
                colour = level
              ),
              linetype = 2, show.legend = F
            ) +
            geom_segment(y = 0.5, yend = 0.5, x = 0, xend = max(tmp$x), linetype = 2, show.legend = F)
        }
        ggsave(
          paste0(output_dir, "/kmplot_", var, ".png"),
          plot = survminer::arrange_ggsurvplots(list(p), print = FALSE, ncol = 1),
          width = width, height = height
        )
      }
    }
  }
  for (var in vars) {
    if (is.factor(dat[[var]])) {
      if (identical(ref_levels, "highest")) {
        dat[[var]] <- factor(dat[[var]], levels = rev(levels(dat[[var]])))
      } else if (!identical(ref_levels, "lowest")) {
        level_match <- grepl(paste0(ref_levels, collapse = "|"), levels(dat[[var]]))
        if (any(level_match)) {
          new_ref <- levels(dat[[var]])[level_match][1]
          dat[[var]] <- relevel(dat[[var]], ref = new_ref)
        }
      }
    }
  }

  res_table <- data.frame(matrix(NA, nrow = nrow_res, ncol = ncol_res))
  colnames(res_table) <- c("Terms", "Count", rep(names(model_covs), each = 2))
  res_table$Terms[1] <- paste0(x, " (All)")
  res_table$Count[1] <- length(na.omit(dat$x))
  i <- 2
  for (var in vars) {
    if (is.numeric(dat[[var]])) {
      if (var == "x") {
        res_table$Terms[i] <- "Continuous"
      } else if (var == "x.std") {
        res_table$Terms[i] <- "Continuous, per 1 SD"
      } else if (var == "x.log") {
        res_table$Terms[i] <- "Continuous, logarithm"
      } else {
        res_table$Terms[i] <- paste0("Continuous, per ", str_remove(var, "x\\."))
      }
      i <- i + 1
    } else {
      n_levels <- length(levels(dat[[var]]))
      if (var == "x.quartile") {
        res_table$Terms[i] <- "Grouped by Quartiles"
      } else if (var == "x.median") {
        res_table$Terms[i] <- "Grouped by Median Value"
      } else if (var == "x.clin") {
        res_table$Terms[i] <- "Grouped by Clinical Value"
      } else {
        res_table$Terms[i] <- "Values"
      }
      res_table$Terms[i + 1:n_levels] <- paste0("  ", levels(dat[[var]]))
      res_table$Count[i + 1:n_levels] <- c(table(dat[[var]]))
      i <- i + n_levels + 1
      if (n_levels > 2) {
        res_table$Terms[i] <- "  P for trend"
        i <- i + 1
      }
    }
  }
  for (j in seq_along(model_covs)) {
    col1 <- 2 * j + 1
    col2 <- 2 * j + 2
    res_table[1, col1:col2] <- c(ratio_type, "P")
    i <- 2
    tmp_covs <- covs[match(model_covs[[j]], ori_covs)]
    for (var in vars) {
      model_res <- regression_fit(
        data = dat, y = "y", predictor = var, time = new_time_var,
        covs = tmp_covs, returned = "full"
      )
      model_res <- data.frame(model_res[grepl("x", model_res$term), ])
      for (col in c("estimate", "conf.low", "conf.high")) {
        model_res[, col] <- format(model_res[, col], digits = 1, nsmall = ratio_nsmall)
      }
      tmp <- data.frame(
        term = model_res$term,
        ratio = paste0(model_res$estimate, "(", model_res$conf.low, ",", model_res$conf.high, ")"),
        P = format_pval(model_res$p.value, nsmall = pval_nsmall, eps = pval_eps)
      )
      colnames(tmp)[2] <- ratio_type
      if (is.numeric(dat[[var]])) {
        res_table[i, col1] <- tmp[[ratio_type]]
        res_table[i, col2] <- tmp$P
        i <- i + 1
      } else {
        res_table[i + 2:(nrow(tmp) + 1), col1:col2] <- tmp[, -1]
        res_table[i + 1, col1] <- "1 (Reference)"
        i <- i + nrow(tmp) + 2
        if (nrow(tmp) > 1) {
          dat$tmp <- as.numeric(dat0[[var]])
          model_res <- regression_fit(
            data = dat, y = "y", predictor = "tmp", time = new_time_var,
            covs = tmp_covs, returned = "predictor_combined"
          )
          res_table[i, col2] <- format_pval(model_res$p.value, nsmall = pval_nsmall, eps = pval_eps)
          i <- i + 1
        }
      }
    }
  }
  write.csv(res_table, paste0(output_dir, "/table_", x, ".csv"), row.names = FALSE)
}


#' Forest plot of regression results
#' @description Generate the forest plot of logistic or Cox regression with different models.
#' @param data A data frame.
#' @param x A character string of the predictor variable.
#' @param y A character string of the outcome variable.
#' @param time A character string of the time variable. If `NULL`, logistic regression is used.
#'   Otherwise, Cox proportional hazards regression is used.
#' @param model_covs A character vector or a named list of covariates for different models.
#'   If `NULL`, only the crude model is used.
#' @param ratio_nsmall The minimum number of digits to the right of the decimal point for the OR or HR.
#' @param pval_nsmall The minimum number of digits to the right of the decimal point for the p-value.
#' @param pval_eps The threshold for rounding p values to 0.
#' @param xlab A character string of the x-axis label.
#' @param height The height of the plot.
#' @param width The width of the plot.
#' @param ... Additional arguments passed to the `survminer::ggsurvplot` function for KM curve.
#'
#' @details The function `regression_basic_results` generates the result table of logistic or Cox regression with
#'   different settings of the predictor variable and covariates. The setting of the predictor variable includes
#'   the original `x`, the standardized `x`, the log of `x`, and `x` divided by denominators in `pers` as continuous
#'   variables, and the factorization of the variable including split by median, by quartiles, and by `factor_breaks`
#'   and `quantile_breaks`. The setting of the covariates includes different models with different covariates.
#' @note For factor variables with more than 2 levels, p value for trend is also calculated.
#' @export
#' @examples
#' data(cancer, package = "survival")
#' # coxph model with time assigned
#' regression_basic_results(cancer,
#'   x = "age", y = "status", time = "time",
#'   model_covs = list(Crude = c(), Model1 = c("ph.karno"), Model2 = c("ph.karno", "sex"))
#' )
#'
#' # logistic model with time not assigned
#' cancer$dead <- cancer$status == 2
#' regression_basic_results(cancer,
#'   x = "age", y = "dead", ref_levels = c("Q3", "High"),
#'   model_covs = list(Crude = c(), Model1 = c("ph.karno"), Model2 = c("ph.karno", "sex"))
#' )
regression_forest <- function(data, x, y, time = NULL, model_covs = NULL, ratio_nsmall = 2, pval_nsmall = 3,
                              pval_eps = 1e-3, xlab = NULL, height = 6, width = 6, ...) {

}


#' Scan for significant regression predictors
#' @description Scan for significant regression predictors and output results. Both logistic and Cox
#'   proportional hazards regression models are supported. The predictor variables in the model are can be
#'   used both in linear form or in restricted cubic spline form.
#' @param data A data frame.
#' @param y A character string of the outcome variable.
#' @param time A character string of the time variable. If `NULL`, logistic regression is used.
#'   Otherwise, Cox proportional hazards regression is used.
#' @param predictors The predictor variables to be scanned for interactions. If `NULL`, all variables
#'   except `y` and `time` are taken as predictors.
#' @param covs A character vector of covariate names.
#' @param num_to_factor An integer. Numerical variables with number of unique values below or equal
#'   to this value would be considered a factor.
#' @param p_adjust_method The method to use for p-value adjustment for pairwise comparison. Default is "BH".
#'   See `?p.adjust.methods`.
#' @param save_table A logical value indicating whether to save the results as a table.
#' @param filename The name of the file to save the results. File will be saved in `.csv` format.
#' @return A data frame containing the results of the interaction analysis.
#' @details The function first determines the type of each predictor variable (`numerical`, `factor`,
#'   `num_factor` (numerical but with less unique values than or equal to `num_to_factor`), or
#'   `other`). Then, it performs regression analysis for available transforms of each predictor variable
#'   and saves the results.
#' @section The available transforms for each predictor type are:
#'   - `numerical`: `original`, `logarithm`, `categorized`, `rcs`
#'   - `num_factor`: `original`, `categorized`
#'   - `factor`: `original`
#'   - `other`: none
#' @section The transforms are applied as follows:
#'   - `original`: Fit the regression model with the original variable. Provide HR/OR and p-values in results.
#'   - `logarithm`: If the `numerical` variable is all greater than 0, fit the regression model with the
#'   log-transformed variable. Provide HR/OR and p-values in results.
#'   - `categorized`: For `numerical` variables, fit the regression model with the binarized variable split
#'   at the median value. For `num_factor` variables, fit the regression model with the variable after `as.factor()`.
#'   Provide HR/OR and p-values in results. If the number of levels is greater than 2, no single HR/OR is provided,
#'   but the p-value of the overall test can be provided with TYPE-2 ANOVA from `car::Anova()`.
#'   - `rcs`: Fit the regression model with the restricted cubic spline variable. The overall and nonlinear p-values
#'   are provided in results. These p-vals are calculated by `anova()` of `rms::cph()` or `rms::Glm`.
#' @export
#' @examples
#' data(cancer, package = "survival")
#' regression_scan(cancer, y = "status", time = "time")
regression_scan <- function(data, y, time = NULL, predictors = NULL, covs = NULL, num_to_factor = 5,
                            p_adjust_method = "BH", save_table = TRUE, filename = NULL) {
  supported_var_trans <- list(
    numerical = c("original", "logarithm", "categorized", "rcs"),
    num_factor = c("original", "categorized"),
    factor = c("original"),
    other = c()
  )
  if (is.null(time)) {
    analysis_type <- "logistic"
    ratio_type <- "OR"
  } else {
    analysis_type <- "cox"
    ratio_type <- "HR"
  }
  if (is.null(predictors)) {
    predictors <- setdiff(colnames(data), c(y, time))
    message("Taking all variables as interaction predictors")
  }
  if (any(!predictors %in% colnames(data))) {
    stop("Some predictors are not in the data")
  }

  res_df <- data.frame(matrix(NA, nrow = length(predictors), ncol = 16))
  colnames(res_df) <- c(
    "predictor", "nvalid",
    paste(rep(c("original", "logarithm", "categorized"), each = 3),
      c(ratio_type, "pval", "padj"),
      sep = "."
    ),
    paste("rcs", rep(c("overall", "nonlinear"), each = 2), c("pval", "padj"), sep = "."),
    "best.var.trans"
  )

  res_df$predictor <- predictors
  for (i in seq_along(predictors)) {
    predictor <- predictors[i]
    covs <- remove_conflict(covs, c(y, predictor, time), silent = TRUE)
    dat <- dplyr::select(data, all_of(c(y, predictor, time, covs)))
    dat <- na.omit(dat)
    nvalid <- nrow(dat)
    res_df$nvalid[i] <- nvalid
    if (nvalid < 10) {
      next
    }
    if (is.factor(dat[[predictor]]) || is.logical(dat[[predictor]])) {
      predictor_type <- "factor"
    } else if (is.numeric(dat[[predictor]])) {
      if (length(unique(dat[[predictor]])) <= num_to_factor) {
        predictor_type <- "num_factor"
      } else {
        predictor_type <- "numerical"
      }
    } else {
      predictor_type <- "other"
    }
    for (var_trans in supported_var_trans[[predictor_type]]) {
      tmp_dat <- dat
      rcs_knots <- NULL
      if (var_trans == "logarithm") {
        if (any(dat[[predictor]] <= 0)) {
          next
        }
        tmp_dat[[predictor]] <- log(tmp_dat[[predictor]])
      } else if (var_trans == "categorized") {
        if (predictor_type == "num_factor") {
          tmp_dat[[predictor]] <- as.factor(tmp_dat[[predictor]])
        } else {
          tmp_dat[[predictor]] <- cut_by(
            tmp_dat[[predictor]],
            breaks = 0.5,
            breaks_as_quantiles = TRUE
          )
        }
      } else if (var_trans == "rcs") {
        rcs_knots <- 4
      }
      model_res <- regression_fit(
        data = tmp_dat, y = y, predictor = predictor, time = time,
        covs = covs, rcs_knots = rcs_knots, returned = "predictor_combined"
      )
      if (var_trans == "rcs") {
        res_df$rcs.overall.pval[i] <- model_res$p_overall
        res_df$rcs.nonlinear.pval[i] <- model_res$p_nonlinear
      } else {
        res_df[[paste(var_trans, ratio_type, sep = ".")]][i] <- model_res$estimate
        res_df[[paste(var_trans, "pval", sep = ".")]][i] <- model_res$p.value
      }
    }
  }
  res_df <- res_df[order(res_df$original.pval, decreasing = FALSE), ]
  p_types <- c("original", "logarithm", "categorized", "rcs.overall", "rcs.nonlinear")
  for (p_type in p_types) {
    res_df[[paste(p_type, "padj", sep = ".")]] <- p.adjust(
      res_df[[paste(p_type, "pval", sep = ".")]],
      method = p_adjust_method
    )
  }
  p_table <- res_df[, grepl("pval", colnames(res_df))]
  for (i in seq_along(predictors)) {
    res_df$best.var.trans[i] <- p_types[which.min(unlist(p_table[i, ]))]
  }
  if (save_table) {
    if (is.null(filename)) {
      filename <- paste(analysis_type, y, "regression_scan.csv", sep = "_")
    }
    write.csv(res_df, filename, row.names = FALSE)
  }
  return(res_df)
}

#' Obtain regression results
#' @description This function fit the regression of a predictor in a
#'   logistic or Cox proportional hazards model.
#' @param data A data frame.
#' @param y A character string of the outcome variable.
#' @param predictor A character string of the predictor variable.
#' @param time A character string of the time variable. If `NULL`, logistic regression is used.
#'   Otherwise, Cox proportional hazards regression is used.
#' @param covs A character vector of covariate names.
#' @param rcs_knots The number of rcs knots. If `NULL`, a linear model would be fitted instead.
#' @param returned The return mode of this function.
#'   - `"full"`: return the full regression model.
#'   - `"predictor_split"`: return the regression parameter of the predictor, could have multiple lines.
#'   - `"predictor_combined"`: return the regression parameter of the predictor, test the predictor as a whole and
#'     takes only one line.
#' @return A list containing the regression ratio and p-value of the predictor. If `rcs_knots` is not `NULL`,
#'   the list contains the overall p-value and the nonlinear p-value of the rcs model. If `return_full_result`
#'   is `TRUE`, the complete result of the regression model is returned.
#' @export
#' @examples
#' data(cancer, package = "survival")
#' regression_fit(data = cancer, y = "status", predictor = "age", time = "time", rcs_knots = 4)
regression_fit <- function(data, y, predictor, time = NULL, covs = NULL, rcs_knots = NULL,
                           returned = c("full", "predictor_split", "predictor_combined")) {
  returned <- match.arg(returned)
  if (!is.null(rcs_knots) && rcs_knots == 0) rcs_knots <- NULL
  analysis_type <- ifelse(is.null(time), "logistic", "cox")
  covs <- remove_conflict(covs, c(y, predictor, time))

  predictor_type <- if (is.factor(data[[predictor]]) && length(levels(data[[predictor]])) > 2) {
    "multi_factor"
  } else {
    "num_or_binary"
  }

  formula <- create_formula(y, predictor, time = time, covs = covs, rcs_knots = rcs_knots)
  environment(formula) <- environment()

  if (analysis_type == "cox") {
    model <- survival::coxph(formula, data = data)
  } else {
    model <- stats::glm(formula, data = data, family = stats::binomial())
  }
  full_res <- broom::tidy(model, conf.int = TRUE, exponentiate = TRUE)
  if (analysis_type == "logistic") full_res <- full_res[-1, ]

  if (returned == "full") {
    return(as.data.frame(full_res))
  } else if (predictor_type == "num_or_binary" && is.null(rcs_knots)) {
    return(as.data.frame(full_res[1, ]))
  } else if (returned == "predictor_split") {
    if (is.numeric(data[[predictor]])) {
      res <- as.data.frame(full_res[seq_len(rcs_knots - 1), ])
    } else {
      res <- as.data.frame(full_res[seq_len(length(levels(data[[predictor]])) - 1), ])
    }
    if (any(!grepl(predictor, res$term))) stop("predictor not found in the regression result")
    return(res)
  } else {
    if (is.numeric(data[[predictor]]) && !is.null(rcs_knots)) {
      if (analysis_type == "cox") {
        model <- cph(formula, data = data, x = TRUE, y = TRUE)
      } else {
        model <- Glm(formula, data = data, family = binomial(), x = TRUE, y = TRUE)
      }
      ps <- unname(anova(model, test = "LR")[, "P"])
      return(list(estimate = NA, p_overall = ps[1], p_nonlinear = ps[2]))
    } else {
      return(list(
        estimate = NA,
        p.value = broom::tidy(car::Anova(model, type = 2, test.statistic = "Wald"))$p.value[1]
      ))
    }
  }
}
