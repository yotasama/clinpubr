#' Basic results of logistic or Cox regression.
#' @description Generate the result table of logistic or Cox regression with different settings of the predictor
#'   variable and covariates. Also generate KM curves for Cox regression.
#' @param data A data frame.
#' @param x A character string of the predictor variable.
#' @param y A character string of the outcome variable.
#' @param time A character string of the time variable. If `NULL`, logistic regression is used.
#'   Otherwise, Cox proportional hazards regression is used.
#' @param model_covs A named list of covariates for different models. If `NULL`, only the crude model is used.
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
#'   the original `x`, the standardized `x`, the log of `x`, and `x` devided by denominators in `pers` as continuous
#'   variables, and the factorization of the variable including splitted by median, by quartiles, and by `factor_breaks`
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
    colors <- .color_panel
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
  }
  ref_levels <- str_replace_all(ref_levels, c("\\[" = "\\\\[", "\\(" = "\\\\(", "\\]" = "\\\\]", "\\)" = "\\\\)"))

  covs <- unique(unlist(model_covs))
  if (any(c(y, time, x) %in% covs)) {
    print("conflict of model variables!")
    return()
  }

  dat <- dplyr::select(data, all_of(c(y, x, time, covs)))
  dat <- dat[complete.cases(dat[, c(y, x, time)]), ]
  colnames(dat)[c(1:2)] <- c("y", "x")
  ori_covs <- covs
  if (!is.null(covs)) {
    covs <- paste0(".cov", seq_along(covs))
    if (analysis_type == "cox") {
      start_col <- 4
      colnames(dat)[3] <- new_time_var
    } else {
      start_col <- 3
    }
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
        ggsave(paste0(output_dir, "/kmplot_", var, ".png"), plot = p$plot, width = width, height = height)
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
    covs_tmp <- covs[match(model_covs[[j]], ori_covs)]
    for (var in vars) {
      model_res <- regression_p_value(
        data = dat, y = "y", predictor = var, time = new_time_var,
        covs = covs_tmp, return_full_result = TRUE
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
          model_res <- regression_p_value(
            data = dat, y = "y", predictor = "tmp", time = new_time_var,
            covs = covs_tmp
          )
          res_table[i, col2] <- format_pval(model_res$p.value, nsmall = pval_nsmall, eps = pval_eps)
          i <- i + 1
        }
      }
    }
  }
  write.csv(res_table, paste0(output_dir, "/table_", x, ".csv"), row.names = FALSE)
}

#' Scan for significant regression predictors
#' @description Scan for significant regression predictors and output results. Both logistic and Cox
#'   proportional hazards regression models are supported. The predictor variables in the model are can be
#'   used both in linear form or in ristricted cubic spline form.
#' @param data A data frame.
#' @param y A character string of the outcome variable.
#' @param time A character string of the time variable. If `NULL`, logistic regression is used.
#'   Otherwise, Cox proportional hazards regression is used.
#' @param predictors The predictor variables to be scanned for interactions. If `NULL`, all variables
#'   except `y` and `time` are taken as predictors.
#' @param covs A character vector of covariate names.
#' @param save_table A logical value indicating whether to save the results as a table.
#' @param filename The name of the file to save the results. File will be saved in `.csv` format.
#' @return A data frame containing the results of the interaction analysis.
#' @export
#' @examples
#' data(cancer, package = "survival")
#' int_scan(cancer, y = "status", time = "time")
regression_scan <- function(data, y, time = NULL, predictors = NULL, covs = NULL,
                            try_rcs = TRUE, save_table = TRUE, filename = NULL) {
  if (is.null(time)) {
    analysis_type <- "logistic"
    ratio_type <- "OR"
    new_time_var <- NULL
  } else {
    analysis_type <- "cox"
    ratio_type <- "HR"
    new_time_var <- "time"
  }
  if (is.null(predictors)) {
    predictors <- setdiff(colnames(data), c(y, time))
    message("Taking all variables as interaction predictors")
  }
  if (any(!predictors %in% colnames(data))) {
    stop("Some predictors are not in the data")
  }

  res_df <- data.frame(matrix(NA, nrow = length(predictors), ncol = 15))
  colnames(res_df) <- c(
    "predictor", "nvalid",
    paste0("linear.", ratio_type), "linear.pval", "linear.padj",
    paste0("logarithm.", ratio_type), "logarithm.pval", "logarithm.padj",
    paste0("cat.median.", ratio_type), "cat.median.pval", "cat.median.padj",
    "rcs.overall.pval", "rcs.overall.padj", "rcs.nonlinear.pval", "rcs.nonlinear.padj"
  )

  res_df$predictor <- predictors
  for (i in seq_along(predictors)) {
    predictor <- predictors[i]
    tmp_covs <- remove_conflict(covs, c(y, predictor, time), silent = TRUE)
    dat <- dplyr::select(data, all_of(c(y, predictor, time, tmp_covs)))
    dat <- na.omit(dat)
    nvalid <- nrow(dat)
    if (nvalid < 10) {
      next
    }

    p1 <- int_p_value(data, y, predictor, group_var, time = time, covs = covs)
    if (try_rcs && length(unique(data[, predictor])) > 10) {
      p2 <- tryCatch(
        int_p_value(data, y, predictor, group_var, time = time, covs = covs, rcs_knots = 4),
        error = function(e) {
          NA
        }
      )
    } else {
      p2 <- NA
    }
    res_df$predictor[irow] <- predictor
    res_df$group.by[irow] <- group_var
    res_df$nvalid[irow] <- nvalid
    res_df$lin.pval[irow] <- p1
    res_df$rcs.pval[irow] <- p2
    irow <- irow + 1
  }
  res_df <- res_df[order(res_df$lin.pval, decreasing = FALSE), ]
  res_df$lin.p.adj <- p.adjust(res_df$lin.pval)
  res_df$rcs.p.adj <- p.adjust(res_df$rcs.pval)
  if (try_rcs) {
    res_df <- res_df[!is.na(res_df$predictor), ]
  } else {
    res_df <- res_df[!is.na(res_df$predictor), -c(5, 7)]
  }
  if (save_table) {
    if (is.null(filename)) {
      filename <- paste(analysis_type, y, "interaction_scan.csv", sep = "_")
    }
    write.csv(res_df, filename, row.names = FALSE)
  }
  return(res_df)
}

#' Calculate regression p-value
#' @description This function calculates the regression p-value of a predictor in a
#'   logistic or Cox proportional hazards model.
#' @param data A data frame.
#' @param y A character string of the outcome variable.
#' @param predictor A character string of the predictor variable.
#' @param time A character string of the time variable. If `NULL`, logistic regression is used.
#'   Otherwise, Cox proportional hazards regression is used.
#' @param covs A character vector of covariate names.
#' @param rcs_knots The number of rcs knots. If `NULL`, a linear model would be fitted instead.
#' @param return_full_result A logical value indicating whether to return the complete model. Does not work
#'   for rcs models where `rcs_knots` is not `NULL`.
#' @return A list containing the regression ratio and p-value of the predictor. If `rcs_knots` is not `NULL`,
#'   the list contains the overall p-value and the nonlinear p-value of the rcs model. If `return_full_result`
#'   is `TRUE`, the complete result of the regression model is returned.
#' @export
#' @examples
#' data(cancer, package = "survival")
#' regression_p_value(data = cancer, y = "status", predictor = "age", time = "time", rcs_knots = 4)
regression_p_value <- function(data, y, predictor, time = NULL, covs = NULL, rcs_knots = NULL,
                               return_full_result = FALSE) {
  if (!is.null(rcs_knots) && rcs_knots == 0) rcs_knots <- NULL
  analysis_type <- ifelse(is.null(time), "logistic", "cox")
  covs <- remove_conflict(covs, c(y, predictor, time))
  if (is.factor(data[[predictor]]) && length(levels(data[[predictor]])) > 2) {
    predictor_type <- "multi_factor"
  } else {
    predictor_type <- "num_or_binary"
  }
  formula <- create_formula(y, predictor, time = time, covs = covs, rcs_knots = rcs_knots)
  formula <- as.formula(deparse(formula))

  if (is.factor(data[[predictor]]) || is.null(rcs_knots)) {
    res <- list(estimate = NA, conf.low = NA, conf.high = NA, p.value = NA)
    if (analysis_type == "cox") {
      model <- coxph(formula, data = data)
      full_res <- broom::tidy(model, conf.int = TRUE, exponentiate = TRUE)
      if (return_full_result) {
        return(full_res)
      }
      if (predictor_type == "num_or_binary") {
        predictor_summary <- full_res[1, ]
        res$estimate <- predictor_summary$estimate
        res$conf.low <- predictor_summary$conf.low
        res$conf.high <- predictor_summary$conf.high
      }
    } else {
      model <- glm(formula, data = data, family = binomial())
      full_res <- broom::tidy(model, conf.int = TRUE, exponentiate = TRUE)
      if (return_full_result) {
        return(full_res)
      }
      if (predictor_type == "num_or_binary") {
        predictor_summary <- full_res[2, ]
        res$estimate <- predictor_summary$estimate
        res$conf.low <- predictor_summary$conf.low
        res$conf.high <- predictor_summary$conf.high
      }
    }
    res$p.value <- broom::tidy(car::Anova(model, type = 2, test.statistic = "Wald"))$p.value[1]
    return(res)
  } else {
    if (analysis_type == "cox") {
      model <- cph(formula, data = data)
    } else {
      model <- Glm(formula, data = data, family = binomial())
    }
    ps <- unname(anova(model)[, "P"])
    return(list(p_overall = ps[1], p_nonlinear = ps[2]))
  }
}
