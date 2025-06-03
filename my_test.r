set.seed(1)
load_all()
library(knitr)
library(dtplyr)
var_types <- get_var_types(mtcars, strata = "vs") # Automatically infer variable types
baseline_table(mtcars, var_types = var_types, contDigits = 1, filename = "baseline.csv")

t1=read.csv("baseline.csv", check.names = FALSE)
kable(t1)

data(cancer, package = "survival")
# coxph model with time assigned
regression_basic_results(cancer,
  x = "age", y = "status", time = "time",
  model_covs = list(Crude = c(), Model1 = c("ph.karno"), Model2 = c("ph.karno", "sex")),
  save_output = T
)


time = NULL; model_covs = NULL; pers = c(0.1, 10, 100);
factor_breaks = NULL; factor_labels = NULL; quantile_breaks = NULL;
quantile_labels = NULL; label_with_range = FALSE; save_output = TRUE;
output_dir = NULL; ref_levels = "lowest"; est_nsmall = 2; p_nsmall = 3;
pval_eps = 1e-3; median_nsmall = 0; colors = NULL; xlab = NULL; legend_title = x;
legend_pos = c(0.8, 0.8); height = 6; width = 6; pval_pos = NULL

x = "age"; y = "status"; time = "time";
model_covs = list(Crude = c(), Model1 = c("ph.karno"), Model2 = c("ph.karno", "sex"));
save_output = T
data=cancer

regression_basic_results <- function(data, x, y, time = NULL, model_covs = NULL, pers = c(0.1, 10, 100),
                                     factor_breaks = NULL, factor_labels = NULL, quantile_breaks = NULL,
                                     quantile_labels = NULL, label_with_range = FALSE, save_output = TRUE,
                                     output_dir = NULL, ref_levels = "lowest", est_nsmall = 2, p_nsmall = 3,
                                     pval_eps = 1e-3, median_nsmall = 0, colors = NULL, xlab = NULL, legend_title = x,
                                     legend_pos = c(0.8, 0.8), height = 6, width = 6, pval_pos = NULL, ...) {
  if (is.null(colors)) {
    colors <- emp_colors
  }
  
  if (!is.null(time)) {
    analysis_type <- "cox"
    coef_type <- "HR"
    new_time_var <- "time"
  } else if (length(levels(as.factor(data[[y]]))) == 2) {
    analysis_type <- "logistic"
    coef_type <- "OR"
    new_time_var <- NULL
  } else {
    analysis_type <- "linear"
    coef_type <- "Coefficient"
    new_time_var <- NULL
  }
  
  label_type <- ifelse(label_with_range, "combined", "LMH")
  
  if (is.null(model_covs)) {
    model_covs <- list(Crude = c())
  } else if (is.vector(model_covs, mode = "character")) {
    model_covs <- list(Crude = model_covs)
  } else if (!is.list(model_covs)) {
    stop("`model_covs` should be a character vector of covariates or a named list of covariates of multiple models.")
  }
  
  ref_levels <- str_replace_all(ref_levels, c("\\[" = "\\\\[", "\\(" = "\\\\(", "\\]" = "\\\\]", "\\)" = "\\\\)"))
  
  covars <- unique(unlist(model_covs))
  if (any(c(y, time, x) %in% covars)) {
    stop("conflict of model variables!")
  }
  
  dat <- dplyr::select(data, all_of(c(y, x, time, covars)))
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
  ori_covs <- covars
  if (!is.null(covars)) {
    covars <- paste0(".cov", seq_along(covars))
    colnames(dat)[start_col:(start_col + length(covars) - 1)] <- covars
  }
  if (is.null(output_dir)) {
    output_dir <- paste(analysis_type, "results", x, sep = "_")
  }
  if (save_output && !file.exists(output_dir)) {
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
  plots_list <- list()
  for (var in vars) {
    if (is.factor(dat[[var]])) {
      formula <- create_formula("y", var, time = new_time_var)
      if (analysis_type == "cox") {
        fit <- survminer::surv_fit(formula = formula, data = dat)
        log_rank_p <- survdiff(formula = formula, data = dat)$pvalue
        log_rank_p <- format_pval(log_rank_p,
                                  text_ahead = "Log-rank\np",
                                  nsmall = p_nsmall, eps = pval_eps
        )
        p <- survminer::ggsurvplot(fit,
                                   pval = log_rank_p,
                                   pval_pos = pval_pos,
                                   legend = legend_pos,
                                   legend.title = legend.title,
                                   legend.labs = levels(dat[[var]]),
                                   xlab = xlab,
                                   risk.table = T,
                                   palette = colors
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
        if (save_output) {
          ggsave(
            paste0(output_dir, "/kmplot_", var, ".png"),
            plot = survminer::arrange_ggsurvplots(list(p), print = FALSE, ncol = 1),
            width = width, height = height
          )
        }
        plots_list[[var]] <- p
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
    res_table[1, col1:col2] <- c(coef_type, "P")
    i <- 2
    tmp_covs <- covars[match(model_covs[[j]], ori_covs)]
    for (var in vars) {
      model_res <- regression_fit(
        data = dat, y = "y", predictor = var, time = new_time_var,
        covars = tmp_covs, returned = "full"
      )
      model_res <- data.frame(model_res[grepl("x", model_res$term), ])
      for (col in c("estimate", "conf.low", "conf.high")) {
        model_res[, col] <- format(model_res[, col], digits = 1, nsmall = est_nsmall)
      }
      tmp <- data.frame(
        term = model_res$term,
        ratio = paste0(model_res$estimate, "(", model_res$conf.low, ",", model_res$conf.high, ")"),
        P = format_pval(model_res$p.value, nsmall = p_nsmall, eps = pval_eps)
      )
      colnames(tmp)[2] <- coef_type
      if (is.numeric(dat[[var]])) {
        res_table[i, col1] <- tmp[[coef_type]]
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
            covars = tmp_covs, returned = "predictor_combined"
          )
          res_table[i, col2] <- format_pval(model_res$p.value, nsmall = p_nsmall, eps = pval_eps)
          i <- i + 1
        }
      }
    }
  }
  if (save_output) {
    write.csv(res_table, paste0(output_dir, "/table_", x, ".csv"), row.names = FALSE)
  }
  return(list(table = res_table, plots = plots_list))
}