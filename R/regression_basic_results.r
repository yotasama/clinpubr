#' Basic results of logistic or Cox regression.
#' @description Generate the result table of logistic or Cox regression with different settings of the predictor
#'   variable and covariates. Also generate KM curves for Cox regression.
#' @param data A data frame.
#' @param x A character string of the predictor variable.
#' @param y A character string of the outcome variable.
#' @param time A character string of the time variable. If `NULL`, logistic regression is used.
#'   Otherwise, Cox proportional hazards regression is used.
#'
#' @export
#' @examples
#' data(cancer, package = "survival")
#' # coxph model with time assigned
#' rcs_plot(cancer, x = "age", y = "status", time = "time", covs = "ph.karno")
#'
#' # logistic model with time not assigned
#' cancer$dead <- cancer$status == 2
#' rcs_plot(cancer, x = "age", y = "dead", covs = "ph.karno")
regression_basic_results <- function(data, x, y, time = NULL, model_covs = NULL, pers = c(0.1, 10, 100),
                                     factor_breaks = NULL, factor_labels = NULL, n_pos = c(0.4, 0.9),
                                     quantile_breaks = NULL, quantile_labels = NULL, output_dir = NULL,
                                     ref_levels = "lowest", ratio_nsmall = 2, pval_nsmall = 3, pval_eps = 0.001,
                                     median_nsmall = 0, colors = NULL, xlab = NULL, legend_title = x,
                                     legend_pos = c(0.8, 0.8), height = 6, width = 6, pval_pos = NULL, ...) {
  if (is.null(colors)) {
    colors <- .color_panel
  }

  analysis_type <- ifelse(is.null(time), "logistic", "cox")
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
  colnames(dat)[c(1:2)] <- c("y", "x")
  ori_covs <- covs
  if (!is.null(covs)) {
    covs <- paste0(".cov", seq_along(covs))
    if (analysis_type == "cox") {
      start_col <- 4
      colnames(dat)[3] <- "time"
    }else {
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
    for (per in pers) {
      dat[[paste0("x.", per)]] <- dat$x / per
    }
    dat$x.std <- c(scale(dat$x))
    dat$x.IQR <- cut_by(dat$x, c(1:3) / 4, breaks_as_quantiles = TRUE, labels = paste0("Q", 1:4),
                        label_type = "combined")
    dat$x.median <- cut_by(dat$x, 1 / 2, breaks_as_quantiles = TRUE, label_type = "combined")
    nrow_res <- 12 + length(pers)
    if (!is.null(factor_breaks)) {
      dat$x.clin <- cut_by(dat$x, factor_breaks, labels = factor_labels, label_type = "combined")
      if (length(factor_breaks) == 1) {
        nrow_res <- nrow_res + 3
      } else {
        nrow_res <- nrow_res + length(factor_breaks) + 3
      }
    }
    if (!is.null(quantile_breaks)) {
      dat$x.quantile <- cut_by(dat$x, quantile_breaks, breaks_as_quantiles = T, labels = quantile_labels,
                               label_type = "combined")
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
    if (is.factor(dat[, var])) {
      formula <- as.formula(paste0("Surv(time,y)~", var))
      fit <- surv_fit(formula = formula, data = dat)
      log.rank.p <- survdiff(formula = formula, data = dat)$pvalue
      if (log.rank.p < pval_eps) {
        log.rank.p <- paste0("Log-rank\np < ", pval_eps)
      } else {
        log.rank.p <- base::format.pval(log.rank.p, digits = 1, nsmall = pval_nsmall, eps = pval_eps)
        log.rank.p <- paste0("Log-rank\np = ", log.rank.p)
      }
      p <- ggsurvplot(fit,
        pval = log.rank.p,
        pval_pos = pval_pos,
        legend = legend_pos,
        legend_title = legend_title,
        # surv.median.line ='v',
        legend.labs = levels(dat[, var]),
        xlab = xlab,
        risk.table = T,
        palette = palette,
        ...
      )
      dt <- p$data.survplot
      if (!is.null(fit$strata) | is.matrix(fit$surv)) {
        .table <- as.data.frame(summary(fit)$table)
      } else {
        .table <- t(as.data.frame(summary(fit)$table))
        rownames(.table) <- "All"
      }
      xline <- round(as.vector(.table[, "median"]), median_nsmall)
      tmp <- data.frame(level = levels(dat[, var]), x = xline, text = paste0("t=", xline))
      grob <- grobTree(textGrob(paste0("N = ", sum(p$data.survtable$n.risk[p$data.survtable$time == 0])),
        x = n_pos[1], y = n_pos[2], hjust = 0, gp = gpar(col = "black", fontsize = 12)
      ))
      if (nrow(tmp) > 0) {
        tmp$y <- c((1:10) / 20)[1:nrow(tmp)]
        p$plot <- p$plot +
          annotation_custom(grob) +
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
      graph2png(x = print(p), file = paste0(output_dir, "/kmplot_", var, ".png"), width = width, height = height)
    }
  }
  for (var in vars) {
    if (is.factor(dat[, var])) {
      if (identical(ref_levels, "highest")) {
        dat[, var] <- factor(dat[, var], levels = rev(levels(dat[, var])))
      } else if (!identical(ref_levels, "lowest")) {
        if (any(grepl(paste0(ref_levels, collapse = "|"), levels(dat[, var])))) {
          new.ref <- levels(dat[, var])[grepl(paste0(ref_levels, collapse = "|"), levels(dat[, var]))][1]
          dat[, var] <- relevel(dat[, var], ref = new.ref)
        }
      }
    }
  }

  res.table <- data.frame(matrix(NA, nrow = nrow_res, ncol = ncol_res))
  colnames(res.table) <- c("Terms", "Count", rep(names(model_covs), each = 2))
  res.table$Terms[1] <- paste0(x, " (All)")
  res.table$Count[1] <- length(na.omit(dat$x))
  i <- 2
  for (var in vars) {
    if (is.numeric(dat[, var])) {
      if (var == "x") {
        res.table$Terms[i] <- "Continuous"
      } else if (var == "x.std") {
        res.table$Terms[i] <- "Continuous, per 1 SD"
      } else {
        res.table$Terms[i] <- paste0("Continuous, per ", str_remove(var, "x\\."))
      }
      i <- i + 1
    } else {
      n.levels <- length(levels(dat[, var]))
      if (var == "x.IQR") {
        res.table$Terms[i] <- "Grouped by Interquartile Values"
      } else if (var == "x.median") {
        res.table$Terms[i] <- "Grouped by Median Value"
      } else if (var == "x.clin") {
        res.table$Terms[i] <- "Grouped by Clinical Value"
      } else {
        res.table$Terms[i] <- "Values"
      }
      res.table$Terms[i + 1:n.levels] <- levels(dat[, var])
      res.table$Count[i + 1:n.levels] <- c(table(dat[, var]))
      i <- i + n.levels + 1
      if (n.levels > 2) {
        res.table$Terms[i] <- "P for trend"
        i <- i + 1
      }
    }
  }
  for (j in 1:length(model_covs)) {
    col1 <- 2 * j + 1
    col2 <- 2 * j + 2
    res.table[1, col1:col2] <- c("HR", "P")
    i <- 2
    covs.model <- covs[match(model_covs[[j]], ori_covs)]
    for (var in vars) {
      formula <- as.formula(paste0("Surv(time,y)~", paste0(c(var, covs.model), collapse = "+")))
      model <- coxph(formula = formula, data = dat)
      model.res <- tidy(model, conf.int = T, exponentiate = T)
      model.res <- data.frame(model.res[grepl("x", model.res$term), ])
      for (col in c("estimate", "conf.low", "conf.high")) {
        model.res[, col] <- format(model.res[, col], digits = 1, nsmall = ratio_nsmall)
      }
      tmp <- data.frame(
        term = model.res$term,
        HR = paste0(model.res$estimate, "(", model.res$conf.low, ",", model.res$conf.high, ")"),
        P = base::format.pval(model.res$p.value, digits = 1, nsmall = pval_nsmall, eps = pval_eps)
      )
      if (is.numeric(dat[, var])) {
        res.table[i, col1] <- tmp$HR
        res.table[i, col2] <- tmp$P
        i <- i + 1
      } else {
        res.table[i + 2:(nrow(tmp) + 1), col1:col2] <- tmp[, -1]
        res.table[i + 1, col1] <- "1 (Reference)"
        i <- i + nrow(tmp) + 2
        if (nrow(tmp) > 1) {
          dat$tmp <- as.numeric(dat0[, var])
          formula <- as.formula(paste0("Surv(time,y)~", paste0(c("tmp", covs.model), collapse = "+")))
          model <- coxph(formula = formula, data = dat)
          model.res <- tidy(model)
          res.table[i, col2] <- base::format.pval(model.res$p.value[1], digits = 1, nsmall = pval_nsmall, eps = pval_eps)
          i <- i + 1
        }
      }
    }
  }
  write.xlsx(res.table, paste0(output_dir, "/table_", x, ".xlsx"))
}