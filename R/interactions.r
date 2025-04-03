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
#' @param covs A character vector of covariate names.
#' @param try_rcs A logical value indicating whether to perform ristricted cubic spline interaction analysis.
#' @param save_table A logical value indicating whether to save the results as a table.
#' @param filename The name of the file to save the results. Support both `.xlsx` and `.csv` formats.
#' @return A data frame containing the results of the interaction analysis.
#' @export
#' @examples
#' data(cancer, package = "survival")
#' int_scan(cancer, y = "status", time = "time")
int_scan <- function(data, y, time = NULL, predictors = NULL, group_vars = NULL, covs = NULL,
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

  res_df <- data.frame(matrix(NA, nrow = length(predictors) * length(group_vars), ncol = 5))
  colnames(res_df) <- c("predictor", "group.by", "nvalid", "lin.pval", "rcs.pval")

  irow <- 1
  for (predictor in predictors) {
    for (group_var in group_vars) {
      if (group_var != predictor) {
        nvalid = sum(complete.cases(data[, c(time, y, predictor, group_var)]))
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
    }
  }
  res_df <- res_df[order(res_df$lin.pval, decreasing = F), ]
  res_df$lin.p.adj = p.adjust(res_df$lin.pval)
  res_df$rcs.p.adj = p.adjust(res_df$rcs.pval)
  if (try_rcs) {
    res_df = res_df[!is.na(res_df$predictor), ]
  }else {
    res_df = res_df[!is.na(res_df$predictor), -c(5, 7)]
  }
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
  return(res_df)
}

#' Plot interactions
int_plot <- function(data, y, predictor, group_var, time = NULL, covs = NULL, group_colors = NULL,
                     save_plot = TRUE, filename = NULL, height = 4, width = 4, xlab = predictor,
                     group_title = group_var, ...) {

  analysis_type <- ifelse(is.null(time), "logistic", "cox")
  if (is.null(group_colors)) {
    group_colors <- .color_panel
  }
  if (any(c(y, time, predictor, group_var) %in% covs)) {
    stop("Conflict of model variables!")
  }
  if (is.null(filename)) {
    filename = paste0(paste0(c("interaction", predictor, "by", group_var, paste0("with_", length(covs), "covs")),
                             collapse = "_"), ".png")
  }

  dat <- dplyr::select(data, all_of(c(y, time, predictor, group_var, covs)))
  dat <- na.omit(dat)
  dat[[group_var]] <- to_factor(dat[[group_var]])
  levels(dat[[group_var]]) <- paste0(levels(dat[[group_var]]), " (n=", table(dat[[group_var]]), ")")

  if (!is.numeric(dat[[predictor]])) {
    dat[[predictor]] <- as.factor(dat[[predictor]])
    predictor_lvl <- levels(dat[[predictor]])
    prefix <- "factor_"
  } else {
    predictor_lvl <- seq(min(dat[[predictor]]), max(dat[[predictor]]), length.out = 100)
    prefix <- "lin_"
  }
  dd <<- rms::datadist(dat)
  old <- options()
  on.exit(options(old))
  options(datadist = "dd")
  tryCatch(
    {
      formula1 <- paste0("Surv(time,y)~predictor+group_var", cov.terms)
      formula2 <- paste0("Surv(time,y)~predictor*group_var", cov.terms)
      # model1=coxph(as.formula(formula1),data=dat)
      # model2=coxph(as.formula(formula2),data=dat)
      # tmp1=anova(model1,model2,test = 'LRT')
      model1 <- cph(as.formula(formula1), data = dat)
      model2 <- cph(as.formula(formula2), data = dat)
      logLik_model1 <- logLik(model1)
      logLik_model2 <- logLik(model2)
      LR_statistic <- 2 * (logLik_model2 - logLik_model1)
      df <- model2$stats[["d.f."]] - model1$stats[["d.f."]]
      lin_p_value <- int_p_value(data, y, predictor, group_var, time = time, covs = covs)
      # print(paste0('lin p diff:',tmp1$`Pr(>|Chi|)`[2]/lin_p_value))
      # pdata = expand.grid(x=predictor_lvl,y=group_lvl)
      # for(var in covs){
      #   pdata[,var]=median(dat[,var])
      # }
      # colnames(pdata)[c(1,2)]=c(predictor,group_var)

      y1 <- as.data.frame(Predict(model2, predictor, group_var,
        fun = exp,
        type = "predictions", conf.int = 0.95, digits = 2
      ))
      grob <- grobTree(textGrob(paste0("N = ", nrow(dat)),
        x = 0.5, y = 0.9,
        gp = gpar(col = "black", fontsize = 12)
      ))
      # ypred1 = predict(model2, newdata=pdata, se=TRUE)
      # y1 = ypred1$linear.predictors + outer(ypred1$se.fit, c(0, -1.96, 1.96), '*')-mean(ypred1$linear.predictors)
      # y1=cbind(pdata[,1:2],exp(y1))
      # colnames(y1)=c('predictor','group_var','y','ylb','yub')
      plt1 <- ggplot(data = y1, aes(x = predictor, y = yhat, ymin = lower, ymax = upper, fill = group_var, color = group_var))
      if (is.factor(dat[[predictor]])) {
        plt1 <- plt1 +
          geom_point(position = position_dodge(width = 1)) +
          geom_errorbar(position = position_dodge(width = 1))
      } else {
        plt1 <- plt1 +
          geom_ribbon(lty = 2, alpha = 0.2, linewidth = 1) +
          geom_line(linewidth = 1)
      }
      plt1 <- plt1 +
        scale_y_log10() + # 对数变换
        scale_color_manual(values = group_colors) +
        scale_fill_manual(values = group_colors) +
        labs(
          x = xlab, y = "HR (95% CI)", color = group_title, fill = group_title,
          title = paste0(
            "p interaction : ",
            base::format.pval(lin_p_value,
              digits = 1,
              nsmall = 3, eps = 0.001
            )
          )
        ) +
        annotation_custom(grob) +
        theme_classic() +
        geom_hline(yintercept = 1, linetype = 3, color = "black", linewidth = 1) +
        theme(
          legend.position = c(0.05, 0.95),
          legend.justification = c(0, 1),
          legend.box.margin = margin(6, 6, 6, 6),
          legend.background = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 15),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 12),
          axis.title = element_text(size = 15)
        )
      if (save_plot) {
        ggsave(paste0(prefix, filename), plt1, height = height, width = width)
      }
    },
    error = function(e) {
    }
  )
  if (length(unique(dat[[predictor]])) > 5) {
    tryCatch(
      {
        formula3 <- paste0("Surv(time,y)~rcs(predictor,4) + group_var", cov.terms)
        formula4 <- paste0("Surv(time,y)~rcs(predictor,4) * group_var", cov.terms)
        # model3=coxph(as.formula(formula3),data=dat)
        # model4=coxph(as.formula(formula4),data=dat)
        # tmp2=anova(model3,model4,test = 'LRT')
        model3 <- cph(as.formula(formula3), data = dat)
        model4 <- cph(as.formula(formula4), data = dat)
        logLik_model3 <- logLik(model3)
        logLik_model4 <- logLik(model4)
        LR_statistic <- 2 * (logLik_model4 - logLik_model3)
        df <- model4$stats[["d.f."]] - model3$stats[["d.f."]]
        rcs_p_value <- c(1 - pchisq(LR_statistic, df))
        # print(paste0('rcs p diff:',tmp2$`Pr(>|Chi|)`[2]/rcs_p_value))
        # ypred2 = predict(model4, newdata=pdata, se=TRUE)
        # y2 = ypred2$fit + outer(ypred2$se, c(0, -1.96, 1.96), '*')-mean(ypred2$fit)
        # y2=cbind(pdata[,1:2],exp(y2))
        # colnames(y2)=c('predictor','group_var','y','ylb','yub')
        y2 <- as.data.frame(Predict(model4, predictor, group_var,
          fun = exp,
          type = "predictions", conf.int = 0.95, digits = 2
        ))
        plt2 <- ggplot(data = y2, aes(x = predictor, y = yhat, ymin = lower, ymax = upper, fill = group_var, color = group_var)) +
          geom_line(linewidth = 1) +
          geom_ribbon(lty = 2, alpha = 0.2, linewidth = 1) +
          scale_y_log10() + # 对数变换
          scale_color_manual(values = group_colors) +
          scale_fill_manual(values = group_colors) +
          labs(
            x = xlab, y = "HR (95% CI)", color = group_title, fill = group_title,
            title = paste0("p interaction : ", base::format.pval(rcs_p_value,
              digits = 1,
              nsmall = 3, eps = 0.001
            ))
          ) +
          annotation_custom(grob) +
          theme_classic() +
          geom_hline(yintercept = 1, linetype = 3, color = "black", linewidth = 1) +
          theme(
            legend.position = c(0.05, 0.95),
            legend.justification = c(0, 1),
            legend.box.margin = margin(6, 6, 6, 6),
            legend.background = element_blank(),
            plot.title = element_text(hjust = 0.5, size = 15),
            axis.text = element_text(size = 12),
            legend.text = element_text(size = 12),
            axis.title = element_text(size = 15)
          )
        ggsave(paste0("rcs_", fname), plt2, height = height, width = width)
      },
      error = function(e) {
      }
    )
  }
}

#' Calculate interaction p-value
#' @description This function calculates the interaction p-value between a predictor and a group variable in a
#'   logistic or Cox proportional hazards model.
#' @param data A data frame.
#' @param y A character string of the outcome variable.
#' @param predictor A character string of the predictor variable.
#' @param group_var A character string of the group variable. The variable should be categorical. If a
#'   numeric variable is provided, it will be splited by the median value.
#' @param time A character string of the time variable. If `NULL`, logistic regression is used.
#'   Otherwise, Cox proportional hazards regression is used.
#' @param covs A character vector of covariate names.
#' @param rcs_knots The number of rcs knots. If `NULL`, a linear model would be fitted instead.
#' @return The interaction p-value
#' @export
#' @examples
#' data(cancer, package = "survival")
#' int_p_value(data = cancer, y = "status", predictor = "age", group_var = "sex", time = "time", rcs_knots = 3)
int_p_value <- function(data, y, predictor, group_var, time = NULL, covs = NULL, rcs_knots = NULL) {
  analysis_type <- ifelse(is.null(time), "logistic", "cox")
  data[[group_var]] <- to_factor(data[[group_var]])
  covs <- remove_conflict(covs, c(y, predictor, group_var, time))
  if (analysis_type == "cox") {
    outcome <- paste0("Surv(", time, ",", y, ")")
  }else {
    outcome <- y
  }
  if (!is.null(rcs_knots)) {
    predictor <- paste0("rcs(", predictor, ",", rcs_knots, ")")
  }

  formula1 <- formula_add_covs(paste0(outcome, "~", predictor, "+", group_var), covs)
  formula2 <- formula_add_covs(paste0(outcome, "~", predictor, "*", group_var), covs)

  if (analysis_type == "cox") {
    model1 <- coxph(formula1, data = data)
    model2 <- coxph(formula2, data = data)
  }else {
    model1 <- glm(formula1, data = data, family = binomial())
    model2 <- glm(formula2, data = data, family = binomial())
  }
  tmp1 <- anova(model1, model2, test = "LRT")
  return(broom::tidy(tmp1)$p.value[2])
}