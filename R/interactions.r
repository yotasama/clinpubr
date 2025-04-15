#' Scan for interactions between variables
#' @description Scan for interactions between variables and output results. Both logistic and Cox
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
#' @param filename The name of the file to save the results. File will be saved in `.csv` format.
#' @return A data frame containing the results of the interaction analysis.
#' @export
#' @examples
#' data(cancer, package = "survival")
#' interaction_scan(cancer, y = "status", time = "time")
interaction_scan <- function(data, y, time = NULL, predictors = NULL, group_vars = NULL, covs = NULL,
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
  if (any(!predictors %in% colnames(data))) {
    stop("Some predictors are not in the data")
  }
  if (any(!group_vars %in% colnames(data))) {
    stop("Some group variables are not in the data")
  }

  res_df <- data.frame(matrix(NA, nrow = length(predictors) * length(group_vars), ncol = 5))
  colnames(res_df) <- c("predictor", "group.by", "nvalid", "linear.p.int", "rcs.p.int")

  irow <- 1
  for (predictor in predictors) {
    for (group_var in group_vars) {
      if (group_var != predictor) {
        nvalid <- sum(complete.cases(data[, c(time, y, predictor, group_var, covs)]))
        if (nvalid < 10) {
          next
        }

        p1 <- interaction_p_value(data, y, predictor, group_var, time = time, covs = covs)
        if (try_rcs && length(unique(data[, predictor])) > 10) {
          p2 <- tryCatch(
            interaction_p_value(data, y, predictor, group_var, time = time, covs = covs, rcs_knots = 4),
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

#' Plot interactions
#' @description Plot interactions between variables. Both logistic and Cox proportional hazards regression models
#'   are supported. The predictor variables in the model are can be used both in linear form or in ristricted cubic
#'   spline form.
#' @param data A data frame.
#' @param data A data frame.
#' @param y A character string of the outcome variable.
#' @param predictor A character string of the predictor variable.
#' @param group_var A character string of the group variable. The variable should be categorical. If a
#'   numeric variable is provided, it will be splited by the median value.
#' @param time A character string of the time variable. If `NULL`, logistic regression is used.
#'   Otherwise, Cox proportional hazards regression is used.
#' @param covs A character vector of covariate names.
#' @param group_colors A character vector of colors for the plot. If `NULL`, the default colors are used.
#' @param save_plot A logical value indicating whether to save the plot.
#' @param filename The name of the file to save the plot. Support both `.png` and `.pdf` formats.
#' @param height The height of the plot.
#' @param width The width of the plot.
#' @param xlab The label of the x-axis.
#' @param group_title The title of the group variable.
#' @param ... Additional arguments passed to the `ggplot` function.
#' @return A `ggplot` object.
#' @export
#' @examples
#' data(cancer, package = "survival")
#' interaction_plot(cancer, y = "status", time = "time", predictor = "age", group_var = "sex")
interaction_plot <- function(data, y, predictor, group_var, time = NULL, covs = NULL, group_colors = NULL,
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
    filename <- paste0(paste0(c("interaction", predictor, "by", group_var, paste0("with_", length(covs), "covs")),
                              collapse = "_"), ".png")
  }

  dat <- dplyr::select(data, all_of(c(y, predictor, group_var, time, covs)))
  if (".predictor" %in% c(y, time, covs)) stop("Colname '.predictor' is reserved!")
  if (".group_var" %in% c(y, time, covs)) stop("Colname '.group_var' is reserved!")
  colnames(dat)[c(2, 3)] <- c(".predictor", ".group_var")
  dat <- na.omit(dat)
  dat$.group_var <- to_factor(dat$.group_var)
  levels(dat$.group_var) <- paste0(levels(dat$.group_var), " (n=", table(dat$.group_var), ")")

  if (!is.numeric(dat$.predictor)) {
    dat$.predictor <- as.factor(dat$.predictor)
    predictor_lvl <- levels(dat$.predictor)
    prefix <- "factor_"
  } else {
    predictor_lvl <- seq(min(dat$.predictor), max(dat$.predictor), length.out = 100)
    prefix <- "lin_"
  }
  dd <<- rms::datadist(dat)
  old <- options()
  on.exit(options(old))
  options(datadist = "dd")

  plt1 <- NULL
  plt2 <- NULL

  tryCatch(
    {
      formula <- create_formula(y, ".predictor", group_var = ".group_var", time = time, covs = covs, interaction = TRUE)
      if (analysis_type == "cox") {
        model <- rms::cph(formula, data = dat)
      } else {
        model <- rms::Glm(formula, data = dat, family = binomial(link = "logit"))
      }
      p_value <- interaction_p_value(dat, y, ".predictor", ".group_var", time = time, covs = covs)
      y1 <- as.data.frame(Predict(model, .predictor, .group_var,
        fun = exp, type = "predictions", conf.int = 0.95, digits = 2
      ))

      plt1 <- ggplot(data = y1, aes(
        x = .predictor, y = yhat, ymin = lower, ymax = upper,
        fill = .group_var, color = .group_var
      ))
      if (is.factor(dat$.predictor)) {
        plt1 <- plt1 +
          geom_point(position = position_dodge(width = 1)) +
          geom_errorbar(position = position_dodge(width = 1))
      } else {
        plt1 <- plt1 +
          geom_ribbon(lty = 2, alpha = 0.2, linewidth = 1) +
          geom_line(linewidth = 1)
      }
      plt1 <- plt1 +
        scale_y_log10() +
        scale_color_manual(values = group_colors) +
        scale_fill_manual(values = group_colors) +
        labs(
          x = xlab, y = ifelse(analysis_type == "cox", "HR (95% CI)", "OR (95% CI)"),
          color = group_title, fill = group_title,
          title = paste0(
            "p interaction : ",
            format_pval(p_value)
          )
        )
      plt1 <- plt1 +
        annotate("text",
          label = paste0("N = ", nrow(dat)), size = 5,
          x = mean(ggplot_build(plt1)$layout$panel_params[[1]]$x.range),
          y = 10^(max(ggplot_build(plt1)$layout$panel_params[[1]]$y.range) * 0.9),
          hjust = 0.5, vjust = 0.5
        ) +
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
  if (length(unique(dat$.predictor)) > 5) {
    tryCatch(
      {
        formula2 <- create_formula(y, ".predictor",
          group_var = ".group_var", time = time, covs = covs, rcs_knots = 4,
          interaction = TRUE
        )
        if (analysis_type == "cox") {
          model2 <- rms::cph(formula2, data = dat)
        } else {
          model2 <- rms::Glm(formula2, data = dat, family = binomial(link = "logit"))
        }
        rcs_p_value <- interaction_p_value(dat, y, ".predictor", ".group_var",
          time = time, covs = covs,
          rcs_knots = 4
        )
        y2 <- as.data.frame(Predict(model2, .predictor, .group_var,
          fun = exp, type = "predictions", conf.int = 0.95, digits = 2
        ))
        plt2 <- ggplot(data = y2, aes(
          x = .predictor, y = yhat, ymin = lower, ymax = upper,
          fill = .group_var, color = .group_var
        )) +
          geom_line(linewidth = 1) +
          geom_ribbon(lty = 2, alpha = 0.2, linewidth = 1) +
          scale_y_log10() +
          scale_color_manual(values = group_colors) +
          scale_fill_manual(values = group_colors) +
          labs(
            x = xlab, y = ifelse(analysis_type == "cox", "HR (95% CI)", "OR (95% CI)"),
            color = group_title, fill = group_title,
            title = paste0(
              "p interaction : ",
              format_pval(rcs_p_value)
            )
          )
        plt2 <- plt2 +
          annotate("text",
            label = paste0("N = ", nrow(dat)), size = 5,
            x = mean(ggplot_build(plt2)$layout$panel_params[[1]]$x.range),
            y = 10^(max(ggplot_build(plt2)$layout$panel_params[[1]]$y.range) * 0.9),
            hjust = 0.5, vjust = 0.5
          ) +
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
          ggsave(paste0("rcs_", filename), plt2, height = height, width = width)
        }
      },
      error = function(e) {
      }
    )
    return(list(plt1 = plt1, plt2 = plt2))
  } else {
    return(plt1)
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
#' interaction_p_value(
#'   data = cancer, y = "status", predictor = "age", group_var = "sex",
#'   time = "time", rcs_knots = 4
#' )
interaction_p_value <- function(data, y, predictor, group_var, time = NULL, covs = NULL, rcs_knots = NULL) {
  analysis_type <- ifelse(is.null(time), "logistic", "cox")
  data[[group_var]] <- to_factor(data[[group_var]])
  covs <- remove_conflict(covs, c(y, predictor, group_var, time))

  formula1 <- create_formula(y, predictor, group_var,
    time = time, covs = covs, rcs_knots = rcs_knots,
    interaction = FALSE
  )
  formula2 <- create_formula(y, predictor, group_var,
    time = time, covs = covs, rcs_knots = rcs_knots,
    interaction = TRUE
  )

  if (analysis_type == "cox") {
    model1 <- coxph(formula1, data = data)
    model2 <- coxph(formula2, data = data)
  } else {
    model1 <- glm(formula1, data = data, family = binomial())
    model2 <- glm(formula2, data = data, family = binomial())
  }
  return(broom::tidy(anova(model1, model2, test = "LRT"))$p.value[2])
}
