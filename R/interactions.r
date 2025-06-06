#' Scan for interactions between variables
#' @description Scan for interactions between variables and output results. Both logistic and Cox
#'   proportional hazards regression models are supported. The predictor variables in the model are can be
#'   used both in linear form or in restricted cubic spline form.
#' @param data A data frame.
#' @param y A character string of the outcome variable.
#' @param time A character string of the time variable. If `NULL`, logistic regression is used.
#'   Otherwise, Cox proportional hazards regression is used.
#' @param predictors The predictor variables to be scanned for interactions. If `NULL`, all variables
#'   except `y` and `time` are taken as predictors.
#' @param group_vars The group variables to be scanned for interactions. If `NULL`, all variables
#'   except `y` and `time` are taken as group variables. The group variables should be categorical. If a
#'   numeric variable is included, it will be split by the median value.
#' @param covars A character vector of covariate names.
#' @param try_rcs A logical value indicating whether to perform restricted cubic spline interaction analysis.
#' @param p_adjust_method The method to use for p-value adjustment for pairwise comparison. Default is "BH".
#'   See `?p.adjust.methods`.
#' @param save_table A logical value indicating whether to save the results as a table.
#' @param filename The name of the file to save the results. File will be saved in `.csv` format.
#' @returns A data frame containing the results of the interaction analysis.
#' @export
#' @examples
#' data(cancer, package = "survival")
#' interaction_scan(cancer, y = "status", time = "time", save_table = FALSE)
interaction_scan <- function(data, y, time = NULL, predictors = NULL, group_vars = NULL, covars = NULL,
                             try_rcs = TRUE, p_adjust_method = "BH", save_table = TRUE, filename = NULL) {
  analysis_type <- if (!is.null(time)) {
    "cox"
  } else if (length(levels(as.factor(data[[y]]))) == 2) {
    "logistic"
  } else {
    "linear"
  }
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
        nvalid <- sum(complete.cases(data[, c(time, y, predictor, group_var, covars)]))
        if (nvalid < 10) {
          next
        }

        p1 <- interaction_p_value(data, y, predictor, group_var, time = time, covars = covars)
        if (try_rcs && length(unique(data[, predictor])) > 10) {
          p2 <- tryCatch(
            interaction_p_value(data, y, predictor, group_var, time = time, covars = covars, rcs_knots = 4),
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
        res_df$linear.p.int[irow] <- p1
        res_df$rcs.p.int[irow] <- p2
        irow <- irow + 1
      }
    }
  }
  res_df <- res_df[order(res_df$linear.p.int, decreasing = FALSE), ]
  res_df$linear.p.adj <- p.adjust(res_df$linear.p.int, method = p_adjust_method)
  res_df$rcs.p.adj <- p.adjust(res_df$rcs.p.int, method = p_adjust_method)
  if (try_rcs) {
    res_df <- res_df[!is.na(res_df$predictor), ]
  } else {
    res_df <- res_df[!is.na(res_df$predictor), -c(5, 7)]
  }
  if (save_table) {
    if (is.null(filename)) {
      filename <- paste0(paste(analysis_type, "interaction_scan", y, sep = "_"), ".csv")
    }
    write.csv(res_df, filename, row.names = FALSE, na = "")
  }
  return(res_df)
}

#' Plot interactions
#' @description Plot interactions between variables. Both logistic and Cox proportional hazards regression models
#'   are supported. The predictor variables in the model are can be used both in linear form or in restricted cubic
#'   spline form.
#' @param data A data frame.
#' @param data A data frame.
#' @param y A character string of the outcome variable.
#' @param predictor A character string of the predictor variable.
#' @param group_var A character string of the group variable. The variable should be categorical. If a
#'   numeric variable is provided, it will be split by the median value.
#' @param time A character string of the time variable. If `NULL`, logistic regression is used.
#'   Otherwise, Cox proportional hazards regression is used.
#' @param covars A character vector of covariate names.
#' @param group_colors A character vector of colors for the plot. If `NULL`, the default colors are used.
#' @param save_plot A logical value indicating whether to save the plot.
#' @param filename The name of the file to save the plot. Support both `.png` and `.pdf` formats.
#' @param height The height of the saved plot.
#' @param width The width of the saved plot.
#' @param xlab The label of the x-axis.
#' @param ylab The label of the y-axis.
#' @param show_n A logical value indicating whether to show the number of observations in the plot.
#' @param group_title The title of the group variable.
#' @param ... Additional arguments passed to the `ggplot` function.
#' @returns A `ggplot` object.
#' @export
#' @examples
#' data(cancer, package = "survival")
#' interaction_plot(cancer,
#'   y = "status", time = "time", predictor = "age", group_var = "sex",
#'   save_plot = FALSE
#' )
#' interaction_plot(cancer,
#'   y = "status", predictor = "age", group_var = "sex",
#'   save_plot = FALSE
#' )
#' interaction_plot(cancer,
#'   y = "wt.loss", predictor = "age", group_var = "sex",
#'   save_plot = FALSE
#' )
interaction_plot <- function(data, y, predictor, group_var, time = NULL, covars = NULL, group_colors = NULL,
                             save_plot = TRUE, filename = NULL, height = 4, width = 4, xlab = predictor,
                             ylab = NULL, show_n = TRUE, group_title = group_var, ...) {
  if (!is.null(time)) {
    analysis_type <- "cox"
    if (is.null(ylab)) {
      ylab <- "HR (95% CI)"
    }
    pred_fun <- exp
  } else if (length(levels(as.factor(data[[y]]))) == 2) {
    analysis_type <- "logistic"
    if (is.null(ylab)) {
      ylab <- "OR (95% CI)"
    }
    pred_fun <- exp
  } else {
    analysis_type <- "linear"
    if (is.null(ylab)) {
      ylab <- paste0("Predicted ", y, " (95% CI)")
    }
    pred_fun <- NULL
  }
  if (is.null(group_colors)) {
    group_colors <- emp_colors
  }
  if (any(c(y, time, predictor, group_var) %in% covars)) {
    stop("Conflict of model variables!")
  }
  if (is.null(filename)) {
    filename <- paste0(
      paste0(c(analysis_type, "interaction", y, "with", predictor, "by", group_var, "with", length(covars), "covars"),
        collapse = "_"
      ), ".png"
    )
  }
  default_expansion <- c(0.05, 0, 0.15, 0)

  dat <- dplyr::select(data, all_of(c(y, predictor, group_var, time, covars)))
  if (".predictor" %in% c(y, time, covars)) stop("Colname '.predictor' is reserved!")
  if (".group_var" %in% c(y, time, covars)) stop("Colname '.group_var' is reserved!")
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
  dd <- rms::datadist(dat, q.display = c(0.025, 0.975))
  old_datadist <- getOption("datadist")
  on.exit(
    {
      options(datadist = old_datadist)
    },
    add = TRUE
  )
  options(datadist = dd)

  plt1 <- NULL
  plt2 <- NULL

  tryCatch(
    {
      formula <- create_formula(y, ".predictor",
        group_var = ".group_var", time = time,
        covars = covars, interaction = TRUE
      )
      model <- fit_model(formula, data = dat, analysis_type = analysis_type, rms = TRUE)
      p_value <- interaction_p_value(dat, y, ".predictor", ".group_var", time = time, covars = covars)
      y1 <- as.data.frame(Predict(model, .predictor, .group_var,
        fun = pred_fun, type = "predictions", conf.int = 0.95, digits = 2,
        ref.zero = analysis_type %in% c("cox", "logistic")
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
        scale_color_manual(values = group_colors) +
        scale_fill_manual(values = group_colors) +
        labs(
          x = xlab, y = ylab,
          color = group_title, fill = group_title,
          title = paste0(
            "p interaction : ",
            format_pval(p_value)
          )
        )

      if (analysis_type %in% c("cox", "logistic")) {
        plt1 <- plt1 +
          scale_y_log10(expand = default_expansion) +
          geom_hline(yintercept = 1, linetype = 3, color = "black", linewidth = 1)
        plt1_panel_params <- ggplot_build(plt1)$layout$panel_params[[1]]
        n_y <- 10^(max(plt1_panel_params$y.range))
      } else {
        plt1 <- plt1 +
          scale_y_continuous(expand = default_expansion)
        plt1_panel_params <- ggplot_build(plt1)$layout$panel_params[[1]]
        n_y <- max(plt1_panel_params$y.range)
      }
      n_x <- mean(plt1_panel_params$x.range)
      if (show_n) {
        plt1 <- plt1 +
          annotate("text",
            label = paste0("N = ", nrow(dat)), size = 5,
            x = n_x,
            y = n_y,
            hjust = 0.5, vjust = 0.5
          )
      }
      plt1 <- plt1 +
        theme_classic() +
        theme(
          legend.position = "inside",
          legend.position.inside = c(0.05, 0.95),
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
          group_var = ".group_var", time = time, covars = covars, rcs_knots = 4,
          interaction = TRUE
        )
        model2 <- fit_model(formula2, data = dat, analysis_type = analysis_type, rms = TRUE)
        rcs_p_value <- interaction_p_value(dat, y, ".predictor", ".group_var",
          time = time, covars = covars,
          rcs_knots = 4
        )
        y2 <- as.data.frame(Predict(model2, .predictor, .group_var,
          fun = pred_fun, type = "predictions", conf.int = 0.95, digits = 2,
          ref.zero = analysis_type %in% c("cox", "logistic")
        ))
        plt2 <- ggplot(data = y2, aes(
          x = .predictor, y = yhat, ymin = lower, ymax = upper,
          fill = .group_var, color = .group_var
        )) +
          geom_line(linewidth = 1) +
          geom_ribbon(lty = 2, alpha = 0.2, linewidth = 1) +
          scale_color_manual(values = group_colors) +
          scale_fill_manual(values = group_colors) +
          labs(
            x = xlab, y = ylab,
            color = group_title, fill = group_title,
            title = paste0(
              "p interaction : ",
              format_pval(rcs_p_value)
            )
          )
        if (analysis_type %in% c("cox", "logistic")) {
          plt2 <- plt2 +
            scale_y_log10(expand = default_expansion) +
            geom_hline(yintercept = 1, linetype = 3, color = "black", linewidth = 1)
          plt2_panel_params <- ggplot_build(plt2)$layout$panel_params[[1]]
          n_y <- 10^(max(plt2_panel_params$y.range))
        } else {
          plt2 <- plt2 +
            scale_y_continuous(expand = default_expansion)
          plt2_panel_params <- ggplot_build(plt2)$layout$panel_params[[1]]
          n_y <- max(plt2_panel_params$y.range)
        }
        n_x <- mean(plt2_panel_params$x.range)

        if (show_n) {
          plt2 <- plt2 +
            annotate("text",
              label = paste0("N = ", nrow(dat)), size = 5,
              x = n_x,
              y = n_y,
              hjust = 0.5, vjust = 0.5
            )
        }

        plt2 <- plt2 +
          theme_classic() +
          theme(
            legend.position = "inside",
            legend.position.inside = c(0.05, 0.95),
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
    return(list(lin = plt1, rcs = plt2))
  } else {
    return(plt1)
  }
}

#' Calculate interaction p-value
#' @description This function calculates the interaction p-value between a predictor and a group variable in a
#'   linear, logistic, or Cox proportional hazards model.
#' @param data A data frame.
#' @param y A character string of the outcome variable. The variable should be binary or numeric and determines
#'   the type of model to be used. If the variable is binary, logistic or Cox regression is used. If the variable is
#'   numeric, linear regression is used.
#' @param predictor A character string of the predictor variable.
#' @param group_var A character string of the group variable. The variable should be categorical. If a
#'   numeric variable is provided, it will be split by the median value.
#' @param time A character string of the time variable. If `NULL`, linear or logistic regression is used.
#'   Otherwise, Cox proportional hazards regression is used.
#' @param covars A character vector of covariate names.
#' @param rcs_knots The number of rcs knots. If `NULL`, a linear model would be fitted instead.
#' @returns A numerical, the interaction p-value
#' @export
#' @examples
#' data(cancer, package = "survival")
#' interaction_p_value(
#'   data = cancer, y = "status", predictor = "age", group_var = "sex",
#'   time = "time", rcs_knots = 4
#' )
interaction_p_value <- function(data, y, predictor, group_var, time = NULL, covars = NULL, rcs_knots = NULL) {
  analysis_type <- if (!is.null(time)) {
    "cox"
  } else if (length(levels(as.factor(data[[y]]))) == 2) {
    "logistic"
  } else {
    "linear"
  }
  data[[group_var]] <- to_factor(data[[group_var]])
  covars <- remove_conflict(covars, c(y, predictor, group_var, time))

  formula1 <- create_formula(y, predictor, group_var,
    time = time, covars = covars, rcs_knots = rcs_knots,
    interaction = FALSE
  )
  formula2 <- create_formula(y, predictor, group_var,
    time = time, covars = covars, rcs_knots = rcs_knots,
    interaction = TRUE
  )

  model1 <- fit_model(formula1, data = data, analysis_type = analysis_type)
  model2 <- fit_model(formula2, data = data, analysis_type = analysis_type)

  return(broom::tidy(anova(model1, model2, test = "LRT"))$p.value[2])
}
