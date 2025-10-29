#' Plot the effect of a predictor variable
#' @description This is a versatile function to plot the relationship between a predictor variable and the outcome.
#' It supports numeric (linear or RCS) and categorical predictors for logistic, linear, and Cox models.
#' It can display the distribution of the predictor variable as a histogram (for numeric) or bar plot (for categorical).
#' @param data A data frame.
#' @param x A character string of the predictor variable.
#' @param y A character string of the outcome variable.
#' @param time A character string of the time variable for Cox models. If `NULL`, logistic or linear regression is used.
#' @param time2 A character string of the ending time for interval-censored or counting process data.
#' @param covars A character vector of covariate names.
#' @param cluster A character string of the cluster variable for robust variance estimation.
#' @param method A character string specifying the method for handling the predictor `x`.
#'   Can be `"auto"`, `"rcs"`, `"linear"`, or `"categorical"`. If `"auto"`, the function decides based on the type of `x`.
#' @param knot The number of knots for RCS. If `NULL`, AIC is used to find the optimal number.
#' @param add_hist A logical value. If `TRUE`, add a distribution plot (histogram or bar plot).
#' @param ref The reference value for numeric predictors, or the reference level for categorical predictors.
#'   For numeric `x`, can be `"x_median"`, `"x_mean"`, `"ratio_min"`, or a numeric value.
#' @param ref_digits The number of digits for the reference value label.
#' @param show_total_n A logical value. If `TRUE`, show the total number of samples.
#' @param group_by_ref A logical value. If `TRUE` and `x` is numeric, split the histogram at the reference value.
#' @param group_title A character string for the group legend title.
#' @param group_labels A character vector for group labels.
#' @param group_colors A character vector of colors for the distribution plot. If `NULL`, the default colors are used.
#'   If `group_by_ref` is `FALSE`, the first color is used as fill color.
#' @param breaks The number of breaks for the histogram.
#' @param line_color The color for the effect line/points.
#' @param print_p_ph A logical value. If `TRUE` (and model is Cox), print the p-value for the proportional hazards test.
#' @param trans The transformation for the y-axis. Passed to `ggplot2::scale_y_continuous(transform = trans)`.
#' @param save_plot A logical value indicating whether to save the plot.
#' @param create_dir A logical value for creating the save directory.
#' @param filename A character string for the saved plot filename.
#' @param y_lim The y-axis limits.
#' @param hist_max The maximum value for the histogram y-axis.
#' @param xlim The x-axis limits for numeric predictors. If `NULL`, the limits are the `0.025` and `0.975` quantiles.
#'   The actual plot range might be slightly larger than this range to fit the histogram.
#' @param height The height of the saved plot.
#' @param width The width of the saved plot.
#' @param return_details A logical value indicating whether to return plot details.
#' @returns A `ggplot` object, or a list with the plot and details if `return_details` is `TRUE`.
#' @export
#' @examples
#' data(cancer, package = "survival")
#' data$dead <- data$status == 2
#' data <- data[!is.na(data$inst), ]
#' predictor_effect_plot(
#'   data = data,
#'   x = "age",
#'   y = "dead",
#'   method = "linear",
#'   covars = "ph.karno",
#'   add_hist = FALSE,
#'   trans = "log2",
#'   save_plot = FALSE,
#'   cluster = "inst"
#' )
predictor_effect_plot <- function(data, x, y, time = NULL, time2 = NULL, covars = NULL, cluster = NULL,
                                  method = "auto", knot = 4, add_hist = TRUE,
                                  ref = "x_median", ref_digits = 3, show_total_n = TRUE,
                                  group_by_ref = TRUE, group_title = NULL, group_labels = NULL, group_colors = NULL,
                                  breaks = 20, line_color = "#e23e57", print_p_ph = TRUE, trans = "identity", save_plot = FALSE,
                                  create_dir = FALSE, filename = NULL, y_lim = NULL, hist_max = NULL, xlim = NULL, height = 6,
                                  width = 6, return_details = FALSE) {
  # --- Initial Checks & Setup ---
  if (!is.null(xlim) && length(xlim) != 2) stop("`xlim` must be a vector of length 2")
  if (is.null(group_colors)) group_colors <- emp_colors
  if (add_hist && trans != "identity") stop("`trans` must be `identity` when `add_hist` is `TRUE`")
  if (anyDuplicated(c(x, y, time, time2, cluster))) {
    stop("`x`, `y`, `time`, `time2`, and `cluster` must be different variables.")
  }

  # --- Determine Analysis and Predictor Type ---
  if (method == "auto") {
    if (is.numeric(data[[x]]) && length(unique(data[[x]])) > 5) {
      method <- "rcs" # Default to RCS for numeric with many unique values
    } else if (is.numeric(data[[x]])) {
      method <- "linear"
    } else {
      method <- "categorical"
    }
  }

  if (!is.null(time)) {
    analysis_type <- "cox"
    ylab <- "HR (95% CI)"
    pred_fun <- exp
  } else if (length(levels(as.factor(data[[y]]))) == 2) {
    analysis_type <- "logistic"
    ylab <- "OR (95% CI)"
    pred_fun <- exp
  } else {
    analysis_type <- "linear"
    ylab <- "predicted value (95% CI)"
    pred_fun <- NULL
  }

  # --- Data Preparation ---
  covars <- remove_conflict(covars, c(y, x, time, time2, cluster))
  all_vars <- c(y, x, time, time2, covars, cluster)
  indf <- dplyr::select(data, all_of(all_vars))

  nmissing <- sum(!complete.cases(indf))
  if (nmissing > 0) warning(paste0(nmissing, " incomplete cases excluded."))
  indf <- indf[complete.cases(indf), ]

  if (method == "categorical" && !is.factor(indf[[x]])) {
    indf[[x]] <- as.factor(indf[[x]])
  }

  dd <- rms::datadist(indf, q.display = c(0.025, 0.975))
  old_datadist <- getOption("datadist")
  on.exit(
    {
      options(datadist = old_datadist)
    },
    add = TRUE
  )
  options(datadist = dd)

  # --- Model Fitting ---
  aics <- NULL
  if (method == "rcs" && is.null(knot)) {
    knot_range <- 3:7
    for (i in knot_range) {
      formula <- create_formula(y, x, time = time, time2 = time2, covars = covars, rcs_knots = i)
      model <- fit_model(formula, data = indf, analysis_type = analysis_type)
      aics <- c(aics, AIC(model))
    }
    knot <- knot_range[which.min(aics)]
  }

  rcs_knots <- if (method == "rcs") knot else NULL
  formula <- create_formula(y, x, time = time, time2 = time2, covars = covars, rcs_knots = rcs_knots)
  model <- fit_model(formula, data = indf, analysis_type = analysis_type, rms = TRUE, cluster = cluster)

  # --- Post-Hoc Tests (Cox PH) ---
  phassump <- NULL
  phresidual <- NULL
  pvalue_ph <- NA
  if (analysis_type == "cox") {
    tryCatch(
      {
        phassump <- survival::cox.zph(model)
        pvalue_ph <- phassump$table[1, 3]
      },
      error = function(e) warning("Cox proportional hazards test failed with error: ", e)
    )
    tryCatch(
      {
        phresidual <- survminer::ggcoxzph(phassump)
      },
      error = function(e) {
        warning("Graphical Test of Proportional Hazards failed with error: ", e)
      }
    )
  }

  # --- Get Predictions ---
  anova_fit <- anova(model)
  pvalue_all <- anova_fit[1, "P"]
  pvalue_nonlin <- if (method == "rcs") anova_fit[2, "P"] else NA

  df_pred <- rms::Predict(model,
    name = x, fun = pred_fun, type = "predictions", conf.int = 0.95,
    ref.zero = analysis_type %in% c("cox", "logistic")
  )
  df_pred <- as.data.frame(df_pred)

  # --- Handle Reference Value ---
  ref_val <- NULL
  if (method %in% c("rcs", "linear")) {
    if (ref == "ratio_min") {
      ref_val <- df_pred[[x]][which.min(df_pred$y)]
    } else if (ref == "x_median") {
      ref_val <- median(indf[[x]])
    } else if (ref == "x_mean") {
      ref_val <- mean(indf[[x]])
    } else {
      ref_val <- ref
    }
    # determine xlim from datadist defaults if not provided
    if (is.null(xlim)) {
      xlim <- dd[["limits"]][c("Low:prediction", "High:prediction"), x]
    }
    # if histogram requested and breaks is a single number, generate breaks including ref
    if (add_hist && length(breaks) == 1) {
      breaks <- break_at(xlim, breaks, ref_val)
      xlim <- breaks[c(1, length(breaks))]
    }
    # Ensure prediction limits reflect final xlim used for plotting
    dd[["limits"]][c("Low:prediction", "High:prediction"), x] <- xlim
  } else { # categorical
    # For categorical, if ref is one of the special numeric ones, default to first level
    if (is.null(ref) || ref %in% c("x_median", "x_mean")) {
      ref_val <- levels(indf[[x]])[1]
    } else if (is.null(ref) || ref == "ratio_min") {
      ref_val <- levels(indf[[x]])[which.min(df_pred$y)]
    } else {
      ref_val <- ref
    }
  }
  dd[["limits"]]["Adjust to", x] <- ref_val
  options(datadist = dd)

  # --- Re-fit with new reference and get final predictions ---
  # Re-fit model with new datadist (especially for new reference)
  model <- fit_model(formula, data = indf, analysis_type = analysis_type, rms = TRUE, cluster = cluster)
  df_pred <- rms::Predict(model,
    name = x, fun = pred_fun, type = "predictions", conf.int = 0.95,
    ref.zero = analysis_type %in% c("cox", "logistic")
  )
  df_plot <- as.data.frame(dplyr::select(df_pred, all_of(c(x, "yhat", "lower", "upper"))))
  colnames(df_plot) <- c("x", "y", "lower", "upper")

  # --- Plotting Setup ---
  xtitle <- x
  ylab <- paste0(ifelse(is.null(covars), "Unadjusted", "Adjusted"), " ", ylab)
  ytitle2 <- "Percentage of population (%)"
  pvalue_text <- format_pval(pvalue_all, text_ahead = "P-overall")
  if (method == "rcs" && !is.na(pvalue_nonlin)) {
    pvalue_text <- paste0(pvalue_text, "\n", format_pval(pvalue_nonlin, text_ahead = "P-non-linear"))
  }
  if (analysis_type == "cox" && print_p_ph && !is.na(pvalue_ph)) {
    pvalue_text <- paste0(pvalue_text, "\n", format_pval(pvalue_ph, text_ahead = "P-proportional"))
  }

  p <- ggplot2::ggplot()

  # --- Y-axis limits ---
  if (is.null(y_lim)) {
    pred_ymin <- min(df_plot[, "y"], na.rm = TRUE)
    pred_ymax <- max(df_plot[, "y"], na.rm = TRUE)
    ymin <- pred_ymin * 2 - pred_ymax
    ymax1 <- pred_ymax * 2 - pred_ymin
    if (analysis_type %in% c("cox", "logistic")) {
      if (trans == "identity") {
        ymin <- max(ymin, 0)
      } else {
        ymin <- min(df_plot[, "lower"], na.rm = TRUE)
        ymax1 <- max(df_plot[, "upper"], na.rm = TRUE)
      }
    }
  } else {
    ymin <- y_lim[1]
    ymax1 <- y_lim[2]
  }
  df_plot$upper[df_plot$upper > ymax1] <- ymax1
  df_plot$lower[df_plot$lower < ymin] <- ymin


  # --- Add Distribution Plot (Histogram/Bar) ---
  df_hist_plot <- NULL
  if (add_hist) {
    if (method %in% c("rcs", "linear")) {
      df_hist_data <- indf[indf[[x]] >= xlim[1] & indf[[x]] <= xlim[2], ]
      h <- graphics::hist(df_hist_data[[x]], breaks = breaks, right = FALSE, plot = FALSE)
      df_hist_plot <- data.frame(x = h$mids, freq = h$counts, den = h$counts / nrow(indf) * 100)
      ori_widths <- diff(breaks)
      relative_width <- 0.9
      df_hist_plot$xmin <- df_hist_plot$x - ori_widths * relative_width / 2
      df_hist_plot$xmax <- df_hist_plot$x + ori_widths * relative_width / 2

      if (is.null(hist_max)) ymax2 <- ceiling(max(df_hist_plot$den * 1.5) / 5) * 5 else ymax2 <- hist_max
      scale_factor <- ymax2 / (ymax1 - ymin)

      if (group_by_ref && !is.null(ref_val)) {
        df_hist_plot$Group <- cut_by(df_hist_plot$x, ref_val, labels = group_labels, label_type = "LMH")
        tmp_group <- cut_by(indf[[x]], ref_val, labels = group_labels, label_type = "LMH")
        levels(df_hist_plot$Group) <- paste0(levels(df_hist_plot$Group), " (n=", table(tmp_group), ")")
        p <- p + geom_rect(data = df_hist_plot, aes(
          xmin = xmin, xmax = xmax, ymin = ymin,
          ymax = den / scale_factor + ymin, fill = Group
        )) +
          scale_fill_manual(values = group_colors, name = group_title)
      } else {
        p <- p + geom_rect(
          data = df_hist_plot,
          aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = den / scale_factor + ymin),
          fill = group_colors[1], show.legend = FALSE
        )
      }
    } else { # Bar plot for categorical
      df_bar_plot <- as.data.frame(prop.table(table(indf[[x]])))
      colnames(df_bar_plot) <- c("x", "den")
      df_bar_plot$den <- df_bar_plot$den * 100
      df_bar_plot$label <- paste0(round(df_bar_plot$den), "%")
      df_bar_plot$xmin <- as.numeric(df_bar_plot$x)-0.45
      df_bar_plot$xmax <- as.numeric(df_bar_plot$x)+0.45

      if (is.null(hist_max)) ymax2 <- ceiling(max(df_bar_plot$den * 1.5) / 5) * 5 else ymax2 <- hist_max
      scale_factor <- ymax2 / (ymax1 - ymin)

      p <- p + geom_rect(data = df_bar_plot, aes(
          xmin = xmin, xmax = xmax, ymin = ymin,
          ymax = den / scale_factor + ymin, fill = x
        ), show.legend = FALSE) +
        geom_text(data = df_bar_plot, aes(x = x, y = den / scale_factor + ymin, label = label), vjust = -0.5) +
        scale_fill_manual(values = group_colors)
    }
  }

  # --- Add Effect Plot ---
  if (analysis_type %in% c("cox", "logistic")) {
    p <- p +
      geom_hline(yintercept = 1, linewidth = 1, linetype = 2, color = "grey") +
      geom_point(aes(x = ref_val, y = 1), color = line_color, size = 2)
  }

  if (method %in% c("rcs", "linear")) {
    p <- p +
      geom_ribbon(data = df_plot, aes(x = x, ymin = lower, ymax = upper), fill = line_color, alpha = 0.2) +
      geom_line(data = df_plot, aes(x = x, y = y), color = line_color, linewidth = 1)
    if (analysis_type %in% c("cox", "logistic") && !is.null(ref_val)) {
      p <- p + geom_text(aes(x = ref_val, y = 0.9 * (1 - ymin) + ymin, label = paste0("Ref=", format(ref_val, digits = ref_digits))), size = 5)
    }
  } else { # categorical
    p <- p + geom_crossbar(data = df_plot, aes(x = x, y = y, ymin = lower, ymax = upper), fill = line_color, alpha = 0.2, width = 0.5,
      color = line_color, linewidth = 1)
  }

  p <- p + annotation_custom(
    grob = grid::textGrob(pvalue_text, x = unit(0.95, "npc"), y = unit(0.9, "npc"), just = "right", gp = grid::gpar(fontsize = 12))
  )

  # --- Final Touches & Theming ---
  p <- p +
    labs(x = xtitle, y = ylab) +
    theme_bw() +
    theme(
      text = element_text(size = 15), axis.text = element_text(size = 15),
      axis.title = element_text(size = 18), legend.text = element_text(size = 15),
      axis.line = element_line(), panel.grid = element_blank(),
      panel.border = element_blank(), legend.position = "top"
    )

  if (method %in% c("rcs", "linear")) {
    p <- p + scale_x_continuous(limits = xlim, expand = c(0.01, 0))
  }

  if (add_hist) {
    p <- p + scale_y_continuous(ylab,
      expand = c(0, 0), limits = c(ymin, ymax1), transform = trans,
      sec.axis = sec_axis(name = ytitle2, transform = ~ (. - ymin) * scale_factor)
    )
  } else {
    p <- p + scale_y_continuous(ylab, expand = c(0, 0), limits = c(ymin, ymax1), transform = trans)
  }

  # Manual legend and annotations
  segment1 <- grid::segmentsGrob(
    x0 = unit(0.05, "npc"), y0 = unit(0.92, "npc"), x1 = unit(0.12, "npc"), y1 = unit(0.92, "npc"),
    gp = grid::gpar(col = line_color, lwd = 3, lineend = "butt")
  )
  segment2 <- grid::segmentsGrob(
    x0 = unit(0.05, "npc"), y0 = unit(0.88, "npc"), x1 = unit(0.12, "npc"), y1 = unit(0.88, "npc"),
    gp = grid::gpar(col = line_color, lwd = 12, alpha = 0.2, lineend = "butt")
  )

  p <- p +
    annotation_custom(segment1) +
    annotation_custom(segment2) +
    annotation_custom(
      grob = grid::textGrob("Estimation", x = unit(0.13, "npc"), y = unit(0.92, "npc"), just = "left", gp = grid::gpar(fontsize = 12))
    ) +
    annotation_custom(
      grob = grid::textGrob("95% CI", x = unit(0.13, "npc"), y = unit(0.88, "npc"), just = "left", gp = grid::gpar(fontsize = 12))
    )

  if (show_total_n) {
    p <- p +
      annotation_custom(
        grob = grid::textGrob(paste0("N = ", nrow(indf)),
          x = unit(0.5, "npc"),
          y = unit(0.9, "npc"),
          gp = grid::gpar(fontsize = 15, fontface = "bold")
        )
      )
  }


  if (save_plot) {
    if (is.null(filename)) {
      filename <- paste0(c(analysis_type, method, y, "with", x, if (!is.null(rcs_knots)) paste0(knot, "knots"), "with", length(covars), "covars"), collapse = "_")
      filename <- gsub("__", "_", filename)
      filename <- paste0(filename, ".png")
    }
    ggsave(filename, p, width = width, height = height, create.dir = create_dir)
  }

  if (return_details) {
    details <- list(
      aics = aics, knot = knot, n.valid = nrow(indf), n.plot = if (!is.null(df_hist_data)) nrow(df_hist_data) else NULL,
      phassump = phassump, phresidual = phresidual,
      pvalue_all = pvalue_all,
      pvalue_nonlin = pvalue_nonlin,
      ref = ref_val, plot = p
    )
    return(details)
  } else {
    p
  }
}
