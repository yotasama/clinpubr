#' Plot restricted cubic spline
#' @description Plot restricted cubic spline based on package `rms`. Support both logistic and cox model.
#' @param data A data frame.
#' @param x A character string of the predictor variable.
#' @param y A character string of the outcome variable.
#' @param time A character string of the time variable. If `NULL`, logistic regression is used.
#'   Otherwise, Cox proportional hazards regression is used.
#' @param covars A character vector of covariate names.
#' @param knot The number of knots. If `NULL`, the number of knots is determined by AIC minimum.
#' @param add_hist A logical value. If `TRUE`, add histogram to the plot.
#' @param ref The reference value for the plot. Could be `"x_median"`, `"x_mean"`, `"ratio_min"`, or a numeric value.
#'   If `"x_median"`, the median of the predictor variable is used. If `"ratio_min"`, the value of the
#'   predictor variable that has the minium predicted risk is used. If a numeric value, that value is used.
#' @param ref_digits The number of digits for the reference value.
#' @param group_by_ref A logical value. If `TRUE`, split the histogram at the reference value from `ref` into
#'   two groups.
#' @param group_title A character string of the title for the group. Ignored if `group_by_ref` is `FALSE`.
#' @param group_labels A character vector of the labels for the group. If `NULL`, the labels are generated
#'   automatically. Ignored if `group_by_ref` is `FALSE`.
#' @param group_colors A character vector of colors for the plot. If `NULL`, the default colors are used.
#'   If `group_by_ref` is `FALSE`, the first color is used as fill color.
#' @param breaks The number of breaks for the histogram.
#' @param rcs_color The color for the restricted cubic spline.
#' @param print_p_ph A logical value. If `TRUE`, print the p-value of the proportional hazards test
#'   (`survival::cox.zph()`) in the plot.
#' @param trans The transformation for the y axis in the plot.
#'   Passed to `ggplot2::scale_y_continuous(transform = trans)`.
#' @param save_plot A logical value indicating whether to save the plot.
#' @param filename A character string specifying the filename for the plot. If `NULL`, a default filename is used.
#' @param y_lim The range of effect value of the plot. If `NULL`, the numbers are determined automatically.
#' @param hist_max The maximum value for the histogram. If `NULL`, the maximum value is determined automatically.
#' @param xlim The x-axis limits for the plot. If `NULL`, the limits are the `0.025` and `0.975` quantiles.
#'   The actual plot range might be slightly larger than this range to fit the histogram.
#' @param return_details A logical value indicating whether to return the details of the plot.
#'
#' @returns A `ggplot` object, or a list containing the `ggplot` object and other details if `return_details` is `TRUE`.
#' @export
#' @examples
#' data(cancer, package = "survival")
#' # coxph model with time assigned
#' rcs_plot(cancer, x = "age", y = "status", time = "time", covars = "ph.karno", save_plot = FALSE)
#'
#' # logistic model with time not assigned
#' cancer$dead <- cancer$status == 2
#' rcs_plot(cancer, x = "age", y = "dead", covars = "ph.karno", save_plot = FALSE)
rcs_plot <- function(data, x, y, time = NULL, covars = NULL, knot = 4, add_hist = TRUE, ref = "x_median", ref_digits = 3,
                     group_by_ref = TRUE, group_title = NULL, group_labels = NULL, group_colors = NULL, breaks = 20,
                     rcs_color = "#e23e57", print_p_ph = TRUE, trans = "identity", save_plot = TRUE, filename = NULL,
                     y_lim = NULL, hist_max = NULL, xlim = NULL, return_details = FALSE) {
  if (!is.null(xlim) && length(xlim) != 2) stop("`xlim` must be a vector of length 2")
  if (is.null(group_colors)) {
    group_colors <- emp_colors
  }
  if (add_hist && trans != "identity") {
    stop("`trans` must be `identity` when `add_hist` is `TRUE`")
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

  covars <- remove_conflict(covars, c(y, x, time))
  indf <- dplyr::select(data, all_of(c(y, x, time, covars)))

  nmissing <- sum(!complete.cases(indf))
  if (nmissing > 0) {
    warning(paste0(nmissing, " incomplete cases excluded."))
  }
  indf <- indf[complete.cases(indf), ]

  dd <- rms::datadist(indf, q.display = c(0.025, 0.975))
  old_datadist <- getOption("datadist")
  on.exit(
    {
      options(datadist = old_datadist)
    },
    add = TRUE
  )
  options(datadist = dd)

  aics <- NULL
  if (is.null(knot)) {
    for (i in 3:7) {
      formula <- create_formula(y, x, time = time, covars = covars, rcs_knots = i)
      model <- fit_model(formula, data = indf, analysis_type = analysis_type)
      aics <- c(aics, AIC(model))
      kn <- seq(3, 7)[which.min(aics)]
    }
    knot <- kn
  }

  formula <- create_formula(y, x, time = time, covars = covars, rcs_knots = knot)
  model <- fit_model(formula, data = indf, analysis_type = analysis_type, rms = TRUE)

  phassump <- NULL
  phresidual <- NULL
  if (analysis_type == "cox") {
    phassump <- survival::cox.zph(model, transform = "km")
    phresidual <- survminer::ggcoxzph(phassump)
    pvalue_ph <- phassump$table[1, 3]
  }

  anova_fit <- anova(model)
  pvalue_all <- anova_fit[1, "P"]
  pvalue_nonlin <- anova_fit[2, "P"]
  df_pred <- rms::Predict(model,
    name = x, fun = pred_fun, type = "predictions", conf.int = 0.95, digits = 2,
    ref.zero = analysis_type %in% c("cox", "logistic")
  )

  df_pred <- data.frame(df_pred)
  if (ref == "ratio_min") {
    ref_val <- df_pred[[x]][which.min(df_pred$yhat)]
  } else if (ref == "x_median") {
    ref_val <- median(indf[[x]])
  } else if (ref == "x_mean") {
    ref_val <- mean(indf[[x]])
  } else {
    ref_val <- ref
  }

  if (is.null(xlim)) {
    xlim <- dd[["limits"]][c("Low:prediction", "High:prediction"), x]
  }
  if (add_hist && length(breaks) == 1) {
    breaks <- break_at(xlim, breaks, ref_val)
    xlim <- breaks[c(1, length(breaks))]
  }

  dd[["limits"]]["Adjust to", x] <- ref_val
  dd[["limits"]][c("Low:prediction", "High:prediction"), x] <- xlim
  options(datadist = dd)
  model <- update(model)
  df_pred <- rms::Predict(model,
    name = x, fun = pred_fun, type = "predictions", conf.int = 0.95, digits = 2,
    ref.zero = analysis_type %in% c("cox", "logistic")
  )
  df_rcs <- as.data.frame(dplyr::select(df_pred, all_of(c(x, "yhat", "lower", "upper"))))

  colnames(df_rcs) <- c("x", "y", "lower", "upper")

  if (is.null(y_lim)) {
    pred_ymin <- min(df_rcs[, "y"], na.rm = TRUE)
    pred_ymax <- max(df_rcs[, "y"], na.rm = TRUE)
    ymin <- pred_ymin * 2 - pred_ymax
    ymax1 <- pred_ymax * 2 - pred_ymin
    if (analysis_type %in% c("cox", "logistic")) {
      ymin <- max(ymin, 0)
    }
  } else {
    ymin <- y_lim[1]
    ymax1 <- y_lim[2]
  }
  df_rcs$upper[df_rcs$upper > ymax1] <- ymax1
  df_rcs$lower[df_rcs$lower < ymin] <- ymin

  xtitle <- x
  ylab <- paste0(ifelse(is.null(covars), "Unadjusted", "Adjusted"), " ", ylab)
  ytitle2 <- "Percentage of Population (%)"
  label2 <- paste0(
    format_pval(pvalue_all, text_ahead = "P-overall"), "\n",
    format_pval(pvalue_nonlin, text_ahead = "P-non-linear")
  )
  if (analysis_type == "cox" && print_p_ph) {
    label2 <- paste0(label2, "\n", format_pval(pvalue_ph, text_ahead = "P-proportional"))
  }

  p <- ggplot2::ggplot()

  if (add_hist) {
    df_hist <- indf[indf[[x]] >= xlim[1] & indf[[x]] <= xlim[2], ]
    h <- graphics::hist(df_hist[[x]], breaks = breaks, right = FALSE, plot = FALSE)

    df_hist_plot <- data.frame(x = h[["mids"]], freq = h[["counts"]], den = h[["counts"]] / nrow(indf) * 100)
    ori_widths <- breaks[-1] - breaks[-length(breaks)]
    relative_width <- 0.9
    df_hist_plot$xmin <- df_hist_plot$x - ori_widths * relative_width / 2
    df_hist_plot$xmax <- df_hist_plot$x + ori_widths * relative_width / 2
    if (is.null(hist_max)) {
      ymax2 <- ceiling(max(df_hist_plot$den * 1.5) / 5) * 5
    } else {
      ymax2 <- hist_max
    }
    scale_factor <- ymax2 / (ymax1 - ymin)

    if (group_by_ref) {
      df_hist_plot$Group <- cut_by(df_hist_plot$x, ref_val, labels = group_labels, label_type = "LMH")
      tmp_group <- cut_by(indf[[x]], ref_val, labels = group_labels, label_type = "LMH")
      levels(df_hist_plot$Group) <- paste0(levels(df_hist_plot$Group), " (n=", table(tmp_group), ")")
      p <- p +
        geom_rect(
          data = df_hist_plot,
          aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = den / scale_factor + ymin, fill = Group),
        ) +
        scale_fill_manual(values = group_colors, name = group_title)
    } else {
      p <- p +
        geom_rect(
          data = df_hist_plot,
          aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = den / scale_factor + ymin, fill = "1"),
          show.legend = FALSE
        ) +
        scale_fill_manual(values = group_colors)
    }
  }

  if (analysis_type %in% c("cox", "logistic")) {
    p <- p +
      geom_hline(yintercept = 1, linewidth = 1, linetype = 2, color = "grey") +
      geom_point(aes(x = ref_val, y = 1), color = rcs_color, size = 2)
  }
  p <- p +
    geom_ribbon(
      data = df_rcs, aes(x = x, ymin = lower, ymax = upper),
      fill = rcs_color, alpha = 0.1
    )
  if (analysis_type %in% c("cox", "logistic")) {
    p <- p +
      geom_text(aes(
        x = ref_val, y = 0.9,
        label = paste0("Ref=", format(ref_val, digits = ref_digits))
      ))
  }
  p <- p +
    geom_line(data = df_rcs, aes(x = x, y = y), color = rcs_color, linewidth = 1) +
    scale_x_continuous(xtitle, limits = xlim, expand = c(0.01, 0))
  if (add_hist) {
    p <- p +
      scale_y_continuous(
        ylab,
        expand = c(0, 0),
        limits = c(ymin, ymax1),
        transform = trans,
        sec.axis = sec_axis(
          name = ytitle2, transform = ~ (. - ymin) * scale_factor,
        )
      )
  } else {
    p <- p +
      scale_y_continuous(
        ylab,
        expand = c(0, 0),
        limits = c(ymin, ymax1),
        transform = trans
      )
  }
  p_panel_params <- ggplot_build(p)$layout$panel_params[[1]]
  x1 = p_panel_params$x.range[1]
  x2 = p_panel_params$x.range[2]
  y1 = p_panel_params$y.range[1]
  y2 = p_panel_params$y.range[2]
  offsetx1 <- (x2 - x1) * 0.02
  offsety1 <- (y2 - y1) * 0.02
  labelx1 <- x1 + (x2 - x1) * 0.15
  labelx2 <- x1 + (x2 - x1) * 0.95
  labely <- (y2 - y1) * 0.9 + y1
  label1_1 <- "Estimation"
  label1_2 <- "95% CI"
  p <- p +
    annotate("text",
      label = paste0("N = ", nrow(indf)), size = 5,
      x = mean(p_panel_params$x.range),
      y = labely
    ) +
    geom_segment(
      aes(
        x = c(labelx1 - offsetx1 * 5, labelx1 - offsetx1 * 5),
        xend = c(labelx1 - offsetx1, labelx1 - offsetx1),
        y = c(labely + offsety1, labely - offsety1),
        yend = c(labely + offsety1, labely - offsety1)
      ),
      linetype = 1,
      color = rcs_color,
      linewidth = 1,
      alpha = c(1, 0.1)
    ) +
    geom_text(aes(x = labelx1, y = labely + offsety1, label = label1_1), hjust = 0) +
    geom_text(aes(x = labelx1, y = labely - offsety1, label = label1_2), hjust = 0) +
    geom_text(aes(x = labelx2, y = labely, label = label2), hjust = 1) +
    theme_bw() +
    theme(
      axis.line = element_line(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      legend.position = "top"
    )

  if (save_plot) {
    if (is.null(filename)) {
      filename <- paste0(
        paste0(c(analysis_type, "rcs", y, "with", x, knot, "knots", "with", length(covars), "covars"),
          collapse = "_"
        ), ".png"
      )
    }
    ggsave(filename, p, width = 6, height = 6)
  }

  if (return_details) {
    details <- list(
      aics = aics, knot = knot, n.valid = nrow(indf), n.plot = nrow(df_hist),
      phassump = phassump, phresidual = phresidual,
      pvalue_all = pvalue_all,
      pvalue_nonlin = pvalue_nonlin,
      ref = ref_val, plot = p
    )
    return(details)
  } else {
    return(p)
  }
}

#' Generate breaks for histogram
#' @description Generate breaks for histogram that covers xlim and includes a ref_val.
#' @param xlim A vector of length 2.
#' @param breaks The number of breaks.
#' @param ref_val The reference value to include in breaks.
#'
#' @returns A vector of breaks of length `breaks + 1`.
#' @export
#' @examples
#' break_at(xlim = c(0, 10), breaks = 12, ref_val = 3.12)
break_at <- function(xlim, breaks, ref_val) {
  if (length(xlim) != 2) stop("`xlim` must be a vector of length 2")
  bks <- seq(xlim[1], xlim[2], length.out = breaks + 1)
  if (!ref_val %in% bks) {
    bks <- seq(xlim[1], xlim[2], length.out = breaks)
    h <- (xlim[2] - xlim[1]) / (breaks - 1)
    bks <- c(bks[1] - h, bks)
    tmp <- ref_val - bks
    tmp <- tmp[tmp > 0]
    bks <- bks + tmp[length(tmp)]
  }
  bks
}

#' Filter predictors for RCS
#' @description Filter predictors that can be used to fit for RCS models.
#' @param data A data frame.
#' @param predictors A vector of predictor names to be filtered.
#'
#' @returns A vector of predictor names. These variables are numeric and have more than 5 unique values.
#' @export
#' @examples
#' filter_rcs_predictors(mtcars)
filter_rcs_predictors <- function(data, predictors = NULL) {
  if (is.null(predictors)) {
    predictors <- colnames(data)
  }
  res <- c()
  for (x in predictors) {
    if (is.numeric(data[[x]]) && length(na.omit(unique(data[[x]]))) > 5) {
      res <- union(res, x)
    }
  }
  return(res)
}
