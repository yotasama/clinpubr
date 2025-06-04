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

rcs_plot(cancer, x = "age", y = "status", time = "time", covars = "ph.karno", save_plot = FALSE)

time = NULL; covars = NULL; knot = 4; add_hist = TRUE; ref = "x_median"; ref_digits = 3;
group_by_ref = TRUE; group_title = NULL; group_labels = NULL; group_colors = NULL; breaks = 20;
rcs_color = "#e23e57"; print_p_ph = TRUE; trans = "identity"; save_plot = TRUE; filename = NULL;
y_max = NULL; y_min = NULL; hist_max = NULL; xlim = NULL; return_details = FALSE
data=cancer; x = "age"; y = "status"; time = "time"; covars = "ph.karno"; save_plot = FALSE
rcs_plot <- function(data, x, y, time = NULL, covars = NULL, knot = 4, add_hist = TRUE, ref = "x_median", ref_digits = 3,
                     group_by_ref = TRUE, group_title = NULL, group_labels = NULL, group_colors = NULL, breaks = 20,
                     rcs_color = "#e23e57", print_p_ph = TRUE, trans = "identity", save_plot = TRUE, filename = NULL,
                     y_max = NULL, y_min = NULL, hist_max = NULL, xlim = NULL, return_details = FALSE) {
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
  
  dd[["limits"]]["Adjust to", x] <- ref_val
  
  if (!is.null(xlim)) {
    dd[["limits"]][c("Low:prediction", "High:prediction"), x] <- xlim
  } else {
    xlim <- dd[["limits"]][c("Low:prediction", "High:prediction"), x]
  }
  options(datadist = dd)
  model <- update(model)
  df_pred <- rms::Predict(model,
                          name = x, fun = pred_fun, type = "predictions", conf.int = 0.95, digits = 2,
                          ref.zero = analysis_type %in% c("cox", "logistic")
  )
  df_rcs <- as.data.frame(dplyr::select(df_pred, all_of(c(x, "yhat", "lower", "upper"))))
  
  colnames(df_rcs) <- c("x", "y", "lower", "upper")
  if (is.null(y_max)) {
    pred_ymax <- max(df_rcs[, "y"], na.rm = TRUE)
    plot_ymax <- if (pred_ymax > 0) {
      1.5 * pred_ymax
    } else {
      0.5 * pred_ymax
    }
    ymax1 <- plot_ymax # min(max(df_rcs[, "upper"], na.rm = TRUE), plot_ymax)
  } else {
    ymax1 <- y_max
  }
  df_rcs$upper[df_rcs$upper > ymax1] <- ymax1
  
  if (is.null(y_min)) {
    if (analysis_type == "linear") {
      pred_ymin <- min(df_rcs[, "y"], na.rm = TRUE)
      plot_ymin <- if (pred_ymin > 0) {
        0.5 * pred_ymax
      } else {
        1.5 * pred_ymax
      }
      ymin <- plot_ymin # max(min(df_rcs[, "lower"], na.rm = TRUE), plot_ymin)
    } else {
      ymin <- 0
    }
  } else {
    ymin <- y_min
  }
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
    if (length(breaks) == 1) {
      breaks <- break_at(xlim, breaks, ref_val)
      xlim <- breaks[c(1, length(breaks))]
    }
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
  
  offsetx1 <- (xlim[2] - xlim[1]) * 0.02
  offsety1 <- (ymax1 - ymin) * 0.02
  labelx1 <- xlim[1] + (xlim[2] - xlim[1]) * 0.15
  labely1 <- (ymax1 - ymin) * 0.9 + ymin
  label1_1 <- "Estimation"
  label1_2 <- "95% CI"
  labelx2 <- xlim[1] + (xlim[2] - xlim[1]) * 0.95
  labely2 <- (ymax1 - ymin) * 0.9 + ymin
  df_rcs <- filter(df_rcs, x >= xlim[1], x <= xlim[2]) %>% as.data.frame()
  
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
    geom_segment(
      aes(
        x = c(labelx1 - offsetx1 * 5, labelx1 - offsetx1 * 5),
        xend = c(labelx1 - offsetx1, labelx1 - offsetx1),
        y = c(labely1 + offsety1, labely1 - offsety1),
        yend = c(labely1 + offsety1, labely1 - offsety1)
      ),
      linetype = 1,
      color = rcs_color,
      linewidth = 1,
      alpha = c(1, 0.1)
    ) +
    geom_text(aes(x = labelx1, y = labely1 + offsety1, label = label1_1), hjust = 0) +
    geom_text(aes(x = labelx1, y = labely1 - offsety1, label = label1_2), hjust = 0) +
    geom_text(aes(x = labelx2, y = labely2, label = label2), hjust = 1) +
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
  p <- p +
    annotate("text",
             label = paste0("N = ", nrow(indf)), size = 5,
             x = mean(p_panel_params$x.range),
             y = max(p_panel_params$y.range) * 0.9,
             hjust = 0.5, vjust = 0.5
    ) +
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
