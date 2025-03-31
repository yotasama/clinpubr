#' Plot ristricted cubic spline
#' @description Plot ristricted cubic spline based on rms package. Support both logistic and cox model.
#' @param data A data frame.
#' @param x A character string of the predictor variable.
#' @param y A character string of the outcome variable.
#' @param time A character string of the time variable. If `NULL`, logistic regression is used.
#'   Otherwise, Cox proportional hazards regression is used.
#' @param covs A character vector of covariate names.
#' @param knot The number of knots. If `NULL`, the number of knots is determined by AIC minimum.
#' @param add_hist A logical value. If `TRUE`, add histogram to the plot.
#' @param ref The reference value for the plot. Could be `"median"`, `"min"`, or a numeric value.
#'   If `"median"`, the median of the predictor variable is used. If `"min"`, the value of the
#'   predictor variable that has the minium predicted risk is used. If a numeric value, that value is used.
#' @param ref_digits The number of digits for the reference value.
#' @param group_by_ref A logical value. If `TRUE`, split the histogram at the reference value from `ref` into
#'   two groups.
#' @param group_title A character string of the title for the group.
#' @param group_labels A character vector of the labels for the group. If `NULL`, the labels are generated
#'   automatically.
#' @param group_colors A character vector of colors for the plot. If `NULL`, the default colors are used.
#' @param breaks The number of breaks for the histogram.
#' @param print_p_ph A logical value. If `TRUE`, print the p-value of the proportional hazards test
#'   (`survival::cox.zph()`) in the plot.
#' @param trans The transformation for the y axis in the plot.
#'   Passed to `ggplot2::scale_y_continuous(transform = trans)`.
#' @param save_plot A logical value indicating whether to save the plot.
#' @param filename A character string specifying the filename for the plot. If `NULL`, a default filename is used.
#' @param ratio_max The maximum ratio of the plot. If `NULL`, the maximum ratio is determined automatically.
#' @param hist.max The maximum value for the histogram. If `NULL`, the maximum value is determined automatically.
#' @param xlim The x-axis limits for the plot. If `NULL`, the limits are determined automatically.
#' @param return_details A logical value indicating whether to return the details of the plot.
#'
#' @returns A ggplot2 object, or a list containing the ggplot2 object and other details if `return_details` is `TRUE`.
#' @export
#' @examples
#' data(cancer, package = "survival")
#' rcs_plot(cancer, x = "ph.ecog", y = "status", time = "time", covs = "ph.karno")
rcs_plot <- function(data, x, y, time = NULL, covs = NULL, knot = 4, add_hist = TRUE, ref = "median", ref_digits = 3,
                     group_by_ref = TRUE, group_title = NULL, group_labels = NULL, group_colors = NULL, breaks = 20,
                     print_p_ph = T, trans = "identity", save_plot = TRUE, filename = NULL,
                     ratio_max = NULL, hist.max = NULL, xlim = NULL, return_details = FALSE) {
  load_packages(c(
    "rms", "ggplot2", "survminer", "survival", "dplyr",
    "patchwork", "Cairo", "RColorBrewer", "grid"
  ))
  # if (!is.null(knot)) {warning("please be sure of knot by AIC min(default) or preliminary investigation suggested")}
  if (is.null(group_colors)) {
    group_colors <- c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3")
  }

  analysis_type <- ifelse(is.null(time), "logistic", "cox")
  covs <- setdiff(covs, c(y, x, time))
  if (analysis_type == "cox") {
    indf <- dplyr::select(data, all_of(c(y, x, time, covs)))
    colnames(indf)[1:3] <- c("y", "x", "time")
  } else {
    indf <- dplyr::select(data, all_of(c(y, x, covs)))
    colnames(indf)[1:2] <- c("y", "x")
  }

  nmissing <- sum(!complete.cases(indf))
  if (nmissing > 0) {
    warning(paste0(nmissing, " incomplete cases excluded."))
  }
  indf <- indf[complete.cases(indf), ]
  dd <- NULL
  dd <- rms::datadist(indf)
  old <- options()
  on.exit(options(old))
  options(datadist = "dd")

  aics <- NULL
  formula_base <- if (analysis_type == "cox") {
    "Surv(time, status) ~ "
  } else {
    "status ~ "
  }
  if (is.null(knot)) {
    for (i in 3:7) {
      formula <- formula_add_covs(paste0(formula_base, "rcs(x, ", i, ")"), covs)
      if (analysis_type == "cox") {
        fit <- rms::cph(formula, data = indf, x = TRUE, y = TRUE, se.fit = TRUE,
                        tol = 1e-25, surv = TRUE)
      } else {
        fit <- rms::Glm(formula, data = indf, x = TRUE, y = TRUE, family = binomial(link = "logit"))
      }
      aics <- c(aics, AIC(fit))
      kn <- seq(3, 7)[which.min(aics)]
    }
    knot <- kn
  }

  formula <- formula_add_covs(paste0(formula_base, "rcs(x, ", knot, ")"), covs)
  if (analysis_type == "cox") {
    fit <- rms::cph(formula, data = indf, x = TRUE, y = TRUE, se.fit = TRUE,
                    tol = 1e-25, surv = TRUE)
    phassump <- survival::cox.zph(fit, transform = "km")
    phresidual <- survminer::ggcoxzph(phassump)
  } else {
    fit <- rms::Glm(formula, data = indf, x = TRUE, y = TRUE, family = binomial(link = "logit"))
  }

  anova_fit <- anova(fit)
  pvalue_all <- anova_fit[1, 3]
  pvalue_nonlin <- round(anova_fit[2, 3], 3)
  pvalue_ph <- phassump$table[1, 3]
  model_pred <- rms::Predict(fit, "x", fun = exp, type = "predictions", ref.zero = T, conf.int = 0.95, digits = 2)

  ushap <- data.frame(model_pred)
  if (ref == "min") {
    ushap.cutoff <- ushap$x[which.min(ushap$yhat)]
  } else if (ref == "median") {
    ushap.cutoff <- median(indf$x)
  } else {
    ushap.cutoff <- ref
  }

  dd <- rms::datadist(indf)
  dd[["limits"]]["Adjust to", "x"] <= ushap.cutoff
  old <- options()
  on.exit(options(old))
  options(datadist = "dd")

  fit <- update(fit)
  model_pred <- rms::Predict(fit, x,
    fun = exp, type = "predictions", ref.zero = T, conf.int = 0.95, digits = 2
  )

  newdf1 <- as.data.frame(dplyr::select(model_pred, x, yhat, lower, upper))
  if (!is.null(xlim)) {
    newdf1 <- filter(newdf1, (x >= xlim[1]) & (x <= xlim[2]))
  }
  colnames(newdf1) <- c("x", "y", "lower", "upper")
  xmin <- min(newdf1[, "x"])
  xmax <- max(newdf1[, "x"])
  if (is.null(ratio_max)) {
    ymax1 <- ceiling(max(newdf1[, "upper"]))
  } else {
    ymax1 <- ratio_max
  }
  newdf2 <- indf[indf[, "x"] >= xmin & indf[, "x"] <= xmax, ]
  if (length(breaks) == 1) {
    bks <- seq(min(newdf2$x), max(newdf2$x), length.out = breaks + 1)
    if (!ushap.cutoff %in% bks) {
      bks <- seq(min(newdf2$x), max(newdf2$x), length.out = breaks)
      h <- (max(newdf2$x) - min(newdf2$x)) / (breaks - 1)
      bks <- c(bks[1] - h, bks)
      tmp <- ushap.cutoff - bks
      tmp <- tmp[tmp > 0]
      bks <- bks + tmp[length(tmp)]
    }
    breaks <- bks
  }
  h <- hist(newdf2$x, breaks = breaks, right = FALSE, plot = F)

  newdf3 <- data.frame(x = h[["mids"]], freq = h[["counts"]], pct = h[["counts"]] / sum(h[["counts"]]))

  if (is.null(hist.max)) {
    ymax2 <- ceil(max(newdf3$pct * 1.5) * 20) * 5
  } else {
    ymax2 <- hist.max
  }
  scale_factor <- ymax2 / ymax1
  xtitle <- x
  ytitle1 <- ifelse(is.null(covs), "Unadjusted HR (95% CI)", "Adjusted HR (95% CI)")
  ytitle2 <- "Percentage of Population (%)"
  offsetx1 <- (xmax - xmin) * 0.02
  offsety1 <- ymax1 * 0.02
  labelx1 <- xmin + (xmax - xmin) * 0.05
  labely1 <- ymax1 * 0.9
  label1 <- paste0("Estimation", "\n", "95% CI")
  labelx2 <- xmin + (xmax - xmin) * 0.6
  labely2 <- ymax1 * 0.9
  label2 <- paste0(
    "P-overall ",
    ifelse(pvalue_all < 0.001, "< 0.001", paste0("= ", sprintf("%.3f", pvalue_all))),
    "\nP-non-linear ",
    ifelse(pvalue_nonlin < 0.001, "< 0.001", paste0("= ", sprintf("%.3f", pvalue_nonlin)))
  )
  if (print_p_ph) {
    label2 <- paste0(
      label2, "\nP-proportional ",
      ifelse(pvalue_ph < 0.001, "< 0.001", paste0("= ", sprintf("%.3f", pvalue_ph)))
    )
  }


  newdf3$Group <- cut_by(newdf3$x, ushap.cutoff, labels = fill_labels, label_type = "LMH")
  tmp_group <- cut_by(indf$x, ushap.cutoff, labels = fill_labels, label_type = "LMH")
  levels(newdf3$Group) <- paste0(levels(newdf3$Group), " (n=", table(tmp_group), ")")
  grob <- grobTree(textGrob(paste0("N = ", nrow(indf)),
    x = 0.5, y = 0.9,
    gp = gpar(col = "black", fontsize = 11)
  ))

  p <- ggplot2::ggplot() +
    geom_bar(
      data = newdf3, aes(x = x, y = pct * 100 / scale_factor, fill = Group),
      stat = "identity", # width=0.05,
      # fill="#f9f7f7",
      # color="white"
    ) +
    scale_fill_manual(value = group_colors, name = group_title) +
    geom_hline(yintercept = 1, linewidth = 1, linetype = 2, color = "grey") +
    geom_ribbon(
      data = newdf1, aes(x = x, ymin = lower, ymax = upper),
      fill = "#e23e57", alpha = 0.1
    ) +
    # geom_line(data=newdf1, aes(x=x, y=lower), linetype=2, color="#ff9999",linewidth=0.8) +
    # geom_line(data=newdf1, aes(x=x, y=upper), linetype=2, color="#ff9999",linewidth=0.8) +
    geom_line(data = newdf1, aes(x = x, y = y), color = "#e23e57", linewidth = 1) +
    geom_point(aes(x = ushap.cutoff, y = 1), color = "#e23e57", size = 2) +
    geom_segment(
      aes(
        x = c(labelx1 - offsetx1 * 5, labelx1 - offsetx1 * 5),
        xend = c(labelx1 - offsetx1, labelx1 - offsetx1),
        y = c(labely1 + offsety1, labely1 - offsety1),
        yend = c(labely1 + offsety1, labely1 - offsety1)
      ),
      linetype = 1,
      color = "#e23e57",
      linewidth = 1,
      alpha = c(1, 0.1)
    ) +
    geom_text(aes(
      x = ushap.cutoff, y = 0.9,
      label = paste0("Ref=", format(ushap.cutoff, digits = ref_digits))
    )) +
    geom_text(aes(x = labelx1, y = labely1, label = label1), hjust = 0) +
    geom_text(aes(x = labelx2, y = labely2, label = label2), hjust = 0) +
    annotation_custom(grob) +
    scale_x_continuous(xtitle, expand = c(0.01, 0.01)) +
    scale_y_continuous(
      ytitle1,
      expand = c(0, 0),
      limit = c(0, ymax1),
      transform = trans,
      sec.axis = sec_axis(
        name = ytitle2, transform = ~ . * scale_factor,
      )
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
      filename = paste0(paste0(c(x, paste0(knot, "knot"), paste0("with_", length(covs), "covs")),
                               collapse = "_"), ".png")
    }
    ggsave(filename, p, width = 6, height = 6)
  }

  message.print2 <- list(
    aics = aics, knot = knot, n.valid = nrow(indf), n.plot = nrow(newdf2),
    phassump = phassump, phresidual = phresidual,
    pvalue_all = pvalue_all,
    pvalue_nonlin = pvalue_nonlin,
    ref = ushap.cutoff, plot = plot.ushap.type2
  )
  return(message.print2)
}
