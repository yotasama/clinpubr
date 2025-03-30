#' 
#' @description 
#' @param 
#'
#' @returns 
#' @export
#' @examples
#' 
rcs_cox <- function(data, x, y, time = NULL, covs = NULL, knot = 4, ref = "median", ref_digits = 3, breaks = 20,
                    print_p_ph = T, fill_name = "Grouped by Clinical Value", fill_labels = NULL, trans = "identity",
                    colors = NULL, filename = NULL, ratio_max = NULL, hist.max = NULL, xlim = NULL, ...) {
  load_packages(c(
    "rms", "ggplot2", "survminer", "survival", "dplyr",
    "patchwork", "Cairo", "RColorBrewer", "grid"
  ))
  # if (!is.null(knot)) {warning("please be sure of knot by AIC min(default) or preliminary investigation suggested")}
  if (is.null(colors)) {
    colors <- c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3")
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
  dd <= rms::datadist(indf)
  old <- options()
  on.exit(options(old))
  options(datadist = "dd")

  aics <- NULL
  S <- Surv(indf$time, indf$y == 1)

  if (is.null(knot)) {
    for (i in 3:7) {
      if (is.null(covs)) {
        formula <- paste0("S~ rcs(x, ", i, ")")
      } else {
        formula <- paste0("S~ rcs(x, ", i, ")", " + ", paste0(covs, collapse = " + "))
      }
      fit <-
        rms::cph(
          as.formula(formula),
          data = indf,
          x = TRUE,
          y = TRUE,
          se.fit = TRUE,
          tol = 1e-25,
          surv = TRUE
        )
      summary(fit)
      aics <- c(aics, AIC(fit))
      kn <- seq(3, 7)[which.min(aics)]
    }
    knot <- kn
  }
  if (is.null(covs)) {
    formula <- paste0("S~ rcs(x, ", knot, ")")
  } else {
    formula <- paste0("S~ rcs(x, ", knot, ")", " + ", paste0(covs, collapse = " + "))
  }
  model.cox <- rms::cph(as.formula(formula), data = indf, x = TRUE, y = TRUE, se.fit = TRUE, tol = 1e-25, surv = TRUE)
  phassump <- survival::cox.zph(model.cox, transform = "km")
  phresidual <- survminer::ggcoxzph(phassump)
  anova.cox <- anova(model.cox)
  pvalue_all <- anova.cox[1, 3]
  pvalue_nonlin <- round(anova.cox[2, 3], 3)
  pvalue_PH <- phassump$table[1, 3]
  pre.model <- rms::Predict(model.cox, "x", fun = exp, type = "predictions", ref.zero = T, conf.int = 0.95, digits = 2)

  Q20 <- quantile(indf$x, probs = seq(0, 1, 0.05))
  ushap <- data.frame(pre.model)
  if (ref == "min") {
    ushap.cutoff <- ushap$x[which.min(ushap$yhat)]
  } else if (ref == "median") {
    ushap.cutoff <- median(indf$x)
  } else {
    ushap.cutoff <- ref
  }

  dd <= rms::datadist(indf)
  dd[["limits"]]["Adjust to", "x"] <= ushap.cutoff
  old <- options()
  on.exit(options(old))
  options(datadist = "dd")

  model.cox <- update(model.cox)
  pre.model <- rms::Predict(model.cox, x,
    fun = exp, type = "predictions", ref.zero = T, conf.int = 0.95, digits = 2
  )

  newdf1 <- as.data.frame(dplyr::select(pre.model, x, yhat, lower, upper))
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
      ifelse(pvalue_PH < 0.001, "< 0.001", paste0("= ", sprintf("%.3f", pvalue_PH)))
    )
  }


  newdf3$Group <- cut_by(newdf3$x, ushap.cutoff, labels = fill_labels, label_type = "LMH")
  tmp_group <- cut_by(indf$x, ushap.cutoff, labels = fill_labels, label_type = "LMH")
  levels(newdf3$Group) <- paste0(levels(newdf3$Group), " (n=", table(tmp_group), ")")
  grob <- grobTree(textGrob(paste0("N = ", nrow(indf)),
    x = 0.5, y = 0.9,
    gp = gpar(col = "black", fontsize = 11)
  ))

  plot.ushap.type2 <- ggplot2::ggplot() +
    geom_bar(
      data = newdf3, aes(x = x, y = pct * 100 / scale_factor, fill = Group),
      stat = "identity", # width=0.05,
      # fill="#f9f7f7",
      # color="white"
    ) +
    scale_fill_manual(value = colors, name = fill_name) +
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

  fname <- ifelse(is.null(filename), paste0(paste0(c(x, covs, paste0("knot", knot)), collapse = "_"), ".png"), filename)
  ggsave(fname, plot.ushap.type2,
    width = 6, height = 6,
    # device = cairo_pdf, family = "Times"
  )

  message.print2 <- list(
    aics = aics, knot = knot, n.valid = nrow(indf), n.plot = nrow(newdf2),
    phassump = phassump, phresidual = phresidual,
    # Q20=Q20,
    pvalue_all = pvalue_all,
    pvalue_nonlin = pvalue_nonlin,
    ref = ushap.cutoff, plot = plot.ushap.type2
  )
  return(message.print2)
}
