
subgroup_forest(cancer,
  var_subgroups = c("age", "sex", "wt.loss"), x = "ph.ecog", y = "status",
  time = "time", covs = c("sex","ph.karno"), ticks_at = c(1, 2)
)

subgroup_forest <- function(data, var_subgroups, x, y, time = NULL, covs = NULL, decimal_est = 2, p_nsmall = 3,
                            group_cut_quantiles = 0.5, save_plot = TRUE, filename = NULL, ...) {
  if (!is.numeric(data[[x]]) && (!is.factor(data[[x]]) || length(levels(data[[x]])) != 2)) {
    stop("x must be numeric or a factor with 2 levels")
  }

  analysis_type <- ifelse(is.null(time), "logistic", "cox")
  covs <- remove_conflict(covs, c(y, x, time))
  ori_covs <- covs
  var_subgroups <- remove_conflict(var_subgroups, c(y, x, time))
  if (length(var_subgroups) == 0) stop("No valid var_subgroups specified.")

  indf <- dplyr::select(data, all_of(c(y, x, time, covs)))

  if (!is.null(covs)) {
    covs <- paste0(".cov", seq_along(covs))
    if (any(covs %in% colnames(data))) stop("Colnames start with '.cov' are reserved.")
    start_col <- ifelse(analysis_type == "cox", 4, 3)
    colnames(indf)[start_col:(start_col + length(covs) - 1)] <- covs
  }

  indf <- cbind(indf, dplyr::select(data, all_of(var_subgroups)))
  indf <- indf[complete.cases(indf[, c(y, x, time, covs)]), ]
  plot_nrow <- 4 + length(var_subgroups)
  for (var in var_subgroups) {
    indf[[var]] <- to_factor(indf[[var]])
    plot_nrow <- plot_nrow + length(levels(indf[[var]]))
  }
  print(head(indf))
  overall_res <- regression_p_value(indf, y, x, time = time, covs = covs)

  res <- data.frame(
    Variable = "Overall",
    Count = nrow(indf),
    Percent = 100,
    `Point Estimate` = overall_res$estimate,
    Lower = overall_res$conf.low,
    Upper = overall_res$conf.high,
    `P value` = overall_res$p.value,
    `P for interaction` = NA,
    check.names = FALSE
  )

  for (var in var_subgroups) {
    tmp_covs <- covs[ori_covs != var]

    p_int <- int_p_value(indf, y, x, var, time = time, covs = tmp_covs)

    res <- rbind(res, data.frame(
      Variable = var,
      Count = NA,
      Percent = NA,
      `Point Estimate` = NA,
      Lower = NA,
      Upper = NA,
      `P value` = NA,
      `P for interaction` = p_int,
      check.names = FALSE
    ))

    lvls <- levels(indf[[var]])
    tmp_res <- NULL
    for (lvl in lvls) {
      subset_data <- indf[which(indf[[var]] == lvl), ]
      lvl_res <- regression_p_value(subset_data, y, x, time = time, covs = tmp_covs)

      tmp_res <- rbind(
        tmp_res,
        data.frame(
          Variable = paste("  ", lvl), Count = nrow(subset_data),
          Percent = NA, `Point Estimate` = lvl_res$estimate,
          Lower = lvl_res$conf.low, Upper = lvl_res$conf.high,
          `P value` = lvl_res$p.value, `P for interaction` = NA, check.names = FALSE
        )
      )
    }

    tmp_res$Percent <- round(tmp_res$Count / sum(tmp_res$Count) * 100, 1)
    res <- rbind(res, tmp_res)
  }

  for (col in c("P value", "P for interaction")) {
    res[[col]] <- ifelse(is.na(res[[col]]), "",
                         base::format.pval(as.numeric(res[[col]]),
                                           digits = 1,
                                           nsmall = p_nsmall, eps = 0.001
                         )
    )
  }

  effect_label <- ifelse(analysis_type == "cox", "HR (95% CI)", "OR (95% CI)")
  plot_df <- res
  plot_df[[effect_label]] <- ifelse(is.na(plot_df$`Point Estimate`), "",
                                    sprintf(
                                      paste0("%.", decimal_est, "f (%.", decimal_est, "f to %.", decimal_est, "f)"),
                                      plot_df$`Point Estimate`, plot_df$Lower, plot_df$Upper
                                    )
  )

  na_cols <- c("Count", "Percent", "P value", "P for interaction")
  plot_df[na_cols][is.na(plot_df[na_cols])] <- " "
  plot_df$` ` <- paste(rep(" ", 20), collapse = " ")

  plot_columns <- c("Variable", "Count", "Percent", " ", effect_label, "P value", "P for interaction")

  p <- forestploter::forest(
    plot_df[, plot_columns],
    est = plot_df$`Point Estimate`,
    lower = plot_df$Lower,
    upper = plot_df$Upper,
    ci_column = 4,
    ref_line = 1,
    x_trans = "log10",
    ...
  )
  if (save_plot) {
    if (is.null(filename)) {
      filename <- paste0(paste0(
        c("subgroup_forest", x, paste0(
          "with_", length(var_subgroups),
          "subgroups_and_", length(covs), "covs"
        )),
        collapse = "_"
      ), ".png")
    }
    ggplot2::ggsave(filename, p, width = 10, height = plot_nrow / 4)
  }
  p
}



regression_p_value <- function(data, y, predictor, time = NULL, covs = NULL, rcs_knots = NULL) {
  if (!is.null(rcs_knots) && rcs_knots == 0) rcs_knots <- NULL
  analysis_type <- ifelse(is.null(time), "logistic", "cox")
  covs <- remove_conflict(covs, c(y, predictor, time))
  if (is.factor(data[[predictor]]) && length(levels(data[[predictor]])) > 2) {
    predictor_type <- "multi_factor"
  } else {
    predictor_type <- "num_or_binary"
  }
  print(data)
  formula <- create_formula(y, predictor, time = time, covs = covs, rcs_knots = rcs_knots)

  if (is.factor(data[[predictor]]) || is.null(rcs_knots)) {
    res <- list(estimate = NA, conf.low = NA, conf.high = NA, p.value = NA)
    if (analysis_type == "cox") {
      model <- coxph(formula, data = data)
      if (predictor_type == "num_or_binary") {
        predictor_summary <- broom::tidy(model, conf.int = TRUE, exponentiate = TRUE)[1, ]
        res$estimate <- predictor_summary$estimate
        res$conf.low <- predictor_summary$conf.low
        res$conf.high <- predictor_summary$conf.high
      }
    } else {
      model <- glm(formula, data = data, family = binomial())
      if (predictor_type == "num_or_binary") {
        predictor_summary <- broom::tidy(model, conf.int = TRUE, exponentiate = TRUE)[2, ]
        res$estimate <- predictor_summary$estimate
        res$conf.low <- predictor_summary$conf.low
        res$conf.high <- predictor_summary$conf.high
      }
    }
    res$p.value <- broom::tidy(car::Anova(model, type = 2, test.statistic = "Wald"))$p.value[1]
    return(res)
  } else {
    if (analysis_type == "cox") {
      model <- cph(formula, data = data)
    } else {
      model <- Glm(formula, data = data, family = binomial())
    }
    ps <- unname(anova(model)[, "P"])
    return(list(p_overall = ps[1], p_nonlinear = ps[2]))
  }
}
