print(1+1)

data(cancer, package = "survival")
# coxph model with time assigned
subgroup_forest(cancer,
  var_subgroups = c("age", "sex", "wt.loss"), x = "ph.ecog", y = "status",
  time = "time", covs = "ph.karno", ticks_at = c(1, 2)
)

# logistic model with time not assigned
cancer$dead <- cancer$status == 2
subgroup_forest(cancer,
  var_subgroups = c("age", "sex", "wt.loss"), x = "ph.ecog", y = "dead",
  covs = "ph.karno", ticks_at = c(1, 2)
)

cancer$ph.ecog =factor(cancer$ph.ecog,levels=c(0:3),labels=c("0","1","≥2","≥2"))
subgroup_forest(cancer,
  var_subgroups = c("age", "sex", "wt.loss"), x = "ph.ecog", y = "dead",
  covs = "ph.karno", ticks_at = c(1, 2)
)

data=cancer; var_subgroups=c("age", "sex", "wt.loss"); x= "ph.ecog"; y= "dead"; time = NULL; covs = NULL; decimal_est = 2; p_nsmall = 3;
group_cut_quantiles = 0.5; save_plot = TRUE; filename = NULL

subgroup_forest <- function(data, var_subgroups, x, y, time = NULL, covs = NULL, decimal_est = 2, p_nsmall = 3,
                            group_cut_quantiles = 0.5, save_plot = TRUE, filename = NULL, ...) {
  
  x_type <- ifelse(!is.factor(data[[x]]), "number", "factor")
  analysis_type <- ifelse(is.null(time), "logistic", "cox")
  covs <- remove_conflict(covs, c(y, x, time))
  ori_covs <- covs
  var_subgroups <- remove_conflict(var_subgroups, c(y, x, time))
  if (length(var_subgroups) == 0) stop("No valid var_subgroups specified.")
  
  indf <- dplyr::select(data, all_of(c(y, x, time, covs)))
  
  if (!is.null(covs)) {
    covs <- paste0("tmp_cov", seq_along(covs))
    if (any(covs %in% colnames(data))) stop("Colnames start with 'tmp_cov' are reserved.")
    start_col <- ifelse(analysis_type == "cox", 4, 3)
    colnames(indf)[start_col:(start_col + length(covs) - 1)] <- covs
  }
  
  indf <- cbind(indf, dplyr::select(data, all_of(var_subgroups)))
  indf <- indf[complete.cases(indf[, c(y, x, time, covs)]), ]
  plot_nrow <- 4 + length(var_subgroups)
  if (x_type == "factor") {
    plot_nrow <- plot_nrow + length(levels(indf[[x]])) - 1
    n_plot_levels <- length(levels(indf[[x]])) - 1
  } else {
    n_plot_levels <- 1
  }
  
  for (var in var_subgroups) {
    indf[[var]] <- to_factor(indf[[var]])
    plot_nrow <- plot_nrow + length(levels(indf[[var]])) * n_plot_levels
  }
  
  overall_res <- regression_fit(
    data = indf, y = y, predictor = x, time = time, covs = covs, returned = "predictor_split"
  )
  if (x_type == "number") {
    res <- data.frame(
      Subgroup = "Overall",
      Count = nrow(indf),
      Percent = 100,
      `Point Estimate` = overall_res$estimate,
      Lower = overall_res$conf.low,
      Upper = overall_res$conf.high,
      `P value` = overall_res$p.value,
      `P for interaction` = NA,
      check.names = FALSE
    )
  } else {
    res <- rbind(
      data.frame(
        Subgroup = "Overall",
        Count = nrow(indf),
        Percent = 100,
        level = levels(indf[[x]])[1],
        `Point Estimate` = 1,
        Lower = NA,
        Upper = NA,
        `P value` = NA,
        `P for interaction` = NA,
        check.names = FALSE
      ),
      data.frame(
        Subgroup = NA,
        Count = NA,
        Percent = NA,
        level = levels(indf[[x]])[-1],
        `Point Estimate` = overall_res$estimate,
        Lower = overall_res$conf.low,
        Upper = overall_res$conf.high,
        `P value` = overall_res$p.value,
        `P for interaction` = NA,
        check.names = FALSE
      )
    )
  }
  
  for (var in var_subgroups) {
    tmp_covs <- covs[ori_covs != var]
    if (length(tmp_covs) == 0) tmp_covs <- NULL
    
    p_int <- interaction_p_value(indf, y, x, var, time = time, covs = tmp_covs)
    
    res <- rbind(res, data.frame(
      Subgroup = var,
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
      lvl_res <- regression_fit(subset_data, y, x, time = time, covs = tmp_covs, returned = "predictor_split")
      
      if (x_type == "number") {
        lvl_res <- data.frame(
          Subgroup = paste("  ", lvl), Count = nrow(subset_data),
          Percent = NA, `Point Estimate` = lvl_res$estimate,
          Lower = lvl_res$conf.low, Upper = lvl_res$conf.high,
          `P value` = lvl_res$p.value, `P for interaction` = NA, check.names = FALSE
        )
      } else {
        lvl_res <- data.frame(
          Subgroup = paste("  ", lvl), Count = nrow(subset_data),
          Percent = NA, level = levels(indf[[x]])[-1], `Point Estimate` = lvl_res$estimate,
          Lower = lvl_res$conf.low, Upper = lvl_res$conf.high,
          `P value` = lvl_res$p.value, `P for interaction` = NA, check.names = FALSE
        )
        lvl_res$Subgroup[-1] <- NA
        lvl_res$Count[-1] <- NA
        lvl_res$Percent[-1] <- NA
      }
      
      tmp_res <- rbind(tmp_res, lvl_res)
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
  
  plot_columns <- if (x_type == "number") {
    c("Subgroup", "Count", "Percent", " ", effect_label, "P value", "P for interaction")
  } else {
    c("Subgroup", "Count", "Percent", "level", " ", effect_label, "P value", "P for interaction")
  }
  
  p <- forestploter::forest(
    plot_df[, plot_columns],
    est = plot_df$`Point Estimate`,
    lower = plot_df$Lower,
    upper = plot_df$Upper,
    ci_column = 4,
    ref_line = 1,
    x_trans = "log10"
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