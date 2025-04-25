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

cancer$ph.ecog_cat=factor(cancer$ph.ecog,levels=c(0:3),labels = c("0","1","≥2","≥2"))

subgroup_forest(cancer,
                var_subgroups = c("age", "sex", "wt.loss"), x = "ph.ecog_cat", y = "dead",
                covs = "ph.karno", ticks_at = c(1, 2)
)

subgroup_forest <- function(data, var_subgroups, x, y, time = NULL, covs = NULL, decimal_est = 2, p_nsmall = 3,
                            group_cut_quantiles = 0.5, save_plot = TRUE, filename = NULL, ...) {
  if (is.factor(data[[x]])) {
    row_count=length(levels(data[[x]]))-1
  }else{
    row_count=1
  }
  multi_row=row_count>1
  
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
  for (var in var_subgroups) {
    indf[[var]] <- to_factor(indf[[var]])
    plot_nrow <- plot_nrow + length(levels(indf[[var]]))*row_count
  }
  
  overall_res <- regression_p_value(indf, y, x, time = time, covs = covs)
  
  model_res <- regression_p_value(
    data = dat, y = "y", predictor = var, time = new_time_var,
    covs = tmp_covs, return_full_result = TRUE
  )
  model_res <- data.frame(model_res[grepl("x", model_res$term), ])
  for (col in c("estimate", "conf.low", "conf.high")) {
    model_res[, col] <- format(model_res[, col], digits = 1, nsmall = ratio_nsmall)
  }
  tmp <- data.frame(
    term = model_res$term,
    ratio = paste0(model_res$estimate, "(", model_res$conf.low, ",", model_res$conf.high, ")"),
    P = format_pval(model_res$p.value, nsmall = pval_nsmall, eps = pval_eps)
  )
  colnames(tmp)[2] <- ratio_type
  if (is.numeric(dat[[var]])) {
    res_table[i, col1] <- tmp[[ratio_type]]
    res_table[i, col2] <- tmp$P
    i <- i + 1
  } else {
    res_table[i + 2:(nrow(tmp) + 1), col1:col2] <- tmp[, -1]
    res_table[i + 1, col1] <- "1 (Reference)"
    i <- i + nrow(tmp) + 2
    if (nrow(tmp) > 1) {
      dat$tmp <- as.numeric(dat0[[var]])
      model_res <- regression_p_value(
        data = dat, y = "y", predictor = "tmp", time = new_time_var,
        covs = tmp_covs
      )
      res_table[i, col2] <- format_pval(model_res$p.value, nsmall = pval_nsmall, eps = pval_eps)
      i <- i + 1
    }
  }
  
  
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
    if (length(tmp_covs) == 0) tmp_covs <- NULL
    
    p_int <- interaction_p_value(indf, y, x, var, time = time, covs = tmp_covs)
    
    model_res <- regression_p_value(
      data = dat, y = "y", predictor = var, time = new_time_var,
      covs = tmp_covs, return_full_result = TRUE
    )
    model_res <- data.frame(model_res[grepl("x", model_res$term), ])
    for (col in c("estimate", "conf.low", "conf.high")) {
      model_res[, col] <- format(model_res[, col], digits = 1, nsmall = ratio_nsmall)
    }
    tmp <- data.frame(
      term = model_res$term,
      ratio = paste0(model_res$estimate, "(", model_res$conf.low, ",", model_res$conf.high, ")"),
      P = format_pval(model_res$p.value, nsmall = pval_nsmall, eps = pval_eps)
    )
    colnames(tmp)[2] <- ratio_type
    if (is.numeric(dat[[var]])) {
      res_table[i, col1] <- tmp[[ratio_type]]
      res_table[i, col2] <- tmp$P
      i <- i + 1
    } else {
      res_table[i + 2:(nrow(tmp) + 1), col1:col2] <- tmp[, -1]
      res_table[i + 1, col1] <- "1 (Reference)"
      i <- i + nrow(tmp) + 2
      if (nrow(tmp) > 1) {
        dat$tmp <- as.numeric(dat0[[var]])
        model_res <- regression_p_value(
          data = dat, y = "y", predictor = "tmp", time = new_time_var,
          covs = tmp_covs
        )
        res_table[i, col2] <- format_pval(model_res$p.value, nsmall = pval_nsmall, eps = pval_eps)
        i <- i + 1
      }
    }
    
    
    
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
    ci_column = ifelse(multi_row,5,4),
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


regression_basic_results(cancer,
                   x = "age", y = "status", time = "time",
                   model_covs = list(Crude = c(), Model1 = c("ph.karno"), Model2 = c("ph.karno", "sex"))
                 )