load_all()

data(cancer, package = "survival")
# coxph model with time assigned
subgroup_forest(cancer,
  subgroup_vars = c("age", "sex", "wt.loss"), x = "ph.ecog", y = "status",
  time = "time", covs = "ph.karno", ticks_at = c(1, 2)
)

data(cancer, package = "survival")
cancer$ph.ecog_cat <- factor(cancer$ph.ecog, levels = c(0:3), labels = c("0", "1", "≥2", "≥2"))
data=cancer; model_vars = c("age", "sex", "wt.loss","ph.ecog_cat"); y = "status"; time = "time"; est_nsmall = 2; p_nsmall = 3; show_vars = NULL;
save_plot = TRUE; filename = NULL;as_univariate = FALSE

regression_forest <- function(data, model_vars, y, time = NULL, as_univariate = FALSE, est_nsmall = 2, 
                              p_nsmall = 3, show_vars = NULL, save_plot = TRUE, filename = NULL, ...) {
  analysis_type <- ifelse(is.null(time), "logistic", "cox")
  effect_label <- ifelse(analysis_type == "cox", "HR (95% CI)", "OR (95% CI)")
  
  show_model_names = TRUE
  if (is.vector(model_vars, mode = "character")) {
    if(as_univariate){
      model_vars <- as.list(model_vars)
      names(model_vars)=paste0("M",seq_along(model_vars))
    }else{
      model_vars <- list(M= model_vars)
    }
    show_model_names = FALSE
  } else if (!is.list(model_vars)) {
    stop("model_vars should be a character vector or named list of variables")
  }
  
  vars <- unique(unlist(model_vars))
  if(is.null(show_vars)) show_vars=vars
  data <- na.omit(dplyr::select(data, all_of(c(y, time, vars))))
  
  res_list <- list()
  for (model_name in names(model_vars)) {
    tmp_vars <- model_vars[[model_name]]
    
    fit_res <- regression_fit(
      data = data,
      y = y,
      predictor = tmp_vars[1],
      time = time,
      covs = tmp_vars[-1],
      returned = "full"
    )
    
    tmp_res=NULL
    for(var in intersect(tmp_vars,show_vars)){
      if(!is.factor(data[[var]])||length(levels(data[[var]]))==2){
        tmp=fit_res[fit_res$term=="var"]
        tmp=data.frame(
          Model = model_name,
          Variable = var,
          Estimate = fit_res$estimate,
          Lower = fit_res$conf.low,
          Upper = fit_res$conf.high,
          `P value` = fit_res$p.value,
          stringsAsFactors = FALSE
        )
      }else{
        tmp=fit_res[fit_res$term=="var"]
        tmp=data.frame(
          Model = model_name,
          Variable = var,
          Estimate = fit_res$estimate,
          Lower = fit_res$conf.low,
          Upper = fit_res$conf.high,
          `P value` = fit_res$p.value,
          stringsAsFactors = FALSE
        )
      }
    }
    
    res_list[[model_name]] <- data.frame(
      Model = model_name,
      Variable = model_vars[[model_name]],
      Estimate = fit_res$estimate,
      Lower = fit_res$conf.low,
      Upper = fit_res$conf.high,
      Pvalue = fit_res$p.value,
      stringsAsFactors = FALSE
    )
  }
  
  plot_df <- do.call(rbind, res_list)
  plot_df$Effect <- sprintf("%.*f (%.*f to %.*f)", 
                            est_nsmall, plot_df$Estimate,
                            est_nsmall, plot_df$Lower,
                            est_nsmall, plot_df$Upper)
  plot_df$Pvalue <- format.pval(plot_df$Pvalue, digits = 1, eps = 0.001)
  
  if (!is.null(show_vars)) {
    plot_df <- plot_df[plot_df$Variable %in% show_vars, ]
  }
  
  p <- forestploter::forest(
    plot_df[, c("Model", "Variable", "Effect", "Pvalue")],
    est = plot_df$Estimate,
    lower = plot_df$Lower,
    upper = plot_df$Upper,
    ci_column = 3,
    ref_line = 1,
    x_trans = if(analysis_type=="cox") "log10" else "identity",
    ...
  )
  
  if (save_plot) {
    if (is.null(filename)) {
      filename <- paste("forestplot", analysis_type, "models", length(model_vars), "vars", length(unique(plot_df$Variable)), "png", sep = ".")
    }
    ggplot2::ggsave(filename, p, width = 10, height = 4 + nrow(plot_df)*0.4)
  }
  
  invisible(p)
}