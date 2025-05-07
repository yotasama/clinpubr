load_all()

cancer$dead <- cancer$status == 2
cancer$ph.ecog_cat <- factor(cancer$ph.ecog, levels = c(0:3), labels = c("0", "1", "≥2", "≥2"))
subgroup_forest(cancer,
  subgroup_vars = c("sex", "wt.loss"), x = "ph.ecog_cat", y = "dead",
  covs = "ph.karno", ticks_at = c(1, 2)
)

format_pval(c(0.001, 0.0001, 0.05, 0.1123456))
format_pval(c(0.001, 0.0001, 0.05, 0.1123456, NA), text_ahead = "p value", na_empty = F)

data(cancer, package = "survival")
cancer$ph.ecog_cat <- factor(cancer$ph.ecog, levels = c(0:3), labels = c("0", "1", "≥2", "≥2"))
regression_forest(cancer,
  model_vars = c("age", "sex", "wt.loss", "ph.ecog_cat", "meal.cal"), y = "status", time = "time",
  as_univariate = TRUE
)

regression_forest(cancer,
  model_vars = c("age", "sex", "wt.loss", "ph.ecog_cat", "meal.cal"), y = "status", time = "time",
  show_vars = c("age", "sex", "ph.ecog_cat", "meal.cal")
)

regression_forest(cancer,
  model_vars = list(M0=c("age"),
                    M1=c("age", "sex", "wt.loss", "ph.ecog_cat", "meal.cal"),
                    M2=c("age", "sex", "wt.loss", "ph.ecog_cat", "meal.cal", "pat.karno")),
  y = "status", time = "time",
  show_vars = c("age", "sex", "ph.ecog_cat", "meal.cal")
)

data=cancer; model_vars = c("age", "sex", "wt.loss","ph.ecog_cat"); y = "status"; time = "time"; est_nsmall = 2; p_nsmall = 3;
show_vars = c("age", "sex", "ph.ecog_cat");save_plot = TRUE; filename = NULL;as_univariate = FALSE

regression_forest(data=cancer, model_vars = c("age", "sex", "wt.loss","ph.ecog_cat"), y = "status", time = "time")

regression_forest <- function(data, model_vars, y, time = NULL, as_univariate = FALSE, est_nsmall = 2,
                              p_nsmall = 3, show_vars = NULL, save_plot = TRUE, filename = NULL, ...) {
  if (is.null(time)) {
    analysis_type <- "logistic"
    effect_label <- "OR (95% CI)"
    new_time_var <- NULL
  } else {
    analysis_type <- "cox"
    effect_label <- "HR (95% CI)"
    new_time_var <- "time"
  }
  ref_val <- ifelse(analysis_type %in% c("cox", "logistic"), 1, 0)

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

  new_vars = paste0("v",
                    str_pad(seq_along(vars),ceiling(log10(length(vars)+1)),'left','0'),
                    "_")
  colnames(data)=c("y",new_time_var, new_vars)
  show_var_ids = as.vector(na.omit(match(show_vars,vars)))
  res_list <- list()
  for (model_name in names(model_vars)) {
    tmp_var_ids = match(model_vars[[model_name]],vars)

    fit_res <- regression_fit(
      data = data,
      y = "y",
      predictor = new_vars[tmp_var_ids[1]],
      time = new_time_var,
      covs = new_vars[tmp_var_ids[-1]],
      returned = "full"
    )

    tmp_res=NULL
    for(var_id in intersect(tmp_var_ids,show_var_ids)){
      if(!is.factor(data[[new_vars[var_id]]])||length(levels(data[[new_vars[var_id]]]))==2){
        tmp=fit_res[fit_res$term==new_vars[var_id],]
        tmp=data.frame(
          Model = model_name,
          Variable = vars[var_id],
          Level = NA,
          Estimate = tmp$estimate,
          Lower = tmp$conf.low,
          Upper = tmp$conf.high,
          `P value` = tmp$p.value,
          check.names = FALSE
        )
      }else{
        tmp=fit_res[grepl(new_vars[var_id],fit_res$term),]
        tmp=data.frame(
          Model = c(model_name,rep(NA,nrow(tmp))),
          Variable = c(vars[var_id],rep(NA,nrow(tmp))),
          Level = levels(data[,new_vars[var_id]]),
          Estimate = c(ref_val,tmp$estimate),
          Lower = c(ref_val,tmp$conf.low),
          Upper = c(ref_val,tmp$conf.high),
          `P value` = c(NA,tmp$p.value),
          check.names = FALSE
        )
      }
      tmp_res=rbind(tmp_res,tmp)
      tmp_res$Model[-1]=NA
    }
    res_list[[model_name]]=tmp_res
  }

  plot_df <- do.call(rbind, res_list)
  plot_df[[effect_label]] <- ifelse(!is.na(plot_df$Variable)&!is.na(plot_df$Level),
                                    "Reference",
                                    sprintf("%.*f (%.*f to %.*f)",
                            est_nsmall, plot_df$Estimate,
                            est_nsmall, plot_df$Lower,
                            est_nsmall, plot_df$Upper))
  max_var_width=max(str_width(paste(plot_df$Variable,"  ",plot_df$Level)))
  # plot_df$Variable = ifelse(is.na(plot_df$Level),plot_df$Variable,
  #                           ifelse(is.na(plot_df$Variable),
  #                                  str_pad(plot_df$Level,max_var_width,'left',' '),
  #                                  paste0(plot_df$Variable,str_pad(plot_df$Level,max_var_width-str_width(plot_df$Variable),'left',' '))))
  na_cols <- c("Model", "Variable", "Level")

  plot_df$`P value` <- format_pval(plot_df$`P value`, nsmall = p_nsmall)
  plot_df[na_cols][is.na(plot_df[na_cols])] <- " "
  plot_df$` ` <- paste(rep(" ", 20), collapse = " ")
  if(any(!is.na(plot_df$Level))){
    plot_columns=c("Model", "Variable", "Level", " ", effect_label, "P value")
    ci_column=4
  }else{
    plot_columns=c("Model", "Variable", " ", effect_label, "P value")
    ci_column=3
  }

  p <- forestploter::forest(
    plot_df[, plot_columns],
    est = plot_df$Estimate,
    lower = plot_df$Lower,
    upper = plot_df$Upper,
    ci_column = ci_column,
    ref_line = ref_val,
    x_trans = "log10",
    ...
  )
  if (save_plot) {
    if (is.null(filename)) {
      filename <- paste0(paste0(
        c("regression_forest_with",length(model_vars),"models"),
        collapse = "_"
      ), ".png")
    }
    ggplot2::ggsave(filename, p, width = 10, height = nrow(plot_df) / 4+1)
  }
  p
}
