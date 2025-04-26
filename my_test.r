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

set.seed(1)
a=25; b=20
n=a*b
p=0.2
all_data=ifelse(runif(n)<p,NA,1)
df=matrix(all_data,nrow=a,ncol=b)
tmp=is.na(df)
hist(colSums(tmp))
hist(rowSums(tmp))
tmp=is.na(df)
df2=df[,colSums(tmp)<0.25*nrow(tmp)]
tmp=is.na(df2)
df2=df2[rowSums(tmp)<0.25*ncol(tmp),]
tmp=is.na(df)
df2=df[,colSums(tmp)<0.23*nrow(tmp)]
tmp=is.na(df2)
df2=df2[rowSums(tmp)<0.23*ncol(tmp),]
tmp=is.na(df2)
df2=df2[,colSums(tmp)<=0.2*nrow(tmp)]
tmp=is.na(df2)
df2=df2[rowSums(tmp)<=0.2*ncol(tmp),]
tmp=is.na(df2)
df2=df2[,colSums(tmp)<=0.2*nrow(tmp)]
tmp=is.na(df2)
df2=df2[rowSums(tmp)<=0.2*ncol(tmp),]

max(colSums(is.na(df2))/nrow(df2))
max(rowSums(is.na(df2))/ncol(df2))

val_id=find_optimal_subset(df,0.2,0.2,row_priority = 0.2)
val_set=df[val_id$rows,val_id$cols]
max(colSums(is.na(val_set))/nrow(val_set))
max(rowSums(is.na(val_set))/ncol(val_set))

find_optimal_subset <- function(df, row_na_ratio, col_na_ratio, row_priority = 1) {
  # 参数验证
  if (row_na_ratio < 0 || row_na_ratio > 1 || col_na_ratio < 0 || col_na_ratio > 1) {
    stop("row_na_ratio和col_na_ratio必须在0到1之间")
  }
  ori_nrow = nrow(df)
  ori_ncol = ncol(df)
  # 预计算缺失矩阵和初始索引
  na_mat <- as.matrix(is.na(df))
  current_rows <- seq_len(ori_nrow)
  current_cols <- seq_len(ori_ncol)
  
  # 计算动态权重函数
  score_weight <- function(type, id) {
    if (type == "row") {
      sum(na_mat[id, current_cols]) / length(current_cols) - row_na_ratio  # 行缺失比例
    } else {
      sum(na_mat[current_rows, id]) / length(current_rows) - col_na_ratio # 列缺失比例
    }
  }
  
  # 迭代优化主循环
  while (TRUE) {
    # 计算当前缺失情况
    row_missing <- rowSums(na_mat[current_rows, current_cols, drop = FALSE])
    col_missing <- colSums(na_mat[current_rows, current_cols, drop = FALSE])
    
    # 检测违规项
    bad_rows <- current_rows[(row_missing / length(current_cols)) > row_na_ratio]
    bad_cols <- current_cols[(col_missing / length(current_rows)) > col_na_ratio]
    
    # 终止条件
    if (length(bad_rows) + length(bad_cols) == 0) break
    
    # 生成候选列表（核心改进点）
    candidates <- list()
    
    row_base = (length(current_rows)-1)^row_priority * length(current_cols)
    col_base = length(current_rows)^row_priority * (length(current_cols)-1)
    
    # 行候选（优先处理缺失率高的行）
    for (r in bad_rows) {
      new_rows <- setdiff(current_rows, r)
      score <- row_base * 
        (1 - score_weight("row", r))  # 缺失率越高权重越大
      candidates <- append(candidates, list(list(type = "row", id = r, score = score)))
    }
    
    # 列候选（优先处理缺失率高的列）
    for (c in bad_cols) {
      new_cols <- setdiff(current_cols, c)
      score <- col_base * 
        (1 - score_weight("col", c))  # 缺失率越高权重越大
      candidates <- append(candidates, list(list(type = "col", id = c, score = score)))
    }
    
    # 选择最佳候选（分数越低越优先删除）
    if (length(candidates) == 0) break
    best <- candidates[[which.min(sapply(candidates, function(x) x$score))]]
    
    # 执行删除
    if (best$type == "row") {
      current_rows <- setdiff(current_rows, best$id)
    } else {
      current_cols <- setdiff(current_cols, best$id)
    }
  }
  
  # 返回有序结果
  list(rows = sort(current_rows), cols = sort(current_cols))
}
