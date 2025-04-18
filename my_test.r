data(cancer, package = "survival")
# coxph model with time assigned
data=dat
y="结局"
time="最大狼疮脑随访天数"
predictors = NULL; covs = NULL; num_to_factor = 5;
p_adjust_method = "BH"; save_table = TRUE; filename = NULL
regression_scan <- function(data, y, time = NULL, predictors = NULL, covs = NULL, num_to_factor = 5,
                            p_adjust_method = "BH", save_table = TRUE, filename = NULL) {
  supported_var_trans <- list(
    numerical = c("original", "logarithm", "categorized", "rcs"),
    num_factor = c("original", "categorized"),
    factor = c("original"),
    other = c()
  )
  if (is.null(time)) {
    analysis_type <- "logistic"
    ratio_type <- "OR"
  } else {
    analysis_type <- "cox"
    ratio_type <- "HR"
  }
  if (is.null(predictors)) {
    predictors <- setdiff(colnames(data), c(y, time))
    message("Taking all variables as interaction predictors")
  }
  if (any(!predictors %in% colnames(data))) {
    stop("Some predictors are not in the data")
  }

  res_df <- data.frame(matrix(NA, nrow = length(predictors), ncol = 15))
  colnames(res_df) <- c(
    "predictor", "nvalid",
    paste(rep(c("original", "logarithm", "categorized"), each = 3),
          c(ratio_type, "pval", "padj"),
          sep = "."
    ),
    paste("rcs", rep(c("overall", "nonlinear"), each = 2), c("pval", "padj"), sep = ".")
  )

  res_df$predictor <- predictors
  for (i in seq_along(predictors)) {
    predictor <- predictors[i]
    tmp_covs <- remove_conflict(covs, c(y, predictor, time), silent = TRUE)
    dat <- dplyr::select(data, all_of(c(y, predictor, time, tmp_covs)))
    dat <- na.omit(dat)
    nvalid <- nrow(dat)
    res_df$nvalid[i] <- nvalid
    if (nvalid < 10) {
      next
    }
    if (is.factor(dat[[predictor]])) {
      predictor_type <- "factor"
    } else if (is.numeric(dat[[predictor]])) {
      if (length(unique(dat[[predictor]])) <= num_to_factor) {
        predictor_type <- "num_factor"
      } else {
        predictor_type <- "numerical"
      }
    } else {
      predictor_type <- "other"
    }
    for (var_trans in supported_var_trans[[predictor_type]]) {
      tmp_dat <- dat
      rcs_knots <- NULL
      if (var_trans == "logarithm") {
        if (any(dat[[predictor]] <= 0)) {
          next
        }
        tmp_dat[[predictor]] <- log(tmp_dat[[predictor]])
      } else if (var_trans == "categorized") {
        if (predictor_type == "num_factor") {
          tmp_dat[[predictor]] <- as.factor(tmp_dat[[predictor]])
        } else {
          tmp_dat[[predictor]] <- cut_by(
            tmp_dat[[predictor]],
            breaks = 0.5,
            breaks_as_quantiles = TRUE
          )
        }
      } else if (var_trans == "rcs") {
        rcs_knots <- 4
      }
      model_res <- regression_p_value(
        data = tmp_dat, y = y, predictor = predictor, time = time,
        covs = tmp_covs, rcs_knots = rcs_knots
      )
      if (var_trans == "rcs") {
        res_df$rcs.overall.pval[i] <- model_res$p_overall
        res_df$rcs.nonlinear.pval[i] <- model_res$p_nonlinear
      } else {
        res_df[[paste(var_trans, ratio_type, sep = ".")]][i] <- model_res$estimate
        res_df[[paste(var_trans, "pval", sep = ".")]][i] <- model_res$p.value
      }
    }
  }
  res_df <- res_df[order(res_df$original.pval, decreasing = FALSE), ]
  for (p_types in c("original", "logarithm", "categorized", "rcs.overall", "rcs.nonlinear")) {
    res_df[[paste(p_types, "padj", sep = ".")]] <- p.adjust(
      res_df[[paste(p_types, "pval", sep = ".")]],
      method = p_adjust_method
    )
  }
  if (save_table) {
    if (is.null(filename)) {
      filename <- paste(analysis_type, y, "regression_scan.csv", sep = "_")
    }
    write.csv(res_df, filename, row.names = FALSE)
  }
  return(res_df)
}
