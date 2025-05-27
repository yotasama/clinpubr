#' Create subgroup forest plot.
#' @description Create subgroup forest plot with `glm` or `coxph` models. The interaction p-values
#'   are calculated using likelihood ratio tests.
#' @param data A data frame.
#' @param subgroup_vars A character vector of variable names to be used as subgroups. It's recommended that
#'   the variables are categorical. If the variables are continuous, they will be cut into groups.
#' @param x A character string of the predictor variable.
#' @param y A character string of the outcome variable.
#' @param time A character string of the time variable. If `NULL`, logistic regression is used.
#'   Otherwise, Cox proportional hazards regression is used.
#' @param covars A character vector of covariate names. If duplicated with `subgroup_vars`, the duplicated
#'   covariates will be temporarily removed during the corresponding subgroup analysis.
#' @param est_precision An integer specifying the precision for the estimates in the plot.
#' @param p_nsmall An integer specifying the number of decimal places for the p-values.
#' @param group_cut_quantiles A vector of numerical values between 0 and 1, specifying the quantile to use
#'   for cutting continuous subgroup variables.
#' @param save_plot A logical value indicating whether to save the plot.
#' @param filename A character string specifying the filename for the plot. If `NULL`, a default filename is used.
#' @param ... Additional arguments passed to the `forestploter::forest` function.
#'
#' @returns A `gtable` object.
#' @export
#' @examples
#' data(cancer, package = "survival")
#' # coxph model with time assigned
#' subgroup_forest(cancer,
#'   subgroup_vars = c("age", "sex", "wt.loss"), x = "ph.ecog", y = "status",
#'   time = "time", covars = "ph.karno", ticks_at = c(1, 2), save_plot = FALSE
#' )
#'
#' # logistic model with time not assigned
#' cancer$dead <- cancer$status == 2
#' subgroup_forest(cancer,
#'   subgroup_vars = c("age", "sex", "wt.loss"), x = "ph.ecog", y = "dead",
#'   covars = "ph.karno", ticks_at = c(1, 2), save_plot = FALSE
#' )
#'
#' cancer$ph.ecog_cat <- factor(cancer$ph.ecog, levels = c(0:3), labels = c("0", "1", "≥2", "≥2"))
#' subgroup_forest(cancer,
#'   subgroup_vars = c("sex", "wt.loss"), x = "ph.ecog_cat", y = "dead",
#'   covars = "ph.karno", ticks_at = c(1, 2), save_plot = FALSE
#' )
subgroup_forest <- function(data, subgroup_vars, x, y, time = NULL, covars = NULL, est_precision = 3, p_nsmall = 3,
                            group_cut_quantiles = 0.5, save_plot = TRUE, filename = NULL, ...) {
  x_type <- ifelse(!is.factor(data[[x]]), "number", "factor")
  if (!is.null(time)) {
    analysis_type <- "cox"
    effect_label <- "HR (95% CI)"
  } else if (length(levels(as.factor(data[[y]]))) == 2) {
    analysis_type <- "logistic"
    effect_label <- "OR (95% CI)"
  } else {
    analysis_type <- "linear"
    effect_label <- "Coefficient (95% CI)"
  }
  ref_val <- ifelse(analysis_type %in% c("cox", "logistic"), 1, 0)
  x_trans <- ifelse(analysis_type %in% c("cox", "logistic"), "log10", "none")
  covars <- remove_conflict(covars, c(y, x, time))
  ori_covs <- covars
  subgroup_vars <- remove_conflict(subgroup_vars, c(y, x, time))
  if (length(subgroup_vars) == 0) stop("No valid `subgroup_vars` specified.")

  indf <- dplyr::select(data, all_of(c(y, x, time, covars)))

  if (!is.null(covars)) {
    covars <- paste0("tmp_cov", seq_along(covars))
    if (any(covars %in% colnames(data))) stop("Colnames start with 'tmp_cov' are reserved.")
    start_col <- ifelse(analysis_type == "cox", 4, 3)
    colnames(indf)[start_col:(start_col + length(covars) - 1)] <- covars
  }

  indf <- cbind(indf, dplyr::select(data, all_of(subgroup_vars)))
  indf <- indf[complete.cases(indf[, c(y, x, time, covars)]), ]
  if (x_type == "factor") {
    n_plot_levels <- length(levels(indf[[x]])) - 1
  } else {
    n_plot_levels <- 1
  }

  for (var in subgroup_vars) {
    indf[[var]] <- to_factor(indf[[var]])
  }

  overall_res <- regression_fit(
    data = indf, y = y, predictor = x, time = time, covars = covars, returned = "predictor_split"
  )
  if (x_type == "number") {
    res <- data.frame(
      Subgroup = "Overall",
      Count = nrow(indf),
      Percent = 100,
      Estimate = overall_res$estimate,
      Lower = overall_res$conf.low,
      Upper = overall_res$conf.high,
      `P value` = overall_res$p.value,
      `P int` = NA,
      check.names = FALSE
    )
  } else {
    res <- rbind(
      data.frame(
        Subgroup = "Overall",
        Count = nrow(indf),
        Percent = 100,
        Level = levels(indf[[x]])[1],
        Estimate = ref_val,
        Lower = ref_val,
        Upper = ref_val,
        `P value` = NA,
        `P int` = NA,
        check.names = FALSE
      ),
      data.frame(
        Subgroup = NA,
        Count = NA,
        Percent = NA,
        Level = levels(indf[[x]])[-1],
        Estimate = overall_res$estimate,
        Lower = overall_res$conf.low,
        Upper = overall_res$conf.high,
        `P value` = overall_res$p.value,
        `P int` = NA,
        check.names = FALSE
      )
    )
  }

  for (var in subgroup_vars) {
    tmp_covs <- covars[ori_covs != var]
    if (length(tmp_covs) == 0) tmp_covs <- NULL

    p_int <- interaction_p_value(indf, y, x, var, time = time, covars = tmp_covs)

    res <- rbind(
      res,
      if (x_type == "number") {
        data.frame(
          Subgroup = var,
          Count = NA,
          Percent = NA,
          Estimate = NA,
          Lower = NA,
          Upper = NA,
          `P value` = NA,
          `P int` = p_int,
          check.names = FALSE
        )
      } else {
        data.frame(
          Subgroup = var,
          Count = NA,
          Percent = NA,
          Level = NA,
          Estimate = NA,
          Lower = NA,
          Upper = NA,
          `P value` = NA,
          `P int` = p_int,
          check.names = FALSE
        )
      }
    )

    lvls <- levels(indf[[var]])
    tmp_res <- NULL
    for (lvl in lvls) {
      subset_data <- indf[which(indf[[var]] == lvl), ]
      lvl_res <- regression_fit(subset_data, y, x, time = time, covars = tmp_covs, returned = "predictor_split")

      if (x_type == "number") {
        lvl_res <- data.frame(
          Subgroup = paste("  ", lvl), Count = nrow(subset_data),
          Percent = NA, Estimate = lvl_res$estimate,
          Lower = lvl_res$conf.low, Upper = lvl_res$conf.high,
          `P value` = lvl_res$p.value, `P int` = NA, check.names = FALSE
        )
      } else {
        lvl_res <- data.frame(
          Subgroup = paste("  ", lvl), Count = nrow(subset_data),
          Percent = NA, Level = levels(indf[[x]])[-1], Estimate = lvl_res$estimate,
          Lower = lvl_res$conf.low, Upper = lvl_res$conf.high,
          `P value` = lvl_res$p.value, `P int` = NA, check.names = FALSE
        )
        lvl_res$Subgroup[-1] <- NA
        lvl_res$Count[-1] <- NA
        lvl_res$Percent[-1] <- NA
      }

      tmp_res <- rbind(tmp_res, lvl_res)
    }

    tmp_res$Percent <- round(tmp_res$Count / sum(tmp_res$Count, na.rm = TRUE) * 100, 1)
    res <- rbind(res, tmp_res)
  }

  for (col in c("P value", "P int")) {
    res[[col]] <- ifelse(is.na(res[[col]]), "",
      format_pval(as.numeric(res[[col]]), nsmall = p_nsmall)
    )
  }

  plot_df <- res
  plot_df[[effect_label]] <- ifelse(is.na(plot_df$Estimate), "",
    paste0(
      formatC(plot_df$Estimate, format = "g", digits = est_precision, flag = "#"),
      " (",
      formatC(plot_df$Lower, format = "g", digits = est_precision, flag = "#"),
      " to ",
      formatC(plot_df$Upper, format = "g", digits = est_precision, flag = "#"),
      ")"
    )
  )
  if (x_type == "factor") {
    plot_df[1, effect_label] <- "Reference"
  }

  na_cols <- if (x_type == "number") {
    c("Subgroup", "Count", "Percent", "P value", "P int")
  } else {
    c("Subgroup", "Count", "Percent", "Level", "P value", "P int")
  }
  plot_df[na_cols][is.na(plot_df[na_cols])] <- " "
  plot_df$` ` <- paste(rep(" ", 20), collapse = " ")

  if (x_type == "number") {
    plot_columns <- c("Subgroup", "Count", "Percent", " ", effect_label, "P value", "P int")
    ci_column <- 4
  } else {
    plot_columns <- c("Subgroup", "Count", "Percent", "Level", " ", effect_label, "P value", "P int")
    ci_column <- 5
  }

  p <- forestploter::forest(
    plot_df[, plot_columns],
    est = plot_df$Estimate,
    lower = plot_df$Lower,
    upper = plot_df$Upper,
    ci_column = ci_column,
    ref_line = ref_val,
    x_trans = x_trans,
    ...
  )
  if (save_plot) {
    if (is.null(filename)) {
      filename <- paste0(paste0(
        c(
          analysis_type, "subgroup_forest", y, "with", x, "by", length(subgroup_vars),
          "subgroups_and", length(covars), "covars"
        ),
        collapse = "_"
      ), ".png")
    }
    p_wh <- forestploter::get_wh(p)
    ggplot2::ggsave(filename, p, width = p_wh[1], height = p_wh[2])
  }
  p
}
