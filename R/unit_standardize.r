#' Standardize units of numeric data.
#' @description Standardize units of numeric data, especially for data of medical records with different units.
#' @param df A data frame of medical records that contains test subject, value, and unit cols.
#' @param subject_col The name of the subject column.
#' @param value_col The name of the value column.
#' @param unit_col The name of the unit column.
#' @param change_rules A data frame or a list of lists. If a data frame, it must contain the following columns:
#'   \itemize{
#'     \item `subject`: The subject to be standardized.
#'     \item `unit`: The units of the subject.
#'     \item `label`: The role of the unit, the rule is as follows:
#'       \itemize{
#'         \item `"t"`: the target unit to be standardized to. If not specified,
#'           the function will use the most common unit in the data (retrieved by `first_mode()`).
#'         \item `"r"`: The units to be removed, and the corresponding values be set to `NA`.
#'           Set this when data with this unit cannot be used.
#'         \item A number: Set the multiplier of this unit, the standardized value will be `value * multiplier`.
#'           And `NA` and `""` is considered the same as `1`.
#'   }
#'   If a list of lists, each list contains the following elements:
#'   \itemize{
#'     \item `subject`: The subject to be standardized.
#'     \item `target_unit`: The target unit to be standardized to. If not specified,
#'       the function will use the most common unit in the data (retrieved by `first_mode()`).
#'     \item `units2change`: The units to be changed. If not specified, the function will use
#'       all units except the target unit. Must be specified to apply different `coeffs`.
#'     \item `coeffs`: The coefficients to be used for the conversion. If not specified, the
#'       function will use 1 for all units to be changed.
#'     \item `units2remove`: The units to be removed, and the corresponding values be set to `NA`.
#'           Set this when data with this unit cannot be used.
#'   }
#'
#' @returns A data frame with subject units standardized.
#' @export
#' @examples
#' df <- data.frame(
#'   subject = c("a", "a", "b", "b", "b", "c", "c"), value = c(1, 2, 3, 4, 5, 6, 7),
#'   unit = c(NA, "x", "x", "x", "y", "a", "b")
#' )
#' change_rules <- list(
#'   list(subject = "a", target_unit = "x", units2change = c(NA), coeffs = c(20)),
#'   list(subject = "b"),
#'   list(subject = "c", target_unit = "b")
#' )
#' unit_standardize(df,
#'   subject_col = "subject", value_col = "value", unit_col = "unit",
#'   change_rules = change_rules
#' )
unit_standardize <- function(df, subject_col, value_col, unit_col, change_rules) {
  if (is.data.frame(change_rules)) {
    change_rules <- change_rules[!is.na(change_rules$label), ]
    subjects <- unique(df[[subject_col]])
    change_rules <- lapply(seq_along(subjects), function(i) {
      tmp <- filter(change_rules, subject == subjects[i])
      target_unit <- tmp$unit[which(tmp$label %in% "t")]
      coeffs <- suppressWarnings(as.numeric(tmp$label))
      units2change <- c(tmp$unit[!is.na(coeffs)], tmp$unit[which(tmp$label %in% "r")])
      coeffs <- c(coeffs[!is.na(coeffs)], rep(NA, sum(tmp$label %in% "r")))
      list(
        subject = subjects[i],
        target_unit = target_unit,
        units2change = units2change,
        coeffs = coeffs
      )
    })
  }
  for (i in seq_along(change_rules)) {
    flag <- df[, subject_col] %in% change_rules[[i]]$subject
    df[flag, c(value_col, unit_col)] <- .unit_standardize(df[flag, c(value_col, unit_col)],
      target_unit = change_rules[[i]]$target_unit,
      units2change = change_rules[[i]]$units2change,
      coeffs = change_rules[[i]]$coeffs
    )
  }
  return(df)
}

.unit_standardize <- function(df, target_unit = NULL, units2change = NULL, coeffs = NULL) {
  if (length(target_unit) == 0) {
    if (length(unique(df[, 2])) > 1) {
      target_unit <- first_mode(df[, 2])
    } else {
      return(df)
    }
  }
  if (length(target_unit) > 1) {
    stop("too many targets!")
  }
  if (length(coeffs) > 0 && length(units2change) != length(coeffs)) {
    stop("`coeffs` should have the same length as `units2change`!")
  }
  for (i in seq_along(units2change)) {
    flag <- df[, 2] %in% units2change[i]
    df[flag, 1] <- as.numeric(df[flag, 1]) * coeffs[i]
  }
  df[, 2] <- target_unit
  return(df)
}

#' Generate a table of conflicting units.
#' @description Get a table of conflicting units for the clinical data, along with the some useful information,
#'   this table could be labeled and used for unit standardization.
#' @param df A data frame of medical records that contains test subject, value, and unit cols.
#' @param subject_col The name of the subject column.
#' @param value_col The name of the value column.
#' @param unit_col The name of the unit column.
#' @param quantiles A vector of quantiles to be shown in the table.
#' @param save_table A logical value indicating whether to save the table to a csv file.
#' @param filename The name of the csv file to be saved.
#'
#' @returns A data frame of conflicting units.
#' @export
#' @examples
#' x <- data.frame(subject = sample(c("a", "b"), 1000, replace = T), value = runif(1000))
#' x$unit <- NA
#' x$unit[x$subject == "a"] <- sample(c("mg/L", "g/l", "g/L"), sum(x$subject == "a"), replace = T)
#' x$value[x$subject == "a" & x$unit == "mg/L"] <- x$value[x$subject == "a" & x$unit == "mg/L"] * 1000
#' x$unit[x$subject == "b"] <- sample(c(NA, "g", "mg"), sum(x$subject == "b"), replace = T)
#' x$value[x$subject == "b" & x$unit %in% "mg"] <- x$value[x$subject == "b" & x$unit %in% "mg"] * 1000
#' x$value[x$subject == "b" & is.na(x$unit)] <- x$value[x$subject == "b" & is.na(x$unit)] *
#'   sample(c(1, 1000), size = sum(x$subject == "b" & is.na(x$unit)), replace = T)
unit_view <- function(df, subject_col, value_col, unit_col, quantiles = c(0.025, 0.975), save_table = TRUE,
                      filename = NULL) {
  res <- df %>%
    group_by(!!as.symbol(subject_col), !!as.symbol(unit_col)) %>%
    reframe(
      label = NA, count = n(), nvalid = sum(!is.na(!!as.symbol(value_col))),
      mean = mean(!!as.symbol(value_col), na.rm = TRUE), sd = sd(!!as.symbol(value_col), na.rm = TRUE),
      median = median(!!as.symbol(value_col), na.rm = TRUE),
      data.frame(quant_val = quantile(!!as.symbol(value_col), quantiles, na.rm = TRUE), quant = quantiles)
    ) %>%
    pivot_wider(names_from = quant, values_from = quant_val, names_prefix = "q_") %>%
    group_by(!!as.symbol(subject_col)) %>%
    filter(n() > 1) %>%
    as.data.frame()
  if (save_table) {
    if (is.null(filename)) {
      filename <- "unit_view.csv"
    }
    write.csv(res, file = filename, na = "")
    invisible(res)
  } else {
    return(res)
  }
}
