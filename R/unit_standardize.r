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
#'       }
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
#'   It's recommended to use the labeled result from `unit_view()` as the input.
#'
#' @returns A data frame with subject units standardized.
#' @export
#' @examples
#' # Example 1: Using the list as change_rules is more convenient for small datasets.
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
#'
#' # Example 2: Using the labeled result from `unit_view()` as the input
#' # is more robust for large datasets.
#' df <- data.frame(subject = sample(c("a", "b"), 1000, replace = TRUE), value = runif(1000))
#' df$unit <- NA
#' df$unit[df$subject == "a"] <- sample(c("mg/L", "g/l", "g/L"),
#'   sum(df$subject == "a"),
#'   replace = TRUE
#' )
#' df$value[df$subject == "a" & df$unit == "mg/L"] <-
#'   df$value[df$subject == "a" & df$unit == "mg/L"] * 1000
#' df$unit[df$subject == "b"] <- sample(c(NA, "m.g", "mg"), sum(df$subject == "b"),
#'   prob = c(0.3, 0.05, 0.65), replace = TRUE
#' )
#' df$value[df$subject == "b" & df$unit %in% "mg"] <-
#'   df$value[df$subject == "b" & df$unit %in% "mg"] * 1000
#' df$value[df$subject == "b" & is.na(df$unit)] <- df$value[df$subject == "b" & is.na(df$unit)] *
#'   sample(c(1, 1000), size = sum(df$subject == "b" & is.na(df$unit)), replace = TRUE)
#'
#' unit_table <- unit_view(
#'   df = df, subject_col = "subject",
#'   value_col = "value", unit_col = "unit", save_table = FALSE
#' )
#' unit_table$label <- c("t", NA, 1e-3, NA, NA, "r") # labeling the units
#'
#' df_standardized <- unit_standardize(
#'   df = df, subject_col = "subject", value_col = "value",
#'   unit_col = "unit", change_rules = unit_table
#' )
#' unit_view(
#'   df = df_standardized, subject_col = "subject", value_col = "value", unit_col = "unit",
#'   save_table = FALSE, conflicts_only = FALSE
#' )
unit_standardize <- function(df, subject_col, value_col, unit_col, change_rules) {
  if (is.data.frame(change_rules)) {
    change_rules <- change_rules[!is.na(change_rules$label), ]
    subjects <- unique(df[[subject_col]])
    change_rules <- lapply(seq_along(subjects), function(i) {
      tmp <- dplyr::filter(change_rules, subject == subjects[i])
      if (anyDuplicated(tmp$unit) > 0) {
        stop(paste0("`change_rules` has duplicated units in subject: ", subjects[i]))
      }
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
  } else if (is.list(change_rules)) {
    change_rules <- lapply(change_rules, function(l) {
      if (anyDuplicated(c(l$units2change, l$units2remove, l$target_unit)) > 0) {
        stop(paste0("`change_rules` has duplicated unit roles in subject: ", l$subject))
      }
      l$units2change <- c(l$units2change, l$units2remove)
      l$coeffs <- c(l$coeffs, rep(NA, length(l$units2remove)))
      l$units2remove <- NULL
      return(l)
    })
  } else {
    stop("`change_rules` must be a `data.frame` or a `list`")
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
  if (length(coeffs) > 0 && length(units2change) != length(coeffs)) {
    stop("`coeffs` should have the same length as `units2change`!")
  }
  if (length(target_unit) == 0) {
    if (length(unique(df[, 2])) > 1) {
      target_unit <- first_mode(df[!df[, 2] %in% units2change, 2], empty_return = NULL)
      if (is.null(target_unit)) {
        stop("`target_unit` is not specified and cannot be inferred from the data!")
      }
    } else {
      return(df)
    }
  }
  if (length(target_unit) > 1) {
    stop("too many targets!")
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
#' @param conflicts_only A logical value indicating whether to only show the conflicting units.
#'
#' @returns A data frame of conflicting units.
#' @export
#' @examples
#' df <- data.frame(subject = sample(c("a", "b"), 1000, replace = TRUE), value = runif(1000))
#' df$unit <- NA
#' df$unit[df$subject == "a"] <- sample(c("mg/L", "g/l", "g/L"),
#'   sum(df$subject == "a"),
#'   replace = TRUE
#' )
#' df$value[df$subject == "a" & df$unit == "mg/L"] <-
#'   df$value[df$subject == "a" & df$unit == "mg/L"] * 1000
#' df$unit[df$subject == "b"] <- sample(c(NA, "g", "mg"), sum(df$subject == "b"), replace = TRUE)
#' df$value[df$subject == "b" & df$unit %in% "mg"] <-
#'   df$value[df$subject == "b" & df$unit %in% "mg"] * 1000
#' df$value[df$subject == "b" & is.na(df$unit)] <- df$value[df$subject == "b" & is.na(df$unit)] *
#'   sample(c(1, 1000), size = sum(df$subject == "b" & is.na(df$unit)), replace = TRUE)
#' unit_view(
#'   df = df, subject_col = "subject",
#'   value_col = "value", unit_col = "unit", save_table = FALSE
#' )
unit_view <- function(df, subject_col, value_col, unit_col, quantiles = c(0.025, 0.975), save_table = TRUE,
                      filename = NULL, conflicts_only = TRUE) {
  res <- df %>%
    group_by(!!as.symbol(subject_col), !!as.symbol(unit_col)) %>%
    summarise(
      label = NA,
      count = n(),
      nvalid = sum(!is.na(!!as.symbol(value_col))),
      mean = mean(!!as.symbol(value_col), na.rm = TRUE),
      sd = sd(!!as.symbol(value_col), na.rm = TRUE),
      median = median(!!as.symbol(value_col), na.rm = TRUE),
      across(
        .cols = !!as.symbol(value_col),
        .fns = list(!!!setNames(lapply(quantiles, function(q) {
          function(x) quantile(x, probs = q, na.rm = TRUE)
        }), paste0("q_", quantiles)[seq_along(quantiles)])),
        .names = "{fn}"
      )
    )
  if (conflicts_only) {
    res <- res %>%
      group_by(!!as.symbol(subject_col)) %>%
      dplyr::filter(n() > 1)
  }
  res <- as.data.frame(res)
  if (save_table) {
    if (is.null(filename)) {
      filename <- "unit_view.csv"
    }
    write.csv(res, file = filename, na = "")
  }
  return(res)
}
