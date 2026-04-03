#' Standardize units of numeric data.
#' @description Standardize units of numeric data, especially for data of medical records with different units.
#' @param df A data frame of medical records that contains test subject, value, and unit cols.
#' @param subject_col The name of the subject column.
#' @param value_col The name of the value column.
#' @param unit_col The name of the unit column.
#' @param change_rules A data frame or a list of lists. See details
#' @param extract_numbers A logical value indicating whether to apply `extract_num` to extract numeric values
#'   from the value column.
#' @param verbose A logical value indicating whether to print progress messages.
#' @details `change_rules` can accept two formats:
#'   If a data frame, it must contain the following columns:
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
unit_standardize <- function(df, subject_col, value_col, unit_col, change_rules,
                             extract_numbers = FALSE, verbose = FALSE) {
  .validate_inputs(df, subject_col, value_col, unit_col)

  # Convert value_col to numeric if needed
  if (!is.numeric(df[[value_col]])) {
    df[[value_col]] <- suppressWarnings(as.numeric(df[[value_col]]))
  }

  change_rules <- .parse_change_rules(change_rules, df, subject_col, verbose)

  if (verbose) {
    message(sprintf("Processing %d subjects with %d rows...", length(change_rules), nrow(df)))
  }

  result <- .unit_standardize_optimized(df, subject_col, value_col, unit_col,
                                         change_rules, extract_numbers, verbose)

  return(result)
}

.validate_inputs <- function(df, subject_col, value_col, unit_col) {
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.", call. = FALSE)
  }

  required_cols <- c(subject_col, value_col, unit_col)
  missing_cols <- setdiff(required_cols, colnames(df))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing columns in `df`: %s", paste(missing_cols, collapse = ", ")), call. = FALSE)
  }

  if (!is.numeric(df[[value_col]])) {
    warning(sprintf("`%s` is not numeric. It will be converted to numeric.", value_col))
  }
}

.parse_change_rules <- function(change_rules, df, subject_col, verbose) {
  if (is.data.frame(change_rules)) {
    change_rules <- .parse_df_rules(change_rules, df, subject_col, verbose)
  } else if (is.list(change_rules)) {
    change_rules <- .parse_list_rules(change_rules)
  } else {
    stop("`change_rules` must be a `data.frame` or a `list`", call. = FALSE)
  }
  return(change_rules)
}

.parse_df_rules <- function(change_rules, df, subject_col, verbose) {
  if (!all(c("subject", "unit", "label") %in% colnames(change_rules))) {
    stop("Data frame `change_rules` must contain columns: 'subject', 'unit', 'label'", call. = FALSE)
  }

  change_rules <- change_rules[!is.na(change_rules$label), ]

  if (nrow(change_rules) == 0) {
    warning("No valid rules found in change_rules (all labels are NA). Returning original data.")
    return(list())
  }

  subjects <- unique(df[[subject_col]])

  result <- lapply(seq_along(subjects), function(i) {
    subject <- subjects[i]
    tmp <- change_rules[change_rules$subject == subject, ]

    if (nrow(tmp) == 0) {
      return(NULL)
    }

    if (anyDuplicated(tmp$unit) > 0) {
      stop(sprintf("`change_rules` has duplicated units in subject: %s", subject), call. = FALSE)
    }

    target_unit <- tmp$unit[tmp$label == "t"]
    if (length(target_unit) == 0) {
      target_unit <- NULL
    } else if (length(target_unit) > 1) {
      stop(sprintf("Multiple target units specified for subject '%s'. Only one 't' label allowed.", subject), call. = FALSE)
    }

    coeffs <- suppressWarnings(as.numeric(tmp$label))
    units_to_convert <- tmp$unit[!is.na(coeffs)]
    coeffs_valid <- coeffs[!is.na(coeffs)]

    units_to_remove <- tmp$unit[tmp$label == "r"]

    list(
      subject = subject,
      target_unit = if (length(target_unit) > 0) target_unit else NULL,
      units2change = c(units_to_convert, units_to_remove),
      coeffs = c(coeffs_valid, rep(NA_real_, length(units_to_remove)))
    )
  })

  result <- result[!vapply(result, is.null, logical(1))]

  return(result)
}

.parse_list_rules <- function(change_rules) {
  lapply(change_rules, function(l) {
    if (!"subject" %in% names(l)) {
      stop("Each rule in list format must have a 'subject' field.", call. = FALSE)
    }

    if (!is.null(l$target_unit) && length(l$target_unit) > 1) {
      stop("too many targets!", call. = FALSE)
    }

    units_len <- length(l$units2change %||% character(0))
    coeffs_len <- length(l$coeffs %||% numeric(0))
    if (units_len > 0 && coeffs_len > 0 && units_len != coeffs_len) {
      stop("`coeffs` should have the same length as `units2change`!", call. = FALSE)
    }

    all_units <- c(l$units2change, l$units2remove, l$target_unit)
    all_units <- all_units[!is.na(all_units)]

    if (anyDuplicated(all_units) > 0) {
      stop(sprintf("`change_rules` has duplicated unit roles in subject: %s", l$subject), call. = FALSE)
    }

    list(
      subject = l$subject,
      target_unit = l$target_unit %||% NULL,
      units2change = c(l$units2change %||% character(0), l$units2remove %||% character(0)),
      coeffs = c(l$coeffs %||% numeric(0), rep(NA_real_, length(l$units2remove %||% character(0))))
    )
  })
}

.unit_standardize_optimized <- function(df, subject_col, value_col, unit_col,
                                         change_rules, extract_numbers, verbose) {
  if (length(change_rules) == 0) {
    return(df)
  }

  result <- df
  values <- result[[value_col]]
  units <- result[[unit_col]]
  subjects <- result[[subject_col]]

  rule_lookup <- list()
  target_units <- character(length(change_rules))

  for (i in seq_along(change_rules)) {
    rule <- change_rules[[i]]
    rule_lookup[[rule$subject]] <- list(
      units = rule$units2change,
      coeffs = rule$coeffs,
      target = rule$target_unit
    )
    target_units[i] <- rule$subject
  }

  for (i in seq_along(change_rules)) {
    rule <- change_rules[[i]]
    subject <- rule$subject

    if (verbose && i %% 100 == 1) {
      message(sprintf("Processing subject %d/%d: %s", i, length(change_rules), subject))
    }

    subject_idx <- subjects == subject
    if (!any(subject_idx)) next

    subject_units <- units[subject_idx]
    subject_values <- values[subject_idx]

    target_unit <- rule$target_unit
    if (is.null(target_unit)) {
      unique_units <- unique(subject_units)
      if (length(unique_units) <= 1) next

      remaining_units <- setdiff(unique_units, rule$units2change)
      if (length(remaining_units) == 0) {
        stop("`target_unit` is not specified and cannot be inferred from the data!", call. = FALSE)
      }
      target_unit <- first_mode(subject_units[subject_units %in% remaining_units], empty_return = NULL)
      if (is.null(target_unit)) {
        stop("`target_unit` is not specified and cannot be inferred from the data!", call. = FALSE)
      }
    }

    for (j in seq_along(rule$units2change)) {
      unit_to_change <- rule$units2change[j]
      coeff <- rule$coeffs[j]

      if (is.na(unit_to_change)) {
        unit_idx <- is.na(subject_units)
      } else {
        unit_idx <- subject_units == unit_to_change
      }

      if (length(unit_idx) == 0 || all(!unit_idx, na.rm = TRUE)) next

      if (is.na(coeff)) {
        subject_values[unit_idx] <- NA_real_
      } else {
        if (extract_numbers) {
          subject_values[unit_idx] <- extract_num(subject_values[unit_idx]) * coeff
        } else {
          subject_values[unit_idx] <- subject_values[unit_idx] * coeff
        }
      }
    }

    values[subject_idx] <- subject_values
    units[subject_idx] <- target_unit
  }

  result[[value_col]] <- values
  result[[unit_col]] <- units

  return(result)
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
#' @param verbose A logical value indicating whether to print progress messages.
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
unit_view <- function(df, subject_col, value_col, unit_col, quantiles = c(0.025, 0.975), save_table = FALSE,
                      filename = NULL, conflicts_only = TRUE, verbose = FALSE) {
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.", call. = FALSE)
  }

  required_cols <- c(subject_col, value_col, unit_col)
  missing_cols <- setdiff(required_cols, colnames(df))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing columns in `df`: %s", paste(missing_cols, collapse = ", ")), call. = FALSE)
  }

  if (!is.numeric(df[[value_col]])) {
    warning(sprintf("Ignoring non-numeric values in `%s`!", value_col))
    suppressWarnings(df[[value_col]] <- as.numeric(df[[value_col]]))
  }

  if (verbose) {
    message(sprintf("Processing %d rows...", nrow(df)))
  }

  res <- .unit_view_impl(df, subject_col, value_col, unit_col, quantiles, verbose)

  if (conflicts_only && nrow(res) > 0) {
    res <- res %>%
      group_by(subject) %>%
      dplyr::filter(n() > 1) %>%
      ungroup()
  }

  res <- as.data.frame(res)

  if (save_table) {
    if (is.null(filename)) {
      filename <- "unit_view.csv"
    }
    write.csv(res, file = filename, row.names = FALSE, na = "")
    if (verbose) {
      message(sprintf("Table saved to %s", filename))
    }
  }

  return(res)
}

.unit_view_impl <- function(df, subject_col, value_col, unit_col, quantiles, verbose) {
  q_funs <- lapply(quantiles, function(q) {
    function(x) quantile(x, probs = q, na.rm = TRUE)
  })
  q_names <- paste0("q_", quantiles)

  result <- df %>%
    group_by(!!as.symbol(subject_col), !!as.symbol(unit_col)) %>%
    summarise(
      label = NA_character_,
      count = n(),
      nvalid = sum(!is.na(!!as.symbol(value_col))),
      mean = mean(!!as.symbol(value_col), na.rm = TRUE),
      sd = sd(!!as.symbol(value_col), na.rm = TRUE),
      median = median(!!as.symbol(value_col), na.rm = TRUE),
      .groups = "drop"
    )

  for (i in seq_along(quantiles)) {
    q_col <- q_names[i]
    q_fun <- q_funs[[i]]
    q_vals <- df %>%
      group_by(!!as.symbol(subject_col), !!as.symbol(unit_col)) %>%
      summarise(q_val = q_fun(!!as.symbol(value_col)), .groups = "drop")
    result[[q_col]] <- q_vals$q_val
  }

  result %>%
    rename(
      subject = !!as.symbol(subject_col),
      unit = !!as.symbol(unit_col)
    )
}

`%||%` <- function(x, y) if (is.null(x)) y else x
