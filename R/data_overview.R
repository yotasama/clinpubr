#' Data Overview and Quality Check
#'
#' This function provides a comprehensive overview of a data.frame, including variable types,
#' summary statistics, and potential data quality issues. It serves as a starting point for
#' data cleaning by identifying problems that need attention.
#'
#' @param df A data.frame to be analyzed
#' @param outlier_method Method for detecting outliers, one of "iqr" (default), "zscore", or "mad"
#' @param outlier_threshold Threshold value for detecting outliers. If NULL (default),
#'   uses method-specific defaults:
#'   - For MAD method: 1.4826 * 3 (approximately 3 standard deviations)
#'   - For IQR method: 1.5 (Tukey's rule)
#'   - For Z-score method: 3 (3 standard deviations)
#' @param verbose If TRUE (default), prints result messages
#' @param sample Maximum number of rows to sample for large datasets (default is 10000). Set to `NULL`
#'   `NA`, or `0` to disable sampling.
#'
#' @return A list containing:
#'   - variable_types: Classification of variables by type
#'   - summary_stats: Summary statistics for each variable
#'   - quality_issues: Identified data quality problems
#'   - recommendations: Suggestions for data cleaning
#'
#' @examples
#' # Basic usage
#' data(mtcars)
#' overview <- data_overview(mtcars)
#' print(overview$variable_types)
#' print(overview$quality_issues)
#'
#' @export
data_overview <- function(df, outlier_method = "iqr", outlier_threshold = NULL,
                          verbose = TRUE, sample = 10000) {
  # Check input
  if (!is.data.frame(df)) {
    stop("Input must be a data.frame")
  }

  # Sample data if larger than sample size
  if (!is.null(sample) && !is.na(sample) && sample > 0 && nrow(df) > sample) {
    df <- df[sample(nrow(df), sample), ]
    if (verbose) {
      message(sprintf("Data sampled to %d rows for performance.", sample))
    }
  }

  # Initialize results list
  result <- list()

  # 1. Variable Type Classification
  var_classes <- vapply(df, function(x) class(x)[1], character(1))

  # Detailed variable types
  is_num <- var_classes %in% c("numeric", "integer")
  is_char <- var_classes %in% c("character", "factor")
  is_log <- var_classes == "logical"
  is_date <- var_classes %in% c("Date", "POSIXct", "POSIXlt")
  variable_types <- list(
    numeric = names(var_classes[is_num]),
    character = names(var_classes[is_char]),
    logical = names(var_classes[is_log]),
    date = names(var_classes[is_date]),
    other = names(var_classes[!(is_num | is_char | is_log | is_date)])
  )

  # Remove empty categories
  variable_types <- variable_types[lengths(variable_types) > 0]

  result$variable_types <- variable_types

  # 2. Summary Statistics - compute all stats in one pass per column
  summary_stats <- list()
  n_total <- nrow(df)

  # Numeric variables summary
  if (length(variable_types$numeric) > 0) {
    num_cols <- variable_types$numeric
    n_num <- length(num_cols)
    ns <- integer(n_num)
    miss <- integer(n_num)
    miss_pct <- numeric(n_num)
    means <- numeric(n_num)
    medians <- numeric(n_num)
    mins <- numeric(n_num)
    maxs <- numeric(n_num)
    sds <- numeric(n_num)
    for (k in seq_along(num_cols)) {
      x <- df[[num_cols[k]]]
      x_valid <- x[!is.na(x)]
      n_valid <- length(x_valid)
      ns[k] <- n_valid
      miss[k] <- n_total - n_valid
      miss_pct[k] <- round(miss[k] / n_total * 100, 2)
      if (n_valid > 0) {
        means[k] <- round(mean(x_valid), 2)
        medians[k] <- round(median(x_valid), 2)
        mins[k] <- round(min(x_valid), 2)
        maxs[k] <- round(max(x_valid), 2)
        sds[k] <- round(sd(x_valid), 2)
      }
    }
    summary_stats$numeric <- data.frame(
      variable = num_cols, n = ns, missing = miss, missing_pct = miss_pct,
      mean = means, median = medians, min = mins, max = maxs, sd = sds,
      row.names = NULL
    )
  }

  # Character/factor variables summary
  if (length(variable_types$character) > 0) {
    char_cols <- variable_types$character
    n_char <- length(char_cols)
    ns <- integer(n_char)
    miss <- integer(n_char)
    miss_pct <- numeric(n_char)
    uniques <- integer(n_char)
    top_freqs <- integer(n_char)
    top_pcts <- numeric(n_char)
    top_vals <- character(n_char)
    for (k in seq_along(char_cols)) {
      x <- df[[char_cols[k]]]
      x_valid <- x[!is.na(x)]
      n_valid <- length(x_valid)
      ns[k] <- n_valid
      miss[k] <- n_total - n_valid
      miss_pct[k] <- round(miss[k] / n_total * 100, 2)
      if (n_valid > 0) {
        tab <- table(x_valid)
        uniques[k] <- length(tab)
        mt <- max(tab)
        top_freqs[k] <- mt
        top_pcts[k] <- round(mt / n_valid * 100, 2)
        top_vals[k] <- names(tab)[which.max(tab)]
      } else {
        top_vals[k] <- NA_character_
      }
    }
    summary_stats$character <- data.frame(
      variable = char_cols, n = ns, missing = miss, missing_pct = miss_pct,
      unique = uniques, top_value = top_vals, top_freq = top_freqs, top_pct = top_pcts,
      row.names = NULL
    )
  }

  # Logical variables summary
  if (length(variable_types$logical) > 0) {
    log_cols <- variable_types$logical
    n_log <- length(log_cols)
    ns <- integer(n_log)
    miss <- integer(n_log)
    miss_pct <- numeric(n_log)
    true_pcts <- numeric(n_log)
    false_pcts <- numeric(n_log)
    for (k in seq_along(log_cols)) {
      x <- df[[log_cols[k]]]
      x_valid <- x[!is.na(x)]
      n_valid <- length(x_valid)
      ns[k] <- n_valid
      miss[k] <- n_total - n_valid
      miss_pct[k] <- round(miss[k] / n_total * 100, 2)
      if (n_valid > 0) {
        tp <- mean(x_valid) * 100
        true_pcts[k] <- round(tp, 2)
        false_pcts[k] <- round(100 - tp, 2)
      }
    }
    summary_stats$logical <- data.frame(
      variable = log_cols, n = ns, missing = miss, missing_pct = miss_pct,
      true_pct = true_pcts, false_pct = false_pcts, row.names = NULL
    )
  }

  result$summary_stats <- summary_stats

  # 3. Quality Issues Detection
  quality_issues <- list()
  recommendations <- list()

  # Check for numeric variables stored as characters
  numeric_as_char <- list()
  char_cols_all <- variable_types$character
  if (length(char_cols_all) > 0) {
    for (var in char_cols_all) {
      if (is.factor(df[[var]])) next
      x <- df[[var]]
      x_no_na <- x[!is.na(x)]
      if (length(x_no_na) == 0) next
      num_conv <- suppressWarnings(as.numeric(x_no_na))
      numeric_count <- sum(!is.na(num_conv))
      total_non_na <- length(x_no_na)
      if (numeric_count > 0) {
        is_num <- !is.na(num_conv)
        numeric_as_char[[var]] <- list(
          total_values = total_non_na,
          convertible_values = numeric_count,
          convertible_pct = round(numeric_count / total_non_na * 100, 2),
          sample_values = head(x_no_na[is_num], 3),
          problematic_values = head(x_no_na[!is_num], 3)
        )
      }
    }
  }

  if (length(numeric_as_char) > 0) {
    quality_issues$numeric_as_character <- numeric_as_char
    recommendations$numeric_conversion <- paste(
      "Consider converting these character variables to numeric:",
      paste(names(numeric_as_char), collapse = ", ")
    )
  }

  # Outlier Detection for Numeric Variables
  outliers <- list()
  if (length(variable_types$numeric) > 0) {
    for (var in variable_types$numeric) {
      x <- df[[var]]
      outlier_result <- detect_outliers(x, method = outlier_method, threshold = outlier_threshold)
      if (outlier_result$outlier_count > 0) {
        x_no_na <- x[!is.na(x)]
        outliers[[var]] <- list(
          method = outlier_method,
          outlier_count = outlier_result$outlier_count,
          outlier_pct = outlier_result$outlier_pct,
          summary = outlier_result$summary,
          sample_outliers = head(x_no_na[outlier_result$outlier_mask[!is.na(x)]], 5)
        )
      }
    }
  }

  if (length(outliers) > 0) {
    quality_issues$outliers <- outliers
    recommendations$outliers <- paste(
      "Review outliers in these numeric variables:",
      paste(names(outliers), collapse = ", ")
    )
  }

  # Missing Values Check - vectorized
  high_missing_threshold <- 50
  any_missing_threshold <- 1
  all_miss_pct <- vapply(df, function(x) round(mean(is.na(x)) * 100, 2), numeric(1))
  missing_vars <- all_miss_pct[all_miss_pct >= any_missing_threshold]

  if (length(missing_vars) > 0) {
    missing_vec <- sort(missing_vars, decreasing = TRUE)
    quality_issues$missing_values <- missing_vec
    high_missing <- names(missing_vec[missing_vec >= high_missing_threshold])
    low_missing <- names(missing_vec[missing_vec < high_missing_threshold])
    if (length(high_missing) > 0) {
      recommendations$high_missing <- paste(
        "Variables with >", high_missing_threshold, "% missing values:",
        paste(high_missing, collapse = ", "),
        "- consider dropping or imputation strategy"
      )
    }
    if (length(low_missing) > 0) {
      recommendations$low_missing <- paste(
        "Variables with <", high_missing_threshold, "% missing values:",
        paste(low_missing, collapse = ", "),
        "- consider imputation"
      )
    }
  }

  # Check for near-zero variance variables - vectorized per column
  near_zero_var <- list()
  for (var in names(df)) {
    x <- df[[var]]
    x_no_na <- x[!is.na(x)]
    if (length(x_no_na) == 0) next
    n_unique <- length(unique(x_no_na))
    if (n_unique == 1) {
      near_zero_var[[var]] <- list(type = "constant", value = x_no_na[1])
    } else if (n_unique == 2 && is.character(x)) {
      tab <- table(x_no_na)
      dominant_pct <- max(tab) / sum(tab) * 100
      if (dominant_pct > 95) {
        near_zero_var[[var]] <- list(
          type = "near_binary",
          dominant_value = names(tab)[which.max(tab)],
          dominant_pct = round(dominant_pct, 2)
        )
      }
    }
  }

  if (length(near_zero_var) > 0) {
    quality_issues$near_zero_variance <- near_zero_var
    recommendations$near_zero_var <- paste(
      "Consider removing constant or near-zero variance variables:",
      paste(names(near_zero_var), collapse = ", ")
    )
  }

  # Check for duplicate rows - use data.table for C-level hashing
  # Base duplicated.data.frame is slow on large data.frames; data.table's
  # implementation uses fast C-level row hashing and is orders of magnitude faster.
  # Note: duplicated.data.table is not exported from data.table's NAMESPACE.
  # Use unique() + setdiff to compute duplicate count instead.
  dt_for_dup <- data.table::as.data.table(df)
  duplicate_rows <- nrow(dt_for_dup) - nrow(unique(dt_for_dup))
  rm(dt_for_dup)
  gc()
  if (duplicate_rows > 0) {
    quality_issues$duplicate_rows <- duplicate_rows
    recommendations$duplicate_rows <- paste("Found", duplicate_rows, "duplicate rows - consider removing them")
  }

  # Check for negative values in predominantly positive numeric variables
  negative_in_positive <- list()
  if (length(variable_types$numeric) > 0) {
    for (var in variable_types$numeric) {
      x <- df[[var]]
      x_no_na <- x[!is.na(x)]
      if (length(x_no_na) == 0) next
      n_positive <- sum(x_no_na >= 0)
      n_negative <- sum(x_no_na < 0)
      n_total_valid <- length(x_no_na)
      positive_pct <- n_positive / n_total_valid * 100
      if (positive_pct > 90 && n_negative > 0) {
        negative_in_positive[[var]] <- list(
          n_negative = n_negative,
          negative_pct = round(n_negative / n_total_valid * 100, 2),
          positive_pct = round(positive_pct, 2),
          sample_negative = head(x_no_na[x_no_na < 0], 3)
        )
      }
    }
  }

  if (length(negative_in_positive) > 0) {
    quality_issues$negative_in_positive <- negative_in_positive
    recommendations$negative_in_positive <- paste(
      "Numeric variables with mostly positive values but containing negatives:",
      paste(names(negative_in_positive), collapse = ", ")
    )
  }

  # Check for completely empty columns (all NA) - vectorized
  all_na_cols <- vapply(df, function(x) all(is.na(x)), logical(1))
  empty_columns <- names(all_na_cols)[all_na_cols]

  if (length(empty_columns) > 0) {
    quality_issues$empty_columns <- empty_columns
    recommendations$empty_columns <- paste(
      "Found completely empty columns:",
      paste(empty_columns, collapse = ", "),
      "- consider removing them"
    )
  }

  # Check for completely empty rows (all NA) - use rowSums instead of apply()
  if (ncol(df) > 0) {
    empty_row_idx <- rowSums(is.na(df)) == ncol(df)
    empty_rows <- which(empty_row_idx)
  } else {
    empty_rows <- integer(0)
  }
  if (length(empty_rows) > 0) {
    quality_issues$empty_rows <- empty_rows
    recommendations$empty_rows <- paste(
      "Found", length(empty_rows), "completely empty rows at positions:",
      paste(head(empty_rows, 10), collapse = ", "),
      ifelse(length(empty_rows) > 10, "...", ""),
      "- consider removing them"
    )
  }

  # Check for suspicious date values (year < 1910 or > current year)
  suspicious_dates <- list()
  if (length(variable_types$date) > 0) {
    current_year <- as.numeric(format(Sys.Date(), "%Y"))
    for (var in variable_types$date) {
      x <- df[[var]]
      x_no_na <- x[!is.na(x)]
      if (length(x_no_na) == 0) next
      years <- as.numeric(format(x_no_na, "%Y"))
      suspicious_idx <- which(years < 1910 | years > current_year)
      if (length(suspicious_idx) > 0) {
        suspicious_dates[[var]] <- list(
          n_suspicious = length(suspicious_idx),
          suspicious_pct = round(length(suspicious_idx) / length(years) * 100, 2),
          sample_values = head(x_no_na[suspicious_idx], 5),
          year_range = c(min(years), max(years))
        )
      }
    }
  }

  if (length(suspicious_dates) > 0) {
    quality_issues$suspicious_dates <- suspicious_dates
    recommendations$suspicious_dates <- paste(
      "Review suspicious dates (year < 1910 or > current year) in:",
      paste(names(suspicious_dates), collapse = ", ")
    )
  }

  # Check for variables with few unique values that should be converted to factor
  low_cardinality_vars <- list()
  n_unique_threshold <- 5
  if (length(variable_types$numeric) > 0) {
    for (var in variable_types$numeric) {
      x <- df[[var]]
      x_no_na <- x[!is.na(x)]
      n_unique <- length(unique(x_no_na))
      if (n_unique <= n_unique_threshold && all(x_no_na %in% c(0, 1))) next
      if (n_unique <= n_unique_threshold) {
        low_cardinality_vars[[var]] <- list(
          type = "numeric",
          n_unique = n_unique,
          unique_values = head(sort(unique(x_no_na)), n_unique_threshold)
        )
      }
    }
  }

  if (length(low_cardinality_vars) > 0) {
    quality_issues$low_cardinality <- low_cardinality_vars
    recommendations$low_cardinality <- paste(
      "Consider converting these low-cardinality variables to factor:",
      paste(names(low_cardinality_vars), collapse = ", ")
    )
  }

  # Check for case inconsistency in character variables
  case_issues <- list()
  if (length(variable_types$character) > 0) {
    for (var in variable_types$character) {
      x <- df[[var]]
      x_no_na <- x[!is.na(x)]
      if (length(x_no_na) == 0) next
      x_lower <- tolower(x_no_na)
      n_original <- length(unique(x_no_na))
      n_lower <- length(unique(x_lower))
      if (n_lower < n_original) {
        dup_values <- unique(x_lower[duplicated(x_lower)])
        examples <- list()
        for (val in head(dup_values, 3)) {
          examples[[val]] <- unique(x_no_na[tolower(x_no_na) == val])
        }
        case_issues[[var]] <- list(
          n_original = n_original,
          n_normalized = n_lower,
          reduction = n_original - n_lower,
          examples = examples
        )
      }
    }
  }

  if (length(case_issues) > 0) {
    quality_issues$case_issues <- case_issues
    recommendations$case_issues <- paste(
      "These character variables have case inconsistency issues:",
      paste(names(case_issues), collapse = ", "),
      "- consider standardizing to lowercase or uppercase"
    )
  }

  result$quality_issues <- quality_issues
  result$recommendations <- recommendations

  # Add overall data summary
  result$overall <- list(
    n_rows = nrow(df),
    n_columns = ncol(df),
    n_numeric_vars = length(variable_types$numeric),
    n_character_vars = length(variable_types$character),
    n_logical_vars = length(variable_types$logical),
    n_date_vars = length(variable_types$date),
    n_other_vars = length(variable_types$other),
    n_issues = length(unlist(quality_issues, recursive = FALSE))
  )

  if (verbose) {
    cat("=== Data Overview Summary ===\n")
    cat(sprintf("Dataset: %d rows, %d columns\n", result$overall$n_rows, result$overall$n_columns))
    cat("\nVariable Types:\n")
    for (type in names(variable_types)) {
      cat(sprintf("  %-10s: %d variables\n", type, length(variable_types[[type]])))
    }

    if (result$overall$n_issues > 0) {
      cat(sprintf("\nFound %d potential quality issues:\n", result$overall$n_issues))
      for (issue_type in names(quality_issues)) {
        cat(sprintf("  %-25s: %d cases\n", issue_type, length(quality_issues[[issue_type]])))
      }
    } else {
      cat("\nNo major quality issues detected.\n")
    }

    if (length(recommendations) > 0) {
      cat("\nRecommendations:\n")
      for (rec in recommendations) {
        cat(sprintf("  - %s\n", rec))
      }
    }
  }

  return(invisible(result))
}
