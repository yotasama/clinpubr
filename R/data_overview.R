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
  var_classes <- sapply(df, class)

  # Detailed variable types
  variable_types <- list(
    numeric = names(var_classes[var_classes %in% c("numeric", "integer")]),
    character = names(var_classes[var_classes %in% c("character", "factor")]),
    logical = names(var_classes[var_classes == "logical"]),
    date = names(var_classes[var_classes %in% c("Date", "POSIXct", "POSIXlt")]),
    other = names(var_classes[!var_classes %in% c("numeric", "integer", "character", "factor", "logical", "Date", "POSIXct", "POSIXlt")])
  )

  # Remove empty categories
  variable_types <- variable_types[sapply(variable_types, length) > 0]

  result$variable_types <- variable_types

  # 2. Summary Statistics
  summary_stats <- list()

  # Numeric variables summary
  if (length(variable_types$numeric) > 0) {
    numeric_summary <- data.frame(
      variable = variable_types$numeric,
      n = sapply(df[, variable_types$numeric], function(x) sum(!is.na(x))),
      missing = sapply(df[, variable_types$numeric], function(x) sum(is.na(x))),
      missing_pct = sapply(df[, variable_types$numeric], function(x) round(mean(is.na(x)) * 100, 2)),
      mean = sapply(df[, variable_types$numeric], function(x) round(mean(x, na.rm = TRUE), 2)),
      median = sapply(df[, variable_types$numeric], function(x) round(median(x, na.rm = TRUE), 2)),
      min = sapply(df[, variable_types$numeric], function(x) round(min(x, na.rm = TRUE), 2)),
      max = sapply(df[, variable_types$numeric], function(x) round(max(x, na.rm = TRUE), 2)),
      sd = sapply(df[, variable_types$numeric], function(x) round(sd(x, na.rm = TRUE), 2))
    )
    summary_stats$numeric <- numeric_summary
  }

  # Character/factor variables summary
  if (length(variable_types$character) > 0) {
    character_summary <- data.frame(
      variable = variable_types$character,
      n = sapply(df[, variable_types$character], function(x) sum(!is.na(x))),
      missing = sapply(df[, variable_types$character], function(x) sum(is.na(x))),
      missing_pct = sapply(df[, variable_types$character], function(x) round(mean(is.na(x)) * 100, 2)),
      unique = sapply(df[, variable_types$character], function(x) length(unique(x[!is.na(x)]))),
      top_value = sapply(df[, variable_types$character], function(x) {
        tab <- table(x[!is.na(x)])
        if (length(tab) > 0) names(tab)[which.max(tab)] else NA
      }),
      top_freq = sapply(df[, variable_types$character], function(x) {
        tab <- table(x[!is.na(x)])
        if (length(tab) > 0) max(tab) else 0
      }),
      top_pct = sapply(df[, variable_types$character], function(x) {
        tab <- table(x[!is.na(x)])
        if (length(tab) > 0) round(max(tab) / sum(tab) * 100, 2) else 0
      })
    )
    summary_stats$character <- character_summary
  }

  # Logical variables summary
  if (length(variable_types$logical) > 0) {
    logical_summary <- data.frame(
      variable = variable_types$logical,
      n = sapply(df[, variable_types$logical], function(x) sum(!is.na(x))),
      missing = sapply(df[, variable_types$logical], function(x) sum(is.na(x))),
      missing_pct = sapply(df[, variable_types$logical], function(x) round(mean(is.na(x)) * 100, 2)),
      true_pct = sapply(df[, variable_types$logical], function(x) round(mean(x, na.rm = TRUE) * 100, 2)),
      false_pct = sapply(df[, variable_types$logical], function(x) round((1 - mean(x, na.rm = TRUE)) * 100, 2))
    )
    summary_stats$logical <- logical_summary
  }

  result$summary_stats <- summary_stats

  # 3. Quality Issues Detection
  quality_issues <- list()
  recommendations <- list()

  # Check for numeric variables stored as characters
  numeric_as_char <- list()
  for (var in variable_types$character) {
    if (is.factor(df[[var]])) {
      # Skip factor variables
      next
    }
    
    # Try to convert to numeric
    x <- df[[var]]
    x_no_na <- x[!is.na(x)]

    # Count how many values can be converted to numeric
    numeric_count <- sum(!is.na(suppressWarnings(as.numeric(x_no_na))))
    total_non_na <- length(x_no_na)

    if (numeric_count > 0) {
      numeric_as_char[[var]] <- list(
        total_values = total_non_na,
        convertible_values = numeric_count,
        convertible_pct = round(numeric_count / total_non_na * 100, 2),
        sample_values = head(x_no_na[!is.na(suppressWarnings(as.numeric(x_no_na)))], 3),
        problematic_values = head(x_no_na[is.na(suppressWarnings(as.numeric(x_no_na)))], 3)
      )
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

      # Call detect_outliers function with unified threshold parameter
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

  # Missing Values Check
  missing_vars <- list()
  high_missing_threshold <- 50 # % threshold for high missing values
  any_missing_threshold <- 1 # % threshold for any missing values

  for (var in names(df)) {
    missing_pct <- round(mean(is.na(df[[var]])) * 100, 2)
    if (missing_pct >= any_missing_threshold) {
      missing_vars[[var]] <- missing_pct
    }
  }

  if (length(missing_vars) > 0) {
    # Convert to named vector and sort by missing percentage
    missing_vec <- unlist(missing_vars)
    missing_vec <- sort(missing_vec, decreasing = TRUE)

    quality_issues$missing_values <- missing_vec

    # Recommendations based on missing percentage
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

  # Check for near-zero variance variables
  near_zero_var <- list()
  for (var in names(df)) {
    x <- df[[var]]
    x_no_na <- x[!is.na(x)]

    if (length(unique(x_no_na)) == 1) {
      # Constant variable
      near_zero_var[[var]] <- list(
        type = "constant",
        value = unique(x_no_na)[1]
      )
    } else if (length(unique(x_no_na)) == 2 && is.character(x)) {
      # Binary character variable
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

  # Check for duplicate rows
  duplicate_rows <- sum(duplicated(df))
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
      n_total <- length(x_no_na)
      positive_pct <- n_positive / n_total * 100

      # If >90% are positive and there are negative values, flag it
      if (positive_pct > 90 && n_negative > 0) {
        negative_in_positive[[var]] <- list(
          n_negative = n_negative,
          negative_pct = round(n_negative / n_total * 100, 2),
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

  # Check for completely empty columns (all NA)
  empty_columns <- character(0)
  for (var in names(df)) {
    if (all(is.na(df[[var]]))) {
      empty_columns <- c(empty_columns, var)
    }
  }

  if (length(empty_columns) > 0) {
    quality_issues$empty_columns <- empty_columns
    recommendations$empty_columns <- paste(
      "Found completely empty columns:",
      paste(empty_columns, collapse = ", "),
      "- consider removing them"
    )
  }

  # Check for completely empty rows (all NA)
  empty_rows <- which(apply(df, 1, function(row) all(is.na(row))))
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
    for (var in variable_types$date) {
      x <- df[[var]]
      x_no_na <- x[!is.na(x)]

      if (length(x_no_na) == 0) next

      current_year <- as.numeric(format(Sys.Date(), "%Y"))
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
  n_unique_threshold <- 5 # Variables with <= 5 unique values

  # Check numeric variables with few unique values
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

  # Check for case inconsistency in factor variables
  case_issues <- list()
  if (length(variable_types$character) > 0) {
    for (var in variable_types$character) {
      x <- df[[var]]
      x_no_na <- x[!is.na(x)]

      # Check if normalizing case would reduce unique values
      x_lower <- tolower(x_no_na)
      n_original <- length(unique(x_no_na))
      n_lower <- length(unique(x_lower))

      if (n_lower < n_original) {
        # Find the problematic values
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
    # Print summary for quick overview
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
