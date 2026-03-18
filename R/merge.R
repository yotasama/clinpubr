#' Merge Data Frame by String Key Matching
#'
#' @description This function merges two data frames based on string key matching.
#' It searches for keys from `key_df[[key_col]]` in `data[[search_col]]`
#' and adds corresponding columns from `key_df` to `data`.
#'
#' @param data The primary data frame to be enhanced with additional columns
#' @param key_df A data frame containing string keys and their corresponding values
#' @param search_col Column name in `data` to search for keys (default: "name")
#' @param key_col Column name in `key_df` containing keys to match (default: "key")
#' @param value_cols Column name(s) in `key_df` to add to `data` (default: "value")
#'   Can be a single column name or a character vector of column names
#'
#' @return A data frame with all columns from `data` plus matched columns from `key_df`.
#'   Unmatched rows will have NA values in the added columns.
#'
#' @examples
#' # Basic usage
#' main_data <- data.frame(
#'   name = c("AB", "B,C", "A..", "ACD"),
#'   value = c(1, 2, 3, 4),
#'   stringsAsFactors = FALSE
#' )
#' key_lookup <- data.frame(
#'   key = c("A", "B", "C", "ACD", "AB"),
#'   category = c("cat1", "cat2", "cat3", "cat4", "cat1"),
#'   code = c("001", "002", "003", "004", "001"),
#'   stringsAsFactors = FALSE
#' )
#' result <- merge_by_substring(main_data, key_lookup,
#'   search_col = "name",
#'   key_col = "key", value_cols = c("category", "code")
#' )
#' print(result)
#'
#' @export
merge_by_substring <- function(data, key_df, search_col, key_col, value_cols) {
  # Validate inputs
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame")
  }
  if (!is.data.frame(key_df)) {
    stop("`key_df` must be a data frame")
  }
  if (!search_col %in% names(data)) {
    stop(sprintf("Column '%s' not found in `data`", search_col))
  }
  if (!key_col %in% names(key_df)) {
    stop(sprintf("Column '%s' not found in `key_df`", key_col))
  }

  # Ensure value_cols is a character vector
  if (length(value_cols) == 1) {
    value_cols <- as.character(value_cols)
  }
  if (!all(value_cols %in% names(key_df))) {
    stop("All `value_cols` must exist in `key_df`")
  }

  # Extract unique key values
  search_values <- unique(data[[search_col]])

  # Group key_df by value_cols and extract unique keys for each group
  groups <- key_df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(value_cols))) %>%
    dplyr::summarize(
      key_list = list(unique(na.omit(.data[[key_col]]))),
      .groups = "drop"
    ) %>%
    dplyr::filter(lengths(key_list) > 0)

  if (nrow(groups) == 0) {
    return(data)
  }

  # Process each group
  match_list <- vector("list", nrow(groups))
  k <- 0

  for (i in seq_len(nrow(groups))) {
    keys <- groups$key_list[[i]]
    keys <- keys[!is.na(keys)]

    # Escape special regex characters and build pattern
    keys_escaped <- Hmisc::escapeRegex(as.character(keys))
    regex_pattern <- paste0("(?:", paste(keys_escaped, collapse = "|"), ")")

    # Find matches
    matches <- stringi::stri_detect_regex(search_values, regex_pattern)
    matches[is.na(matches)] <- FALSE

    if (any(matches)) {
      matched_keys <- search_values[matches]

      if (length(matched_keys) > 0) {
        group_info <- groups[i, value_cols, drop = FALSE] %>%
          as.data.frame(stringsAsFactors = FALSE)

        k <- k + 1
        match_list[[k]] <- cbind(
          data.frame(setNames(list(matched_keys), search_col)),
          group_info[rep(1, length(matched_keys)), , drop = FALSE],
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (k == 0) {
    return(data)
  }

  # Combine results and merge with original data
  match_results <- dplyr::bind_rows(match_list[seq_len(k)])
  rownames(match_results) <- NULL

  result <- merge(data, match_results, by = search_col, all.x = TRUE)
  return(result)
}

.coerce_join_value <- function(x, arg_name) {
  if (inherits(x, "Date") || inherits(x, "POSIXt")) {
    return(as.numeric(x))
  }

  if (is.numeric(x) || is.integer(x)) {
    return(as.numeric(x))
  }

  if (is.factor(x)) {
    x <- as.character(x)
  }

  if (is.character(x)) {
    converted <- as.Date(x)
    invalid <- !is.na(x) & is.na(converted)

    if (any(invalid)) {
      stop(sprintf(
        "`%s` must be Date, POSIXt, numeric, or character values coercible to Date",
        arg_name
      ))
    }

    return(as.numeric(converted))
  }

  stop(sprintf(
    "`%s` must be Date, POSIXt, numeric, or character values coercible to Date",
    arg_name
  ))
}

.build_merge_group_key <- function(df, by) {
  if (length(by) == 0) {
    return(rep(".all_rows", nrow(df)))
  }

  key_df <- df[, by, drop = FALSE]
  has_na <- Reduce(`|`, lapply(key_df, is.na))
  group_key <- rep(NA_character_, nrow(df))

  if (any(!has_na)) {
    key_values <- lapply(key_df[!has_na, , drop = FALSE], as.character)
    group_key[!has_na] <- do.call(paste, c(key_values, sep = "\r"))
  }

  group_key
}

.na_like_df <- function(df, n, names_override = names(df)) {
  if (n == 0) {
    out <- df[FALSE, , drop = FALSE]
    names(out) <- names_override
    return(out)
  }

  out <- lapply(df, function(column) rep(column[NA_integer_], length.out = n))
  out <- as.data.frame(out, optional = TRUE, stringsAsFactors = FALSE)
  names(out) <- names_override
  rownames(out) <- NULL
  out
}

.match_range_group_vector <- function(start,
                                      end,
                                      val,
                                      include_start = TRUE,
                                      include_end = TRUE) {
  matched_x <- vector("list", length(val))
  matched_y <- vector("list", length(val))
  match_count <- 0L

  for (val_index in seq_along(val)) {
    lower_ok <- if (include_start) start <= val[val_index] else start < val[val_index]
    upper_ok <- if (include_end) val[val_index] <= end else val[val_index] < end
    keep <- lower_ok & upper_ok

    if (any(keep)) {
      match_count <- match_count + 1L
      matched_x[[match_count]] <- which(keep)
      matched_y[[match_count]] <- rep.int(val_index, sum(keep))
    }
  }

  if (match_count == 0L) {
    return(list(x = integer(0), y = integer(0)))
  }

  list(
    x = unlist(matched_x[seq_len(match_count)], use.names = FALSE),
    y = unlist(matched_y[seq_len(match_count)], use.names = FALSE)
  )
}

.match_range_group_sweep <- function(start,
                                     end,
                                     val,
                                     include_start = TRUE,
                                     include_end = TRUE) {
  n_interval <- length(start)
  n_val <- length(val)

  if (n_interval == 0 || n_val == 0) {
    return(list(x = integer(0), y = integer(0)))
  }

  start_order <- order(start, end)
  end_order <- order(end, start)
  val_order <- order(val)

  active <- rep(FALSE, n_interval)
  start_pointer <- 1L
  end_pointer <- 1L
  matched_x <- vector("list", n_val)
  matched_y <- vector("list", n_val)
  match_count <- 0L

  for (val_index in val_order) {
    val_value <- val[val_index]

    while (start_pointer <= n_interval) {
      start_value <- start[start_order[start_pointer]]
      start_ok <- if (include_start) start_value <= val_value else start_value < val_value

      if (!start_ok) {
        break
      }

      active[start_order[start_pointer]] <- TRUE
      start_pointer <- start_pointer + 1L
    }

    while (end_pointer <= n_interval) {
      end_value <- end[end_order[end_pointer]]
      end_expired <- if (include_end) end_value < val_value else end_value <= val_value

      if (!end_expired) {
        break
      }

      active[end_order[end_pointer]] <- FALSE
      end_pointer <- end_pointer + 1L
    }

    active_index <- which(active)

    if (length(active_index) > 0) {
      match_count <- match_count + 1L
      matched_x[[match_count]] <- active_index
      matched_y[[match_count]] <- rep(val_index, length(active_index))
    }
  }

  if (match_count == 0L) {
    return(list(x = integer(0), y = integer(0)))
  }

  list(
    x = unlist(matched_x[seq_len(match_count)], use.names = FALSE),
    y = unlist(matched_y[seq_len(match_count)], use.names = FALSE)
  )
}

.match_range_group <- function(start,
                               end,
                               val,
                               include_start = TRUE,
                               include_end = TRUE) {
  pair_count <- length(start) * length(val)

  if (pair_count <= 4096L || length(start) <= 32L || length(val) <= 32L) {
    return(.match_range_group_vector(
      start = start,
      end = end,
      val = val,
      include_start = include_start,
      include_end = include_end
    ))
  }

  .match_range_group_sweep(
    start = start,
    end = end,
    val = val,
    include_start = include_start,
    include_end = include_end
  )
}

.merge_by_range_base <- function(x,
                                 y,
                                 by_x,
                                 by_y,
                                 x_start_value,
                                 x_end_value,
                                 x_start_relaxed,
                                 x_end_relaxed,
                                 y_val_value) {
  x_group <- .build_merge_group_key(x, by_x)
  y_group <- .build_merge_group_key(y, by_y)

  x_groups <- split(seq_len(nrow(x))[!is.na(x_group)], x_group[!is.na(x_group)], drop = TRUE)
  y_groups <- split(seq_len(nrow(y))[!is.na(y_group)], y_group[!is.na(y_group)], drop = TRUE)
  common_groups <- intersect(names(x_groups), names(y_groups))

  matched_x <- list()
  matched_y <- list()
  match_count <- 0L

  for (group_name in common_groups) {
    x_idx <- x_groups[[group_name]]
    y_idx <- y_groups[[group_name]]

    x_idx <- x_idx[!is.na(x_start_relaxed[x_idx]) & !is.na(x_end_relaxed[x_idx])]
    y_idx <- y_idx[!is.na(y_val_value[y_idx])]

    if (length(x_idx) == 0 || length(y_idx) == 0) {
      next
    }

    group_matches <- .match_range_group(
      start = x_start_relaxed[x_idx],
      end = x_end_relaxed[x_idx],
      val = y_val_value[y_idx]
    )

    if (length(group_matches$x) > 0) {
      match_count <- match_count + 1L
      matched_x[[match_count]] <- x_idx[group_matches$x]
      matched_y[[match_count]] <- y_idx[group_matches$y]
    }
  }

  if (match_count == 0L) {
    return(list(x = integer(0), y = integer(0)))
  }

  list(
    x = unlist(matched_x[seq_len(match_count)], use.names = FALSE),
    y = unlist(matched_y[seq_len(match_count)], use.names = FALSE)
  )
}

.merge_by_range_data_table <- function(x,
                                       y,
                                       by_x,
                                       by_y,
                                       x_start_value,
                                       x_end_value,
                                       x_start_relaxed,
                                       x_end_relaxed,
                                       y_val_value) {
  check_package("data.table", "fast range joins")

  x_valid <- !is.na(x_start_relaxed) & !is.na(x_end_relaxed)
  y_valid <- !is.na(y_val_value)

  if (length(by_x) > 0) {
    x_valid <- x_valid & stats::complete.cases(x[, by_x, drop = FALSE])
    y_valid <- y_valid & stats::complete.cases(y[, by_y, drop = FALSE])
  }

  if (!any(x_valid) || !any(y_valid)) {
    return(list(x = integer(0), y = integer(0)))
  }

  x_join <- data.table::as.data.table(x[x_valid, by_x, drop = FALSE])
  data.table::set(x_join, j = "cp_join_start", value = x_start_relaxed[x_valid])
  data.table::set(x_join, j = "cp_join_end", value = x_end_relaxed[x_valid])
  data.table::set(x_join, j = "cp_x_row", value = which(x_valid))

  y_join <- data.table::as.data.table(y[y_valid, by_y, drop = FALSE])
  if (length(by_x) > 0 && !identical(by_x, by_y)) {
    data.table::setnames(y_join, by_y, by_x)
  }
  data.table::set(y_join, j = "cp_join_val_start", value = y_val_value[y_valid])
  data.table::set(y_join, j = "cp_join_val_end", value = y_val_value[y_valid])
  data.table::set(y_join, j = "cp_y_row", value = which(y_valid))

  if (nrow(x_join) == 0 || nrow(y_join) == 0) {
    return(list(x = integer(0), y = integer(0)))
  }

  data.table::setkeyv(
    x_join,
    c(by_x, "cp_join_start", "cp_join_end")
  )

  pairs <- data.table::foverlaps(
    x = y_join,
    y = x_join,
    by.x = c(by_x, "cp_join_val_start", "cp_join_val_end"),
    by.y = c(by_x, "cp_join_start", "cp_join_end"),
    type = "within",
    nomatch = 0L
  )

  if (nrow(pairs) == 0) {
    return(list(x = integer(0), y = integer(0)))
  }

  list(x = pairs[["cp_x_row"]], y = pairs[["cp_y_row"]])
}

.merge_by_range_finalize <- function(x,
                                     y,
                                     by_x,
                                     by_y,
                                     matched_x,
                                     matched_y,
                                     x_start_value,
                                     x_end_value,
                                     y_val_value,
                                     all_y = FALSE,
                                     suffixes = c(".x", ".y")) {
  y_row_id_col <- ".cp_y_row_id"
  has_duplicate_warning <- FALSE

  y_keep <- setdiff(names(y), by_y)
  duplicated_cols <- intersect(names(x), y_keep)

  x_out <- x
  y_out <- y[, y_keep, drop = FALSE]

  if (length(by_x) > 0 && !identical(by_x, by_y)) {
    y_out_cols <- names(y_out)
    for (i in seq_along(by_x)) {
      if (by_y[i] %in% y_keep) {
        y_out_cols[y_out_cols == by_y[i]] <- by_x[i]
      }
    }
    names(y_out) <- y_out_cols
    duplicated_cols <- intersect(names(x), names(y_out))
  }

  names(x_out)[names(x_out) %in% duplicated_cols] <- paste0(
    names(x_out)[names(x_out) %in% duplicated_cols],
    suffixes[1]
  )
  names(y_out)[names(y_out) %in% duplicated_cols] <- paste0(
    names(y_out)[names(y_out) %in% duplicated_cols],
    suffixes[2]
  )

  if (length(matched_x) > 0) {
    y_row_id <- matched_y
    since_start <- as.numeric(y_val_value[matched_y] - x_start_value[matched_x])
    beyond_end <- y_val_value[matched_y] > x_end_value[matched_x]

    result <- cbind(
      x_out[matched_x, , drop = FALSE],
      y_out[matched_y, , drop = FALSE],
      since_start = since_start,
      stringsAsFactors = FALSE
    )
    result[[y_row_id_col]] <- y_row_id

    y_dup <- duplicated(y_row_id) | duplicated(y_row_id, fromLast = TRUE)

    if (any(y_dup)) {
      dup_ids <- unique(y_row_id[y_dup])
      rows_to_remove <- integer(0)

      for (id in dup_ids) {
        idx <- which(y_row_id == id)
        if (length(idx) <= 1) next

        current_idx <- idx

        neg_since_start_idx <- current_idx[since_start[current_idx] < 0]
        if (length(neg_since_start_idx) > 0 && length(current_idx) > 1) {
          rows_to_remove <- c(rows_to_remove, neg_since_start_idx)
          current_idx <- setdiff(current_idx, neg_since_start_idx)
        }

        if (length(current_idx) > 1) {
          beyond_end_idx <- current_idx[beyond_end[current_idx]]
          if (length(beyond_end_idx) > 0) {
            rows_to_remove <- c(rows_to_remove, beyond_end_idx)
            current_idx <- setdiff(current_idx, beyond_end_idx)
          }
        }

        if (length(current_idx) > 1) {
          has_duplicate_warning <- TRUE
        }
      }

      if (length(rows_to_remove) > 0) {
        result <- result[-rows_to_remove, , drop = FALSE]
        y_row_id <- y_row_id[-rows_to_remove]
        since_start <- since_start[-rows_to_remove]
        beyond_end <- beyond_end[-rows_to_remove]
      }

      if (has_duplicate_warning) {
        warning(
          "Some rows in `y` matched multiple ranges in `x` after filtering. ",
          "The `.cp_y_row_id` column is retained in the output to identify duplicates."
        )
      } else {
        result[[y_row_id_col]] <- NULL
      }
    } else {
      result[[y_row_id_col]] <- NULL
    }
  } else {
    result <- cbind(
      x_out[FALSE, , drop = FALSE],
      y_out[FALSE, , drop = FALSE],
      since_start = numeric(0)
    )
  }

  if (all_y) {
    unmatched_y <- setdiff(seq_len(nrow(y)), unique(matched_y))
    if (length(unmatched_y) > 0) {
      unmatched_piece <- cbind(
        .na_like_df(x_out, length(unmatched_y)),
        y_out[unmatched_y, , drop = FALSE],
        since_start = rep(NA_real_, length(unmatched_y))
      )
      result <- dplyr::bind_rows(result, unmatched_piece)
    }
  }

  result
}

#' Merge Data Frames by Exact Keys and Value Range
#'
#' @description
#' Merge two data frames where shared keys in `by` must match exactly and the
#' value in `y[[y_val]]` must fall within the range defined by
#' `x[[x_start]]` and `x[[x_end]]`.
#'
#' This function is particularly useful for date-based matching scenarios,
#' where you need to match events (e.g., examinations, treatments) to
#' time intervals (e.g., hospital admissions, visits). While the function
#' accepts any ordered values (numeric, Date, POSIXt), date matching is
#' the primary use case.
#'
#' This avoids constructing the full Cartesian product that would be produced by
#' a regular equality join followed by range filtering.
#'
#' @param x A data frame containing the range columns.
#' @param y A data frame containing the point-in-time value column.
#' @param by Either a character vector of column names that must match exactly
#'   in both data frames, or a named list with elements `x` and `y` specifying
#'   different column names in each data frame (e.g., `list(x = c("id1", "id2"), y = c("ID1", "ID2"))`).
#'   The two vectors must have the same length and are matched by position.
#'   Use `character(0)` when no exact-match keys are needed.
#' @param x_start Column name in `x` containing the range start.
#' @param x_end Column name in `x` containing the range end. If `NULL`, defaults
#'   to `x_start` (treating the range as a single point).
#' @param y_val Column name in `y` containing the value to be matched.
#' @param range_relax A numeric vector of length 2 specifying how to extend the
#'   matching range. The first element extends backwards from `x_start`,
#'   the second extends forwards from `x_end`. Default is `c(0, 0)` (no extension).
#'   Both values must be >= 0.
#' @param all_y Logical, whether to keep rows from `y` that have no match.
#' @param suffixes Character vector of length 2 used for duplicated non-key
#'   column names from `x` and `y`.
#' @param engine Join engine. Use `"auto"` to prefer `data.table` when
#'   installed and otherwise fall back to the base R implementation. Use
#'   `"data.table"` to require the fast non-equi join path explicitly.
#'
#' @return A data frame containing matched rows from `x` and `y`. The output
#'   includes a `since_start` column indicating the numeric difference between
#'   `y_val` and `x_start` (in the units of the values, e.g., days for Date objects).
#'
#'   If any row from `y` matches multiple ranges in `x` after applying the
#'   `range_relax` extension and filtering, a `.cp_y_row_id` column is retained
#'   in the output to identify duplicates, and a warning is issued.
#'
#' @examples
#' admissions <- data.frame(
#'   patient_id = c(1, 1, 2),
#'   date_start = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01")),
#'   date_end = as.Date(c("2024-01-10", "2024-02-10", "2024-03-05")),
#'   ward = c("A", "B", "C")
#' )
#' examinations <- data.frame(
#'   patient_id = c(1, 1, 2, 3),
#'   exam_date = as.Date(c("2024-01-05", "2024-02-10", "2024-03-07", "2024-01-01")),
#'   exam_name = c("CT", "MRI", "XR", "US")
#' )
#'
#' merge_by_range(
#'   x = admissions,
#'   y = examinations,
#'   by = "patient_id",
#'   x_start = "date_start",
#'   x_end = "date_end",
#'   y_val = "exam_date"
#' )
#'
#' @export
merge_by_range <- function(x,
                           y,
                           by,
                           x_start,
                           x_end = NULL,
                           y_val,
                           range_relax = c(0, 0),
                           all_y = FALSE,
                           suffixes = c(".x", ".y"),
                           engine = c("auto", "data.table", "base")) {
  if (!is.data.frame(x)) {
    stop("`x` must be a data frame")
  }
  if (!is.data.frame(y)) {
    stop("`y` must be a data frame")
  }

  by_x <- NULL
  by_y <- NULL

  if (is.list(by) && !is.data.frame(by)) {
    if (!all(c("x", "y") %in% names(by))) {
      stop("When `by` is a list, it must contain elements named 'x' and 'y'")
    }
    by_x <- as.character(by$x)
    by_y <- as.character(by$y)
    if (length(by_x) != length(by_y)) {
      stop("`by$x` and `by$y` must have the same length")
    }
    by <- by_x
  } else {
    by <- as.character(by)
    by_x <- by
    by_y <- by
  }

  if (is.null(x_end)) {
    x_end <- x_start
  }

  required_x <- c(by_x, x_start, x_end)
  required_y <- c(by_y, y_val)

  missing_x <- setdiff(required_x, names(x))
  missing_y <- setdiff(required_y, names(y))

  if (length(missing_x) > 0) {
    stop(sprintf("Columns not found in `x`: %s", paste(missing_x, collapse = ", ")))
  }
  if (length(missing_y) > 0) {
    stop(sprintf("Columns not found in `y`: %s", paste(missing_y, collapse = ", ")))
  }

  if (!is.numeric(range_relax) || length(range_relax) != 2) {
    stop("`range_relax` must be a numeric vector of length 2")
  }
  if (any(range_relax < 0)) {
    stop("All elements of `range_relax` must be >= 0")
  }

  if (!is.logical(all_y) || length(all_y) != 1 || is.na(all_y)) {
    stop("`all_y` must be a single TRUE/FALSE value")
  }
  if (!is.character(suffixes) || length(suffixes) != 2) {
    stop("`suffixes` must be a character vector of length 2")
  }

  engine <- match.arg(engine)
  if (identical(engine, "auto")) {
    engine <- if (requireNamespace("data.table", quietly = TRUE)) "data.table" else "base"
  }

  x_start_value <- .coerce_join_value(x[[x_start]], x_start)
  x_end_value <- .coerce_join_value(x[[x_end]], x_end)
  y_val_value <- .coerce_join_value(y[[y_val]], y_val)

  x_start_relaxed <- x_start_value - range_relax[1]
  x_end_relaxed <- x_end_value + range_relax[2]

  invalid_range <- !is.na(x_start_value) & !is.na(x_end_value) & x_start_value > x_end_value
  if (any(invalid_range)) {
    stop("`x_start` must be earlier than or equal to `x_end` for all non-missing rows")
  }

  matches <- switch(engine,
    data.table = .merge_by_range_data_table(
      x = x,
      y = y,
      by_x = by_x,
      by_y = by_y,
      x_start_value = x_start_value,
      x_end_value = x_end_value,
      x_start_relaxed = x_start_relaxed,
      x_end_relaxed = x_end_relaxed,
      y_val_value = y_val_value
    ),
    base = .merge_by_range_base(
      x = x,
      y = y,
      by_x = by_x,
      by_y = by_y,
      x_start_value = x_start_value,
      x_end_value = x_end_value,
      x_start_relaxed = x_start_relaxed,
      x_end_relaxed = x_end_relaxed,
      y_val_value = y_val_value
    )
  )

  .merge_by_range_finalize(
    x = x,
    y = y,
    by_x = by_x,
    by_y = by_y,
    matched_x = matches$x,
    matched_y = matches$y,
    x_start_value = x_start_value,
    x_end_value = x_end_value,
    y_val_value = y_val_value,
    all_y = all_y,
    suffixes = suffixes
  )
}
