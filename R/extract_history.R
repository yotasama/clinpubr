#' Extract Medical History from Clinical Text
#'
#' @description Extract medical history information (disease, smoking, drinking, surgery, etc.)
#'   from clinical text records, supports Chinese text. The function uses vectorized string operations
#'   for high performance on large datasets.
#'
#' @param text A character vector of clinical text records.
#' @param keywords A character vector of keywords to search for.
#'   Can also be a named list where names are category names and values are keyword vectors;
#'   multiple keywords within one category are treated as synonyms and matched with OR logic.
#' @param extract_duration Logical. If \code{TRUE}, extract duration information (years/months/days)
#'   when available. Default is \code{TRUE}.
#' @param duration_unit Character. The unit for duration output. Can be \code{"original"} (keep as
#'   extracted), \code{"years"} (convert all to years), or \code{"days"}
#'   (convert all to days). Default is \code{"original"}.
#' @param negation_window Integer. The maximum character distance to look for negation
#'   words before the keyword. Default is 20.
#' @param return_format Character. The format of return values. Can be \code{"simple"} (\code{TRUE}/\code{FALSE}/\code{NA}),
#'   \code{"detailed"} (duration strings like "5 years" when available, "yes"/"no" otherwise),
#'   or \code{"data.frame"} (separate columns for status and duration). Default is \code{"simple"}.
#'
#' @return Depending on \code{return_format}:
#' \itemize{
#'   \item \code{"simple"}: A logical vector with values \code{TRUE}, \code{FALSE}, or \code{NA}.
#'   \item \code{"detailed"}: A character vector with duration strings (e.g., "5 years", "30 days") when available,
#'     otherwise \code{TRUE}, \code{FALSE}, or \code{NA}.
#'   \item \code{"data.frame"}: A data frame with columns \code{status} (logical) and \code{duration} (character).
#' }
#'
#' If \code{keywords} is a named list with multiple categories, returns a data frame with
#' one column per category.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' test_data <- c(
#'   "hypertension history 20 years",
#'   "appendectomy surgery history"
#' )
#' extract_history(test_data, "hypertension")
#' }
extract_history <- function(text,
                            keywords,
                            extract_duration = TRUE,
                            duration_unit = c("original", "years", "days"),
                            negation_window = 20,
                            return_format = c("simple", "detailed", "data.frame")) {
  return_format <- match.arg(return_format)
  duration_unit <- match.arg(duration_unit)

  if (length(text) == 0) {
    return(empty_result(return_format))
  }

  keyword_list <- normalize_keywords(keywords)
  categories <- names(keyword_list)

  results <- lapply(keyword_list, function(kw) {
    extract_history_single(
      text = text,
      keyword = kw,
      extract_duration = extract_duration,
      duration_unit = duration_unit,
      negation_window = negation_window,
      return_format = return_format
    )
  })

  if (length(results) == 1) {
    return(results[[1]])
  }

  format_multi_results(results, categories, return_format)
}

#' @keywords internal
normalize_keywords <- function(keywords) {
  if (is.list(keywords) && !is.null(names(keywords))) {
    return(lapply(keywords, as.character))
  }

  if (is.character(keywords) && !is.null(names(keywords)) && length(keywords) > 0) {
    return(as.list(keywords))
  }

  keywords <- as.character(keywords)
  as.list(setNames(keywords, keywords))
}

#' @keywords internal
empty_result <- function(return_format) {
  switch(return_format,
    "simple" = logical(0),
    "detailed" = logical(0),
    "data.frame" = data.frame(
      status = logical(0),
      duration = character(0),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  )
}

#' @keywords internal
format_multi_results <- function(results, categories, return_format) {
  if (return_format == "data.frame") {
    for (i in seq_along(results)) {
      names(results[[i]]) <- paste0(categories[i], c("_status", "_duration"))
    }
    return(do.call(cbind, unname(results)))
  }

  result_df <- do.call(
    data.frame,
    c(unname(results), list(stringsAsFactors = FALSE, check.names = FALSE))
  )
  names(result_df) <- make.names(categories, unique = TRUE)
  result_df
}

#' @title Extract History for Single Category (Internal Function)
#' @param keyword A keyword string or a character vector of synonym keywords
#'   combined with OR logic.
#' @keywords internal
extract_history_single <- function(text,
                                    keyword,
                                    extract_duration = TRUE,
                                    duration_unit = "original",
                                    negation_window = 20,
                                    return_format = "simple") {
  keyword_pattern <- build_keyword_pattern(keyword)
  status <- detect_status(text, keyword_pattern, negation_window)

  duration <- rep(NA_character_, length(text))
  if (extract_duration && return_format != "simple") {
    yes_idx <- which(status == TRUE)
    if (length(yes_idx) > 0) {
      duration[yes_idx] <- extract_durations(text[yes_idx], keyword, keyword_pattern)
    }
  }

  create_result(status, duration, return_format, duration_unit)
}

#' @keywords internal
build_keyword_pattern <- function(keyword) {
  paste0("(?:", paste(escape_keyword(keyword), collapse = "|"), ")")
}

#' @keywords internal
detect_status <- function(text, keyword_pattern, negation_window) {
  n <- length(text)
  status <- rep(NA, n)

  has_kw <- stringi::stri_detect_regex(text, keyword_pattern)
  has_kw[is.na(has_kw)] <- FALSE
  if (!any(has_kw)) {
    return(status)
  }

  subset_text <- text[has_kw]
  has_neg <- detect_negation(subset_text, keyword_pattern, negation_window)

  subset_status <- rep(NA, length(subset_text))
  subset_status[has_neg] <- FALSE
  subset_status[!has_neg] <- TRUE

  status[has_kw] <- subset_status
  status
}

#' @keywords internal
detect_negation <- function(text, keyword_pattern, window) {
  n <- length(text)
  has_neg <- rep(FALSE, n)

  has_any_neg <- stringi::stri_detect_regex(text, "\u5426\u8ba4|\u6ca1\u6709|\u672a\u89c1|\u5426\u5b9a|\u672a|\u65e0")
  has_any_neg[is.na(has_any_neg)] <- FALSE
  if (!any(has_any_neg)) {
    return(has_neg)
  }

  neg_idx <- which(has_any_neg)
  subset_text <- text[neg_idx]

  neg_pattern_pre <- sprintf(
    "(?:\u5426\u8ba4|\u6ca1\u6709|\u672a\u89c1|\u5426\u5b9a|\u672a|\u65e0)(?:[^\uff0c,\u3002\uff1b\u4f46\n\r]{0,%d})%s",
    window, keyword_pattern
  )
  neg_pattern_post <- sprintf(
    "%s(?:[^\uff0c,\u3002\uff1b\n\r]{0,%d})(?:\u5426\u8ba4|\u6ca1\u6709|\u672a\u89c1|\u5426\u5b9a|\u672a|\u65e0)",
    keyword_pattern, window
  )
  subset_has_neg <- stringi::stri_detect_regex(subset_text, neg_pattern_pre) |
    stringi::stri_detect_regex(subset_text, neg_pattern_post)
  subset_has_neg[is.na(subset_has_neg)] <- FALSE

  has_neg[neg_idx] <- subset_has_neg
  has_neg
}

#' @keywords internal
escape_keyword <- function(keyword) {
  stringi::stri_replace_all_fixed(
    keyword,
    c(".", "\\", "+", "*", "?", "^", "$", "|", "(", ")", "[", "{"),
    c("\\.", "\\\\", "\\+", "\\*", "\\?", "\\^", "\\$", "\\|", "\\(", "\\)", "\\[", "\\{"),
    vectorize_all = FALSE
  )
}

#' @keywords internal
extract_durations <- function(text, keyword, keyword_pattern = build_keyword_pattern(keyword)) {
  after_pattern <- sprintf("%s([\\s\\S]{0,50}?)(?:[\u3002\uff1b!\uff01?\uff1f]|$)", keyword_pattern)
  after_matches <- stringi::stri_match_first_regex(text, after_pattern)

  duration <- rep(NA_character_, length(text))
  after_text <- after_matches[, 2]
  valid <- !is.na(after_text)

  if (!any(valid)) {
    return(duration)
  }

  dur_pattern <- "([0-9]+(?:\\.[0-9]+)?)\\s*(?:\u4f59)?\\s*[\u4e2a]?\\s*(\u5e74|\u6708|\u5468|\u5929|\u65e5|year|years|month|months|week|weeks|day|days)"
  after_text_valid <- after_text[valid]

  has_dur <- stringi::stri_detect_regex(after_text_valid, dur_pattern)
  has_dur[is.na(has_dur)] <- FALSE
  if (!any(has_dur)) {
    return(duration)
  }

  valid_idx <- which(valid)[has_dur]
  text_with_dur <- after_text_valid[has_dur]

  duration[valid_idx] <- select_best_duration(
    text = text_with_dur,
    keyword = keyword,
    dur_pattern = dur_pattern
  )

  duration
}

#' @keywords internal
select_best_duration <- function(text, keyword, dur_pattern) {
  n <- length(text)
  duration <- rep(NA_character_, n)

  if (n == 0) {
    return(duration)
  }

  all_locs <- stringi::stri_locate_all_regex(text, dur_pattern)
  all_matches <- stringi::stri_match_all_regex(text, dur_pattern)

  n_matches <- vapply(all_locs, function(x) {
    if (is.null(x) || nrow(x) == 0 || is.na(x[1, 1])) 0L else nrow(x)
  }, integer(1))

  has_matches <- n_matches > 0
  if (!any(has_matches)) {
    return(duration)
  }

  match_rows <- which(has_matches)
  row_idx <- rep(match_rows, n_matches[has_matches])

  nums <- unlist(lapply(all_matches[has_matches], function(m) m[, 2]))
  units <- unlist(lapply(all_matches[has_matches], function(m) m[, 3]))
  starts <- unlist(lapply(all_locs[has_matches], function(m) m[, 1]))

  prefix_start <- pmax(1, starts - 15)
  prefix <- stringi::stri_sub(text[row_idx], from = prefix_start, to = starts - 1)

  is_year <- is_year_like(nums, units)
  is_other <- is_other_entity_prefix(prefix, keyword)
  has_bingcheng <- stringi::stri_detect_regex(prefix, "\u75c5\u7a0b")

  valid <- !is_year & !is_other
  if (!any(valid)) {
    return(duration)
  }

  valid_idx <- row_idx[valid]
  valid_num <- nums[valid]
  valid_unit <- units[valid]
  valid_bingcheng <- has_bingcheng[valid]
  valid_order <- seq_along(valid_idx)

  ord <- order(valid_idx, !valid_bingcheng, valid_order)
  selected <- !duplicated(valid_idx[ord])

  sel_idx <- valid_idx[ord][selected]
  sel_unit <- valid_unit[ord][selected]
  sel_unit <- dplyr::recode(sel_unit,
    "\u5e74" = "years", "\u6708" = "months", "\u5468" = "weeks",
    "\u5929" = "days", "\u65e5" = "days",
    .default = sel_unit
  )
  duration[sel_idx] <- paste0(valid_num[ord][selected], " ", sel_unit)

  duration
}

#' @keywords internal
is_year_like <- function(num, unit) {
  unit %in% c("\u5e74", "year", "years") &
    nchar(num) == 4 &
    !stringi::stri_detect_fixed(num, ".") &
    suppressWarnings(as.numeric(num)) >= 1000 &
    suppressWarnings(as.numeric(num)) <= 9999
}

#' @keywords internal
is_other_entity_prefix <- function(prefix, keywords) {
  entity_pattern <- "([^\uff0c,\u3001\u3002\uff1b\\s]{2,})\u53f2\\s*$"
  entity_match <- stringi::stri_match_first_regex(prefix, entity_pattern)

  entity_name <- entity_match[, 2]
  entity_name <- stringi::stri_replace_all_regex(
    entity_name,
    "^[^\\p{L}\\p{N}]+|[^\\p{L}\\p{N}]+$",
    ""
  )

  matches_keyword <- Reduce(`|`, lapply(keywords, function(kw) {
    stringi::stri_detect_fixed(entity_name, kw) |
      stringi::stri_detect_fixed(kw, entity_name)
  }))

  !is.na(entity_name) &
    nchar(entity_name) >= 2 &
    !matches_keyword
}

#' @keywords internal
create_result <- function(status, duration, return_format, duration_unit) {
  if (duration_unit != "original" && !all(is.na(duration))) {
    duration <- convert_duration(duration, duration_unit)
  }

  switch(return_format,
    "simple" = status,
    "detailed" = {
      res <- rep(NA_character_, length(status))
      res[status == TRUE & is.na(duration)] <- "yes"
      res[status == FALSE] <- "no"
      has_dur <- !is.na(duration) & !is.na(status) & status == TRUE
      res[has_dur] <- duration[has_dur]
      res
    },
    "data.frame" = data.frame(
      status = status,
      duration = duration,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  )
}

#' @keywords internal
convert_duration <- function(duration, unit) {
  if (all(is.na(duration))) {
    return(duration)
  }

  pattern <- "([0-9]+(?:\\.[0-9]+)?)\\s*(?:\u4f59)?\\s*[\u4e2a]?\\s*(\u5e74|\u6708|\u5468|\u5929|\u65e5|year|years|month|months|week|weeks|day|days)"
  matches <- stringi::stri_match_first_regex(duration, pattern)

  num <- suppressWarnings(as.numeric(matches[, 2]))
  orig_unit <- matches[, 3]

  orig_unit <- dplyr::recode(orig_unit,
    "\u5e74" = "years", "\u6708" = "months", "\u5468" = "weeks",
    "\u5929" = "days", "\u65e5" = "days",
    "year" = "years", "month" = "months", "week" = "weeks", "day" = "days",
    .default = orig_unit
  )

  if (unit == "years") {
    converted <- num * ifelse(orig_unit == "years", 1,
      ifelse(orig_unit == "months", 1 / 12,
        ifelse(orig_unit == "weeks", 7 / 365.25,
          ifelse(orig_unit == "days", 1 / 365.25, NA)
        )
      )
    )
    res <- ifelse(!is.na(converted), sprintf("%.2f years", converted), NA_character_)
  } else if (unit == "days") {
    converted <- num * ifelse(orig_unit == "years", 365.25,
      ifelse(orig_unit == "months", 30.44,
        ifelse(orig_unit == "weeks", 7,
          ifelse(orig_unit == "days", 1, NA)
        )
      )
    )
    res <- ifelse(!is.na(converted), sprintf("%.1f days", converted), NA_character_)
  } else {
    res <- duration
  }

  res[is.na(duration)] <- NA_character_
  res
}