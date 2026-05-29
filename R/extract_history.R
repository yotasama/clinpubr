#' Extract Medical History from Clinical Text
#'
#' @description Extract medical history information (disease, smoking, drinking, surgery, etc.)
#'   from clinical text records. The function uses vectorized string operations
#'   for high performance on large datasets.
#'
#' @param text A character vector of clinical text records.
#' @param keywords A character vector of keywords to search for (e.g., "高血压", "吸烟").
#'   Can also be a named list where names are category names and values are keyword vectors.
#' @param extract_duration Logical. If \code{TRUE}, extract duration information (years/months/days)
#'   when available. Default is \code{TRUE}.
#' @param duration_unit Character. The unit for duration output. Can be \code{"original"} (keep as
#'   extracted, e.g., "5年", "3月"), \code{"years"} (convert all to years), or \code{"days"}
#'   (convert all to days). Default is \code{"original"}.
#' @param negation_window Integer. The maximum character distance to look for negation
#'   words before the keyword. Default is 20.
#' @param affirmative_window Integer. The maximum character distance to look for affirmative
#'   words before the keyword. Default is 20.
#' @param return_format Character. The format of return values. Can be \code{"simple"} ("有"/"无"/NA),
#'   \code{"detailed"} (duration included, e.g., "有:5年"/"无"/NA), or \code{"data.frame"} (separate
#'   columns for status and duration). Default is \code{"simple"}.
#'
#' @return Depending on \code{return_format}:
#' \itemize{
#'   \item \code{"simple"}: A character vector with values "有" (yes), "无" (no), or \code{NA} (unknown).
#'   \item \code{"detailed"}: A character vector with duration information when available.
#'   \item \code{"data.frame"}: A data frame with columns \code{status} (有/无/NA) and \code{duration} (duration value).
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
#'   "否认肝炎、结核、疟疾史，否认高血压、心脏病史，否认糖尿病，无手术史，无过敏史。",
#'   "既往有高血压病史20年，口服降压药控制良好。",
#'   "有阑尾炎切除手术史，否认高血压。",
#'   "无高血压、心脏病史，但有糖尿病病史。",
#'   "患者高血压病史、冠心病史，无吸烟史。",
#'   "高血压病史10年，吸烟史15年，每天10支，饮酒史5年。"
#' )
#'
#' # Extract single keyword
#' extract_history(test_data, "高血压")
#'
#' # Extract multiple keywords
#' extract_history(test_data, c("高血压", "吸烟", "饮酒"))
#'
#' # Extract with duration
#' extract_history(test_data, "高血压", return_format = "detailed")
#'
#' # Extract multiple categories with named list
#' keywords <- list(
#'   hypertension = "高血压",
#'   diabetes = "糖尿病",
#'   smoking = c("吸烟", "抽烟"),
#'   drinking = c("饮酒", "喝酒")
#' )
#' extract_history(test_data, keywords)
#' }
extract_history <- function(text,
                            keywords,
                            extract_duration = TRUE,
                            duration_unit = c("original", "years", "days"),
                            negation_window = 20,
                            affirmative_window = 20,
                            return_format = c("detailed", "simple", "data.frame")) {
  return_format <- match.arg(return_format)
  duration_unit <- match.arg(duration_unit)

  if (length(text) == 0) {
    return(switch(return_format,
      "simple" = character(0),
      "detailed" = character(0),
      "data.frame" = data.frame(
        status = character(0),
        duration = character(0),
        stringsAsFactors = FALSE
      )
    ))
  }

  is_named_list <- is.list(keywords) && !is.null(names(keywords))
  is_named_char <- is.character(keywords) && !is.null(names(keywords)) && length(keywords) > 0

  if (is_named_list || is_named_char) {
    categories <- names(keywords)
    results <- lapply(seq_along(categories), function(i) {
      kw <- if (is_named_list) keywords[[i]] else keywords[i]
      extract_history_single(
        text = text,
        keywords = kw,
        extract_duration = extract_duration,
        duration_unit = duration_unit,
        negation_window = negation_window,
        affirmative_window = affirmative_window,
        return_format = return_format
      )
    })

    if (return_format == "data.frame") {
      for (i in seq_along(results)) {
        names(results[[i]]) <- paste0(categories[i], c("_status", "_duration"))
      }
      do.call(cbind, results)
    } else {
      result_df <- do.call(
        data.frame,
        c(results, list(stringsAsFactors = FALSE, check.names = FALSE))
      )
      names(result_df) <- make.names(categories, unique = TRUE)
      result_df
    }
  } else {
    keywords <- as.character(keywords)
    if (length(keywords) == 1) {
      extract_history_single(
        text = text,
        keywords = keywords,
        extract_duration = extract_duration,
        duration_unit = duration_unit,
        negation_window = negation_window,
        affirmative_window = affirmative_window,
        return_format = return_format
      )
    } else {
      results <- lapply(keywords, function(kw) {
        extract_history_single(
          text = text,
          keywords = kw,
          extract_duration = extract_duration,
          duration_unit = duration_unit,
          negation_window = negation_window,
          affirmative_window = affirmative_window,
          return_format = return_format
        )
      })

      if (return_format == "data.frame") {
        for (i in seq_along(results)) {
          names(results[[i]]) <- paste0(keywords[i], c("_status", "_duration"))
        }
        do.call(cbind, results)
      } else {
        result_df <- do.call(
          data.frame,
          c(results, list(stringsAsFactors = FALSE, check.names = FALSE))
        )
        names(result_df) <- make.names(keywords, unique = TRUE)
        result_df
      }
    }
  }
}

#' @title Extract History for Single Category (Internal Function)
#' @keywords internal
extract_history_single <- function(text,
                                    keywords,
                                    extract_duration = TRUE,
                                    duration_unit = "original",
                                    negation_window = 20,
                                    affirmative_window = 20,
                                    return_format = "simple") {
  n <- length(text)
  keyword <- keywords[1]

  keyword_escaped <- stringi::stri_replace_all_fixed(
    keyword,
    c(".", "\\", "+", "*", "?", "^", "$", "|", "(", ")", "[", "{"),
    c("\\.", "\\\\", "\\+", "\\*", "\\?", "\\^", "\\$", "\\|", "\\(", "\\)", "\\[", "\\{"),
    vectorize_all = FALSE
  )

  status <- rep(NA_character_, n)
  duration <- rep(NA_character_, n)

  has_kw <- stringi::stri_detect_regex(text, keyword_escaped)
  has_kw[is.na(has_kw)] <- FALSE
  if (!any(has_kw)) {
    return(create_result(status, duration, return_format, duration_unit))
  }

  subset_text <- text[has_kw]

  # 前置否定（禁止跨越逗号/句尾，允许顿号列举）
  neg_pattern_pre <- sprintf(
    "(?:否认|没有|未见|否定|未|无)(?:[^，,。；但\n\r]{0,%d})%s",
    negation_window, keyword_escaped
  )
  # 后置否定（如：高血压病史否认）
  neg_pattern_post <- sprintf(
    "%s(?:[^，,。；\n\r]{0,%d})(?:否认|没有|未见|否定|未|无)",
    keyword_escaped, negation_window
  )
  has_neg <- stringi::stri_detect_regex(subset_text, neg_pattern_pre) |
             stringi::stri_detect_regex(subset_text, neg_pattern_post)
  has_neg[is.na(has_neg)] <- FALSE

  # 肯定检测（禁止跨越逗号/句尾，允许顿号列举）
  aff_pattern <- sprintf(
    "(?:有|患|既往|确诊|诊断|曾患|曾有)(?:[^，,。；\n\r]{0,%d})%s",
    affirmative_window, keyword_escaped
  )
  has_aff <- stringi::stri_detect_regex(subset_text, aff_pattern)
  has_aff[is.na(has_aff)] <- FALSE

  # 纯提及检测（关键词+史）
  pure_pattern <- sprintf("%s[病]?史", keyword_escaped)
  has_pure <- stringi::stri_detect_regex(subset_text, pure_pattern)
  has_pure[is.na(has_pure)] <- FALSE

  subset_status <- rep(NA_character_, length(subset_text))
  subset_status[has_neg] <- "无"
  subset_status[!has_neg] <- "有"

  if (extract_duration) {
    yes_idx <- which(subset_status == "有")
    if (length(yes_idx) > 0) {
      subset_duration <- rep(NA_character_, length(subset_text))

      # 提取关键词后面的文本（到句子结束，最多50字符）
      after_pattern <- sprintf("%s([\\s\\S]{0,50}?)(?:[。；!！?？]|$)", keyword_escaped)
      after_matches <- stringi::stri_match_first_regex(subset_text[yes_idx], after_pattern)
      after_text <- after_matches[, 2]

      # 查找所有 duration 候选
      dur_pattern <- "([0-9]+(?:\\.[0-9]+)?)\\s*(?:余)?\\s*[个]?\\s*(年|月|周|天|日)"
      all_dur_locs <- stringi::stri_locate_all_regex(after_text, dur_pattern)
      all_dur_matches <- stringi::stri_match_all_regex(after_text, dur_pattern)

      for (j in seq_along(yes_idx)) {
        locs <- all_dur_locs[[j]]
        matches <- all_dur_matches[[j]]

        if (is.null(locs) || nrow(locs) == 0 || is.na(locs[1, 1])) next

        best_dur <- NA_character_
        for (k in seq_len(nrow(locs))) {
          if (is.na(locs[k, 1])) next

          match_start <- locs[k, 1]
          prefix_start <- max(1, match_start - 15)
          prefix <- stringi::stri_sub(after_text[j], from = prefix_start, to = match_start - 1)

          # 检查是否属于其他实体（前缀以 X史 结尾，X 至少2个字符，且 X 不是当前关键词）
          entity_pattern <- "([^，,、。；\\s]{2,})史\\s*$"
          entity_match <- stringi::stri_match_first_regex(prefix, entity_pattern)

          is_other_entity <- FALSE
          if (!is.na(entity_match[1, 1])) {
            entity_name <- entity_match[1, 2]
            if (!stringi::stri_detect_fixed(entity_name, keyword) &&
                !stringi::stri_detect_fixed(keyword, entity_name)) {
              is_other_entity <- TRUE
            }
          }

          if (!is_other_entity) {
            # 优先选择 "病程" 前缀
            if (stringi::stri_detect_regex(prefix, "病程")) {
              best_dur <- paste0(matches[k, 2], matches[k, 3])
              break
            }

            if (is.na(best_dur)) {
              best_dur <- paste0(matches[k, 2], matches[k, 3])
            }
          }
        }

        subset_duration[yes_idx[j]] <- best_dur
      }
    } else {
      subset_duration <- rep(NA_character_, length(subset_text))
    }
  } else {
    subset_duration <- rep(NA_character_, length(subset_text))
  }

  status[has_kw] <- subset_status
  duration[has_kw] <- subset_duration

  create_result(status, duration, return_format, duration_unit)
}

#' @keywords internal
create_result <- function(status, duration, return_format, duration_unit) {
  if (duration_unit != "original" && !all(is.na(duration))) {
    duration <- convert_duration(duration, duration_unit)
  }

  switch(return_format,
    "simple" = status,
    "detailed" = {
      res <- status
      has_dur <- !is.na(duration) & !is.na(status) & status == "有"
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

  pattern <- "([0-9]+(?:\\.[0-9]+)?)\\s*(?:余)?\\s*[个]?\\s*(年|月|周|天|日)"
  matches <- stringi::stri_match_first_regex(duration, pattern)

  num <- suppressWarnings(as.numeric(matches[, 2]))
  orig_unit <- matches[, 3]

  if (unit == "years") {
    converted <- num * ifelse(orig_unit == "年", 1,
      ifelse(orig_unit == "月", 1 / 12,
        ifelse(orig_unit == "周", 7 / 365.25,
          ifelse(orig_unit %in% c("天", "日"), 1 / 365.25, NA)
        )
      )
    )
    res <- ifelse(!is.na(converted), sprintf("%.2f年", converted), NA_character_)
  } else if (unit == "days") {
    converted <- num * ifelse(orig_unit == "年", 365.25,
      ifelse(orig_unit == "月", 30.44,
        ifelse(orig_unit == "周", 7,
          ifelse(orig_unit %in% c("天", "日"), 1, NA)
        )
      )
    )
    res <- ifelse(!is.na(converted), sprintf("%.1f天", converted), NA_character_)
  } else {
    res <- duration
  }

  res[is.na(duration)] <- NA_character_
  res
}

#' Create Empty Result Vector
#' @param n Number of elements.
#' @param return_format Format of return values.
#' @return A vector of appropriate type.
#' @keywords internal
create_result_vector <- function(n, return_format) {
  switch(return_format,
    "simple" = character(n),
    "detailed" = character(n),
    "data.frame" = data.frame(
      status = character(n),
      duration = character(n),
      stringsAsFactors = FALSE
    )
  )
}
