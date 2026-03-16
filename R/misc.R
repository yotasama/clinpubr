#' @include utils.R
NULL
#' Replace NA values with FALSE
#' @description  Replace `NA` values with `FALSE` in logical vectors.
#'   For other vectors, the behavior relies on R's automatic conversion rules.
#' @param x A vector.
#'
#' @returns A vector with `NA` values replaced by `FALSE`.
#' @export
#' @examples
#' na2false(c(TRUE, FALSE, NA, TRUE, NA))
#' na2false(c(1, 2, NA))
na2false <- function(x) {
  x[is.na(x)] <- FALSE
  x
}

#' Safe min and max functions that return NA if all values are NA
#' @description Instead of returning `-Inf` or `Inf`, returns `NA` if all values are `NA`.
#'   It also ignores `NA` values by default, which is different from base R functions.
#'   This is useful when summarizing data frames with `dplyr::summarise()`.
#' @param x A numeric vector.
#' @param na.rm A logical value indicating whether to remove `NA` values before computation.
#'   Defaults to `TRUE` instead of `FALSE` in base R functions.
#'
#' @returns The minimum or maximum value of the vector or `NA` if all values are `NA`.
#' @export
#' @examples
#' na_max(c(1, 2, 3, NA))
#' na_min(c(NA, NA, NA))
na_max <- function(x, na.rm = TRUE) {
  if (all(is.na(x))) {
    NA
  } else {
    max(x, na.rm = na.rm)
  }
}

#' @rdname na_max
#' @export
na_min <- function(x, na.rm = TRUE) {
  if (all(is.na(x))) {
    NA
  } else {
    min(x, na.rm = na.rm)
  }
}

#' Generate code from string vector
#' @description Generate the code that can be used to generate the string vector.
#'   `name2code()` is a wrapper of `vec2code(names(x))` to generate code for names of a
#'   vector, list, data frame, or any object with names.
#' @param x A string vector.
#'
#' @returns A string that contains the code to generate the vector.
#' @export
#' @examples
#' vec2code(colnames(mtcars))
#' name2code(mtcars)
vec2code <- function(x) {
  if (length(x) == 0) {
    "c()"
  } else {
    x_str <- ifelse(is.na(x), "NA", paste0("'", x, "'"))
    paste0("c(", paste0(x_str, collapse = ", "), ")")
  }
}

#' @rdname vec2code
#' @export
name2code <- function(x) {
  vec2code(names(x))
}

#' Get common prefix of a string vector
#'
#' @param x A string vector.
#'
#' @returns A string that is the common prefix of the input vector.
#' @export
#' @examples
#' common_prefix(c("Q1_a", "Q1_b", "Q1_c"))
common_prefix <- function(x) {
  if (length(x) == 1) {
    return(x)
  }
  min_len <- min(nchar(x))
  prefix <- character(0)
  for (i in 1:min_len) {
    chars <- substr(x, i, i)
    if (length(unique(chars)) == 1) {
      prefix <- c(prefix, chars[1])
    } else {
      break
    }
  }
  paste(prefix, collapse = "")
}

#' Format p-value for publication
#' @description Format p-value with modified default settings suitable for publication.
#' @param p The numerical p values to be formatted.
#' @param text_ahead A string to be added before the p value. If not `NULL`, this string
#'   will be connected to the formatted p value with `"="` or `"<"`.
#' @param digits The number of digits to be used. Same as in `base::format.pval`.
#' @param nsmall The number of digits after the decimal point. Same as in `base::format.pval`.
#' @param eps The threshold for rounding p values to 0. Same as in `base::format.pval`.
#' @param na_empty If `TRUE`, replace `"NA"` in result with an empty string.
#'
#' @returns A string vector of formatted p values.
#' @export
#' @examples
#' format_pval(c(0.001, 0.0001, 0.05, 0.1123456))
#' format_pval(c(0.001, 0.0001, 0.05, 0.1123456), text_ahead = "p value")
format_pval <- function(p, text_ahead = NULL, digits = 1, nsmall = 2, eps = 1e-3, na_empty = TRUE) {
  p_text <- base::format.pval(p, digits = digits, nsmall = nsmall, eps = eps)
  if (!is.null(text_ahead)) {
    p_text <- str_replace(p_text, "<", " < ")
    p_text <- ifelse(
      p < eps,
      paste0(text_ahead, p_text),
      paste0(text_ahead, " = ", p_text)
    )
    p_text[is.na(p_text)] <- paste0(text_ahead, " NA")
  }
  if (na_empty) {
    p_text <- ifelse(endsWith(p_text, "NA"), "", p_text)
  }
  p_text
}

#' Calculate the first mode
#' @description  Calculate the first mode of a vector. Ignore NA values.
#'   Can be used if any mode is acceptable.
#' @param x A vector.
#' @param empty_return The value to return if the vector is empty.
#'
#' @returns The first mode of the vector.
#' @export
#' @examples
#' first_mode(c(1, 1, 2, 2, 3, 3, 3, NA, NA, NA))
first_mode <- function(x, empty_return) {
  x <- na.omit(x)
  l <- length(unique(x))
  if (l == 0) {
    if(!missing(empty_return)) {
      empty_return
    } else {
      x[NA]
    }
  } else if (l > 1 && l < length(x)) {
    x[1] <- DescTools::Mode(x)[1]
    x[1]
  } else {
    x[1]
  }
}

#' Merging vectors while maintaining order
#' @description Merge multiple vectors into one while trying to maintain
#'   the order of elements in each vector. The relative order of elements
#'   is compared by their first occurrence in the vectors in the list.
#'   This function is useful when merging slightly different vectors,
#'   such as questionnaires of different versions.
#' @param vectors A list of vectors to be merged.
#'
#' @returns A vector.
#' @export
#' @examples
#' merge_ordered_vectors(list(c(1, 3, 4, 5, 7, 10), c(2, 5, 6, 7, 8), c(1, 7, 5, 10)))
merge_ordered_vectors <- function(vectors) {
  if (length(vectors) == 0) {
    return(NULL)
  }
  all_elements <- unique(unlist(vectors))

  # bubble sort all_elements based on the order of vectors
  n <- length(all_elements)
  if (n == 1) {
    return(all_elements)
  }
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      order_result <- .calculate_order(all_elements[i], all_elements[j], vectors)
      if (order_result == -1) {
        temp <- all_elements[i]
        all_elements[i] <- all_elements[j]
        all_elements[j] <- temp
      }
    }
  }
  return(all_elements)
}

#' Adding lists element-wise
#' @description Combine lists by adding element-wise.
#' @param l1,l2 A pair of lists.
#'
#' @returns A list.
#' @export
#' @examples
#' l1 <- list(a = 1, b = 2)
#' l2 <- list(a = 3, b = 4, c = 5)
#' add_lists(l1, l2)
add_lists <- function(l1, l2) {
  names1 <- names(l1)
  names2 <- names(l2)

  all_names <- unique(c(names1, names2))

  result <- setNames(vector("list", length(all_names)), all_names)
  for (name in all_names) {
    if (name %in% names1 && name %in% names2) {
      result[[name]] <- l1[[name]] + l2[[name]]
    } else if (name %in% names1) {
      result[[name]] <- l1[[name]]
    } else if (name %in% names2) {
      result[[name]] <- l2[[name]]
    }
  }

  return(result)
}

#' Determine duplicate elements including their first occurrence.
#' @description If an element is duplicated, all of its occurrence will be labeled `TRUE`.
#'   Useful to list and compare all duplicates.
#' @param x A vector.
#'
#' @returns A logical vector.
#' @export
#' @examples
#' indicate_duplicates(c(1, 2, NA, NA, 1))
#' indicate_duplicates(c(1, 2, 3, 4, 4))
#'
#' # Useful to check duplicates in data frames.
#' df <- data.frame(
#'   id = c(1, 2, 1, 2, 3), year = c(2010, 2011, 2010, 2010, 2011),
#'   value = c(1, 2, 3, 4, 5)
#' )
#' df[indicate_duplicates(df[, c("id", "year")]), ]
indicate_duplicates <- function(x) {
  c(duplicated(x) | duplicated(x, fromLast = TRUE))
}

#' Replacing elements in a vector
#' @param x A vector.
#' @param from A vector of elements to be replaced.
#' @param to A vector of elements to replace the original ones.
#'
#' @returns A vector.
#' @export
#' @examples
#' replace_elements(c("a", "x", "1", NA, "a"), c("a", "b", NA), c("A", "B", "XX"))
replace_elements <- function(x, from, to) {
  y <- x
  if (length(from) != length(to)) {
    stop("`from` and `to` should have the same length!")
  }
  for (i in seq_along(from)) {
    y[y %in% from[i]] <- to[i]
  }
  y
}

#' Fill NA values with the last valid value
#' @description Fill NA values with the last valid value. Can be used to fill excel combined cells.
#' @param x A vector.
#'
#' @returns A vector.
#' @export
#' @examples
#' fill_with_last(c(1, 2, NA, 4, NA, 6))
fill_with_last <- function(x) {
  if (length(x) >= 2) {
    for (i in 2:length(x)) {
      if (is.na(x[i])) {
        x[i] <- x[i - 1]
      }
    }
  }
  x
}

#' Match string and replace with corresponding value
#' @description Partially match a string and replace with corresponding value. This function is useful to recover
#'   the original names of variables after legalized using `make.names` or modified by other functions.
#' @param x A vector.
#' @param to_match A vector of strings to be matched.
#' @param to_replace A vector of strings to replace the matched ones, must have the same length as `to_match`.
#'
#' @returns A vector.
#' @export
#' @examples
#' ori_names <- c("xx (mg/dl)", "b*x", "Covid-19")
#' modified_names <- c("v1", "v2", "v3")
#' x <- c("v1.v2", "v3.yy", "v4")
#' str_match_replace(x, modified_names, ori_names)
str_match_replace <- function(x, to_match, to_replace) {
  if (length(to_match) != length(to_replace)) stop("`to_match` and `to_replace` must have the same length!")

  len <- nchar(to_match)
  ord <- order(len, decreasing = TRUE)
  match_ord <- to_match[ord]
  replace_ord <- to_replace[ord]

  for (i in seq_along(match_ord)) {
    x <- gsub(match_ord[i], replace_ord[i], x, fixed = TRUE)
  }
  return(x)
}

#' Fast long-to-wide conversion with item selection
#' @description Convert long-format data to wide format by grouping keys,
#'   using one column as item names and one column as values. When there are
#'   multiple values under the same key-item combination, values are reduced
#'   by `agg_fun`. Designed to convert long-format clinical data in database
#'   to wide format for analysis and publication.
#' @param df A data frame in long format.
#' @param keys A character vector of key column names.
#' @param item_col A single column name that contains item names.
#' @param value_col A single column name that contains values to spread.
#' @param items Optional character vector of items to keep in wide format.
#'   If provided, output item columns follow this order and missing items are
#'   kept as `NA` columns.
#' @param agg_fun Aggregation function used when key-item combinations have
#'   multiple values. Default is `get_valid()`.
#'
#' @returns A wide-format data frame.
#' @export
#' @examples
#' df <- data.frame(
#'   id = c(1, 1, 1, 2, 2),
#'   visit = c("v1", "v1", "v1", "v1", "v1"),
#'   item = c("A", "A", "B", "A", "C"),
#'   value = c(3, 5, 2, 1, 9)
#' )
#'
#' to_wide(
#'   df,
#'   keys = c("id", "visit"),
#'   item_col = "item",
#'   value_col = "value",
#'   items = c("A", "B", "C"),
#'   agg_fun = max
#' )
to_wide <- function(df, keys, item_col, value_col, items = NULL, agg_fun = get_valid) {
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.")
  }
  if (!is.character(keys) || length(keys) == 0) {
    stop("`keys` must be a non-empty character vector.")
  }
  if (!is.character(item_col) || length(item_col) != 1) {
    stop("`item_col` must be a single character string.")
  }
  if (!is.character(value_col) || length(value_col) != 1) {
    stop("`value_col` must be a single character string.")
  }
  if (!is.function(agg_fun)) {
    stop("`agg_fun` must be a function.")
  }

  required_cols <- unique(c(keys, item_col, value_col))
  missing_cols <- setdiff(required_cols, colnames(df))
  if (length(missing_cols) > 0) {
    stop("Missing columns in `df`: ", paste(missing_cols, collapse = ", "))
  }

  if (!is.null(items)) {
    if (!is.character(items)) {
      stop("`items` must be NULL or a character vector.")
    }
    items <- unique(items)
    df <- df %>%
      dplyr::filter(!!rlang::sym(item_col) %in% items) %>%
      dplyr::mutate(!!rlang::sym(item_col) := factor(!!rlang::sym(item_col), levels = items))
  }

  res <- df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(keys, item_col)))) %>%
    dplyr::summarise(.value = agg_fun(.data[[value_col]]), .groups = "drop") %>%
    tidyr::pivot_wider(
      names_from = !!rlang::sym(item_col),
      values_from = .value,
      names_expand = !is.null(items)
    )

  if (!is.null(items)) {
    res <- res %>%
      dplyr::select(dplyr::all_of(keys), dplyr::any_of(items))
  }

  res
}

#' Keep string segment by regex keyword position
#' @description Desensitize a character vector by removing the unneeded part of each string.
#'   The retained part is determined by keyword matches from a regular expression.
#' @param x A character vector.
#' @param keyword A regular expression keyword pattern.
#' @param from Left boundary of retained text:
#'   - `"start"`: start of the string.
#'   - `"first"`: first keyword match.
#'   - `"last"`: last keyword match.
#' @param to Right boundary of retained text:
#'   - `"first"`: first keyword match.
#'   - `"last"`: last keyword match.
#'   - `"end"`: end of the string.
#' @param include_keyword Logical. Whether to include the keyword match used as split point
#'   in output.
#'
#' @returns A character vector with retained text only.
#' @export
#' @examples
#' urls <- c(
#'   "https://hospital.example.com/patient/123?token=abc",
#'   "https://trial.example.org/visit/456"
#' )
#' # Keep domain only
#' keep_by_keyword(urls, "com|org|net", from = "start", to = "last", include_keyword = TRUE)
#'
#' ids <- c("SITE-2026-0001", "CTR-2025-0912")
#' # Keep site prefix before first '-'
#' keep_by_keyword(ids, "-", from = "start", to = "first", include_keyword = FALSE)
keep_by_keyword <- function(x,
                            keyword,
                            from = c("start", "first", "last"),
                            to = c("first", "last", "end"),
                            include_keyword = TRUE) {
  from <- match.arg(from)
  to <- match.arg(to)

  if (!is.character(keyword) || length(keyword) != 1 || is.na(keyword) || keyword == "") {
    stop("`keyword` must be one non-empty regular expression string")
  }

  if (!is.character(x)) {
    x <- as.character(x)
  }
  if (!is.logical(include_keyword) || length(include_keyword) != 1 || is.na(include_keyword)) {
    stop("`include_keyword` must be one non-NA logical value")
  }
  if (c(start = 1L, first = 2L, last = 3L)[[from]] > c(first = 2L, last = 3L, end = 4L)[[to]]) {
    stop("`from` must not be after `to`")
  }
  if(from =="start" && to == "end") {
    warning("`from = 'start'` and `to = 'end'` will keep the whole string, no keyword filtering applied.")
    return(x)
  }

  vapply(
    x,
    FUN.VALUE = character(1),
    USE.NAMES = FALSE,
    FUN = function(s) {
      if (is.na(s)) {
        return(NA_character_)
      }

      m <- gregexpr(keyword, s, perl = TRUE)[[1]]
      if (length(m) == 1 && m[1] == -1) {
        return("")
      }
      mlen <- attr(m, "match.length")

      first_start <- m[1]
      first_end <- m[1] + mlen[1] - 1
      last_start <- m[length(m)]
      last_end <- m[length(m)] + mlen[length(mlen)] - 1
      n <- nchar(s)

      start_pos <- switch(from,
        start = 1L,
        first = if (include_keyword) first_start else first_end + 1L,
        last = if (include_keyword) last_start else last_end + 1L
      )
      end_pos <- switch(to,
        first = if (include_keyword) first_end else first_start - 1L,
        last = if (include_keyword) last_end else last_start - 1L,
        end = n
      )

      if (start_pos > end_pos || start_pos > n || end_pos < 1L) {
        return("")
      }
      substr(s, max(1L, start_pos), min(n, end_pos))
    }
  )
}

#' Unmake names
#' @description Inverse function of `make.names`. You can use `make.names` to make colnames legal for
#'   subsequent processing and analysis in R. Then use this function to switch back for publication.
#' @param x A vector of names generated by `base::make.names()`.
#' @param ori_names A vector of original names.
#'
#' @details The function will try to match the names in `x` with the names in `ori_names`.
#'   If the names in `x` are not in `ori_names`, the function will return `NA`.
#' @returns A vector of original names.
#' @export
#' @examples
#' ori_names <- c("xx (mg/dl)", "b*x", "Covid-19")
#' x <- c(make.names(ori_names), "aa")
#' unmake_names(x, ori_names)
unmake_names <- function(x, ori_names) {
  out <- ori_names[match(x, make.names(ori_names))]
  out
}

#' Add covariates to a formula
#' @description Add covariates to a formula. Support both formula and character string.
#' @param formula A formula. Should be a formula or a character string of formula.
#' @param covars A vector of covariates.
#'
#' @returns A formula.
#' @export
#' @examples
#' formula_add_covs("y ~ a + b", c("c", "d"))
formula_add_covs <- function(formula, covars) {
  if (!class(formula) %in% c("formula", "character")) stop("`formula` should be a formula or a character string")
  if (is.null(covars)) {
    res <- formula
  } else {
    if (inherits(formula, "formula")) {
      res <- paste0(c(deparse(formula), covars), collapse = "+")
    } else {
      res <- paste0(c(formula, covars), collapse = "+")
    }
  }
  as.formula(res, env = parent.frame(n = 2))
}

#' QQ plot
#' @description QQ plot for a sample.
#' @param x A sample.
#' @param title Title of the plot.
#' @param save If TRUE, save the plot.
#' @param filename Filename of the plot.
#' @param width Width of the plot.
#' @param height Height of the plot.
#'
#' @returns A plot.
#' @export
#' @examples
#' qq_show(rnorm(100))
qq_show <- function(x,
                    title = NULL,
                    save = FALSE,
                    filename = "QQplot.png",
                    width = 2,
                    height = 2) {
  dat <- data.frame(sample = scale(na.omit(x)))
  p <- ggplot(dat, aes(sample = sample)) +
    stat_qq(size = 0.5) +
    geom_abline(slope = 1, intercept = 0, alpha = 0.3, lwd = 0.5) +
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text = element_text(colour = "black")
    )
  if (!is.null(title)) {
    p <- p + labs(title = title)
  }
  if (save) {
    ggsave(filename, p, width = width, height = height)
  }
  p
}

tidy.rms <- function(x, conf.int = FALSE, exponentiate = FALSE, ...) {
  coef <- coef(x)
  se <- sqrt(diag(vcov(x)))

  wald_z <- coef / se
  p_value <- 2 * pnorm(-abs(wald_z))

  result_df <- data.frame(
    term = names(coef),
    estimate = coef,
    std.error = se,
    statistic = wald_z,
    p.value = p_value
  )

  if (conf.int) {
    tmp <- as.data.frame(confint.default(x))
    colnames(tmp) <- c("conf.low", "conf.high")
    result_df <- cbind(result_df, tmp)
  }

  if (exponentiate) {
    result_df <- result_df %>%
      dplyr::mutate(
        dplyr::across(dplyr::any_of(c("estimate", "conf.low", "conf.high")), exp)
      )
  }
  result_df
}
