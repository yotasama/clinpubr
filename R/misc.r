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

#' Generate code from string vector
#' Generate the code that can be used to generate the string vector.
#' @param x A string vector.
#'
#' @returns A string that contains the code to generate the vector.
#' @export
#' @examples
#' vec2code(colnames(mtcars))
vec2code <- function(x) {
  if (length(x) == 0) {
    "c()"
  } else {
    paste0("c('", paste0(x, collapse = "','"), "')")
  }
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
first_mode <- function(x, empty_return = NA) {
  x <- na.omit(x)
  l <- length(unique(x))
  if (l == 0) {
    empty_return
  } else if (l == 1 || l == length(x)) {
    x[1]
  } else {
    DescTools::Mode(x)[1]
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
