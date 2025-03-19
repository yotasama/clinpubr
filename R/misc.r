#' @include utils.R
NULL
#' Generate code from string vector
#' Genearte the code that can be used to generate the string vector.
#' @param x A string vector.
#'
#' @returns A string that contains the code to generate the vector.
#' @export
#' @examples
#' vec2code(colnames(mtcars))
vec2code <- function(x) {
  paste0("c('", paste0(x, collapse = "','"), "')")
}


#' Format p-value for publication
#' @param p The numerical p values to be formated.
#'
#' @returns A string vector of formated p values.
#' @export
#' @examples
#' format_pval(c(0.001, 0.0001, 0.05, 0.1123456))
format_pval <- function(p) {
  base::format.pval(p, digits = 1, nsmall = 2, eps = 1e-3)
}

#' @importFrom DescTools Mode
NULL
#' Calculate the first mode
#' @description  Calculate the first mode of a vector. Ignore NA values.
#'   Can be used if any mode is acceptable.
#' @param x A vector.
#'
#' @returns The first mode of the vector.
#' @export
#' @examples
#' first_mode(c(1, 1, 2, 2, 3, 3, 3, NA, NA, NA))
first_mode <- function(x) {
  x <- na.omit(x)
  l <- length(unique(x))
  if (l == 0) {
    NA
  } else if (l == 1 | l == length(x)) {
    x[1]
  } else {
    Mode(x)[1]
  }
}

#' Merging vectors while maintaining order
#' @description Merge multiple vectors into one while trying to maintain
#'   the order of elements in each vector. The relative order of elements
#'   is compared by their first occurrence in the vectors in the list.
#'   This function is useful when merging slightly different vectors,
#'   such as questionares of different version.
#' @param vectors A list of vectors to be merged.
#'
#' @returns A vector that tried to keep the order.
#' @export
#' @examples
#' merge_ordered_vectors(list(c(1,3,4,5,7,10),c(2,5,6,7,8),c(1,7,5,10)))
merge_ordered_vectors <- function(vectors) {
  all_elements <- unique(unlist(vectors))

  # bubble sort all_elements based on the order of vectors
  n <- length(all_elements)
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


#' Split multichoice data into columns
#' @description Split multichoice data into columns, each new column consists of
#'   booleans whether a choice is presented.
#' @param df A data frame.
#' @param quest_cols A vector of column names that contain multichoice data.
#' @param split A string to split the data. Default is "".
#' @param remove_space A boolean to remove space in the data. Default is TRUE.
#' @param link A string to link the column name and the option. Default is "_".
#' @param remove_cols A boolean to remove the original columns. Default is TRUE.
#'
#' @returns A data frame with additional columns.
#' @export
#' @examples
#' df <- data.frame(q1 = c("a b", "c d a", "b a"), q2 = c("a b", "a c", "d"))
#' split_multichoice(df, quest_cols = c("q1", "q2"))
split_multichoice <- function(df, quest_cols, split = "", remove_space = T, 
                              link = "_", remove_cols = T) {

  for (col in quest_cols) {
    if (remove_space) {
      df[, col] <- str_remove_all(df[, col], " ")
    }

    if (str_length(split) <= 1) {
      tmp_split <- strsplit(df[, col], split, fixed = TRUE)
    } else {
      tmp_split <- strsplit(df[, col], split)
    }

    unique_options <- na.omit(unique(unlist(tmp_split)))

    for (opt in unique_options) {
      df[, paste0(col, link, opt)] <- sapply(tmp_split, function(x) opt %in% x)
    }
  }
  if (remove_cols) {
    df <- df[, !(colnames(df) %in% quest_cols)]
  }

  return(df)
}