#' Check answers of multiple choice questions
#' @description Check answers of multiple choice questions by matching the answers with the correct sequence.
#' @details If `multi_column` is TRUE, the answers for Multiple-Answer Questions should be in multiple columns
#'   of logicals, with each column representing a choice. The `seq` should be a string of `"T"` and `"F"`.
#'   If `multi_column` is `FALSE`, the answers for Multiple-Answer Questions should be in one column, and the function
#'   would expect an exact match of `seq`.
#' @param dat A data frame of answers.
#' @param seq A vector of correct answers, one element for each question.
#' @param multi_column Logical, whether the multi-answers are in multiple columns.
#' @returns A data frame of boolean values, with ncol equals the number of questions.
#' @export
#' @examples
#' dat <- data.frame(Q1 = c("A", "B", "C"), Q2 = c("AD", "AE", "ABF"))
#' seq <- c("A", "AE")
#' answer_check(dat, seq)
#' dat <- data.frame(
#'   Q1 = c("A", "B", "C"), Q2.A = c(TRUE, TRUE, FALSE),
#'   Q2.B = c(TRUE, FALSE, TRUE), Q2.C = c(FALSE, TRUE, FALSE)
#' )
#' seq <- c("A", "TFT")
#' answer_check(dat, seq, multi_column = TRUE)
answer_check <- function(dat, seq, multi_column = FALSE) {
  if ((multi_column && ncol(dat) != sum(sapply(seq, str_length))) ||
    (!multi_column && ncol(dat) != length(seq))) {
    stop("width not equal!")
  }
  icol <- 0
  res <- data.frame(matrix(NA, nrow = nrow(dat), ncol = length(seq)))
  for (i in seq_along(seq)) {
    string <- seq[i]
    if (multi_column) {
      l <- str_length(string)
      tmp <- data.frame(dat[, 1:l + icol])
      if (inherits(tmp[, 1], "logical")) {
        for (j in 1:l) {
          x <- tmp[, j]
          tmp[which(x), j] <- "T"
          tmp[which(!x), j] <- "F"
        }
      }
      tmp <- apply(tmp, 1, paste0, collapse = "")
      icol <- icol + l
    } else {
      tmp <- dat[, i]
    }
    res[, i] <- tmp == string
  }
  res[is.na(res)] <- FALSE
  return(res)
}
