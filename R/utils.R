#' @import survival
#' @import broom
#' @import ggplot2
#' @import dplyr
#' @import stringr
#' @import rlang
#' @import forestploter
#' @importFrom magrittr %>%
#' @importFrom tibble as_tibble

# Load packages, install if necessary
load_packages <- function(pkgs) {
  for (pkg in pkgs) {
    if (!require(pkg, character.only = T, quietly = T)) {
      install.packages(pkg, character.only = T)
      require(pkg, character.only = T, quietly = T)
    }
  }
}

# Compare the order of two elements in a list of vectors
#   based on the first occurrence
.calculate_order <- function(elem1, elem2, vectors) {
  count_before <- 0
  count_after <- 0
  for (vec in vectors) {
    idx1 <- match(elem1, vec)
    idx2 <- match(elem2, vec)
    if (!is.na(idx1) && !is.na(idx2)) {
      if (idx1 < idx2) {
        count_before <- count_before + 1
      } else if (idx1 > idx2) {
        count_after <- count_after + 1
      }
    }
  }
  if (count_before > count_after) {
    return(1)
  } else if (count_before < count_after) {
    return(-1)
  } else {
    return(0)
  }
}