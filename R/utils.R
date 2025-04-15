#' @import survival
#' @import rms
#' @import ggplot2
#' @import dplyr
#' @import stringr
#' @import rlang
#' @import tableone
#' @import ResourceSelection
#' @importFrom caret confusionMatrix
#' @importFrom broom tidy
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom forestploter forest
#' @importFrom fBasics shapiroTest lillieTest adTest jarqueberaTest sfTest

.color_panel <- c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854",
                  "#FFD92F", "#E5C494", "#B3B3B3", "#ad4c5e", "#474747")

# Load packages, install if necessary
load_packages <- function(pkgs) {
  for (pkg in pkgs) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      install.packages(pkg, character.only = TRUE)
      require(pkg, character.only = TRUE, quietly = TRUE)
    }
  }
}

# Create a formula for coxph or glm
create_formula <- function(y, predictor, group_var = NULL, time = NULL, covs = NULL, rcs_knots = NULL,
                           interaction = FALSE) {
  if (!is.null(time)) {
    outcome <- paste0("Surv(", time, ",", y, ")")
  } else {
    outcome <- y
  }
  if (!is.null(rcs_knots)) {
    predictor <- paste0("rcs(", predictor, ",", rcs_knots, ")")
  }
  if (!is.null(group_var)) {
    if (interaction) {
      predictor <- paste0(predictor, "*", group_var)
    } else {
      predictor <- paste0(predictor, "+", group_var)
    }
  }

  formula_add_covs(paste0(outcome, "~", predictor), covs)
}

# Convert a numeric vector to a factor
to_factor <- function(x, max_numerical_groups = 5, na_as_level = FALSE) {
  if (is.numeric(x) && (length(na.omit(unique(x))) > max_numerical_groups)) {
    x <- cut_by(x, 0.5, breaks_as_quantiles = TRUE)
  } else {
    x <- as.factor(x)
  }
  if (na_as_level) {
    levels(x) <- c(levels(x), "NA")
    x[is.na(x)] <- "NA"
  }
  x
}

# Remove conflict variables
remove_conflict <- function(x, y) {
  if (any(x %in% y)) {
    warning(paste0(x[x %in% y], collapse = ", "), " are removed to resolve variable conflict.")
    x <- x[!x %in% y]
  }
  if (length(x) == 0) x <- NULL
  x
}

# Get wilcox test p-value
wilcox_test_pval <- function(...) {
  tryCatch(
    wilcox.test(...)$p.value,
    error = function(e) {
      NA
    }
  )
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
