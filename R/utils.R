#' @import rms
#' @import ggplot2
#' @import stats
#' @import stringr
#' @import rlang
#' @import survival
#' @importFrom utils read.csv write.csv
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom dplyr select mutate all_of group_by summarise ungroup reframe ntile n across
#' @importFrom fBasics shapiroTest lillieTest adTest jarqueberaTest sfTest
NULL

utils::globalVariables(c(
  "Var1", "Var2", "Freq", "comparison", "p.adj", "group1", "group2",
  "name", "value", "decile", "obsRate", "predRate", "label",
  ".predictor", ".group_var", "yhat", "lower", "upper",
  "xmin", "xmax", "den", "Group", "text", "level", "subject",
  "quant", "quant_val"
))

#' default color palette for `clinpubr` plots
#' @export
emp_colors <- c(
  "#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854",
  "#FFD92F", "#E5C494", "#B3B3B3", "#ad4c5e", "#474747"
)

wrap_backticks <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  ifelse(!grepl("^`", x) & !grepl("`$", x), paste0("`", x, "`"), x)
}

# Create a formula for coxph or glm
create_formula <- function(y, predictor, group_var = NULL, time = NULL, covars = NULL, rcs_knots = NULL,
                           interaction = FALSE, wrap_backtick = TRUE) {
  if (wrap_backtick) {
    y <- wrap_backticks(y)
    predictor <- wrap_backticks(predictor)
    group_var <- wrap_backticks(group_var)
    time <- wrap_backticks(time)
    covars <- wrap_backticks(covars)
  }
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

  as.formula(formula_add_covs(paste0(outcome, "~", predictor), covars), env = parent.frame(n = 2))
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
remove_conflict <- function(x, y, silent = FALSE) {
  if (any(x %in% y)) {
    if (!silent) warning(paste0(x[x %in% y], collapse = ", "), " are removed to resolve variable conflict.")
    x <- x[!x %in% y]
  }
  if (length(x) == 0) x <- NULL
  x
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
