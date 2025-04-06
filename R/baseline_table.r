#' Get variable types for baseline table
#' @description Automatic variable type and method determination for baseline table.
#' @param data A data frame.
#' @param strata A character string indicating the column name of the strata variable.
#' @param norm_test_by_group A logical value indicating whether to perform normality tests by group.
#' @param omit_factor_above An integer indicating the maximum number of levels for a variable to be 
#'   considered a factor.
#' @param num_to_factor An integer. Numerical variables with number of unique values below or equal 
#'   to this value would be considered a factor.
#' @param save_qqplots A logical value indicating whether to save QQ plots. Sometimes the normality
#'   tests do not work well for some variables, and the QQ plots can be used to check the distribution.
#' @param folder_name A character string indicating the folder name for saving QQ plots.
#'
#' @returns An object from class `var_types`, which is just list containing the following elements:
#'   \item{factvars}{A character vector of variables that are factors.}
#'   \item{exactvars}{A character vector of variables that require fisher exact test.}
#'   \item{nonvars}{A character vector of variables that are nonnormal.}
#'   \item{omitvars}{A character vector of variables that are excluded form the baseline table.}
#'   \item{strata}{A character vector of the strata variable.}
#' @note This function performs normality tests on the variables in the data frame and determines 
#'   whether they are normal. This is done by performing Shapiro-Wilk, Lilliefors, Anderson-Darling,
#'   Jarque-Bera, and Shapiro-Francia tests. If at least two of these tests indicate that the variable
#'   is nonnormal, then it is considered nonnormal. To alieviate the problem that normality tests become
#'   too sensitive when sample size gets larger, the alpha level is determined by an experience formula 
#'   that decrease with sample size.
#' @note This function also marks the factor variables that require fisher exact tests if any cell haves 
#'   expected frequency less than or equal to 5. Note that this criterion less strict than the commonly used one.
#'   
#' @export
#' @examples
#' data(cancer, package = "survival")
#' get_var_types(cancer, strata = "sex")
#'
#' var_types <- get_var_types(cancer, strata = "sex", save_qqplots = T)
#' # for some reason we want the variable "pat.karno" ro be considered normal.
#' var_types$nonvars <- setdiff(var_types$nonvars, "pat.karno")
get_var_types <- function(data, strata = NULL, norm_test_by_group = T, omit_factor_above = 20,
                          num_to_factor = 5, save_qqplots = F, folder_name = "qqplots") {
  if (save_qqplots && !file.exists(folder_name)) {
    dir.create(folder_name)
  }

  dat_list <- list()
  normal_tests <- list(
    `Shapiro-Wilk` = shapiroTest,
    `Lilliefors` = lillieTest,
    `Anderson-Darling` = adTest,
    `Jarque-Bera` = jarqueberaTest,
    `Shapiro-Francia` = sfTest
  )
  if (is.null(strata) || !norm_test_by_group) {
    dat_list[[1]] <- data
  } else {
    tmp <- data[[strata]]
    if (is.null(tmp)) {
      stop("strata not in data!")
    }
    groups <- na.omit(unique(tmp))
    for (i in seq_along(groups)) {
      dat_list[[i]] <- filter(data, tmp == groups[i])
    }
  }
  alphas <- sapply(sapply(dat_list, nrow), alpha_by_n)
  nonvars <- c()
  factvars <- c()
  exactvars <- c()
  omitvars <- c()
  vars <- colnames(data)
  for (var in vars) {
    if ((length(na.omit(unique(data[[var]]))) <= num_to_factor) || !is.numeric(data[[var]])) {
      if ((!is.numeric(data[[var]])) && (length(na.omit(unique(data[[var]]))) > omit_factor_above)) {
        omitvars <- union(omitvars, var)
        warning(paste0(var, " excluded due to too many levels."))
      } else {
        factvars <- union(factvars, var)
        if (any(table(data[, var]) <= 5)) {
          exactvars <- union(exactvars, var)
        }else if(is.null(strata)) {
          next
        }else {
          x = table(data[, var], data[, strata])
          nr <- as.integer(nrow(x))
          nc <- as.integer(ncol(x))
          if (is.na(nr) || is.na(nc) || is.na(nr * nc)) 
              stop("invalid nrow(x) or ncol(x)", domain = NA)
          sr <- rowSums(x)
          sc <- colSums(x)
          E <- outer(sr, sc) / sum(x)
          if (any(E <= 5)) {
            exactvars <- union(exactvars, var)
          }
        }
      }
    } else {
      all.pos <- all(data[[var]] >= 0, na.rm = T)
      for (i in seq_along(dat_list)) {
        dat <- dat_list[[i]]
        x <- c(scale(dat[, var]))
        ps <- c()
        for (j in seq_along(normal_tests)) {
          ps[j] <- tryCatch(
            {
              tmp <- normal_tests[[j]](x)@test
              if (names(normal_tests)[j] == "Anderson-Darling" & tmp$p.value[1] == 1 & tmp$statistic > 200) {
                0
              } else {
                tmp$p.value[1]
              }
            },
            error = function(e) {
              NA
            }
          )
        }
        if ((all.pos && (sd(x, na.rm = T) < mean(x, na.rm = T))) |
          (sum(ps < alphas[i], na.rm = T) >= sum(!is.na(ps)) - 2)) {
          nonvars <- union(nonvars, var)
          prefix <- "nonnormal"
        } else {
          prefix <- "normal"
        }
        if (save_qqplots) {
          title <- var
          if (!is.null(strata)) {
            title <- paste(title, "by", strata, groups[i], sep = "_")
          }
          qq_name <- paste0(paste(prefix, title, sep = "_"), ".png")
          qq_show(x, title = title, save = TRUE, filename = paste0(folder_name, "/", qq_name))
        }
      }
    }
  }
  res <- list(factvars = factvars, exactvars = exactvars, nonvars = nonvars, omitvars = omitvars, strata = strata)
  class(res) <- "var_types"
  res
}

# 基线表格一键生成
baseline.table <- function(data, var_types = NULL, strata = NULL, vars = setdiff(colnames(data), strata),
                           factor.vars = NULL, exact.vars = NULL, nonnormal.vars = NULL, seed = NULL,
                           filename = "baseline.csv", p.adjust.method = "BH", ...) {
  if (!is.null(var_types) && !"var_types" %in% class(var_types)) {
    stop("Invalid 'var_types' arguement! Please use result from get_var_types function.")
  }
  if (!grepl(".csv", filename)) {
    stop("please save as .csv file")
  }
  if (is.null(strata) & !is.null(var_types)) {
    strata <- var_types$strata
  }
  if (is.null(factor.vars) & !is.null(var_types)) {
    factor.vars <- var_types$factvars
  }
  if (is.null(exact.vars) & !is.null(var_types)) {
    exact.vars <- var_types$exactvars
  }
  if (is.null(nonnormal.vars) & !is.null(var_types)) {
    nonnormal.vars <- var_types$nonvars
  }
  if (!is.null(var_types$omitvars)) {
    vars <- setdiff(vars, var_types$omitvars)
  }
  data <- data[!is.na(data[[strata]]), ]

  load_packages(c("tableone", "stringr", "tidyr"))
  if (is.null(strata)) {
    tab1 <- CreateTableOne(
      vars = vars, argsNormal = list(var.equal = F),
      argsExact = list(workspace = 2 * 10^5, simulate.p.value = TRUE, B=1e4),
      data = data, factorVars = factor.vars, addOverall = TRUE
    )
  } else {
    tab1 <- CreateTableOne(
      vars = vars, strata = strata, argsNormal = list(var.equal = F),
      argsExact = list(workspace = 2 * 10^5, simulate.p.value = TRUE, B=1e4),
      data = data, factorVars = factor.vars, addOverall = TRUE
    )
  }
  tab4Mat <- print(tab1,
    nonnormal = nonnormal.vars, exact = exact.vars,
    quote = FALSE, noSpaces = TRUE, printToggle = FALSE, ...
  )
  write.csv(tab4Mat, file = filename)

  missing.df <- as.data.frame(is.na(data))
  for (i in 1:ncol(missing.df)) {
    missing.df[, i] <- factor(missing.df[, i], levels = c(F, T))
  }
  if (is.null(strata)) {
    tab2 <- CreateTableOne(
      vars = vars,
      data = missing.df, addOverall = TRUE
    )
  } else {
    missing.df$.strata <- data[[strata]]
    tab2 <- CreateTableOne(
      vars = vars, strata = ".strata",
      data = missing.df, addOverall = TRUE
    )
  }
  tab2Mat <- print(tab2, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, ...)
  write.csv(tab2Mat, file = str_replace(filename, ".csv", "_missing.csv"))

  if (length(na.omit(unique(data[[strata]]))) > 2) {
    g <- factor(data[[strata]])
    pairwise.result <- data.frame()
    for (var in vars) {
      if (var %in% exact.vars) {
        cont.table <- table(data[[var]], g)
        compare.levels <- function(i, j) {
          tryCatch(
            {
              fisher.test(cont.table[, c(i, j)], simulate.p.value = TRUE, B=1e4)$p.value
            },
            error = function(e) {
              NA
            }
          )
        }
        pt <- pairwise.table(compare.levels, levels(g), p.adjust.method)
      } else if (var %in% factor.vars) {
        cont.table <- table(data[[var]], g)
        compare.levels <- function(i, j) {
          chisq.test(cont.table[, c(i, j)])$p.value
        }
        pt <- pairwise.table(compare.levels, levels(g), p.adjust.method)
      } else if (var %in% nonnormal.vars) {
        compare.levels <- function(i, j) {
          xi <- data[as.integer(g) == i, var]
          xj <- data[as.integer(g) == j, var]
          wilcox_test_pval(xi, xj)
        }
        pt <- pairwise.table(compare.levels, levels(g), p.adjust.method)
      } else {
        pt <- pairwise.t.test(data[[var]], g, p.adjust.method = p.adjust.method)$p.value
      }
      tmp <- as.data.frame(as.table(pt))
      tmp$Var1 <- factor(tmp$Var1, levels = levels(g))
      tmp$Var2 <- factor(tmp$Var2, levels = levels(g))
      p_values_long <- tmp %>%
        filter(as.numeric(Var1) > as.numeric(Var2)) %>%
        mutate(Comparison = paste(Var1, Var2, sep = "_")) %>%
        select(Comparison, Freq)
      p_values_wide <- as.data.frame(pivot_wider(p_values_long, names_from = Comparison, values_from = Freq))
      pairwise.result <- rbind(pairwise.result, p_values_wide)
    }
    rownames(pairwise.result) <- vars
    write.csv(pairwise.result, file = str_replace(filename, ".csv", "_pairwise.csv"))
  }
}

# Calculate alpha by sample size with an experience formula
alpha_by_n <- function(n) {
  if (n < 100) {
    0.05
  } else {
    10^(-exp((log10(n) - 2) * 2 + log(-log10(0.05))))
  }
}