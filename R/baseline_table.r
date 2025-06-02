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
#'   \item{factor_vars}{A character vector of variables that are factors.}
#'   \item{exact_vars}{A character vector of variables that require fisher exact test.}
#'   \item{nonnormal_vars}{A character vector of variables that are nonnormal.}
#'   \item{omit_vars}{A character vector of variables that are excluded form the baseline table.}
#'   \item{strata}{A character vector of the strata variable.}
#' @note This function performs normality tests on the variables in the data frame and determines
#'   whether they are normal. This is done by performing Shapiro-Wilk, Lilliefors, Anderson-Darling,
#'   Jarque-Bera, and Shapiro-Francia tests. If at least two of these tests indicate that the variable
#'   is nonnormal, then it is considered nonnormal. To alleviate the problem that normality tests become
#'   too sensitive when sample size gets larger, the alpha level is determined by an experience formula
#'   that decrease with sample size.
#' @note This function also marks the factor variables that require fisher exact tests if any cell haves
#'   expected frequency less than or equal to 5. Note that this criterion less strict than the commonly
#'   used one.
#'
#' @export
#' @examples
#' data(cancer, package = "survival")
#' get_var_types(cancer, strata = "sex") # set save_qqplots = TRUE to check the QQ plots
#'
#' var_types <- get_var_types(cancer, strata = "sex")
#' # for some reason we want the variable "pat.karno" ro be considered normal.
#' var_types$nonnormal_vars <- setdiff(var_types$nonnormal_vars, "pat.karno")
get_var_types <- function(data, strata = NULL, norm_test_by_group = TRUE, omit_factor_above = 20,
                          num_to_factor = 5, save_qqplots = FALSE, folder_name = "qqplots") {
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
      dat_list[[i]] <- dplyr::filter(data, tmp == groups[i])
    }
  }
  alphas <- sapply(sapply(dat_list, nrow), alpha_by_n)
  nonnormal_vars <- c()
  factor_vars <- c()
  exact_vars <- c()
  omit_vars <- c()
  vars <- colnames(data)
  for (var in vars) {
    if (length(na.omit(data[[var]])) == 0) {
      omit_vars <- union(omit_vars, var)
      next
    }
    if ((length(na.omit(unique(data[[var]]))) <= num_to_factor) || !is.numeric(data[[var]])) {
      if ((!is.numeric(data[[var]])) && (length(na.omit(unique(data[[var]]))) > omit_factor_above)) {
        omit_vars <- union(omit_vars, var)
        warning(paste0(var, " excluded due to too many levels."))
      } else {
        factor_vars <- union(factor_vars, var)
        if (any(table(data[[var]]) <= 5)) {
          exact_vars <- union(exact_vars, var)
        } else if (is.null(strata)) {
          next
        } else {
          x <- table(data[[var]], data[[strata]])
          if (sum(x) == 0) {
            omit_vars <- union(omit_vars, var)
            next
          }
          sr <- rowSums(x)
          sc <- colSums(x)
          E <- outer(sr, sc) / sum(x)
          if (any(E <= 5)) {
            exact_vars <- union(exact_vars, var)
          }
        }
      }
    } else {
      all.pos <- all(data[[var]] >= 0, na.rm = TRUE)
      for (i in seq_along(dat_list)) {
        dat <- dat_list[[i]]
        x <- c(scale(dat[[var]]))
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
        if ((all.pos && (sd(x, na.rm = TRUE) < mean(x, na.rm = TRUE))) ||
          (sum(ps < alphas[i], na.rm = TRUE) >= sum(!is.na(ps)) - 2)) {
          nonnormal_vars <- union(nonnormal_vars, var)
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
  res <- list(
    factor_vars = factor_vars, exact_vars = exact_vars, nonnormal_vars = nonnormal_vars,
    omit_vars = omit_vars, strata = strata
  )
  class(res) <- "var_types"
  res
}

#' Create a baseline table for a dataset.
#' @description Create a baseline table and a table of missing values. If the strata variable has more
#'   than 2 levels, a pairwise comparison table will also be created.
#' @param data A data frame.
#' @param var_types An object from class `var_types` returned by `get_var_types` function.
#' @param strata A variable to stratify the table. Overwrites the strata variable in `var_types`.
#' @param vars A vector of variables to include in the table.
#' @param factor_vars A vector of factor variables. Overwrites the factor variables in `var_types`.
#' @param exact_vars A vector of variables to test for exactness. Overwrites the exact variables in `var_types`.
#' @param nonnormal_vars A vector of variables to test for normality. Overwrites the nonnormal variables in `var_types`.
#' @param smd A logical value indicating whether to include SMD in the table. Passed to `tableone::print.TableOne`.
#' @param seed A seed for the random number generator. This seed can be set for consistent simulation when
#'   performing fisher exact tests.
#' @param omit_missing_strata A logical value indicating whether to omit missing values in the strata variable.
#' @param save_table A logical value indicating whether to save the result tables.
#' @param filename The name of the file to save the table. The file names for accompanying tables will
#'   be the same as the main table, but with "_missing" and "_pairwise" appended.
#' @param multiple_comparison_test A logical value indicating whether to perform multiple comparison tests. Variables in
#'   `factor_vars` and `exact_vars` are tested with pairwise `chisq.test` or `fisher.test`, and other variables are
#'   tested with `rstatix::dunn_test` or `rstatix::games_howell_test`.
#' @param p_adjust_method The method to use for p-value adjustment for pairwise comparison. Default is "BH".
#'   See `?p.adjust.methods`.
#' @param ... Additional arguments passed to `tableone::print.TableOne`.
#' @returns A list containing the baseline table and accompanying tables.
#' @export
#' @examples
#' withr::with_tempdir(
#'   {
#'     data(cancer, package = "survival")
#'     var_types <- get_var_types(cancer, strata = "sex")
#'     baseline_table(cancer, var_types = var_types, filename = "baseline.csv")
#'
#'     # baseline table with pairwise comparison
#'     cancer$ph.ecog_cat <- factor(cancer$ph.ecog,
#'       levels = c(0:3),
#'       labels = c("0", "1", ">=2", ">=2")
#'     )
#'     var_types <- get_var_types(cancer, strata = "ph.ecog_cat")
#'     baseline_table(cancer, var_types = var_types, filename = "baselineV2.csv")
#'     print(paste0("files saved to: ", getwd()))
#'   },
#'   clean = FALSE
#' )
baseline_table <- function(data, var_types = NULL, strata = NULL, vars = NULL, factor_vars = NULL, exact_vars = NULL,
                           nonnormal_vars = NULL, seed = NULL, omit_missing_strata = FALSE, save_table = TRUE,
                           filename = NULL, multiple_comparison_test = TRUE, p_adjust_method = "BH", smd = FALSE, ...) {
  if (!is.null(var_types) && !"var_types" %in% class(var_types)) {
    stop("Invalid 'var_types' arguement! Please use result from get_var_types function.")
  }
  if (is.null(strata) && !is.null(var_types)) strata <- var_types$strata
  if (!is.null(strata)) {
    data[[strata]] <- as.factor(data[[strata]])
    data[[strata]] <- droplevels(data[[strata]])
    levels(data[[strata]]) <- paste0(strata, ": ", levels(data[[strata]]))

    if (omit_missing_strata) {
      data <- data[!is.na(data[[strata]]), ]
    }
  }
  if (is.null(factor_vars) && !is.null(var_types)) factor_vars <- var_types$factor_vars
  if (is.null(exact_vars) && !is.null(var_types)) exact_vars <- var_types$exact_vars
  if (is.null(nonnormal_vars) && !is.null(var_types)) nonnormal_vars <- var_types$nonnormal_vars
  if (is.null(vars)) vars <- setdiff(colnames(data), strata)
  if (!is.null(var_types$omit_vars)) vars <- setdiff(vars, var_types$omit_vars)
  if (is.null(seed)) set.seed(seed)
  if (is.null(filename)) filename <- paste0("baseline_by_", strata, ".csv")
  if (!endsWith(filename, ".csv")) stop("please save as `.csv` file")
  factor_vars <- union(factor_vars, exact_vars)

  if (is.null(strata)) {
    tab1 <- tableone::CreateTableOne(
      vars = vars, argsNormal = list(var.equal = FALSE),
      argsExact = list(workspace = 2 * 10^5, simulate.p.value = TRUE, B = 1e4),
      data = data, factorVars = factor_vars, smd = smd, addOverall = TRUE
    )
  } else {
    tab1 <- tableone::CreateTableOne(
      vars = vars, strata = strata, argsNormal = list(var.equal = FALSE),
      argsExact = list(workspace = 2 * 10^5, simulate.p.value = TRUE, B = 1e4),
      data = data, factorVars = factor_vars, smd = smd, addOverall = TRUE
    )
  }
  printed_table <- print(tab1,
    nonnormal = nonnormal_vars, exact = exact_vars, smd = smd,
    quote = FALSE, noSpaces = TRUE, printToggle = FALSE, ...
  )
  if (save_table) {
    write.csv(printed_table, file = filename)
  }

  missing_df <- as.data.frame(is.na(data))
  for (i in seq_len(ncol(missing_df))) {
    missing_df[, i] <- factor(missing_df[, i], levels = c(FALSE, TRUE))
  }
  if (is.null(strata)) {
    tab2 <- tableone::CreateTableOne(
      vars = vars,
      data = missing_df, addOverall = TRUE
    )
  } else {
    missing_df[[strata]] <- data[[strata]]
    tab2 <- tableone::CreateTableOne(
      vars = vars, strata = strata,
      data = missing_df, addOverall = TRUE
    )
  }
  missing_table <- print(tab2, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, smd = smd, ...)
  if (save_table) {
    write.csv(missing_table, file = str_replace(filename, ".csv", "_missing.csv"))
  }
  if (!is.null(strata) && length(na.omit(unique(data[[strata]]))) > 2) {
    g <- factor(data[[strata]])
    pairwise_result <- data.frame()
    for (var in vars) {
      if (var %in% exact_vars) {
        cont_table <- table(data[[var]], g)
        compare_levels <- function(i, j) {
          tryCatch(
            {
              fisher.test(cont_table[, c(i, j)], simulate.p.value = TRUE, B = 1e4)$p.value
            },
            error = function(e) {
              NA
            }
          )
        }
        pt <- pairwise.table(compare_levels, levels(g), p_adjust_method)
      } else if (var %in% factor_vars) {
        cont_table <- table(data[[var]], g)
        compare_levels <- function(i, j) {
          chisq.test(cont_table[, c(i, j)])$p.value
        }
        pt <- pairwise.table(compare_levels, levels(g), p_adjust_method)
      } else if (var %in% nonnormal_vars) {
        pt <- rstatix::dunn_test(data, as.formula(paste0(var, "~", strata)), p.adjust.method = p_adjust_method)
      } else {
        pt <- rstatix::games_howell_test(data, as.formula(paste0(var, "~", strata)))
      }

      if (var %in% c(exact_vars, factor_vars)) {
        tmp <- as.data.frame(as.table(pt))
        tmp$Var1 <- factor(tmp$Var1, levels = levels(g))
        tmp$Var2 <- factor(tmp$Var2, levels = levels(g))
        p_values_long <- tmp %>%
          dplyr::filter(as.numeric(Var1) > as.numeric(Var2)) %>%
          mutate(comparison = paste(Var2, Var1, sep = "_"), p.adj = Freq) %>%
          select(comparison, p.adj)
      } else {
        p_values_long <- pt %>%
          reframe(comparison = paste(group1, group2, sep = "_"), p.adj)
      }
      p_values_wide <- as.data.frame(pivot_wider(p_values_long, names_from = comparison, values_from = p.adj))
      pairwise_result <- rbind(pairwise_result, p_values_wide)
    }
    rownames(pairwise_result) <- vars
    if (save_table) {
      write.csv(pairwise_result, file = str_replace(filename, ".csv", "_pairwise.csv"))
    }
    return(list(baseline = printed_table, missing = missing_table, pairwise = pairwise_result))
  } else {
    return(list(baseline = printed_table, missing = missing_table))
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
