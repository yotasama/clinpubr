generate_illegal_colnames <- function(n) {
  # 定义一些常见的非法列名模式
  patterns <- c(
    # 以数字开头的列名
    "^[0-9]",
    # 包含空格的列名
    " ",
    # 包含特殊字符的列名
    "[!@#$%^&*()+=|<>?{}~-]",
    # 以点开头的列名
    "^\\.",
    # 保留字作为列名
    "^if$|^else$|^repeat$|^while$|^function$|^for$|^in$|^next$|^break$|^TRUE$|^FALSE$|^NULL$|^Inf$|^NaN$|^NA$"
  )

  # 生成非法列名
  illegal_colnames <- character(n)

  for (i in 1:n) {
    # 随机选择一种非法模式
    pattern <- sample(patterns, 1)

    # 根据模式生成非法列名
    if (grepl("^[0-9]", pattern)) {
      illegal_colnames[i] <- paste0(sample(0:9, 1), sample(letters, 5, replace = TRUE))
    } else if (grepl(" ", pattern)) {
      illegal_colnames[i] <- paste(sample(letters, 2), collapse = " ")
    } else if (grepl("[!@#$%^&*()+=|<>?{}~-]", pattern)) {
      illegal_colnames[i] <- paste0(sample(letters, 1), sample(c("!", "@", "#", "$", "%", "^", "&", "*"), 1), sample(letters, 1))
    } else if (grepl("^\\.", pattern)) {
      illegal_colnames[i] <- paste0(".", sample(letters, 5, replace = TRUE))
    } else {
      illegal_colnames[i] <- sample(c("if", "else", "repeat", "while", "function", "for", "in", "next", "break", "TRUE", "FALSE", "NULL", "Inf", "NaN", "NA"), 1)
    }
  }

  return(illegal_colnames)
}

# 使用示例：生成10个非法列名
ori_names <- c("xx (mg/dl)", "b*x", "Covid-19")
modified_names <- c("v1", "v2", "v3")
x <- c("v1.v2", "v3.yy", "v4")
str_match_replace(x, modified_names, ori_names)
data(cancer, package = "survival")
cancer$ph.ecog_cat <- factor(cancer$ph.ecog, levels = c(0:3), labels = c("0", "1", "≥2", "≥2"))
var_types <- get_var_types(cancer, strata = "ph.ecog_cat")
baseline_table(cancer, var_types = var_types,filename = "test2.csv")

data=cancer
strata = NULL; vars = NULL;
factor_vars = NULL; exact_vars = NULL; nonnormal_vars = NULL; seed = NULL;
omit_missing_strata = FALSE; filename = NULL; p_adjust_method = "BH"

baseline_table <- function(data, var_types = NULL, strata = NULL, vars = NULL, factor_vars = NULL, exact_vars = NULL,
                           nonnormal_vars = NULL, seed = NULL, omit_missing_strata = FALSE, filename = NULL,
                           multiple_comparison_test = TRUE, p_adjust_method = "BH", ...) {
  if (!is.null(var_types) && !"var_types" %in% class(var_types)) {
    stop("Invalid 'var_types' arguement! Please use result from get_var_types function.")
  }
  if (is.null(strata) && !is.null(var_types)) strata <- var_types$strata
  if (!is.null(strata) && omit_missing_strata) data <- data[!is.na(data[[strata]]), ]
  if (is.null(factor_vars) && !is.null(var_types)) factor_vars <- var_types$factor_vars
  if (is.null(exact_vars) && !is.null(var_types)) exact_vars <- var_types$exact_vars
  if (is.null(nonnormal_vars) && !is.null(var_types)) nonnormal_vars <- var_types$nonnormal_vars
  if (is.null(vars)) vars <- setdiff(colnames(data), strata)
  if (!is.null(var_types$omitvars)) vars <- setdiff(vars, var_types$omitvars)
  if (is.null(seed)) set.seed(seed)
  if (is.null(filename)) filename <- paste0("baseline_by", strata, ".csv")
  if (!grepl(".csv", filename)) stop("please save as .csv file")
  factor_vars <- union(factor_vars, exact_vars)

  if (is.null(strata)) {
    tab1 <- CreateTableOne(
      vars = vars, argsNormal = list(var.equal = FALSE),
      argsExact = list(workspace = 2 * 10^5, simulate.p.value = TRUE, B = 1e4),
      data = data, factorVars = factor_vars, addOverall = TRUE
    )
  } else {
    tab1 <- CreateTableOne(
      vars = vars, strata = strata, argsNormal = list(var.equal = FALSE),
      argsExact = list(workspace = 2 * 10^5, simulate.p.value = TRUE, B = 1e4),
      data = data, factorVars = factor_vars, addOverall = TRUE
    )
  }
  printed_table <- print(tab1,
                         nonnormal = nonnormal_vars, exact = exact_vars,
                         quote = FALSE, noSpaces = TRUE, printToggle = FALSE, ...
  )
  write.csv(printed_table, file = filename)

  missing_df <- as.data.frame(is.na(data))
  for (i in seq_len(ncol(missing_df))) {
    missing_df[, i] <- factor(missing_df[, i], levels = c(FALSE, TRUE))
  }
  if (is.null(strata)) {
    tab2 <- CreateTableOne(
      vars = vars,
      data = missing_df, addOverall = TRUE
    )
  } else {
    missing_df[[strata]] <- data[[strata]]
    tab2 <- CreateTableOne(
      vars = vars, strata = strata,
      data = missing_df, addOverall = TRUE
    )
  }
  printed_table <- print(tab2, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, ...)
  write.csv(printed_table, file = str_replace(filename, ".csv", "_missing.csv"))

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
        # compare_levels <- function(i, j) {
        #   xi <- data[as.integer(g) == i, var]
        #   xj <- data[as.integer(g) == j, var]
        #   wilcox_test_pval(xi, xj)
        # }
        # pt <- pairwise.table(compare_levels, levels(g), p_adjust_method)
        pt <- rstatix::dunn_test(data, as.formula(paste0(var, "~", strata)), p.adjust.method = p_adjust_method)
      } else {
        # pt <- pairwise.t.test(data[[var]], g, p.adjust.method = p_adjust_method)$p.value
        pt <- rstatix::games_howell_test(data, as.formula(paste0(var, "~", strata)), p.adjust.method = p_adjust_method)
      }

      if (var %in% c(exact_vars, factor_vars)) {
        tmp <- as.data.frame(as.table(pt))
        tmp$Var1 <- factor(tmp$Var1, levels = levels(g))
        tmp$Var2 <- factor(tmp$Var2, levels = levels(g))
        p_values_long <- tmp %>%
          filter(as.numeric(Var1) > as.numeric(Var2)) %>%
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
    write.csv(pairwise_result, file = str_replace(filename, ".csv", "_pairwise.csv"))
  }
  invisible(NULL)
}
