# 其他工具----
# 加载R包
load_packages <- function(pkgs) {
  for (pkg in pkgs) {
    if (!require(pkg, character.only = T, quietly = T)) {
      install.packages(pkg, character.only = T)
      require(pkg, character.only = T, quietly = T)
    }
  }
}

# 将向量中的NA替换为FALSE
na2false <- function(x) {
  x[is.na(x)] <- FALSE
  x
}

# 合并表格文件
combine_files <- function(path = ".", pattern = NULL, unique_only = T, reader_fun = openxlsx::read.xlsx, ...) {
  files <- list.files(path = path, pattern = pattern, full.names = T)
  if (length(files) > 0) {
    for (f in files) {
      tmp <- reader_fun(f, ...)
      if (f == files[1]) {
        dat <- tmp
      } else {
        dat <- rbind(dat, tmp)
      }
    }
    if (unique_only) {
      dat <- unique(dat)
    }
    return(dat)
  } else {
    return(NULL)
  }
}

# 输出代码形式的vector
vec2code <- function(x) {
  paste0("c('", paste0(x, collapse = "','"), "')")
}

# 通用p.val format
format_pval <- function(p) {
  base::format.pval(p, digits = 1, nsmall = 2, eps = 1e-3)
}

# 数据清洗工具箱----
load_packages(c("stringr", "DescTools"))
# 计算众数
first_mode <- function(x) {
  x <- na.omit(x)
  l <- length(unique(x))
  if (l == 0) {
    NA
  } else if (l == 1 | l == length(x)) {
    x[1]
  } else {
    DescTools::Mode(x)[1]
  }
}

# 保序合并向量
merge_ordered_vectors <- function(vectors) {
  # 提取所有元素
  all_elements <- unique(unlist(vectors))

  # 定义一个函数来计算元素对的顺序
  calculate_order <- function(elem1, elem2, vectors) {
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

  # 使用冒泡排序对元素进行排序
  n <- length(all_elements)
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      order_result <- calculate_order(all_elements[i], all_elements[j], vectors)
      if (order_result == -1) {
        temp <- all_elements[i]
        all_elements[i] <- all_elements[j]
        all_elements[j] <- temp
      }
    }
  }

  return(all_elements)
}

# 多选题拆分结果
split_multichoice <- function(df, quest_cols, split = "",
                              remove_space = T, link = "_") {
  load_packages("stringr")

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

  return(df)
}

# 按行分组匹配数据，可用于选择题打分
answer_check <- function(dat, seq, multi_column = FALSE) {
  if ((multi_column && ncol(dat) != sum(sapply(seq, str_length)) ||
    !multi_column && ncol(dat) != length(seq))) {
    stop("width not equal!")
  }
  icol <- 0
  res <- data.frame(matrix(NA, nrow = nrow(dat), ncol = length(seq)))
  for (i in seq_along(seq)) {
    string <- seq[i]
    if (multi_column) {
      l <- str_length(string)
      tmp <- data.frame(dat[, 1:l + icol])
      if (class(tmp[, 1]) == "logical") {
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

calculate_index <- function(df, ..., weight = 1, na_replace = 0) {
  conditions <- rlang::enquos(...)
  n_conds <- length(conditions)

  if (n_conds == 0) stop("Expressions must be provided")
  if (!length(weight) %in% c(1, n_conds)) {
    stop("weight must be of length 1 or equal to the number of conditions")
  }
  if (!length(na_replace) %in% c(1, n_conds)) {
    stop("na_replace must be of length 1 or equal to the number of conditions")
  }

  weight <- rep(weight, length.out = n_conds)
  na_replace <- rep(na_replace, length.out = n_conds)

  total_score <- numeric(nrow(df))

  for (i in seq_len(n_conds)) {
    cond_result <- rlang::eval_tidy(conditions[[i]], data = df)
    num_vec <- as.integer(cond_result)
    current_score <- num_vec * weight[i]

    current_score <- ifelse(
      is.na(current_score),
      na_replace[i],
      current_score
    )

    total_score <- total_score + current_score
  }
  total_score
}

# 预清洗
num_simple_cleaning <- function(x) {
  x <- chartr(
    "０-９Ａ-Ｚａ-ｚ．！＠＃＄％＾＆＊：＝（）＿＋",
    "0-9A-Za-z.!@#$%^&*:=()_+", x
  )
  x <- str_replace_all(x, c(" " = "", "\\.+" = "\\."))
  x[which(x == "")] <- NA
  x
}

# 查看变量中的非数值
check_nonnum <- function(x, return_idx = F, show_unique = T) {
  x2 <- suppressWarnings(as.numeric(x))
  idx <- which(!is.na(x) & is.na(x2))
  y <- x[idx]
  if (return_idx) {
    list(value = y, idx = idx)
  } else if (show_unique) {
    unique(y)
  } else {
    y
  }
}

# 提取数字
extract_num <- function(x, res_type = c("first", "range"), multimatch2na = FALSE, leq_1 = FALSE,
                        allow_neg = TRUE, zero_regexp = "阴性|未见", max_regexp = "满", max_quantile = 0.95) {
  res_type <- match.arg(res_type)
  if (!is.null(zero_regexp)) {
    flag_zero <- grepl(zero_regexp, x)
  }
  if (!is.null(max_regexp)) {
    flag_max <- grepl(max_regexp, x)
  }
  if (allow_neg) {
    my_expr <- "-?[0-9]+\\.?[0-9]*|-?\\.[0-9]+"
  } else {
    my_expr <- "[0-9]+\\.?[0-9]*|\\.[0-9]+"
  }
  match_res <- regmatches(x, gregexpr(my_expr, x))
  if (res_type == "first") {
    res <- as.numeric(sapply(match_res, `[`, 1))
    if (multimatch2na) {
      res[sapply(match_res, length) != 1] <- NA
    }
    if (leq_1) {
      res[res > 1] <- NA
    }
  } else if (res_type == "range") {
    res <- ifelse(
      sapply(match_res, length) == 1,
      as.numeric(sapply(match_res, `[`, 1)),
      ifelse(
        sapply(match_res, length) == 2,
        (as.numeric(sapply(match_res, `[`, 1)) + as.numeric(sapply(match_res, `[`, 2))) / 2,
        NA
      )
    )
  }
  if (!is.null(max_regexp)) {
    res[flag_max] <- quantile(res, max_quantile, na.rm = TRUE, names = FALSE)
  }
  if (!is.null(zero_regexp)) {
    res[flag_zero] <- 0
  }
  res
}

# 数值序列分割为factor
cut_by <- function(x, breaks,
                   breaks_as_quantiles = FALSE,
                   labels = NULL,
                   label_type = "ori", # 'LMH' to set to 'Low Medium High' style
                   ...) {
  cut.labels <- NULL
  if (label_type == "LMH") {
    if (length(breaks) == 1) {
      cut.labels <- c("Low", "High")
    } else if (length(breaks) == 2) {
      cut.labels <- c("Low", "Medium", "High")
    }
  }
  if (!is.null(labels)) {
    cut.labels <- labels
  }
  if (breaks_as_quantiles) {
    cut(x, c(quantile(x, c(0, breaks, 1), na.rm = T)),
      right = F,
      include.lowest = T, labels = cut.labels, ...
    )
  } else {
    cut(x, c(-Inf, breaks, Inf),
      right = F,
      include.lowest = T, labels = cut.labels, ...
    )
  }
}

# 将两个list按照names相加
add_lists <- function(l1, l2) {
  # 获取两个list的names
  names1 <- names(l1)
  names2 <- names(l2)

  # 合并两个list的names，去除重复
  all_names <- unique(c(names1, names2))

  # 遍历所有names，并相加相同name的元素
  result <- setNames(vector("list", length(all_names)), all_names)
  for (name in all_names) {
    # 如果两个list都有这个name，则相加
    if (name %in% names1 && name %in% names2) {
      result[[name]] <- l1[[name]] + l2[[name]]
    } else if (name %in% names1) { # 如果只有list1有这个name
      result[[name]] <- l1[[name]]
    } else if (name %in% names2) { # 如果只有list2有这个name
      result[[name]] <- l2[[name]]
    }
  }

  return(result)
}

# 用新元素替换原有元素，可用于列名
replace_elements <- function(x, from, to) {
  y <- x
  if (length(from) != length(to)) {
    stop("from and to should have the same length!")
  }
  for (i in seq_along(from)) {
    y[y %in% from[i]] <- to[i]
  }
  y
}


# 统一数据单位,core function,dat:value,unit
unit_standardize_ <- function(dat, target_unit = NULL, units2change = NULL, coeffs = NULL) {
  if (is.null(target_unit)) {
    if (length(unique(dat[, 2])) > 1) {
      target_unit <- first_mode(dat[, 2])
    } else {
      return(dat)
    }
  }
  if (length(target_unit) > 1) {
    stop("too many targets!")
  }
  if (is.null(units2change)) {
    units2change <- setdiff(unique(dat[, 2]), target_unit)
  }
  if (!is.null(coeffs) && length(units2change) != length(coeffs)) {
    stop("coeffs should have the same length as units2change!")
  } else if (is.null(coeffs)) {
    coeffs <- rep(1, length(units2change))
  }
  for (i in seq_along(units2change)) {
    flag <- dat[, 2] %in% units2change[i]
    dat[flag, 1] <- as.numeric(dat[flag, 1]) * coeffs[i]
  }
  dat[, 2] <- target_unit
  return(dat)
}

# 统一数据单位
# change_list=list(list(subject='x',target_unit='a',units2change=c('b','c'),coeffs=c(2,0.5)))
unit_standardize <- function(dat, subject_col, value_col, unit_col, change_list) {
  for (i in seq_along(change_list)) {
    flag <- dat[, subject_col] %in% change_list[[i]]$subject
    dat[flag, c(value_col, unit_col)] <- unit_standardize_(dat[flag, c(value_col, unit_col)],
      target_unit = change_list[[i]]$target_unit,
      units2change = change_list[[i]]$units2change,
      coeffs = change_list[[i]]$coeffs
    )
  }
  return(dat)
}

# 提取向量中的非NA值，model=c('first','mid','last')，
# disjoint使3种方式不取到相同元素，优先级为first,last,mid
get_valid <- function(l, mode = c("first", "mid", "last"), disjoint = F) {
  mode <- match.arg(mode)
  tmp <- na.omit(l)
  if (length(tmp) > 0) {
    if (disjoint) {
      if (mode == "first") {
        tmp[1]
      } else if ((mode == "last") && (length(tmp) > 1)) {
        tmp[length(tmp)]
      } else if ((mode == "mid") && (length(tmp) > 2)) {
        tmp[round((length(tmp) + 0.5) / 2)]
      } else {
        NA
      }
    } else {
      if (mode == "first") {
        tmp[1]
      } else if ((mode == "last")) {
        tmp[length(tmp)]
      } else if ((mode == "mid")) {
        tmp[round((length(tmp) + 0.5) / 2)]
      } else {
        NA
      }
    }
  } else {
    NA
  }
}

# 常见中国日期格式识别
.to_date <- function(x, from_excel = T, print_failure = T,
                     try_formats = c("%Y-%m-%d", "%Y/%m/%d", "%Y%m%d", "%Y.%m.%d")) {
  if (suppressWarnings((!is.na(as.numeric(x))) && (as.numeric(x) < 100000) && from_excel)) {
    as.Date(as.numeric(x), origin = "1899-12-30")
  } else {
    tryCatch(
      {
        as.Date(as.character(x), tryFormats = try_formats)
      },
      error = function(e) {
        if (print_failure) {
          print(paste0("cannot process:", as.character(x)))
        }
        NA
      }
    )
  }
}
to_date <- function(x, from_excel = T, print_failure = T,
                    try_formats = c("%Y-%m-%d", "%Y/%m/%d", "%Y%m%d", "%Y.%m.%d")) {
  if (is.numeric(x)) {
    if (from_excel) {
      y <- as.Date(x, origin = "1899-12-30")
    } else {
      y <- as.Date(x)
    }
  } else {
    y <- as.Date(sapply(x, .to_date,
      from_excel = from_excel, print_failure = print_failure,
      try_formats = try_formats, USE.NAMES = FALSE
    ))
  }
  y
}

# excel里合并的行ID，全部填充
fill_with_last <- function(x) {
  for (i in 2:length(x)) {
    if (is.na(x[i])) {
      x[i] <- x[i - 1]
    }
  }
  x
}

# 用众数补全信息
completion_with_mode <- function(dat, cols_to_complete, group_vars = NULL) {
  load_packages(c("dplyr"))
  tmp <- dat
  if (!is.null(group_vars)) {
    tmp <- tmp %>%
      group_by(pick(group_vars))
  }
  tmp <- tmp %>%
    mutate(across(cols_to_complete, first_mode))
  tmp
}

# 将make.names之后的名字匹配到原名
unmake_names <- function(x, ori_names, wrap_backtick = T) {
  out <- ori_names[match(x, make.names(ori_names))]
  if (wrap_backtick) {
    paste0("`", out, "`")
  } else {
    out
  }
}

# 限制性立方COX图----
rcs_cox <- function(data, knot = 4, y, time, x, covs = NULL, ref = "median", ref.digits = 3, breaks = 20, print_pPH = T,
                    fill.name = "Grouped by Clinical Value", fill.labels = NULL, trans = "identity",
                    col = NULL, filename = NULL, hr.max = NULL, hist.max = NULL, xlim = NULL, ...) {
  load_packages(c(
    "rms", "ggplot2", "survminer", "survival", "dplyr",
    "patchwork", "Cairo", "RColorBrewer", "grid"
  ))
  # if (!is.null(knot)) {warning("please be sure of knot by AIC min(default) or preliminary investigation suggested")}
  if (missing(data)) {
    stop("data required.")
  }
  if (missing(x)) {
    stop("x required.")
  }
  if (missing(time)) {
    stop("time required.")
  }
  if (is.null(col)) {
    col <- brewer.pal(8, "Set2")
  }

  call <- match.call()
  # print(call)
  data <- as.data.frame(data)
  y <- y
  x <- x
  time <- time
  if (is.null(covs)) {
    indf <- dplyr::select(data, y, x, time)
  } else {
    assign("covs", covs)
    indf <- dplyr::select(data, y, x, time, covs)
  }

  colnames(indf)[1:3] <- c("y", "x", "time")
  nmissing <- sum(!complete.cases(indf))
  if (nmissing > 0) {
    warning(paste0(nmissing, " incomplete cases excluded."))
  }
  indf <- indf[complete.cases(indf), ]
  dd <- NULL
  dd <= rms::datadist(indf)
  old <- options()
  on.exit(options(old))
  options(datadist = "dd")

  aics <- NULL
  S <- Surv(indf$time, indf$y == 1)

  if (is.null(knot)) {
    for (i in 3:7) {
      if (is.null(covs)) {
        formula <- paste0("S~ rcs(x, ", i, ")")
      } else {
        formula <- paste0("S~ rcs(x, ", i, ")", " + ", paste0(covs, collapse = " + "))
      }
      fit <-
        rms::cph(
          as.formula(formula),
          data = indf,
          x = TRUE,
          y = TRUE,
          se.fit = TRUE,
          tol = 1e-25,
          surv = TRUE
        )
      summary(fit)
      aics <- c(aics, AIC(fit))
      kn <- seq(3, 7)[which.min(aics)]
    }
    knot <- kn
  }
  if (is.null(covs)) {
    formula <- paste0("S~ rcs(x, ", knot, ")")
  } else {
    formula <- paste0("S~ rcs(x, ", knot, ")", " + ", paste0(covs, collapse = " + "))
  }
  model.cox <- rms::cph(as.formula(formula), data = indf, x = TRUE, y = TRUE, se.fit = TRUE, tol = 1e-25, surv = TRUE)
  phassump <- survival::cox.zph(model.cox, transform = "km")
  phresidual <- survminer::ggcoxzph(phassump)
  anova.cox <- anova(model.cox)
  pvalue_all <- anova.cox[1, 3]
  pvalue_nonlin <- round(anova.cox[2, 3], 3)
  pvalue_PH <- phassump$table[1, 3]
  pre.model <- rms::Predict(model.cox, "x", fun = exp, type = "predictions", ref.zero = T, conf.int = 0.95, digits = 2)

  Q20 <- quantile(indf$x, probs = seq(0, 1, 0.05))
  ushap <- data.frame(pre.model)
  if (ref == "min") {
    ushap.cutoff <- ushap$x[which.min(ushap$yhat)]
  } else if (ref == "median") {
    ushap.cutoff <- median(indf$x)
  } else {
    ushap.cutoff <- ref
  }

  dd <= rms::datadist(indf)
  dd[["limits"]]["Adjust to", "x"] <= ushap.cutoff
  old <- options()
  on.exit(options(old))
  options(datadist = "dd")

  model.cox <- update(model.cox)
  pre.model <- rms::Predict(model.cox, x,
    fun = exp, type = "predictions", ref.zero = T, conf.int = 0.95, digits = 2
  )

  newdf1 <- as.data.frame(dplyr::select(pre.model, x, yhat, lower, upper))
  if (!is.null(xlim)) {
    newdf1 <- filter(newdf1, (x >= xlim[1]) & (x <= xlim[2]))
  }
  colnames(newdf1) <- c("x", "y", "lower", "upper")
  xmin <- min(newdf1[, "x"])
  xmax <- max(newdf1[, "x"])
  if (is.null(hr.max)) {
    ymax1 <- ceiling(max(newdf1[, "upper"]))
  } else {
    ymax1 <- hr.max
  }
  newdf2 <- indf[indf[, "x"] >= xmin & indf[, "x"] <= xmax, ]
  if (length(breaks) == 1) {
    bks <- seq(min(newdf2$x), max(newdf2$x), length.out = breaks + 1)
    if (!ushap.cutoff %in% bks) {
      bks <- seq(min(newdf2$x), max(newdf2$x), length.out = breaks)
      h <- (max(newdf2$x) - min(newdf2$x)) / (breaks - 1)
      bks <- c(bks[1] - h, bks)
      tmp <- ushap.cutoff - bks
      tmp <- tmp[tmp > 0]
      bks <- bks + tmp[length(tmp)]
    }
    breaks <- bks
  }
  h <- hist(newdf2$x, breaks = breaks, right = FALSE, plot = F)

  newdf3 <- data.frame(x = h[["mids"]], freq = h[["counts"]], pct = h[["counts"]] / sum(h[["counts"]]))

  if (is.null(hist.max)) {
    ymax2 <- ceil(max(newdf3$pct * 1.5) * 20) * 5
  } else {
    ymax2 <- hist.max
  }
  scale_factor <- ymax2 / ymax1
  xtitle <- x
  ytitle1 <- ifelse(is.null(covs), "Unadjusted HR (95% CI)", "Adjusted HR (95% CI)")
  ytitle2 <- "Percentage of Population (%)"
  offsetx1 <- (xmax - xmin) * 0.02
  offsety1 <- ymax1 * 0.02
  labelx1 <- xmin + (xmax - xmin) * 0.05
  labely1 <- ymax1 * 0.9
  label1 <- paste0("Estimation", "\n", "95% CI")
  labelx2 <- xmin + (xmax - xmin) * 0.6
  labely2 <- ymax1 * 0.9
  label2 <- paste0(
    "P-overall ",
    ifelse(pvalue_all < 0.001, "< 0.001", paste0("= ", sprintf("%.3f", pvalue_all))),
    "\nP-non-linear ",
    ifelse(pvalue_nonlin < 0.001, "< 0.001", paste0("= ", sprintf("%.3f", pvalue_nonlin)))
  )
  if (print_pPH) {
    label2 <- paste0(
      label2, "\nP-proportional ",
      ifelse(pvalue_PH < 0.001, "< 0.001", paste0("= ", sprintf("%.3f", pvalue_PH)))
    )
  }


  newdf3$Group <- cut_by(newdf3$x, ushap.cutoff, labels = fill.labels, label_type = "LMH")
  tmp_group <- cut_by(indf$x, ushap.cutoff, labels = fill.labels, label_type = "LMH")
  levels(newdf3$Group) <- paste0(levels(newdf3$Group), " (n=", table(tmp_group), ")")
  grob <- grobTree(textGrob(paste0("N = ", nrow(indf)),
    x = 0.5, y = 0.9,
    gp = gpar(col = "black", fontsize = 11)
  ))

  plot.ushap.type2 <- ggplot2::ggplot() +
    geom_bar(
      data = newdf3, aes(x = x, y = pct * 100 / scale_factor, fill = Group),
      stat = "identity", # width=0.05,
      # fill="#f9f7f7",
      # color="white"
    ) +
    scale_fill_manual(values = col, name = fill.name) +
    geom_hline(yintercept = 1, linewidth = 1, linetype = 2, color = "grey") +
    geom_ribbon(
      data = newdf1, aes(x = x, ymin = lower, ymax = upper),
      fill = "#e23e57", alpha = 0.1
    ) +
    # geom_line(data=newdf1, aes(x=x, y=lower), linetype=2, color="#ff9999",linewidth=0.8) +
    # geom_line(data=newdf1, aes(x=x, y=upper), linetype=2, color="#ff9999",linewidth=0.8) +
    geom_line(data = newdf1, aes(x = x, y = y), color = "#e23e57", linewidth = 1) +
    geom_point(aes(x = ushap.cutoff, y = 1), color = "#e23e57", size = 2) +
    geom_segment(
      aes(
        x = c(labelx1 - offsetx1 * 5, labelx1 - offsetx1 * 5),
        xend = c(labelx1 - offsetx1, labelx1 - offsetx1),
        y = c(labely1 + offsety1, labely1 - offsety1),
        yend = c(labely1 + offsety1, labely1 - offsety1)
      ),
      linetype = 1,
      color = "#e23e57",
      linewidth = 1,
      alpha = c(1, 0.1)
    ) +
    geom_text(aes(
      x = ushap.cutoff, y = 0.9,
      label = paste0("Ref=", format(ushap.cutoff, digits = ref.digits))
    )) +
    geom_text(aes(x = labelx1, y = labely1, label = label1), hjust = 0) +
    geom_text(aes(x = labelx2, y = labely2, label = label2), hjust = 0) +
    annotation_custom(grob) +
    scale_x_continuous(xtitle, expand = c(0.01, 0.01)) +
    scale_y_continuous(
      ytitle1,
      expand = c(0, 0),
      limit = c(0, ymax1),
      transform = trans,
      sec.axis = sec_axis(
        name = ytitle2, transform = ~ . * scale_factor,
      )
    ) +
    theme_bw() +
    theme(
      axis.line = element_line(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      legend.position = "top"
    )

  fname <- ifelse(is.null(filename), paste0(paste0(c(x, covs, paste0("knot", knot)), collapse = "_"), ".png"), filename)
  ggsave(fname, plot.ushap.type2,
    width = 6, height = 6,
    # device = cairo_pdf, family = "Times"
  )

  message.print2 <- list(
    aics = aics, knot = knot, n.valid = nrow(indf), n.plot = nrow(newdf2),
    phassump = phassump, phresidual = phresidual,
    # Q20=Q20,
    pvalue_all = pvalue_all,
    pvalue_nonlin = pvalue_nonlin,
    ref = ushap.cutoff, plot = plot.ushap.type2
  )
  return(message.print2)
}

# 交互作用筛查----
int.scan <- function(data, y, time, targets = NULL, group.vars = NULL, group.cut.quantiles = 0.5, filename = NULL) {
  load_packages(c("survival", "openxlsx", "dplyr", "broom", "ggplot2", "splines", "rms"))
  # if (!is.null(knot)) {warning("please be sure of knot by AIC min(default) or preliminary investigation suggested")}
  if (missing(data)) {
    stop("data required.")
  }
  if (missing(y)) {
    stop("y (outcome) required.")
  }
  if (missing(time)) {
    stop("time required.")
  }
  if (is.null(group.vars)) {
    group.vars <- setdiff(colnames(data), c(y, time))
  }
  if (is.null(targets)) {
    targets <- group.vars
    print("taking all variables as interaction targets")
  }
  if (is.null(filename)) {
    filename <- "交互作用亚组分析.xlsx"
  }

  res.table <- NULL
  for (target in targets) {
    dat <- data
    for (var in group.vars) {
      if (var != target) {
        if (is.numeric(dat[, var]) && length(unique(dat[, var])) > 5) {
          dat[, var] <- cut_by(dat[, var], group.cut.quantiles, breaks_as_quantiles = T)
        } else {
          dat[, var] <- as.factor(dat[, var])
        }
        if (sum(complete.cases(dat[, c(time, y, target, var)])) < 10) {
          next
        }
        formula1 <- paste0("Surv(", time, ",", y, ")~", target, "+", var)
        formula2 <- paste0("Surv(", time, ",", y, ")~", target, "*", var)
        model1 <- coxph(as.formula(formula1), data = dat)
        model2 <- coxph(as.formula(formula2), data = dat)
        tmp1 <- anova(model1, model2, test = "LRT")
        p1 <- tidy(tmp1)[[2, 5]]
        if (length(unique(dat[, target])) > 5) {
          p2 <- tryCatch(
            {
              formula3 <- paste0("Surv(", time, ",", y, ")~rcs(", target, ",4) + ", var)
              formula4 <- paste0("Surv(", time, ",", y, ")~rcs(", target, ",4) * ", var)
              model3 <- coxph(as.formula(formula3), data = dat)
              model4 <- coxph(as.formula(formula4), data = dat)
              tmp2 <- anova(model3, model4, test = "LRT")
              tidy(tmp2)[[2, 5]]
            },
            error = function(e) {
              NA
            }
          )
        } else {
          p2 <- NA
        }

        # print(tidy(tmp2))
        tmp <- data.frame(
          target = target, group.by = var,
          nvalid = sum(complete.cases(dat[, c(time, y, target, var)])),
          lin.pval = p1, rcs.pval = p2
        )
        res.table <- rbind(res.table, tmp)
      }
    }
  }
  res.table <- res.table[order(res.table$lin.pval, decreasing = F), ]
  # res.table$lin.p.adj=p.adjust(res.table$lin.pval)
  # res.table$rcs.p.adj=p.adjust(res.table$rcs.pval)
  write.xlsx(res.table, filename)
}

# 交互作用作图----
int.plot <- function(data, y, time, x1, x2, covs = NULL, x2.breaks = 0.5,
                     x2.breaks.as.quantiles = T, x2.cut.labels = NULL,
                     col = NULL, filename = NULL, height = 4, width = 4,
                     xlab = x1, legend.title = x2, ...) {
  load_packages(c(
    "ggplot2", "survminer", "survival", "dplyr", "RColorBrewer", "splines",
    "export", "rms", "grid"
  ))
  if (missing(data)) {
    stop("data required.")
  }
  if (missing(x1)) {
    stop("x1 (continuous) required.")
  }
  if (missing(x2)) {
    stop("x2 (categorical) required.")
  }
  if (missing(time)) {
    stop("time required.")
  }
  if (is.null(col)) {
    col <- brewer.pal(8, "Set2")
  }
  if (any(c(y, time, x1, x2) %in% covs)) {
    print("conflict of model variables!")
    return()
  }
  if (is.null(filename)) {
    if (is.null(covs)) {
      fname <- paste0(paste0(c(x1, "groupedby", x2), collapse = "_"), ".png")
    } else {
      fname <- paste0(paste0(c(x1, "groupedby", x2, "adjustedby", covs), collapse = "_"), ".png")
    }
  } else {
    fname <- filename
  }
  dat <- data[, c(y, time, x1, x2, covs)]
  colnames(dat)[1:4] <- c("y", "time", "x1", "x2")
  if (is.numeric(dat$x2) && (length(na.omit(unique(dat$x2))) > 5)) {
    dat$x2 <- cut_by(dat$x2, x2.breaks,
      breaks_as_quantiles = x2.breaks.as.quantiles,
      labels = x2.cut.labels, label_type = "LMH"
    )
  } else {
    dat$x2 <- as.factor(dat$x2)
  }
  levels(dat$x2) <- paste0(levels(dat$x2), " (n=", table(dat$x2), ")")
  dat <- na.omit(dat)
  x2.lvl <- levels(dat$x2)
  x2.nlvl <- length(x2.lvl)
  used.col <- col[1:x2.nlvl]
  if (is.null(covs)) {
    cov.terms <- ""
  } else {
    cov.terms <- paste0(" + ", paste0(covs, collapse = " + "))
  }
  if (length(unique(dat$x1)) <= 5) {
    dat$x1 <- as.factor(dat$x1)
    x1.lvl <- levels(dat$x1)
    prefix <- "factor_"
  } else {
    x1.lvl <- seq(min(dat$x1), max(dat$x1), length.out = 100)
    prefix <- "lin_"
  }
  dd <= rms::datadist(dat)
  old <- options()
  on.exit(options(old))
  options(datadist = "dd")
  tryCatch(
    {
      formula1 <- paste0("Surv(time,y)~x1+x2", cov.terms)
      formula2 <- paste0("Surv(time,y)~x1*x2", cov.terms)
      # model1=coxph(as.formula(formula1),data=dat)
      # model2=coxph(as.formula(formula2),data=dat)
      # tmp1=anova(model1,model2,test = 'LRT')
      model1 <- cph(as.formula(formula1), data = dat)
      model2 <- cph(as.formula(formula2), data = dat)
      logLik_model1 <- logLik(model1)
      logLik_model2 <- logLik(model2)
      LR_statistic <- 2 * (logLik_model2 - logLik_model1)
      df <- model2$stats[["d.f."]] - model1$stats[["d.f."]]
      lin_p_value <- 1 - pchisq(LR_statistic, df)
      # print(paste0('lin p diff:',tmp1$`Pr(>|Chi|)`[2]/lin_p_value))
      # pdata = expand.grid(x=x1.lvl,y=x2.lvl)
      # for(var in covs){
      #   pdata[,var]=median(dat[,var])
      # }
      # colnames(pdata)[c(1,2)]=c(x1,x2)

      y1 <- as.data.frame(Predict(model2, x1, x2,
        fun = exp,
        type = "predictions", conf.int = 0.95, digits = 2
      ))
      grob <- grobTree(textGrob(paste0("N = ", nrow(dat)),
        x = 0.5, y = 0.9,
        gp = gpar(col = "black", fontsize = 12)
      ))
      # ypred1 = predict(model2, newdata=pdata, se=TRUE)
      # y1 = ypred1$linear.predictors + outer(ypred1$se.fit, c(0, -1.96, 1.96), '*')-mean(ypred1$linear.predictors)
      # y1=cbind(pdata[,1:2],exp(y1))
      # colnames(y1)=c('x1','x2','y','ylb','yub')
      plt1 <- ggplot(data = y1, aes(x = x1, y = yhat, ymin = lower, ymax = upper, fill = x2, color = x2))
      if (is.factor(dat$x1)) {
        plt1 <- plt1 +
          geom_point(position = position_dodge(width = 1)) +
          geom_errorbar(position = position_dodge(width = 1))
      } else {
        plt1 <- plt1 +
          geom_ribbon(lty = 2, alpha = 0.2, linewidth = 1) +
          geom_line(linewidth = 1)
      }
      plt1 <- plt1 +
        scale_y_log10() + # 对数变换
        scale_color_manual(values = used.col) +
        scale_fill_manual(values = used.col) +
        labs(
          x = xlab, y = "HR (95% CI)", color = legend.title, fill = legend.title,
          title = paste0(
            "p interaction : ",
            base::format.pval(lin_p_value,
              digits = 1,
              nsmall = 3, eps = 0.001
            )
          )
        ) +
        annotation_custom(grob) +
        theme_classic() +
        geom_hline(yintercept = 1, linetype = 3, color = "black", linewidth = 1) +
        theme(
          legend.position = c(0.05, 0.95),
          legend.justification = c(0, 1),
          legend.box.margin = margin(6, 6, 6, 6),
          legend.background = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 15),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 12),
          axis.title = element_text(size = 15)
        )
      ggsave(paste0(prefix, fname), plt1, height = height, width = width)
    },
    error = function(e) {
    }
  )
  if (length(unique(dat$x1)) > 5) {
    tryCatch(
      {
        formula3 <- paste0("Surv(time,y)~rcs(x1,4) + x2", cov.terms)
        formula4 <- paste0("Surv(time,y)~rcs(x1,4) * x2", cov.terms)
        # model3=coxph(as.formula(formula3),data=dat)
        # model4=coxph(as.formula(formula4),data=dat)
        # tmp2=anova(model3,model4,test = 'LRT')
        model3 <- cph(as.formula(formula3), data = dat)
        model4 <- cph(as.formula(formula4), data = dat)
        logLik_model3 <- logLik(model3)
        logLik_model4 <- logLik(model4)
        LR_statistic <- 2 * (logLik_model4 - logLik_model3)
        df <- model4$stats[["d.f."]] - model3$stats[["d.f."]]
        rcs_p_value <- c(1 - pchisq(LR_statistic, df))
        # print(paste0('rcs p diff:',tmp2$`Pr(>|Chi|)`[2]/rcs_p_value))
        # ypred2 = predict(model4, newdata=pdata, se=TRUE)
        # y2 = ypred2$fit + outer(ypred2$se, c(0, -1.96, 1.96), '*')-mean(ypred2$fit)
        # y2=cbind(pdata[,1:2],exp(y2))
        # colnames(y2)=c('x1','x2','y','ylb','yub')
        y2 <- as.data.frame(Predict(model4, x1, x2,
          fun = exp,
          type = "predictions", conf.int = 0.95, digits = 2
        ))
        plt2 <- ggplot(data = y2, aes(x = x1, y = yhat, ymin = lower, ymax = upper, fill = x2, color = x2)) +
          geom_line(linewidth = 1) +
          geom_ribbon(lty = 2, alpha = 0.2, linewidth = 1) +
          scale_y_log10() + # 对数变换
          scale_color_manual(values = used.col) +
          scale_fill_manual(values = used.col) +
          labs(
            x = xlab, y = "HR (95% CI)", color = legend.title, fill = legend.title,
            title = paste0("p interaction : ", base::format.pval(rcs_p_value,
              digits = 1,
              nsmall = 3, eps = 0.001
            ))
          ) +
          annotation_custom(grob) +
          theme_classic() +
          geom_hline(yintercept = 1, linetype = 3, color = "black", linewidth = 1) +
          theme(
            legend.position = c(0.05, 0.95),
            legend.justification = c(0, 1),
            legend.box.margin = margin(6, 6, 6, 6),
            legend.background = element_blank(),
            plot.title = element_text(hjust = 0.5, size = 15),
            axis.text = element_text(size = 12),
            legend.text = element_text(size = 12),
            axis.title = element_text(size = 15)
          )
        ggsave(paste0("rcs_", fname), plt2, height = height, width = width)
      },
      error = function(e) {
      }
    )
  }
}

# 亚组分析森林图----
subgroup_forest <- function(data, var_subgroups, x, y, time = NULL, covs = NULL, decimal_est = 2, p_nsmall = 3,
                            group_cut_quantiles = 0.5, filename = NULL, ...) {
  if (!is.numeric(data[[x]]) && (!is.factor(data[[x]]) || length(levels(data[[x]])) != 2)) {
    stop("x must be numeric or a factor with 2 levels")
  }

  analysis_type <- ifelse(is.null(time), "logistic", "cox")
  covs <- setdiff(covs, c(y, x, time))
  var_subgroups <- setdiff(var_subgroups, c(y, x, time))
  ori_covs <- covs

  if (analysis_type == "cox") {
    indf <- dplyr::select(data, all_of(c(y, x, time, covs)))
    colnames(indf)[1:3] <- c("status", "x", "time")
  } else {
    indf <- dplyr::select(data, all_of(c(y, x, covs)))
    colnames(indf)[1:2] <- c("status", "x")
  }

  if (length(covs) > 0) {
    covs <- paste0("cov", seq_along(covs))
    start_col <- ifelse(analysis_type == "cox", 4, 3)
    colnames(indf)[start_col:(start_col + length(covs) - 1)] <- covs
  }

  indf <- cbind(indf, dplyr::select(data, all_of(var_subgroups)))
  plot_nrow <- 4 + length(var_subgroups)

  process_variable <- function(var) {
    if (is.numeric(indf[[var]])) {
      if (length(unique(indf[[var]])) == 2) {
        indf[[var]] <<- factor(indf[[var]], labels = c("No", "Yes"))
      } else if (length(unique(indf[[var]])) > 5) {
        indf[[var]] <<- cut_by(indf[[var]], group_cut_quantiles, breaks_as_quantiles = TRUE)
      } else {
        indf[[var]] <<- as.factor(indf[[var]])
      }
    } else {
      indf[[var]] <<- as.factor(indf[[var]])
    }
    plot_nrow <<- plot_nrow + length(levels(indf[[var]]))
  }

  sapply(var_subgroups, process_variable)

  formula_base <- if (analysis_type == "cox") {
    "Surv(time, status) ~ x"
  } else {
    "status ~ x"
  }
  formula0 <- paste(c(formula_base, covs), collapse = " + ")

  if (analysis_type == "cox") {
    model <- coxph(as.formula(formula0), data = indf)
    overall_res <- broom::tidy(model, conf.int = TRUE, exponentiate = TRUE)[1, ]
  } else {
    model <- glm(as.formula(formula0), data = indf, family = binomial())
    overall_res <- broom::tidy(model, conf.int = TRUE, exponentiate = TRUE)[2, ]
  }

  res <- data.frame(
    Variable = "Overall",
    Count = if (analysis_type == "cox") model$n else stats::nobs(model),
    Percent = 100,
    `Point Estimate` = overall_res$estimate,
    Lower = overall_res$conf.low,
    Upper = overall_res$conf.high,
    `P value` = overall_res$p.value,
    `P for interaction` = NA,
    check.names = FALSE
  )

  for (var in var_subgroups) {
    tmp_covs <- covs[ori_covs != var]

    if (analysis_type == "cox") {
      formula1 <- paste(c(paste0("Surv(time, status) ~ x + ", var), tmp_covs), collapse = " + ")
      formula2 <- paste(c(paste0("Surv(time, status) ~ x * ", var), tmp_covs), collapse = " + ")
      model1 <- coxph(as.formula(formula1), data = indf)
      model2 <- coxph(as.formula(formula2), data = indf)
    } else {
      formula1 <- paste(c(paste0("status ~ x + ", var), tmp_covs), collapse = " + ")
      formula2 <- paste(c(paste0("status ~ x * ", var), tmp_covs), collapse = " + ")
      model1 <- glm(as.formula(formula1), data = indf, family = binomial())
      model2 <- glm(as.formula(formula2), data = indf, family = binomial())
    }

    tmp1 <- anova(model1, model2, test = "LRT")

    res <- rbind(res, data.frame(
      Variable = var,
      Count = NA,
      Percent = NA,
      `Point Estimate` = NA,
      Lower = NA,
      Upper = NA,
      `P value` = NA,
      `P for interaction` = broom::tidy(tmp1)$p.value[2],
      check.names = FALSE
    ))

    lvls <- levels(indf[[var]])
    tmp_res <- NULL
    for (lvl in lvls) {
      subset_data <- indf[indf[[var]] == lvl, ]
      if (analysis_type == "cox") {
        formula_str <- paste(c("Surv(time, status) ~ x", tmp_covs), collapse = " + ")
        model <- coxph(as.formula(formula_str), data = subset_data)
        lvl_res <- broom::tidy(model, conf.int = TRUE, exponentiate = TRUE)[1, ]
      } else {
        formula_str <- paste(c("status ~ x", tmp_covs), collapse = " + ")
        model <- glm(as.formula(formula_str), data = subset_data, family = binomial())
        lvl_res <- broom::tidy(model, conf.int = TRUE, exponentiate = TRUE)[2, ]
      }

      tmp_res <- rbind(
        tmp_res,
        data.frame(
          Variable = paste("  ", lvl), Count = if (analysis_type == "cox") model$n else stats::nobs(model),
          Percent = NA, `Point Estimate` = lvl_res$estimate,
          Lower = lvl_res$conf.low, Upper = lvl_res$conf.high,
          `P value` = lvl_res$p.value, `P for interaction` = NA, check.names = F
        )
      )
    }

    tmp_res$Percent <- round(tmp_res$Count / sum(tmp_res$Count) * 100, 1)
    res <- rbind(res, tmp_res)
  }

  for (col in c("P value", "P for interaction")) {
    res[[col]] <- ifelse(is.na(res[[col]]), "",
      base::format.pval(as.numeric(res[[col]]),
        digits = 1,
        nsmall = p_nsmall, eps = 0.001
      )
    )
  }

  effect_label <- ifelse(analysis_type == "cox", "HR (95% CI)", "OR (95% CI)")
  plot_df <- res
  plot_df[[effect_label]] <- ifelse(is.na(plot_df$`Point Estimate`), "",
    sprintf(
      paste0("%.", decimal_est, "f (%.", decimal_est, "f to %.", decimal_est, "f)"),
      plot_df$`Point Estimate`, plot_df$Lower, plot_df$Upper
    )
  )

  na_cols <- c("Count", "Percent", "P value", "P for interaction")
  plot_df[na_cols][is.na(plot_df[na_cols])] <- " "
  plot_df$` ` <- paste(rep(" ", 20), collapse = " ")

  plot_columns <- c("Variable", "Count", "Percent", " ", effect_label, "P value", "P for interaction")

  p <- forestploter::forest(
    plot_df[, plot_columns],
    est = plot_df$`Point Estimate`,
    lower = plot_df$Lower,
    upper = plot_df$Upper,
    ci_column = 4,
    ref_line = 1,
    x_trans = "log10",
    ...
  )

  if (!is.null(filename)) {
    ggplot2::ggsave(filename, p, width = 10, height = plot_nrow / 4)
  }
  p
}


# 自动基线表格----
# 绘制qqnorm曲线
qqshow <- function(x,
                   save = T,
                   title = NULL,
                   filename = "QQplot.png",
                   width = 2,
                   height = 2) {
  load_packages(c("ggplot2"))
  dat <- data.frame(sample = scale(x))
  p <- ggplot(dat, aes(sample = sample)) +
    stat_qq(size = 0.5) +
    geom_abline(slope = 1, intercept = 0) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5))
  if (!is.null(title)) {
    p <- p + labs(title = title)
  }
  if (save) {
    ggsave(filename, p, width = width, height = height)
  }
  p
}

# 令wilcox.test报错时直接返回NA
my.wilcox.test <- function(...) {
  tryCatch(
    wilcox.test(...)$p.value,
    error = function(e) {
      NA
    }
  )
}

# 根据实验结果计算正态性检验p值阈值
alpha.by.n <- function(n) {
  if (n < 100) {
    0.05
  } else {
    10^(-exp((log10(n) - 2) * 2 + log(-log10(0.05))))
  }
}

# 将变量自动归类，同时为数值变量输出QQ图帮助人工检验正态性
get.var.types <- function(data, strata = NULL, test.as.whole = F, omit.factor.above = 50,
                          num.to.fact = 5, save.qqplots = T, folder_name = "qqplots") {
  load_packages(c("fBasics", "dplyr"))
  if (save.qqplots & !file.exists(folder_name)) {
    dir.create(folder_name)
  }

  dat.list <- list()
  nor.tests <- list(
    `Shapiro-Wilk` = shapiroTest,
    `Lilliefors` = lillieTest,
    `Anderson-Darling` = adTest,
    `Jarque-Bera` = jarqueberaTest,
    `Shapiro-Francia` = sfTest
  )
  if (is.null(strata) || test.as.whole) {
    dat.list[[1]] <- data
  } else {
    tmp <- data[, strata]
    if (is.null(tmp)) {
      stop("strata not in data!")
    }
    groups <- na.omit(unique(tmp))
    for (i in 1:length(groups)) {
      dat.list[[i]] <- filter(data, tmp == groups[i])
    }
  }
  p.sigs <- sapply(sapply(dat.list, nrow), alpha.by.n)
  nonvars <- c()
  factvars <- c()
  exactvars <- c()
  omitvars <- c()
  vars <- colnames(data)
  for (var in vars) {
    if ((length(na.omit(unique(data[, var]))) <= num.to.fact) | !is.numeric(data[, var])) {
      if ((!is.numeric(data[, var])) & (length(na.omit(unique(data[, var]))) > omit.factor.above)) {
        omitvars <- c(omitvars, var)
        warning(paste0(var, " excluded due to too many levels."))
      } else {
        factvars <- c(factvars, var)
        if (any(table(data[, var]) <= 5) | (!is.null(strata) && any(table(data[, var], data[, strata]) <= 5))) {
          exactvars <- c(exactvars, var)
        }
      }
    } else {
      all.pos <- all(data[, var] >= 0, na.rm = T)
      for (i in 1:length(dat.list)) {
        dat <- dat.list[[i]]
        x <- c(scale(dat[, var]))
        ps <- c()
        for (j in 1:length(nor.tests)) {
          tryCatch(
            {
              tmp <- nor.tests[[j]](x)@test
              if (names(nor.tests)[j] == "Anderson-Darling" & tmp$p.value[1] == 1 & tmp$statistic > 200) {
                ps[j] <- 0
              } else {
                ps[j] <- tmp$p.value[1]
              }
            },
            error = function(e) {
              ps[j] <- NA
            }
          )
        }
        if ((all.pos & (sd(x, na.rm = T) < mean(x, na.rm = T))) |
          (sum(ps < p.sigs[i], na.rm = T) >= sum(!is.na(ps)) - 2)) {
          nonvars <- union(nonvars, var)
          prefix <- "nonnormal"
        } else {
          prefix <- "normal"
        }
        if (save.qqplots) {
          title <- var
          if (!is.null(strata)) {
            title <- paste(title, "by", strata, groups[i], sep = "_")
          }
          qqname <- paste0(paste(prefix, title, sep = "_"), ".png")
          qqshow(x, title = title, filename = paste0(folder_name, "/", qqname))
        }
      }
    }
  }
  res <- list(factvars = factvars, exactvars = exactvars, nonvars = nonvars, omitvars = omitvars, strata = strata)
  class(res) <- "auto.var.types"
  res
}

# 基线表格一键生成
baseline.table <- function(data, auto.var.types = NULL, strata = NULL, vars = setdiff(colnames(data), strata),
                           factor.vars = NULL, exact.vars = NULL, nonnormal.vars = NULL,
                           filename = "baseline.csv", p.adjust.method = "BH", ...) {
  if (!is.null(auto.var.types) && !"auto.var.types" %in% class(auto.var.types)) {
    stop("Invalid 'auto.var.types' arguement! Please use result from get.var.types function.")
  }
  if (!grepl(".csv", filename)) {
    stop("please save as .csv file")
  }
  if (is.null(strata) & !is.null(auto.var.types)) {
    strata <- auto.var.types$strata
  }
  if (is.null(factor.vars) & !is.null(auto.var.types)) {
    factor.vars <- auto.var.types$factvars
  }
  if (is.null(exact.vars) & !is.null(auto.var.types)) {
    exact.vars <- auto.var.types$exactvars
  }
  if (is.null(nonnormal.vars) & !is.null(auto.var.types)) {
    nonnormal.vars <- auto.var.types$nonvars
  }
  if (!is.null(auto.var.types$omitvars)) {
    vars <- setdiff(vars, auto.var.types$omitvars)
  }
  data <- data[!is.na(data[, strata]), ]

  load_packages(c("tableone", "stringr", "tidyr"))
  if (is.null(strata)) {
    tab1 <- CreateTableOne(
      vars = vars, argsNormal = list(var.equal = F),
      argsExact = list(workspace = 2 * 10^5, simulate.p.value = TRUE),
      data = data, factorVars = factor.vars, addOverall = TRUE
    )
  } else {
    tab1 <- CreateTableOne(
      vars = vars, strata = strata, argsNormal = list(var.equal = F),
      argsExact = list(workspace = 2 * 10^5, simulate.p.value = TRUE),
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
    missing.df$.strata <- data[, strata]
    tab2 <- CreateTableOne(
      vars = vars, strata = ".strata",
      data = missing.df, addOverall = TRUE
    )
  }
  tab2Mat <- print(tab2, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, ...)
  write.csv(tab2Mat, file = str_replace(filename, ".csv", "_missing.csv"))

  if (length(na.omit(unique(data[, strata]))) > 2) {
    g <- factor(data[, strata])
    pairwise.result <- data.frame()
    for (var in vars) {
      if (var %in% exact.vars) {
        cont.table <- table(data[, var], g)
        compare.levels <- function(i, j) {
          tryCatch(
            {
              fisher.test(cont.table[, c(i, j)], simulate.p.value = TRUE)$p.value
            },
            error = function(e) {
              NA
            }
          )
        }
        pt <- pairwise.table(compare.levels, levels(g), p.adjust.method)
      } else if (var %in% factor.vars) {
        cont.table <- table(data[, var], g)
        compare.levels <- function(i, j) {
          chisq.test(cont.table[, c(i, j)])$p.value
        }
        pt <- pairwise.table(compare.levels, levels(g), p.adjust.method)
      } else if (var %in% nonnormal.vars) {
        compare.levels <- function(i, j) {
          xi <- data[as.integer(g) == i, var]
          xj <- data[as.integer(g) == j, var]
          my.wilcox.test(xi, xj)
        }
        pt <- pairwise.table(compare.levels, levels(g), p.adjust.method)
      } else {
        pt <- pairwise.t.test(data[, var], g, p.adjust.method = p.adjust.method)$p.value
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

# COX多模型表格和KM曲线----
cox.table.plot <- function(data, y, time, x, models = NULL, pers = c(0.1, 10, 100),
                           clin.breaks = NULL, clin.labels = NULL, N.pos = c(0.4, 0.9),
                           quantile.breaks = NULL, quantile.labels = NULL, folder.name = NULL,
                           ref.levels = "lowest", hr.nsmall = 2, pval.nsmall = 3, pval.eps = 0.001,
                           median.digits = 0, palette = "lancet", xlab = NULL, legend.title = x,
                           legend.pos = c(0.8, 0.8), height = 6, width = 6, pval.coord = NULL, ...) {
  load_packages(c(
    "ggplot2", "survminer", "survival", "stringr", "dplyr", "export", "broom",
    "openxlsx", "grid"
  ))
  if (missing(data)) {
    stop("data required.")
  }
  if (missing(y)) {
    stop("y (outcome) required.")
  }
  if (missing(time)) {
    stop("time required.")
  }
  if (missing(x)) {
    stop("x variable required.")
  }
  if (is.null(models)) {
    models <- list(Crude = c())
  }
  ref.levels <- str_replace_all(ref.levels, c("\\[" = "\\\\[", "\\(" = "\\\\(", "\\]" = "\\\\]", "\\)" = "\\\\)"))
  ori.covs <- unique(unlist(models))
  if (length(ori.covs) > 0) {
    covs <- paste0("cov", 1:length(ori.covs))
  } else {
    covs <- NULL
  }
  if (is.null(folder.name)) {
    folder.name <- paste0("cox_results_", x)
  }
  if (!file.exists(folder.name)) {
    dir.create(folder.name, recursive = T)
  }
  if (is.null(xlab)) {
    xlab <- time
  }
  if (any(c(y, time, x) %in% covs)) {
    print("conflict of model variables!")
    return()
  }
  # covs[match(models[[1]],ori.covs)]
  dat <- data[, c(y, time, x, ori.covs)]
  colnames(dat) <- c("y", "time", "x", covs)
  cols.res <- length(models) * 2 + 2
  rows.res <- 0
  if (is.numeric(dat$x) & (length(na.omit(unique(dat$x))) > 5)) {
    for (per in pers) {
      dat[, paste0("x.", per)] <- dat$x / per
    }
    dat$x.std <- c(scale(dat$x))
    dat$x.IQR <- cut_number(dat$x, 4, right = F)
    levels(dat$x.IQR) <- paste0(paste0("Q", 1:4), " ", levels(dat$x.IQR))
    dat$x.median <- cut_number(dat$x, 2, right = F)
    levels(dat$x.median) <- paste0(c("Low", "High"), " ", levels(dat$x.median))
    rows.res <- 12 + length(pers)
    if (!is.null(clin.breaks)) {
      dat$x.clin <- cut(dat$x, c(-Inf, clin.breaks, Inf), right = F)
      if (length(clin.breaks) == 1) {
        tmp.label <- c("Low", "High")
        rows.res <- rows.res + 3
      } else if (length(clin.breaks) == 2) {
        tmp.label <- c("Low", "Medium", "High")
        rows.res <- rows.res + 5
      } else {
        tmp.label <- NULL
        rows.res <- rows.res + length(clin.breaks) + 3
      }
      if (!is.null(clin.labels)) {
        if (length(clin.labels) != length(levels(dat$x.clin))) {
          stop("the number of clin labels and levels does not match")
        }
        levels(dat$x.clin) <- paste(clin.labels, levels(dat$x.clin), sep = " ")
      } else if (!is.null(tmp.label)) {
        levels(dat$x.clin) <- paste(tmp.label, levels(dat$x.clin), sep = " ")
      }
    }
    if (!is.null(quantile.breaks)) {
      dat$x.quantile <- cut_by(dat$x, quantile.breaks, breaks_as_quantiles = T)
      if (length(quantile.breaks) == 1) {
        tmp.label <- c("Low", "High")
        rows.res <- rows.res + 3
      } else if (length(quantile.breaks) == 2) {
        tmp.label <- c("Low", "Medium", "High")
        rows.res <- rows.res + 5
      } else {
        tmp.label <- NULL
        rows.res <- rows.res + length(quantile.breaks) + 3
      }
      if (!is.null(quantile.labels)) {
        if (length(quantile.labels) != length(levels(dat$x.quantile))) {
          stop("the number of quantile labels and levels does not match")
        }
        levels(dat$x.quantile) <- paste(quantile.labels, levels(dat$x.quantile), sep = " ")
      } else if (!is.null(tmp.label)) {
        levels(dat$x.quantile) <- paste(tmp.label, levels(dat$x.quantile), sep = " ")
      }
    }
  } else {
    dat$x <- as.factor(dat$x)
    if (length(levels(dat$x)) == 2) {
      rows.res <- 4
    } else {
      rows.res <- length(levels(dat$x)) + 3
    }
  }
  dat0 <- dat
  vars <- colnames(dat)[grep("x", colnames(dat))]
  for (var in vars) {
    if (is.factor(dat[, var])) {
      formula <- as.formula(paste0("Surv(time,y)~", var))
      fit <- surv_fit(formula = formula, data = dat)
      log.rank.p <- survdiff(formula = formula, data = dat)$pvalue
      if (log.rank.p < pval.eps) {
        log.rank.p <- paste0("Log-rank\np < ", pval.eps)
      } else {
        log.rank.p <- base::format.pval(log.rank.p, digits = 1, nsmall = pval.nsmall, eps = pval.eps)
        log.rank.p <- paste0("Log-rank\np = ", log.rank.p)
      }
      p <- ggsurvplot(fit,
        pval = log.rank.p,
        pval.coord = pval.coord,
        legend = legend.pos,
        legend.title = legend.title,
        # surv.median.line ='v',
        legend.labs = levels(dat[, var]),
        xlab = xlab,
        risk.table = T,
        palette = palette,
        ...
      )
      dt <- p$data.survplot
      if (!is.null(fit$strata) | is.matrix(fit$surv)) {
        .table <- as.data.frame(summary(fit)$table)
      } else {
        .table <- t(as.data.frame(summary(fit)$table))
        rownames(.table) <- "All"
      }
      xline <- round(as.vector(.table[, "median"]), median.digits)
      tmp <- data.frame(level = levels(dat[, var]), x = xline, text = paste0("t=", xline))
      grob <- grobTree(textGrob(paste0("N = ", sum(p$data.survtable$n.risk[p$data.survtable$time == 0])),
        x = N.pos[1], y = N.pos[2], hjust = 0, gp = gpar(col = "black", fontsize = 12)
      ))
      if (nrow(tmp) > 0) {
        tmp$y <- c((1:10) / 20)[1:nrow(tmp)]
        p$plot <- p$plot +
          annotation_custom(grob) +
          geom_text(data = tmp, aes(x, y, label = text, color = level), hjust = 1, show.legend = F) +
          geom_segment(
            data = tmp, aes(
              x = x, xend = x, y = 0, yend = 0.5,
              colour = level
            ),
            linetype = 2, show.legend = F
          ) +
          geom_segment(y = 0.5, yend = 0.5, x = 0, xend = max(tmp$x), linetype = 2, show.legend = F)
      }
      graph2png(x = print(p), file = paste0(folder.name, "/kmplot_", var, ".png"), width = width, height = height)
    }
  }
  for (var in vars) {
    if (is.factor(dat[, var])) {
      if (identical(ref.levels, "highest")) {
        dat[, var] <- factor(dat[, var], levels = rev(levels(dat[, var])))
      } else if (!identical(ref.levels, "lowest")) {
        if (any(grepl(paste0(ref.levels, collapse = "|"), levels(dat[, var])))) {
          new.ref <- levels(dat[, var])[grepl(paste0(ref.levels, collapse = "|"), levels(dat[, var]))][1]
          dat[, var] <- relevel(dat[, var], ref = new.ref)
        }
      }
    }
  }

  res.table <- data.frame(matrix(NA, nrow = rows.res, ncol = cols.res))
  colnames(res.table) <- c("Terms", "Count", rep(names(models), each = 2))
  res.table$Terms[1] <- paste0(x, " (All)")
  res.table$Count[1] <- length(na.omit(dat$x))
  i <- 2
  for (var in vars) {
    if (is.numeric(dat[, var])) {
      if (var == "x") {
        res.table$Terms[i] <- "Continuous"
      } else if (var == "x.std") {
        res.table$Terms[i] <- "Continuous, per 1 SD"
      } else {
        res.table$Terms[i] <- paste0("Continuous, per ", str_remove(var, "x\\."))
      }
      i <- i + 1
    } else {
      n.levels <- length(levels(dat[, var]))
      if (var == "x.IQR") {
        res.table$Terms[i] <- "Grouped by Interquartile Values"
      } else if (var == "x.median") {
        res.table$Terms[i] <- "Grouped by Median Value"
      } else if (var == "x.clin") {
        res.table$Terms[i] <- "Grouped by Clinical Value"
      } else {
        res.table$Terms[i] <- "Values"
      }
      res.table$Terms[i + 1:n.levels] <- levels(dat[, var])
      res.table$Count[i + 1:n.levels] <- c(table(dat[, var]))
      i <- i + n.levels + 1
      if (n.levels > 2) {
        res.table$Terms[i] <- "P for trend"
        i <- i + 1
      }
    }
  }
  for (j in 1:length(models)) {
    col1 <- 2 * j + 1
    col2 <- 2 * j + 2
    res.table[1, col1:col2] <- c("HR", "P")
    i <- 2
    covs.model <- covs[match(models[[j]], ori.covs)]
    for (var in vars) {
      formula <- as.formula(paste0("Surv(time,y)~", paste0(c(var, covs.model), collapse = "+")))
      model <- coxph(formula = formula, data = dat)
      model.res <- tidy(model, conf.int = T, exponentiate = T)
      model.res <- data.frame(model.res[grepl("x", model.res$term), ])
      for (col in c("estimate", "conf.low", "conf.high")) {
        model.res[, col] <- format(model.res[, col], digits = 1, nsmall = hr.nsmall)
      }
      tmp <- data.frame(
        term = model.res$term,
        HR = paste0(model.res$estimate, "(", model.res$conf.low, ",", model.res$conf.high, ")"),
        P = base::format.pval(model.res$p.value, digits = 1, nsmall = pval.nsmall, eps = pval.eps)
      )
      if (is.numeric(dat[, var])) {
        res.table[i, col1] <- tmp$HR
        res.table[i, col2] <- tmp$P
        i <- i + 1
      } else {
        res.table[i + 2:(nrow(tmp) + 1), col1:col2] <- tmp[, -1]
        res.table[i + 1, col1] <- "1 (Reference)"
        i <- i + nrow(tmp) + 2
        if (nrow(tmp) > 1) {
          dat$tmp <- as.numeric(dat0[, var])
          formula <- as.formula(paste0("Surv(time,y)~", paste0(c("tmp", covs.model), collapse = "+")))
          model <- coxph(formula = formula, data = dat)
          model.res <- tidy(model)
          res.table[i, col2] <- base::format.pval(model.res$p.value[1], digits = 1, nsmall = pval.nsmall, eps = pval.eps)
          i <- i + 1
        }
      }
    }
  }
  write.xlsx(res.table, paste0(folder.name, "/table_", x, ".xlsx"))
}

# 分类模型，多模型评价---
classif_model_compare <- function(pred.list, target, filename = "model_compare.xlsx") {
  load_packages(c("ResourceSelection", "DescTools", "pROC", "caret", "openxlsx"))
  target <- factor(target)
  model.val.comp <- data.frame(matrix(NA, nrow = length(pred.list), ncol = 13))
  colnames(model.val.comp) <- c(
    "Model", "AUC", "Accuracy", "Sensitivity", "Specificity", "Pos Pred Value",
    "Neg Pred Value", "F1", "Kappa", "Brier", "cutoff", "Youden", "HosLem"
  )
  model.val.comp$Model <- names(pred.list)
  sens.metrics <- c(
    "Sensitivity", "Specificity", "Pos Pred Value",
    "Neg Pred Value", "F1"
  )
  acc.metrics <- c("Accuracy", "Kappa")
  for (i in 1:length(pred.list)) {
    tmp <- coords(roc(target, pred.list[[i]]), "best")
    model.val.comp$cutoff[i] <- tmp$threshold
    model.predict <- cut(pred.list[[i]], c(-Inf, model.val.comp$cutoff[i], Inf))
    levels(model.predict) <- levels(target)
    CM <- confusionMatrix(model.predict, target,
      mode = "everything",
      positive = levels(target)[2]
    )
    print(CM$table)
    aucs <- ci.auc(target, pred.list[[i]])
    aucs <- format(aucs, digits = 2, nsmall = 3)
    model.val.comp$AUC[i] <- paste0(aucs[2], " (", aucs[1], ", ", aucs[3], ")")
    for (j in 1:length(sens.metrics)) {
      model.val.comp[i, sens.metrics[j]] <- CM$byClass[sens.metrics[j]]
    }
    for (j in 1:length(acc.metrics)) {
      model.val.comp[i, acc.metrics[j]] <- CM$overall[acc.metrics[j]]
    }
    model.val.comp$Brier[i] <- BrierScore(as.numeric(target) - 1, pred.list[[i]])
    model.val.comp$Youden[i] <- model.val.comp$Sensitivity[i] + model.val.comp$Specificity[i] - 1
    model.val.comp$HosLem[i] <- hoslem.test(as.numeric(target) - 1, pred.list[[i]])$p.value
  }
  for (i in 3:ncol(model.val.comp)) {
    model.val.comp[, i] <- round(model.val.comp[, i], digits = 3)
  }
  write.xlsx(model.val.comp, filename)
}
