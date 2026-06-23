# frequent tasks for R package development----
check()
test()
load_all()
install()
document()

check_win_devel()
check(remote = TRUE, manual = TRUE)

build()
build_manual()
build_readme()
use_version()
submit_cran()

# codes to be processed--------
## correlation plot----
library(export)
library(corrplot)
vars_to_show <- c("Cystc", "CKD.EPI..2009.", "CKD.EPI.CrCy.2021..ori.", "IgG", "IgA")
tmp <- cor(data[, vars_to_show], use = "pairwise.complete.obs")
corrplot(
  corr = tmp, order = "AOE", type = "upper",
  tl.col = "red",
  tl.pos = "d"
)
corrplot(corr = tmp, add = TRUE, type = "lower", method = "number", order = "AOE", diag = FALSE, tl.pos = "n", cl.pos = "n")
export::graph2png(file = "corrplot.png", width = 4, height = 4)

# test codes----
list.dirs()
file.copy()
df <- data.frame(time = c(1:10, 1:5, 1:3), y = c(rep(1, 10), rep(2, 5), rep(3, 3)), group = c(rep("A", 10), rep("B", 5), rep("C", 3)))
df$y <- df$y + rnorm(n = nrow(df), mean = 0, sd = 0.01)
model <- lm(y ~ time, data = df)
summary(model)

load_all()
model <- regression_fit(
  data = df,
  y = "y",
  predictor = "x",
  cluster = "group"
)

library(geepack)
# 使用可交换相关结构
gee_model <- geeglm(y ~ time,
  id = group, data = df,
  family = gaussian, corstr = "exchangeable"
)
summary(gee_model)

library(lme4)
library(lmerTest) # 提供p值

# 混合效应模型
mixed_model <- lmer(y ~ time + (1 | group), data = df)
summary(mixed_model)
mixed_model <- lmer(y ~ time + (1 | group), data = df)

# 随机截距的分布
library(lattice)
dotplot(ranef(mixed_model, condVar = TRUE))

# 使用ggplot绘制随机效应
rand_eff <- ranef(mixed_model)$group
ggplot(rand_eff, aes(x = `(Intercept)`)) +
  geom_histogram(bins = 10, fill = "lightblue", color = "black") +
  labs(title = "随机截距分布", x = "随机截距值", y = "频数")

# 每个组的随机效应
ranef_data <- as.data.frame(ranef(mixed_model))
ggplot(ranef_data, aes(x = grp, y = condval)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = condval - 2 * condsd, ymax = condval + 2 * condsd), width = 0.2) +
  labs(title = "各组的随机截距估计", x = "组别", y = "随机效应值")

library(broom.mixed)
fixed_effects <- tidy(mixed_model, effects = "fixed")

fixed_effects <- tidy(mixed_model)

ggplot(fixed_effects, aes(x = term, y = estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = estimate - 2 * std.error, ymax = estimate + 2 * std.error), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "固定效应估计", x = "参数", y = "估计值")

# 使用effects包
library(effects)
eff_time <- effect("time", mixed_model)
plot(eff_time, main = "时间效应的边际预测")


library(lme4)
library(lmerTest)

df <- data.frame(
  time = c(1:10, 1:5, 1:3),
  y = c(rep(1, 10), rep(2, 5), rep(3, 3)),
  group = c(rep("A", 10), rep("B", 5), rep("C", 3))
)

# 添加少量噪声
set.seed(123)
df$y <- df$y + rnorm(nrow(df), 0, 0.1)

# 两种模型
lm_fixed <- lm(y ~ time + factor(group), data = df)
mixed_model <- lmer(y ~ time + (1 | group), data = df)

# 比较结果
cat("=== 固定效应模型 ===\n")
print(summary(lm_fixed)$coefficients)

cat("\n=== 混合效应模型 ===\n")
print(summary(mixed_model)$coefficients)

cat("\n=== 随机效应方差 ===\n")
print(VarCorr(mixed_model))


group_effects_mixed <- coef(mixed_model)$group
group_effects_fixed <- c(0, coef(lm_fixed)[3:4]) # 相对于A组的差异

comparison <- data.frame(
  Group = c("A", "B", "C"),
  Mixed = group_effects_mixed$`(Intercept)`,
  Fixed = c(coef(lm_fixed)[1], coef(lm_fixed)[1] + group_effects_fixed[2:3])
)
print(comparison)


df <- data.frame(name = c("AB", "B,C", "A..", "ACD"))
match <- data.frame(
  ori = c("A", "B", "C", "ACD", "AB"),
  new = c("x1", "x2", "x3", "x4", "x5")
)
result <- data.frame(
  name = c("AB", "AB", "B,C", "B,C", "A.."),
  matched = c("x1", "x2", "x2", "x3", "x1")
)

str_contains_merge_stringi <- function(df, match_df, col_name = "name", ori_col = "ori", new_cols = "new") {
  if (!requireNamespace("stringi", quietly = TRUE)) {
    stop("请先安装stringi包: install.packages('stringi')")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("请先安装dplyr包: install.packages('dplyr')")
  }

  names_vec <- df[[col_name]]

  if (length(new_cols) == 1) {
    new_cols <- as.character(new_cols)
  }

  # 按 new_cols 分组并拆分成列表
  grouped <- match_df %>%
    dplyr::group_by(across(all_of(new_cols)))

  group_list <- dplyr::group_split(grouped)

  result_list <- list()

  # 对每个组进行处理
  for (group_df in group_list) {
    ori_vec <- group_df[[ori_col]]
    ori_vec <- ori_vec[!is.na(ori_vec)]

    if (length(ori_vec) == 0) next

    matches <- rep(FALSE, length(names_vec))

    # 创建匹配矩阵
    match_matrix <- sapply(ori_vec, function(ori_pattern) {
      result <- stringi::stri_detect_fixed(names_vec, ori_pattern)
      result[is.na(result)] <- FALSE
      result
    })

    # 行或操作：有任意一列匹配即为匹配
    matches <- rowSums(match_matrix) >= 1

    if (any(matches)) {
      matched_names <- unique(names_vec[matches])
      matched_names <- matched_names[!is.na(matched_names)]

      if (length(matched_names) > 0) {
        group_info <- group_df[1, new_cols, drop = FALSE]

        result_list[[length(result_list) + 1]] <- data.frame(
          name = matched_names,
          group_info
        )
      }
    }
  }

  if (length(result_list) > 0) {
    group_results <- data.table::rbindlist(result_list)
    group_results <- group_results[order(match(group_results[[col_name]], names_vec)), ]
    group_results <- group_results[!duplicated(group_results), ]
    rownames(group_results) <- NULL
    return(group_results)
  } else {
    return(data.frame(name = character()))
  }
}

# 测试函数
df <- data.frame(name = c("AB", "B,C", "A..", "ACD"))
match <- data.frame(
  ori = c("A", "B", "C", "ACD", "AB", "AB", "AB"),
  new = c("x1", "x2", "x3", "x4", "x5", "x6", "x5")
)

# 使用基础版本
result1 <- str_contains_merge_stringi(df, match)
print(result1)

all_equal <- identical(result1, result2) && identical(result1, result3)
cat("所有版本结果一致:", all_equal, "\n")
stringi::stri_detect_fixed(c("AB", "B,C", "A..", "ACD"), "A")


escape_regex <- function(x) {
  gsub("([.\\+*?\\[\\^\\]$(){}=!<>|:\\-\\\\])", "\\\\\\1", x, perl = TRUE)
}

escape_regex("[2-5]+3*.x[1]")
Hmisc::escapeRegex("[2-5]+3*.x[1]")
stringi::stri_detect_regex("3", Hmisc::escapeRegex("2-5"))
stringi::stri_detect_regex("3", "[2-5]")


test_regex_escape <- function() {
  # 测试字符串，包含各种特殊字符
  test_cases <- list(
    "基础字符" = "a.b*c?d[e]f{g}h(i)j|k+l-m\\n",
    "边界情况" = "^$=!<>:",
    "括号" = "()[]{}2-3(())",
    "量词" = "*+?{5}",
    "转义" = "\\t\\n\\r",
    "路径" = "C:\\Users\\test\\file.txt",
    "URL" = "https://example.com/path?query=1&param=2",
    "正则模式" = "^(19|20)\\d\\d[- /.](0[1-9]|1[012])[- /.](0[1-9]|[12][0-9]|3[01])$"
  )

  # 测试两个函数
  for (name in names(test_cases)) {
    str <- test_cases[[name]]
    cat("\n=== ", name, ": ", str, " ===\n")

    # 使用两种方法转义
    custom <- escape_regex(str)
    hmisc <- Hmisc::escapeRegex(str)

    cat("自定义函数: ", custom, "\n")
    cat("Hmisc函数: ", hmisc, "\n")

    # 测试转义后是否安全（字面匹配应该返回TRUE）
    test_regex_safety(str, custom, name = "自定义")
    test_regex_safety(str, hmisc, name = "Hmisc")
  }
}

test_regex_safety <- function(original, escaped, name) {
  # 测试转义后的正则表达式是否能精确匹配原字符串
  tryCatch(
    {
      # 使用grepl测试，应该返回TRUE
      result <- grepl(escaped, original)
      if (result) {
        cat(name, "转义: ✓ 安全 (精确匹配)\n")
      } else {
        cat(name, "转义: ✗ 不安全 (不匹配)\n")
      }
    },
    error = function(e) {
      cat(name, "转义: ✗ 错误 (无效正则):", e$message, "\n")
    }
  )
}
test_regex_escape()
grepl(escape_regex("^$=!<>:"), "^$=!<>:")

matched <- c(1:5)
col_name <- "x"
data.frame(setNames(list(matched), col_name))

data <- df
key_df <- match_df
search_col <- "name"
key_col <- "ori"
value_cols <- "category"

x <- data.frame(matrix(rnorm(5e8), nrow = 1e6))
nrow(x)

y <- list(a = x, b = "x")
y2 <- dplyr::filter(y$a, X1 > 0)


# ---- 10 million rows simulation benchmark for screen_data_list ----
run_screen_data_benchmark_10m <- function(seed = 20260304) {
  set.seed(seed)

  n_lab <- 4e7
  n_diag <- 10e6
  n_pid <- 6e6

  cat("Generating simulation data...\n")
  t_gen <- system.time({
    lab <- data.frame(
      pid = sample.int(n_pid, n_lab, replace = TRUE),
      vid = sample.int(n_pid * 3L, n_lab, replace = TRUE),
      lab_day = sample.int(3650L, n_lab, replace = TRUE),
      Hb = rnorm(n_lab, mean = 10.8, sd = 1.8)
    )

    diagnosis <- data.frame(
      pid = sample.int(n_pid, n_diag, replace = TRUE),
      vid = sample.int(n_pid * 3L, n_diag, replace = TRUE),
      dx_day = sample.int(3650L, n_diag, replace = TRUE),
      icd = sample(c("I10", "I11", "E11", "J18"), n_diag,
        replace = TRUE,
        prob = c(0.22, 0.08, 0.30, 0.40)
      )
    )

    admission <- unique(rbind(
      data.frame(pid = lab$pid, vid = lab$vid, admit_day = lab$lab_day),
      data.frame(pid = diagnosis$pid, vid = diagnosis$vid, admit_day = diagnosis$dx_day)
    ))

    patient <- data.frame(pid = sort(unique(c(lab$pid, diagnosis$pid))))
  })

  cat("Data generation time (sec):", t_gen[["elapsed"]], "\n")
  cat(
    "Rows - patient:", nrow(patient),
    "admission:", nrow(admission),
    "diagnosis:", nrow(diagnosis),
    "lab:", nrow(lab), "\n"
  )
  cat("Approx lab size (GB):", round(as.numeric(object.size(lab)) / 1024^3, 3), "\n")

  gc()

  cat("Running screen_data_list benchmark...\n")
  t_fit <- system.time({
    res <- screen_data_list(
      data_list = list(
        patient = patient,
        admission = admission,
        diagnosis = diagnosis,
        lab = lab
      ),
      entry_expr = any(icd == "I10"),
      entry_level = "patient_id",
      anchor_expr = any(Hb > 11.5),
      anchor_level = "date",
      anchor_window = "from_first_anchor",
      patient_id_map = "pid",
      visit_id_map = c(admission = "vid", diagnosis = "vid", lab = "vid"),
      date_map = c(admission = "admit_day", diagnosis = "dx_day", lab = "lab_day"),
      output = "list",
      return_audit = FALSE,
      verbose = FALSE
    )
  })

  cat("screen_data_list time (sec):", t_fit[["elapsed"]], "\n")
  cat(
    "Output rows - patient:", nrow(res$patient),
    "admission:", nrow(res$admission),
    "diagnosis:", nrow(res$diagnosis),
    "lab:", nrow(res$lab), "\n"
  )

  invisible(list(
    timing_generation = t_gen,
    timing_screen = t_fit,
    output_n = vapply(res, nrow, numeric(1))
  ))
}

# Run manually when needed:
bench_10m <- run_screen_data_benchmark_10m()


patient <- data.frame(pid = 1:4)
admission <- data.frame(
  pid = c(1, 1, 2, 3, 4),
  vid = c(11, 12, 21, 31, 41),
  admit_day = c(1, 5, 2, 3, 4)
)
diagnosis <- data.frame(
  pid = c(1, 2, 3, 4),
  vid = c(11, 21, 31, 41),
  dx_day = c(1, 2, 3, 4),
  icd = c("I10", "I10", "J18", "I11")
)
lab <- data.frame(
  pid = c(1, 1, 2, 3, 4),
  vid = c(11, 12, 21, 31, 41),
  lab_day = c(1, 5, 2, 3, 4),
  Hb = c(9.8, 10.6, 10.7, 8.9, 9.1)
)

# Keep patients with any I10 diagnosis, then keep records from first Hb > 10 onward
res <- screen_data_list(
  data_list = list(patient = patient, admission = admission, diagnosis = diagnosis, lab = lab),
  entry_expr = any(icd == "I10"),
  entry_level = "patient_id",
  anchor_expr = any(Hb > 10),
  anchor_level = "visit_id",
  anchor_window = "from_first_anchor",
  patient_id_map = "pid",
  visit_id_map = "vid",
  date_map = c(admission = "admit_day", diagnosis = "dx_day", lab = "lab_day"),
  output = "joined"
)
dplyr::full_join(res$patient, res$admission, by = "pid") %>%
  dplyr::full_join(res$diagnosis, by = dplyr::join_by(pid, vid, admit_day == dx_day)) %>%
  dplyr::full_join(res$lab, by = dplyr::join_by(pid, vid, admit_day == lab_day))

tt <- function(entry_level = c("patient_id", "visit_id", "date"),
               anchor_level = c("date", "visit_id"),
               anchor_window = c("none", "from_first_anchor")) {
  entry_level <- match.arg(entry_level)
  anchor_level <- match.arg(anchor_level)
  anchor_window <- match.arg(anchor_window)
  print(paste("entry_level:", entry_level))
  print(paste("anchor_level:", anchor_level))
  print(paste("anchor_window:", anchor_window))
}
tt(entry_level = "patient", anchor_level = "visit", anchor_window = "f")

a <- tibble::lst(patient, admission, diagnosis, lab)


clean_dl <- adj_cran_downloads(
  packages = "clinpubr", from = "2025-06-04",
  to = "2026-03-04"
)

# 对比原始数据和清洗后的数据
print(clean_dl)
# 查看不同系统的峰值分布
plot(clean_dl$date, clean_dl$count, type = "l", col = "blue", xlab = "Date", ylab = "Downloads", main = "CFA Package Downloads Over Time")
clean_dl[clean_dl$adjusted_downloads < clean_dl$count, ]
clean_dl$adjusted_total_downloads

#' Benchmark group_by %>% summarise using tictoc
#'
#' This function generates a data.frame with approximately `n` rows and
#' runs `reps` repetitions of a `dplyr` `group_by` + `summarise` pipeline,
#' timing each run with `tictoc` and returning elapsed times.
#'
#' @param n Number of rows to generate (default 1e6).
#' @param n_groups Number of distinct groups (default 100).
#' @param reps Number of repetitions to run (default 3).
#' @param seed Random seed for reproducibility (default 123).
#' @returns A list with `times` (numeric vector, seconds) and `last_summary` (the last summarised data.frame).
#' @examples
#' # Make sure tictoc and dplyr are installed, then:
#' # bench_group_by_summary(1e6, 100, reps = 2)
bench_group_by_summary <- function(n = 1e6, n_groups = 100, reps = 3, seed = 123) {
  if (!requireNamespace("tictoc", quietly = TRUE)) stop("Please install the 'tictoc' package")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Please install the 'dplyr' package")

  n <- as.integer(n)
  n_groups <- as.integer(n_groups)
  reps <- as.integer(reps)

  set.seed(seed)
  message(sprintf("Generating %s rows with %s groups...", format(n, big.mark = ","), n_groups))
  df <- data.frame(
    id = seq_len(n),
    group = sample(seq_len(n_groups), n, replace = TRUE),
    value = rnorm(n)
  )

  times <- numeric(reps)
  last_summary <- NULL

  for (i in seq_len(reps)) {
    tictoc::tic(sprintf("run_%d", i))
    # run the dplyr pipeline to be measured
    last_summary <- df %>%
      dplyr::group_by(group) %>%
      dplyr::summarise(
        mean_value = mean(value, na.rm = TRUE),
        sd_value = sd(value, na.rm = TRUE),
        n = dplyr::n(),
        .groups = "drop"
      )
    # stop the tic and read the logged time
    tictoc::toc(log = TRUE)
    lg <- tictoc::tic.log(format = FALSE)
    # take the most recent log entry's time
    entry <- lg[[length(lg)]]
    times[i] <- as.numeric(entry$time)
    # clear log to avoid accumulation
    tictoc::tic.clearlog()
    message(sprintf("Iteration %d: %.3f sec", i, times[i]))
    # free some memory (not strictly necessary)
    gc()
  }

  invisible(list(times = times, last_summary = last_summary))
}

library(dtplyr)
library(dplyr)
load_all()

n <- 1e7
n_groups <- 1e6
df <- data.frame(
  id = seq_len(n),
  group = sample(1:n_groups, n, replace = TRUE),
  value = sample(c(1:10, NA), n, replace = TRUE)
)

tictoc::tic()
# run the dplyr pipeline to be measured
last_summary <- df %>%
  lazy_dt() %>%
  dplyr::group_by(group) %>%
  dplyr::summarise(
    mean_value = first_mode(value),
    n = dplyr::n(),
    .groups = "drop"
  ) %>%
  as.data.frame()
# stop the tic and read the logged time
tictoc::toc(log = TRUE)

first_mode <- function(x, empty_return = NA) {
  x <- na.omit(x)
  l <- length(unique(x))
  if (l == 0) {
    empty_return
  } else if (l > 1 && l < length(x)) {
    x[1] <- DescTools::Mode(x)[1]
    x[1]
  } else {
    x[1]
  }
}

first_mode <- function(x) {
  x <- na.omit(x)
  l <- length(unique(x))
  if (l == 0) {
    x[NA]
  } else if (l > 1 && l < length(x)) {
    x[1] <- DescTools::Mode(x)[1]
    x[1]
  } else {
    x[1]
  }
}

first_mode <- function(x) {
  x <- na.omit(x)
  l <- length(unique(x))
  if (l == 0) {
    x[NA]
  } else if (l > 1 && l < length(x)) {
    x[1] <- DescTools::Mode(x)[1]
    x[1]
  } else {
    x[1]
  }
}


library(dtplyr)
library(dplyr)
load_all()
x <- sample(c(3:10), 1e6, replace = TRUE)
dat <- data.frame(id = sample(1:1e5, 1e6, replace = TRUE), value = x)
dat <- dat %>%
  group_by(id) %>%
  mutate(end_date = cumsum(value) + 5, start_date = end_date - value + 1) %>%
  ungroup() %>%
  arrange(id, start_date)
head(dat)
dat2 <- data.frame(id = sample(1:1e5, 1e7, replace = TRUE), date = sample(1:200, 1e7, replace = TRUE))
tictoc::tic()
datx <- dat %>%
  merge_by_range(
    y = dat2,
    by = "id",
    x_start = "start_date",
    x_end = "end_date",
    # range_relax = c(0, Inf),
    y_val = "date",
    engine = "data.table",
    all_y = TRUE
  )

# stop the tic and read the logged time
tictoc::toc(log = TRUE)
head(datx)
nrow(datx)
table(is.na(datx$value))

library(data.table)
tictoc::tic()
datx2 <- merge(
  setDT(dat),
  setDT(dat2),
  by = "id",
  allow.cartesian = TRUE
)[date >= start_date & date <= end_date]
# stop the tic and read the logged time
tictoc::toc(log = TRUE)

library(dplyr)

df <- data.frame(pid = c(1, 1, 2, 2), value = c(10, 20, 30, 40), value2 = c(100, 200, 300, 400))

# 默认行为：返回长度 > 1 → 展开多行

f1 <- function(x) {
  if (min(x) < 30) {
    20
  } else {
    c(30, 40)
  }
}
df %>%
  group_by(pid) %>%
  summarise(across(everything(), f1))
# 输出4行，每个pid出现两次

# 显式用 list() 包裹 → 变成列表列，每个分组一行
df %>%
  group_by(pid) %>%
  summarise(q = list(quantile(value, c(0.25, 0.75))))

library(dplyr)
library(data.table)
library(tictoc)
x <- data.frame(x = c(1:1e6), y = rnorm(1e6))
tt <- function(xx) {
  out <- setDT(xx) %>%
    mutate(x = x + 10) %>%
    as.data.frame()
  out
}
mean(tt(x)$x)
mean(x$x)
class(x)
y <- list(x, x, x, x, x, x, x, x, x, x)
tic()
z <- data.table::rbindlist(y)
toc()
tic()
z <- bind_rows(y)
toc()
tic()
z <- rbindlist(y)
toc()
df1 <- data.frame(x = 1:3)
setDT(df1)
class(df1)

library(stringi)
library(tictoc)
x <- sample(letters, 10000000, replace = TRUE)
tic()
tmp <- stringi::stri_detect_regex(x, "b|a")
toc()
tic()
tmp <- grepl("a|b", x)
toc()

x <- iconv(c("你好", NA, "asdf"), from = "UTF-8", to = "GBK")
validUTF8(x)
y <- auto_encoding_repair(x)

library(clinpubr)
set.seed(123)
n=1e3
x=data.frame(matrix(sample(c(0,1),n*n,replace=TRUE,prob=c(0.9,0.1)),ncol=n))
dim(x)
x[x==1]=NA
library(tictoc)
tic()
clean_data <- get_valid_subset(
  x,
  row_na_ratio = 0.1,
  col_na_ratio = 0.1,
  row_priority = 1#, adaptive_scoring = TRUE
)
toc()
dim(clean_data)


library(tidyverse)
library(stringr)

#' 提取任意病史（修正版）
#' @param text 病历文本
#' @param disease 疾病关键词（如 "高血压"）
#' @return 返回 "无"/"有"/"未提及"/"X年"
extract_history2 <- function(text, disease) {
  if (is.na(text)) return(NA)
  
  # 1. 清理文本：替换标点为空格，压缩多余空白
  clean_text <- text %>%
    str_squish() %>%
    str_replace_all("[,，。；;]", " ")
  
  # 2. 否定检测
  deny_pattern <- paste0("(无|否认|未).{0,20}?", disease)
  if (str_detect(clean_text, deny_pattern)) {
    return("无")
  }
  
  # 3. 尝试提取年限（核心修正）
  # 模式：疾病关键词 + (可选的"病史"或"史") + 任意空白 + 数字 + "年"
  year_pattern <- paste0(disease, "(病史|史)?\\s*([0-9]+)(?=年)")
  
  # 使用 str_match 提取括号内的捕获组
  # match 结构：整个匹配字符串, 第一组(病史/史), 第二组(数字)
  match_info <- str_match(clean_text, year_pattern)
  
  if (!is.na(match_info[1])) {
    # 成功提取到年限（捕获组2）
    return(paste0(match_info[,3], "年"))
  }
  
  # 4. 如果没有年限，检查是否有“有/患”或单纯提及病史
  affirm_pattern <- paste0("(有|患).{0,20}?", disease)
  mention_pattern <- paste0(disease, "(病史|史)")
  
  if (str_detect(clean_text, affirm_pattern) || str_detect(clean_text, mention_pattern)) {
    return("有")
  }
  
  # 5. 默认返回NA
  return("NA")
}

# 构建模拟数据
test_data <- data.frame(
  id = 1:6,
  SECTION_TEXT = c(
    # 样例1：截图中的标准混合否定
    "否认肝炎、结核、疟疾史，否认高血压、心脏病史，否认糖尿病，无手术史，无过敏史。",
    
    # 样例2：肯定+具体年限
    "既往有高血压病史20年，口服降压药控制良好。",
    
    # 样例3：肯定手术史，没有明确年限
    "有阑尾炎切除手术史，否认高血压。",
    
    # 样例4：混合的否定+肯定
    "无高血压、心脏病史，但有糖尿病病史。",
    
    # 样例5：纯提及，无明确“有”或“无”
    "患者高血压病史、冠心病史，无吸烟史。",
    
    # 样例6：复杂年限文本
    "高血压病史10年，吸烟史15年，每天10支，饮酒史5年。"
  )
)

disease_list <- c("高血压", "心脏病", "糖尿病", "手术", "肝炎", "吸烟", "饮酒")

# 批量处理
final_result <- test_data %>%
  mutate(
    hypertension = extract_history(SECTION_TEXT, "高血压",return_format = "detailed"),
    heart_disease = extract_history(SECTION_TEXT, "心脏病",return_format = "detailed"),
    diabetes = extract_history(SECTION_TEXT, "糖尿病",return_format = "detailed"),
    surgery = extract_history(SECTION_TEXT, "手术",return_format = "detailed"),
    hepatitis = extract_history(SECTION_TEXT, "肝炎",return_format = "detailed"),
    smoking = extract_history(SECTION_TEXT, "吸烟",return_format = "detailed"),
    drinking = extract_history(SECTION_TEXT, "饮酒",return_format = "detailed")
  ) %>%
  ungroup()
as.data.frame(final_result)
load_all()

library(tidyverse)
library(stringr)

# ----------------------------
# 创建大型测试用例框架
# ----------------------------
test_cases <- data.frame(
  id = 1:100,
  case_type = "",
  text = "",
  expected_hypertension = "",
  expected_heart_disease = "",
  expected_diabetes = "",
  stringsAsFactors = FALSE
)

# 填充用例 (为了代码简洁，只展示20个高价值用例，你可以追加更多)
cases <- list()

# 1. 标准否定（无高血压）
cases[[1]] <- list("否定1", "否认高血压、心脏病史，无糖尿病", "无", "无", "无")

# 2. 标准肯定 + 年限
cases[[2]] <- list("肯定+年限", "高血压病史20年，吸烟史10年", "20年", NA, NA)

# 3. 肯定但无年限
cases[[3]] <- list("肯定无年限", "有高血压病史，无心脏病", "有", "无", NA)

# 4. 混合标点分隔
cases[[4]] <- list("标点混合", "否认高血压;否认心脏病；有糖尿病，吸烟史15年", "无", "无", "有")

# 5. 疾病前有“既往”字眼
cases[[5]] <- list("既往前缀", "既往高血压病史10年、心脏病史5年", "10年", "5年", NA)

# 6. “无” + 顿号 + “有” 同时出现
cases[[6]] <- list("混合否定+肯定", "无高血压、心脏病，但有糖尿病史20年", "无", "无", "20年")

# 7. 不规范的年代写法
cases[[7]] <- list("不完整年限", "高血压病史 10年，糖尿病史10余年", "10年", NA, "10年")

# 8. “否认”后面紧跟疾病，但无空格或标点
cases[[8]] <- list("紧邻否定", "否认高血压心脏病糖尿病", "无", "无", "无")

# 9. “未”作为否定
cases[[9]] <- list("否定词'未'", "未发现有高血压，但有糖尿病", "无", NA, "有")

# 10. 仅提及“高血压”，无任何动词
cases[[10]] <- list("纯提及", "患者高血压，心脏病待排除", "有", "有", NA)

# 11. 年限在句子后面（距离较远）
cases[[11]] <- list("远距离年限", "高血压病史，服用降压药5年，病程约10年", "10年", NA, NA) # 这里意图是高血压病程10年

# 12. 年限带“余”字
cases[[12]] <- list("年限带'余'", "高血压病史10余年", "10年", NA, NA)

# 13. 多个疾病共用否定
cases[[13]] <- list("共用否定", "无高血压、糖尿病、心脏病史", "无", "无", "无")

# 14. 否定词在疾病后面（少见）
cases[[14]] <- list("倒序否定", "高血压病史否认", "无", NA, NA)

# 15. 句子中穿插无关词
cases[[15]] <- list("噪声词", "高血压病史20年（已治愈），目前无心脏病", "20年", "无", NA)

# 16. 未提及任何疾病
cases[[16]] <- list("全未提及", "患者一般情况可，无特殊主诉", NA, NA, NA)

# 17. 手术史冒充疾病（应只提取疾病）
cases[[17]] <- list("手术干扰", "高血压病史，有阑尾炎手术史", "有", NA, NA)

# 18. 中文数字（尽量避免，但可以测试）——暂不处理，返回“有”
cases[[18]] <- list("中文数字", "高血压病史二十年", "有", NA, NA)

# 19. 年与数字之间有空行（模拟真实脏数据）
cases[[19]] <- list("换行符干扰", "高血压病史 \n 20 年", "20年", NA, NA)

# 20. 多种疾病交织
cases[[20]] <- list("复杂交织", "无高血压、糖尿病，有心脏病病史10年，吸烟史5年", "无", "10年", "无")

# 更多可以继续添加...（此处仅示范20条）

# 将列表转换为数据框行
for (i in seq_along(cases)) {
  test_cases[i, "case_type"] <- cases[[i]][[1]]
  test_cases[i, "text"] <- cases[[i]][[2]]
  test_cases[i, "expected_hypertension"] <- cases[[i]][[3]]
  test_cases[i, "expected_heart_disease"] <- cases[[i]][[4]]
  test_cases[i, "expected_diabetes"] <- cases[[i]][[5]]
}

# 只保留前20条已填充的（你可以追加更多）
test_cases <- test_cases[1:20, ]

# ----------------------------
# 执行测试并对比结果
# ----------------------------
test_results <- test_cases %>%
  mutate(
    actual_hypertension = extract_history(text, "高血压"),
    actual_heart_disease = extract_history(text, "心脏病"),
    actual_diabetes = extract_history(text, "糖尿病"),
    match_hypertension = actual_hypertension == expected_hypertension|is.na(actual_hypertension) & is.na(expected_hypertension),
    match_heart_disease = actual_heart_disease == expected_heart_disease|is.na(actual_heart_disease) & is.na(expected_heart_disease),
    match_diabetes = actual_diabetes == expected_diabetes|is.na(actual_diabetes) & is.na(expected_diabetes),
    all_match = match_hypertension & match_heart_disease & match_diabetes
  ) %>%
  ungroup()

# ----------------------------
# 输出报告
# ----------------------------
# 1. 显示全部测试用例及其结果
print(test_results[!test_results$all_match, ])

# 2. 统计通过率
pass_rate <- mean(test_results$all_match)
cat("\n====================\n")
cat("总测试用例数: ", nrow(test_results), "\n")
cat("完全匹配通过率: ", round(pass_rate * 100, 2), "%\n")

# 3. 列出未通过用例
failures <- test_results %>% filter(!all_match)
if (nrow(failures) > 0) {
  cat("\n⚠️ 以下用例未完全通过验证：\n")
  print(failures[, c("id", "case_type", "text", "expected_hypertension", "actual_hypertension")])
}

load_all()
extract_history("心脏病“病史10年，2015年开始使用恩格列净", "心脏病")
