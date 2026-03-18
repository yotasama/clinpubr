# frequent tasks for R package development----
check()
test()
load_all()
install()
document()

check_win_devel()
check(remote = TRUE, manual = TRUE)

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
          group_info,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(result_list) > 0) {
    group_results <- do.call(rbind, result_list)
    group_results <- group_results[order(match(group_results[[col_name]], names_vec)), ]
    group_results <- group_results[!duplicated(group_results), ]
    rownames(group_results) <- NULL
    return(group_results)
  } else {
    return(data.frame(name = character(), stringsAsFactors = FALSE))
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


x <- sample(c(3:10), 1e6, replace = TRUE)
dat <- data.frame(id = sample(1:1e5, 1e6, replace = TRUE), value = x)
dat <- dat %>%
  group_by(id) %>%
  mutate(end_date = cumsum(value), start_date = end_date - value + 1) %>%
  ungroup() %>%
  arrange(id, start_date)
head(dat)
dat2 <- data.frame(id = sample(1:1e5, 1e8, replace = TRUE), date = sample(1:200, 1e8, replace = TRUE))
tictoc::tic()
datx <- dat %>%
  merge_by_range(
    y = dat2,
    by = "id",
    x_start = "start_date",
    x_end = "end_date",
    y_val = "date",
    engine = "data.table"
  )
# stop the tic and read the logged time
tictoc::toc(log = TRUE)

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
