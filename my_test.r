# frequent tasks for R package development----
check()
test()
load_all()
install()
document()

check(remote = TRUE, manual = TRUE)
check_win_devel()

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


df <- data.frame(name = c("AB", "B,C", "A..","ACD"))
match <- data.frame(
  ori = c("A", "B", "C", "ACD","AB"),
  new = c("x1", "x2", "x3","x4","x5")
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
df <- data.frame(name = c("AB", "B,C", "A..","ACD"))
match <- data.frame(
  ori = c("A", "B", "C", "ACD","AB","AB","AB"),
  new = c("x1", "x2", "x3","x4","x5","x6","x5")
)

# 使用基础版本
result1 <- str_contains_merge_stringi(df, match)
print(result1)

all_equal <- identical(result1, result2) && identical(result1, result3)
cat("所有版本结果一致:", all_equal, "\n")
stringi::stri_detect_fixed(c("AB", "B,C", "A..","ACD"), "A")


escape_regex <- function(x) {
  gsub("([.\\+*?\\[\\^\\]$(){}=!<>|:\\-\\\\])", "\\\\\\1", x, perl = TRUE)
}

escape_regex("[2-5]+3*.x[1]")
Hmisc::escapeRegex("[2-5]+3*.x[1]")
stringi::stri_detect_regex("3", Hmisc::escapeRegex("2-5"))
stringi::stri_detect_regex("3","[2-5]")


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
  tryCatch({
    # 使用grepl测试，应该返回TRUE
    result <- grepl(escaped, original)
    if (result) {
      cat(name, "转义: ✓ 安全 (精确匹配)\n")
    } else {
      cat(name, "转义: ✗ 不安全 (不匹配)\n")
    }
  }, error = function(e) {
    cat(name, "转义: ✗ 错误 (无效正则):", e$message, "\n")
  })
}
test_regex_escape()
grepl(escape_regex("^$=!<>:"), "^$=!<>:")

matched=c(1:5)
col_name="x"
data.frame(setNames(list(matched), col_name))
