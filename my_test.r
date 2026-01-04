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
  names_vec <- df[[col_name]]

  if (length(new_cols) == 1) {
    new_cols <- as.character(new_cols)
  }

  new_data <- match_df[, new_cols, drop = FALSE]

  unique_new_combos <- unique(new_data)
  result_list <- list()

  for (i in seq_len(nrow(unique_new_combos))) {
    combo <- unique_new_combos[i, , drop = FALSE]
    combo_values <- as.character(combo[1, ])

    match_idx <- apply(new_data, 1, function(row) {
      all(as.character(row) == combo_values)
    })
    ori_patterns <- match_df[[ori_col]][match_idx]

    ori_patterns <- ori_patterns[!is.na(ori_patterns)]
    if (length(ori_patterns) == 0) next

    ori_regex <- paste(ori_patterns, collapse = "|")

    matches <- stringi::stri_detect_regex(names_vec, ori_regex)
    matches[is.na(matches)] <- FALSE

    if (any(matches, na.rm = TRUE)) {
      matched_names <- unique(names_vec[matches])
      matched_names <- matched_names[!is.na(matched_names)]

      for (name in matched_names) {
        result_list[[length(result_list) + 1]] <- c(
          name = name,
          as.list(combo)
        )
      }
    }
  }

  if (length(result_list) > 0) {
    result <- do.call(rbind, lapply(result_list, as.data.frame, stringsAsFactors = FALSE))

    if (nrow(result) > 0) {
      result <- result[order(match(result[[col_name]], names_vec)), ]
      result <- result[!duplicated(result), ]
    }
    return(result)
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
