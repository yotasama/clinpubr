set.seed(123)

# 创建一个测试用例，其中：
# - 有1列超过阈值（但只超过一点点）
# - 有5行超过阈值（超过很多）
# 传统算法会删除列，但adaptive算法应该能识别出删除行更有利

cat("=== 测试1: 列轻微超过阈值，行严重超过阈值 ===\n\n")

df1 <- data.frame(
  id = 1:100,
  normal1 = rnorm(100),
  normal2 = rnorm(100),
  normal3 = rnorm(100),
  normal4 = rnorm(100),
  normal5 = rnorm(100),
  normal6 = rnorm(100),
  normal7 = rnorm(100),
  normal8 = rnorm(100)
)

# 创建1个轻微超过阈值的列（缺失率0.25，阈值0.2）
df1$problem_col <- c(rep(NA, 25), rnorm(75))

# 创建5个严重超过阈值的行（缺失率0.5，阈值0.3）
# 这些行在problem_col上不是NA
for (i in 96:100) {
  df1[i, c("normal1", "normal2", "normal3", "normal4")] <- NA
  df1[i, "problem_col"] <- rnorm(1)  # 确保不是NA
}

source("R/get_valid_subset.R")

cat("数据概况:\n")
cat("- problem_col缺失率:", mean(is.na(df1$problem_col)), "(阈值0.2)\n")
row_na <- rowMeans(is.na(df1))
cat("- 超过row_na_ratio=0.3的行数:", sum(row_na > 0.3), "\n")

cat("\n传统算法 (adaptive_scoring=FALSE):\n")
result1_traditional <- get_valid_subset(df1, row_na_ratio = 0.3, col_na_ratio = 0.2, 
                                        adaptive_scoring = FALSE, return_index = TRUE)
cat(sprintf("  保留 %d 行, %d 列 (删除 %d 行, %d 列)\n",
            length(result1_traditional$rows), length(result1_traditional$cols),
            nrow(df1) - length(result1_traditional$rows),
            ncol(df1) - length(result1_traditional$cols)))

cat("\n自适应算法 (adaptive_scoring=TRUE):\n")
result1_adaptive <- get_valid_subset(df1, row_na_ratio = 0.3, col_na_ratio = 0.2, 
                                     adaptive_scoring = TRUE, return_index = TRUE)
cat(sprintf("  保留 %d 行, %d 列 (删除 %d 行, %d 列)\n",
            length(result1_adaptive$rows), length(result1_adaptive$cols),
            nrow(df1) - length(result1_adaptive$rows),
            ncol(df1) - length(result1_adaptive$cols)))

cat("\n=== 测试2: 原始测试数据 ===\n\n")

n <- 100
age_vals <- c(sample(18:85, 95, replace = TRUE), 999, 150, -5, 200, 999)
age_vals <- age_vals[1:n]
bmi_vals <- c(rnorm(90, mean = 24, sd = 4), rep(999, 5), rep(NA, 5))
bmi_vals <- bmi_vals[1:n]
gender_vals <- sample(c("M", "F", "Male", "Female", "m", "f", "1", "0", NA), n, replace = TRUE)
systolic_bp_vals <- sample(c(paste0(sample(100:160, 80, replace = TRUE), " mmHg"),
                             as.character(sample(100:160, 15, replace = TRUE)), rep("unknown", 5)), n, replace = TRUE)
visit_date_vals <- sample(c(format(Sys.Date() - sample(1:365, 60, replace = TRUE), "%Y-%m-%d"),
                            format(Sys.Date() - sample(1:365, 30, replace = TRUE), "%Y/%m/%d"),
                            "1900-01-01", "2030-12-31", "N/A", "", rep(NA, 6)), n, replace = TRUE)
creatinine_vals <- c(round(rnorm(80, mean = 1.0, sd = 0.2), 2), round(rnorm(15, mean = 88, sd = 15), 2), rep(NA, 5))
creatinine_vals <- creatinine_vals[1:n]
hba1c_vals <- c(round(rnorm(85, mean = 6.5, sd = 1.2), 2), rep("<4.0", 3), rep(">14", 2), "6,5", "7,2", "5..21", "8..3", rep(NA, 6))
hba1c_vals <- hba1c_vals[1:n]

messy_data <- data.frame(
  patient_id = 1:n, age = age_vals, bmi = bmi_vals, gender = gender_vals,
  systolic_bp = systolic_bp_vals, visit_date = visit_date_vals,
  creatinine = creatinine_vals, hba1c = hba1c_vals, stringsAsFactors = FALSE
)

cat("传统算法 (adaptive_scoring=FALSE):\n")
result2_traditional <- get_valid_subset(messy_data, row_na_ratio = 0.2, col_na_ratio = 0.08,
                                        adaptive_scoring = FALSE, return_index = TRUE)
cat(sprintf("  保留 %d 行, %d 列 (删除 %d 行, %d 列)\n",
            length(result2_traditional$rows), length(result2_traditional$cols),
            nrow(messy_data) - length(result2_traditional$rows),
            ncol(messy_data) - length(result2_traditional$cols)))

cat("\n自适应算法 (adaptive_scoring=TRUE):\n")
result2_adaptive <- get_valid_subset(messy_data, row_na_ratio = 0.2, col_na_ratio = 0.08,
                                     adaptive_scoring = TRUE, return_index = TRUE)
cat(sprintf("  保留 %d 行, %d 列 (删除 %d 行, %d 列)\n",
            length(result2_adaptive$rows), length(result2_adaptive$cols),
            nrow(messy_data) - length(result2_adaptive$rows),
            ncol(messy_data) - length(result2_adaptive$cols)))

cat("\n=== 测试3: 验证row_priority在adaptive模式下起作用 ===\n\n")

cat("不同row_priority下的结果 (adaptive_scoring=TRUE):\n")
for (rp in c(0.001, 1, 1000)) {
  result <- get_valid_subset(df1, row_na_ratio = 0.3, col_na_ratio = 0.2,
                             row_priority = rp, adaptive_scoring = TRUE, return_index = TRUE)
  cat(sprintf("  row_priority = %6g: 保留 %d 行, %d 列\n", rp, length(result$rows), length(result$cols)))
}

cat("\n=== 总结 ===\n")
cat("自适应算法通过评估删除行列对其他维度的改善程度，\n")
cat("能够做出更智能的决策，而不只是基于是否超过阈值。\n")
