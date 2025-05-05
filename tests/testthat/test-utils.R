# 测试to_factor函数

test_that("to_factor正确处理不同输入类型", {
  # 测试数值转因子
  expect_equal(levels(to_factor(c(1,2,3,4,5,6), max_numerical_groups = 3)), 
               c("(1,3.67]", "(3.67,6.33]", "(6.33,9]"))
  
  # 测试字符向量
  expect_equal(to_factor(c("a","b","a")), factor(c("a","b","a")))
  
  # 测试NA处理
  expect_equal(to_factor(c(NA,1,NA), na_as_level = TRUE), 
               factor(c("NA","1","NA")))
})

# 测试wilcox_test_pval函数
test_that("wilcox检验正确处理不同情况", {
  # 正常情况
  expect_true(wilcox_test_pval(rnorm(10), rnorm(10)) > 0.05)
  
  # 错误处理
  expect_true(is.na(wilcox_test_pval(1, "a")))
})

# 测试remove_conflict函数
test_that("变量冲突处理", {
  expect_warning(remove_conflict(c("a","b"), c("b","c")), "b are removed")
  expect_null(remove_conflict(c("a","b"), c("a","b"), silent = TRUE))
})