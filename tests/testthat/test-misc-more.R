# 测试fill_with_last函数
test_that("NA填充功能正常", {
  expect_equal(fill_with_last(c(1, NA, 3)), c(1, 1, 3))
  expect_equal(fill_with_last(c(NA, "a", NA)), c(NA, "a", "a"))
})

# 测试first_mode函数
test_that("首模式识别准确", {
  expect_equal(first_mode(c(1,1,2,3)), 1)
  expect_equal(first_mode(c("a","a","b")), "a")
  expect_true(is.na(first_mode(rep(NA,5))))
})

# 测试str_match_replace函数
test_that("字符串匹配替换正确", {
  modified <- c("v1", "v2")
  original <- c("age", "sex")
  expect_equal(str_match_replace(c("v1", "v2.x"), modified, original),
               c("age", "sex.x"))
})