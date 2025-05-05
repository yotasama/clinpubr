# na2false函数测试
test_that("na2false converts NA to FALSE properly", {
  expect_equal(na2false(c(TRUE, NA, FALSE)), c(TRUE, FALSE, FALSE))
  expect_equal(na2false(c(NA, 1, NA)), c(FALSE, 1, FALSE))
})

# format_pval函数测试
test_that("p-value formatting works", {
  expect_match(format_pval(0.0001), "<0.001")
  expect_match(format_pval(0.05), "0.05")
  expect_match(format_pval(0.0012, digits=2), "0.0012")
})

# replace_elements函数测试
test_that("element replacement works", {
  original <- c("a", "x", NA)
  expect_equal(replace_elements(original, c("a", NA), c("A", "N/A")), 
               c("A", "x", "N/A"))
  expect_error(replace_elements(1:3, 1:2, 1)) # 长度校验
})

# merge_ordered_vectors函数测试
test_that("vector merging maintains order", {
  vec_list <- list(c(1,3,5), c(5,7,1))
  expect_equal(merge_ordered_vectors(vec_list), c(1,5,3,7))
})

# add_lists函数测试
test_that("list addition works element-wise", {
  l1 <- list(a=1, b=2)
  l2 <- list(a=3, c=4)
  expect_equal(add_lists(l1, l2), list(a=4, b=2, c=4))
})