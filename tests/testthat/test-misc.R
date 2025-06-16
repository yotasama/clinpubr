# Test na2false
test_that("na2false replaces NAs with FALSE", {
  expect_equal(na2false(c(TRUE, FALSE, NA, TRUE, NA)), c(TRUE, FALSE, FALSE, TRUE, FALSE))
  expect_equal(na2false(c(1, 2, NA)), c(1, 2, 0)) # Coercion to numeric
  expect_equal(na2false(character(0)), character(0)) # Empty vector
})

# Test vec2code
test_that("vec2code generates correct code", {
  expect_equal(vec2code(c("a", "b", "c")), "c('a','b','c')")
  expect_equal(vec2code(character(0)), "c()") # Empty vector
})

# Test format_pval
test_that("format_pval formats p-values correctly", {
  p_vals <- c(0.001, 0.0001, 0.05, 0.1123456, NA)
  expect_equal(format_pval(p_vals), c("0.001", "<0.001", "0.050", "0.112", ""))
  expect_equal(format_pval(p_vals, text_ahead = "p"), c("p = 0.001", "p < 0.001", "p = 0.050", "p = 0.112", ""))
})

# Test first_mode
test_that("first_mode calculates the first mode", {
  expect_equal(first_mode(c(1, 1, 2, 2, 3, 3, 3, NA, NA, NA)), 3)
  expect_equal(first_mode(c(1, 2, 3)), 1) # All unique, returns first
  expect_equal(first_mode(NA), NA) # All NAs
  expect_null(first_mode(character(0), empty_return = NULL)) # Empty vector
})

# Test merge_ordered_vectors
test_that("merge_ordered_vectors maintains order", {
  vec_list <- list(c(1, 3, 4, 5, 7, 10), c(2, 5, 6, 7, 8), c(1, 7, 5, 10))
  expect_equal(merge_ordered_vectors(vec_list), c(1, 3, 4, 2, 5, 6, 7, 10, 8))
  expect_null(merge_ordered_vectors(list())) # Empty list
  expect_equal(merge_ordered_vectors(list(c("a"), c("a"))), "a") # Duplicates
})

# Test add_lists
test_that("add_lists combines element-wise", {
  l1 <- list(a = 1, b = 2)
  l2 <- list(a = 3, b = 4, c = 5)
  expect_equal(add_lists(l1, l2), list(a = 4, b = 6, c = 5))
  expect_equal(add_lists(list(), list()), list()) # Empty lists
  expect_equal(add_lists(list(x = 10), list()), list(x = 10)) # One empty
})

# Test replace_elements
test_that("replace_elements substitutes values", {
  expect_equal(replace_elements(c("a", "x", "1", NA, "a"), c("a", "b", NA), c("A", "B", "XX")), c("A", "x", "1", "XX", "A"))
  expect_error(replace_elements(c(1, 2), c(1), c(2, 3)), "`from` and `to` should have the same length!") # Mismatched lengths
  expect_equal(replace_elements(NA, NA, "X"), "X") # NA replacement
})

# Test fill_with_last
test_that("fill_with_last replaces NAs with last valid", {
  expect_equal(fill_with_last(c(1, 2, NA, 4, NA, 6)), c(1, 2, 2, 4, 4, 6))
  expect_equal(fill_with_last(c(NA, NA, 5)), c(NA, NA, 5)) # Leading NAs
  expect_equal(fill_with_last(5), 5) # Single element
})

# Test str_match_replace
test_that("str_match_replace partially matches strings", {
  expect_equal(str_match_replace(c("v1.v2", "v3.yy", "v4"), c("v1", "v2", "v3"), c("A", "B", "C")), c("A.B", "C.yy", "v4"))
  expect_equal(str_match_replace("test", c("x"), c("y")), "test") # No match
  expect_equal(str_match_replace("aab", c("aa", "a"), c("A", "B")), "Ab") # Longest match first
})

# Test unmake_names
test_that("unmake_names reverses make.names", {
  ori <- c("xx (mg/dl)", "b*x", "Covid-19")
  made <- make.names(ori)
  expect_equal(unmake_names(made, ori), ori)
  expect_equal(unmake_names(c(made, "aa"), ori), c(ori, NA)) # Non-matching
})

# Test formula_add_covs
test_that("formula_add_covs adds covariates", {
  expect_equal(deparse(formula_add_covs(y ~ a + b, c("c", "d"))), "y ~ a + b + c + d")
  expect_equal(deparse(formula_add_covs("y ~ a", c("b"))), "y ~ a + b")
  expect_equal(formula_add_covs(y ~ x, NULL), y ~ x) # NULL covars
})

# Test qq_show (using vdiffr for image comparison)
test_that("qq_show generates correct plot", {
  set.seed(1)
  withr::with_tempdir({
    p <- qq_show(rnorm(100), title = "Test QQ", save = TRUE, filename = "test_plot.png")
    expect_true(file.exists("test_plot.png")) # Check file saved
    vdiffr::expect_doppelganger("QQ plot", p)
  })
})
