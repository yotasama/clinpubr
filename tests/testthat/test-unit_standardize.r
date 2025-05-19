library(testthat)

# Test data setup
test_data <- data.frame(
  subject = c("a", "a", "a", "b", "b", "b", "c", "c", "c"),
  value = c(1, 2, 3, 10, 20, 30, 100, 200, 300),
  unit = c(NA, "mg", "mg", "mL", "dL", "dL", "mm", "cm", "cm"),
  stringsAsFactors = FALSE
)

# Test 1: Explicit target unit with NA conversion
test_that("NA unit converts to target with coefficient", {
  change_list <- list(
    list(subject = "a", target_unit = "mg", units2change = c(NA), coeffs = c(10))
  )
  result <- unit_standardize(test_data, "subject", "value", "unit", change_list)
  expect_equal(result$unit[result$subject == "a"], rep("mg", 3))
  expect_equal(result$value[result$subject == "a"], c(10, 2, 3))  # 1*10, 2*1, 3*1
})

# Test 2: Automatic target unit selection (most common)
test_that("Auto-selects most common unit", {
  change_list <- list(
    list(subject = "b")  # No target_unit specified
  )
  result <- unit_standardize(test_data, "subject", "value", "unit", change_list)
  expect_equal(result$unit[result$subject == "b"], rep("dL", 3))
  expect_equal(result$value[result$subject == "b"], c(10, 20, 30))  # No conversion needed
})

# Test 3: Multiple units with custom coefficients
test_that("Converts multiple units with coefficients", {
  change_list <- list(
    list(subject = "c", target_unit = "cm", units2change = c("mm"), coeffs = c(0.1))
  )
  result <- unit_standardize(test_data, "subject", "value", "unit", change_list)
  expect_equal(result$unit[result$subject == "c"], rep("cm", 3))
  expect_equal(result$value[result$subject == "c"], c(10, 200, 300))  # 100*0.1, 200*1, 300*1
})

# Test 4: Error when coeffs length mismatch
test_that("Errors on coeffs/units2change length mismatch", {
  change_list <- list(
    list(subject = "a", target_unit = "mg", units2change = c(NA, "g"), coeffs = c(10))
  )
  expect_error(unit_standardize(test_data, "subject", "value", "unit", change_list), "`coeffs` should have the same length as `units2change`!")
})