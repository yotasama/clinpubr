library(testthat)
library(withr)
library(survival)

test_that("get_var_types correctly classifies variables", {
  data(cancer, package = "survival")
  
  with_tempdir({
    res <- get_var_types(cancer, strata = "sex", save_qqplots = TRUE)
  
    expect_s3_class(res, "var_types")
    expect_true(dir.exists("qqplots"))
    expect_equal(length(list.files("qqplots")), 14)
    
    expect_snapshot(res)
  })
})

test_that("baseline_table generates correct output files", {
  data(cancer, package = "survival")
  var_types <- get_var_types(cancer, strata = "sex")
  
  with_tempdir({
    baseline_table(cancer, var_types = var_types, filename = "test_output.csv")
    
    expect_true(file.exists("test_output.csv"))
    expect_true(file.exists("test_output_missing.csv"))
    
    expect_snapshot(read.csv("test_output.csv"))
    expect_snapshot(read.csv("test_output_missing.csv"))
  })
})
