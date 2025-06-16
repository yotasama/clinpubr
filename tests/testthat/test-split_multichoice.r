set.seed(1)
test_that("split_multichoice handles basic splitting", {
  df <- data.frame(q1 = c("ab", "c da", "b a", NA), q2 = c("a b", "a c", "d", "ab"))
  result <- split_multichoice(df, quest_cols = c("q1", "q2"))
  expect_snapshot(result)
})

set.seed(1)

test_that("split_multichoice with remove_space=FALSE", {
  df <- data.frame(q1 = c("a b", "c d", "b a"))
  result <- split_multichoice(df, quest_cols = "q1", remove_space = FALSE)
  expect_snapshot(result)
})

set.seed(1)

test_that("split_multichoice with custom link character", {
  df <- data.frame(q1 = c("ab", "cd"))
  result <- split_multichoice(df, quest_cols = "q1", link = "-")
  expect_snapshot(result)
})

set.seed(1)

test_that("split_multichoice retains original columns when remove_cols=FALSE", {
  df <- data.frame(q1 = c("ab", "cd"))
  result <- split_multichoice(df, quest_cols = "q1", remove_cols = FALSE)
  expect_snapshot(colnames(result))
})

set.seed(1)

test_that("split_multichoice handles empty strings and NAs", {
  df <- data.frame(q1 = c(NA, "", "ab"))
  result <- split_multichoice(df, quest_cols = "q1")
  expect_snapshot(result)
})