load_all()
dat <- data.frame(
  Q1 = c("A", "B", "C"),
  Q2.A = c(TRUE, TRUE, FALSE),
  Q2.B = c(TRUE, FALSE, TRUE),
  Q2.C = c(FALSE, TRUE, FALSE)
)
result <- answer_check(dat, c("A", "TFT"), multi_column = TRUE)