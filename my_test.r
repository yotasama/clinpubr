data(cancer, package = "survival")
# coxph model with time assigned
regression_basic_results(na.omit(cancer),
  x = "age", y = "status", time = "time",
  model_covs = list(Crude = c(), Model1 = c("sex"), Model2 = c("ph.karno", "sex"))
)
model_covs = list(Crude = c(), Model1 = c("sex"), Model2 = c("ph.karno", "sex"))
dat=lung
x=sapply(model_covs, function(tmp_covs) { complete.cases(na.omit(cancer)[, tmp_covs]) })
y=rowSums(x)
any(!y %in% c(0,3))
df <- data.frame(q1 = c("a b", "c d a", "b a",NA), q2 = c("a b", "a c", "d","a"))
split_multichoice(df, quest_cols = c("q1", "q2"))
strsplit(df[, "q1"], " ")
ifelse(is.na(strsplit(df[, "q1"], " ")),NA,
  sapply(
    strsplit(df[, "q1"], " "),
    function(x)  "b" %in% x
  )
)
sapply(strsplit(df[, "q1"], " "), function(x) "a" %in% x)
