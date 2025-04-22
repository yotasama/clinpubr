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
