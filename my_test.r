load_all()
set.seed(1)
data(cancer, package = "survival")
res <- regression_scan(cancer, y = "status", time = "time")
dat=cancer
dat$ph.ecog=as.factor(dat$ph.ecog)
model_res <- regression_fit(
  data = dat, y = "status", time = "time", predictor = "ph.ecog",
  covs = NULL, rcs_knots = NULL, returned = "predictor_combined"
)
