load_all()
results <- regression_basic_results(
  cancer,
  x = "age", y = "status", time = "time", return_results = TRUE,
  model_covs = list(Crude = c(), Model1 = c("ph.karno"))
)
