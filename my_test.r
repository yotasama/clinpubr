set.seed(1)
load_all()
library(knitr)
var_types <- get_var_types(mtcars, strata = "vs") # Automatically infer variable types
baseline_table(mtcars, var_types = var_types, contDigits = 1, filename = "baseline.csv")

t1=read.csv("baseline.csv", check.names = FALSE)
kable(t1)
data(cancer, package = "survival")
regression_fit(data = cancer, y = "status", predictor = "age", time = "time", covs = c("sex", "ph.ecog"))
