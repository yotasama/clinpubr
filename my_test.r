set.seed(1)
load_all()
library(knitr)
var_types <- get_var_types(mtcars, strata = "vs") # Automatically infer variable types
baseline_table(mtcars, var_types = var_types, contDigits = 1, filename = "baseline.csv")

t1=read.csv("baseline.csv", check.names = FALSE)
kable(t1)
data(cancer, package = "survival")
regression_fit(data = cancer, y = "status", predictor = "age", time = "time", covars = c("sex", "ph.ecog"))

data(cancer, package = "survival")

# Performing cox regression, which is inferred by `y` and `time`
rcs_plot(cancer, x = "age", y = "status", time = "time", covars = c("sex", "ph.karno"), save_plot = TRUE)

data(cancer, package = "survival")
cancer$dead <- cancer$status == 2
regression_forest(cancer, model_vars = c("age", "sex", "wt.loss"), y = "dead",
  as_univariate = FALSE, save_plot = TRUE
)

regression_forest(
  cancer,
  model_vars = list(
    Crude = c("age"),
    Model1 = c("age", "sex"),
    Model2 = c("age", "sex", "wt.loss")
  ),
  y = "dead",
  save_plot = TRUE
)

res <- regression_scan(cancer, y = "status", time = "time")
