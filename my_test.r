set.seed(1)
load_all()
library(knitr)
library(dtplyr)
var_types <- get_var_types(mtcars, strata = "vs") # Automatically infer variable types
baseline_table(mtcars, var_types = var_types, contDigits = 1, filename = "baseline.csv")

t1=read.csv("baseline.csv", check.names = FALSE)
kable(t1)


data(cancer, package = "survival")
df <- kidney
df$dead <- ifelse(df$time <= 100 & df$status == 0, NA, df$time <= 100)
df <- na.omit(df[, -c(1:3)])

model0 <- glm(dead ~ age + frail, family = binomial(), data = df)
df$base_pred <- predict(model0, type = "response")
model <- glm(dead ~ ., family = binomial(), data = df)
df$full_pred <- predict(model, type = "response")

x=classif_model_compare(df, target_var ="dead", model_names =c("base_pred", "full_pred"),return_results = T)
kable(x$metric_table)

set.seed(5)
dummy_importance <- runif(20,0.2,0.6)^5
names(dummy_importance) <- paste0("var", 1:20)
importance_plot(dummy_importance, top_n = 15, split_at = 10)

results <- regression_basic_results(
  cancer,
  x = "age", y = "status", time = "time", return_results = T,
  model_covs = list(Crude = c(), Model1 = c("ph.karno"))
)

library(tictoc)
n=1e8
df <- data.frame(subject = sample(c("a", "b"), n, replace = TRUE), value = runif(n))
df$unit <- NA
df$unit[df$subject == "a"] <- sample(c("mg/L", "g/l", "g/L"),
  sum(df$subject == "a"),
  replace = TRUE
)
df$value[df$subject == "a" & df$unit == "mg/L"] <-
  df$value[df$subject == "a" & df$unit == "mg/L"] * 1000
df$unit[df$subject == "b"] <- sample(c(NA, "g", "mg"), sum(df$subject == "b"), replace = TRUE)
df$value[df$subject == "b" & df$unit %in% "mg"] <-
  df$value[df$subject == "b" & df$unit %in% "mg"] * 1000
df$value[df$subject == "b" & is.na(df$unit)] <- df$value[df$subject == "b" & is.na(df$unit)] *
  sample(c(1, 1000), size = sum(df$subject == "b" & is.na(df$unit)), replace = TRUE)

df2=lazy_dt(df)
tic()
x=unit_view(
  df = df, subject_col = "subject", quantiles = NULL,
  value_col = "value", unit_col = "unit", save_table = FALSE
)
toc()
tic()
unit_table=unit_view(
  df = df2, subject_col = "subject",quantiles = NULL,
  value_col = "value", unit_col = "unit", save_table = FALSE
)
toc()

unit_table$label <- c("t", NA, 1e-3, NA, NA, "r") # labeling the units

tic()
df_standardized <- unit_standardize(
  df = df2, subject_col = "subject", value_col = "value",
  unit_col = "unit", change_rules = unit_table
)
df_standardized=as.data.frame(df_standardized)
toc()
