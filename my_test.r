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
model1 <- glm(dead ~ ., family = binomial(), data = df)
df$full_pred <- predict(model1, type = "response")

# Generating most of the useful plots and metrics for model comparison
results <- classif_model_compare(df, "dead", c("base_pred", "full_pred"), save_output = FALSE)